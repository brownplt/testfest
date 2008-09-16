module CS173.Actions where

import System.Log.Logger
import Database.CouchDB
import Text.JSON
import CS173.Data
import System.Time
import Control.Monad
import Control.Monad.Trans

-- |Tests for the machine checker
pendingTestsView = ViewMap "pending"
  "function (o) { if (o.status.stat == 'Submitted') emit(o._id,o.asgnid); }"

-- |Tests approved by the machine and the TAs, available for solutions
availableTestsView = ViewMap "availableTests"
  "function (o) { if (o.status.stat == 'TACheckOK') \
\                   { emit(o.asgnid, o.body); } }"

-- |Tests waiting for approval by the TAs
submittedTestsView = ViewMap "submittedTests"
  "function(o) { if (o.status.stat == 'MachineCheckOK') \
\                  { emit(o.asgnid,o); } }"


-- |Test suites by user
testsByUserView = ViewMap "byuser"
  "function (o) { emit([o.userid,o.asgnid],o); }"

enabledTestsByUser = ViewMap "enabledbyuser"
  "function (o) { if (o.enabled === true) { \
\                   emit([o.asgnid,o.userid],null);    \
\                 } \
\               }"


-- |Number of available test suites, keyed by assignment
numTestsByAsgn = ViewMapReduce "numtests"
  " function(doc) { \
  \   if (doc.status.stat == 'TACheckOK') { emit(doc.asgnid,null); } \
  \ }"
  "function(values) { return values.length; }"

pendingProgView = ViewMap "pending"
  "function (o) { if (o.status == false) emit(o._id,null); }"

progsByUserView = ViewMap "byuser"
  "function (o) { emit([o.userid,o.asgnid],{ 'id': o._id, status: o.status, time: o.time }) }"

initDatabase = do
  liftIO $ putStrLn "Initializing databases ..."
  dropDB "users"
  createDB "users"
  dropDB "suites"
  createDB "suites"
  dropDB "assignment"
  createDB "assignment"
  dropDB "submissions"
  createDB "submissions"
  dropDB "programs"
  createDB "programs"
  dropDB "reports"
  createDB "reports"
  liftIO $ putStrLn "Creating views ..."
  newView "suites" "byasgn" [ numTestsByAsgn ]
  newView "suites" "suites" [ pendingTestsView, testsByUserView,
                              availableTestsView, submittedTestsView,
                              enabledTestsByUser  ]
  newView "programs" "programs" [ pendingProgView, progsByUserView ]

data LoginResult = LoginFailed | LoginAdmin | LoginUser

dbAsgn = db "assignment"
dbUsers = db "users"
dbSuites = db "suites"
dbSubmissions  = db "submissions"
dbPrograms = db "programs"
dbRep = db "reports"

login :: Doc -> String -> CouchMonad LoginResult
login username pw = do
  result <- getDoc dbUsers username
  case result of
    Nothing -> return LoginFailed -- no account
    Just (_,_,User _ False _ _) -> return LoginFailed -- account disabled
    Just (_,_,User _ _ pw' _) | pw /= pw' -> return LoginFailed -- wrong pass
    Just (_,_,User _ _ _ False) -> return LoginUser
    Just (_,_,User _ _ _ True) -> return LoginAdmin


newLogin :: Doc 
         -> String
         -> CouchMonad Bool
newLogin user pass = do
  r <- newNamedDoc dbUsers user (User user True pass False)  -- not admin
  case r of
    Left _ -> return False
    Right _ -> return True

getSubmission :: Doc -> CouchMonad (Maybe String)
getSubmission id = do
  r <- getDoc dbSubmissions id
  case r of
    Just (_,_,(Submission body)) -> return (Just body)
    Nothing -> return Nothing

newSubmission :: String -> CouchMonad Doc
newSubmission body = do
  (id,_) <- newDoc dbSubmissions (Submission body)
  return id

newTest :: TestSuite
        -> CouchMonad Doc
newTest test = do
  asgn <- getAssignment (testSuiteAssignmentId test)
  unless (assignmentEnabled asgn) $
    fail $ "Attempt by " ++ show (testSuiteUserId test) ++ 
           " to submit a test for " ++
           show (assignmentId asgn) ++ ", which is not currently enabled"
  (id,_) <- newDoc dbSuites test
  return id

setTestSuiteStatus testId val = do
  r <- updateTestSuiteStatus (const val) testId
  case r of
    Just _ -> return True
    Nothing -> return False

updateTestSuiteStatus :: (TestSuiteStatus -> TestSuiteStatus)
                      -> Doc
                      -> CouchMonad (Maybe Rev)
updateTestSuiteStatus f testId = 
  getAndUpdateDoc dbSuites testId
    (\testSuite -> testSuite { tsStatus = f (tsStatus testSuite) })


disableExistingTests :: Doc -- ^assignment id
                     -> Doc -- userid
                     -> CouchMonad ()
disableExistingTests asgnId userId = do
  enabledTests <- queryViewKeys dbSuites (doc "suites") (doc "enabledbyuser")
                    [("key",JSArray [ showJSON asgnId, showJSON userId])]
  mapM_ (updateTestSuiteStatus (const TestSuiteSuperseded)) enabledTests

newProgram :: Program -> CouchMonad Doc
newProgram prog = do
  (id,_) <- newDoc dbPrograms prog
  return id

getAssignment :: Doc -> CouchMonad Assignment
getAssignment id = do
  result <- getDoc dbAsgn id
  case result of
    Just (_,_,val) -> return val
    Nothing -> error $ "assignment not found: " ++ show id

addAssignment :: Assignment
              -> CouchMonad Bool -- ^'False' if it exists
addAssignment assignment = do
  result <- newNamedDoc dbAsgn (assignmentId assignment) assignment
  case result of
    Left err -> return False
    Right ok -> return True

updateAssignment :: String -- ^ assignment id
                 -> (Assignment -> Assignment)
                 -> CouchMonad Bool -- ^ 'False' on update conflict
updateAssignment id f = do
  result <- getDoc dbAsgn (doc id)
  case result of
    Nothing -> error $ "assignment does not exist: " ++ id
    Just (id,rev,val) -> do
      result <- updateDoc dbAsgn (id,rev) (f val)
      case result of
        Nothing -> return False
        Just _ -> return True

enableAssignment id isEnabled = 
  updateAssignment id $ \a ->  a { assignmentEnabled = isEnabled }

-- |Returns id, desc
getAllAssignments :: CouchMonad [Assignment]
getAllAssignments = do
  allIds <- getAllDocIds dbAsgn
  mapM getAssignment allIds


getTime :: (Monad m, MonadIO m)
        => m Integer
getTime = do
  (TOD now _) <- liftIO getClockTime
  return now

activeAssignments :: CouchMonad [Assignment]
activeAssignments = do
  allIds <- getAllDocIds dbAsgn
  allAssignments <- mapM getAssignment allIds
  (TOD now _) <- liftIO $ getClockTime
  return (filter (isActiveAssignment now) allAssignments)

updateProgramStatus :: Doc -- ^program id
                    -> TestStatus
                    -> CouchMonad ()
updateProgramStatus progId status = do
  r <- getDoc dbPrograms progId
  case r of
    Nothing -> do
      liftIO $ errorM "tourney.actions" 
        ("Cannot find program with id " ++ show progId)
      fail "updating program status: cannot find the program"
    Just (id,rev,val) -> do
      r <- updateDoc dbPrograms (id,rev) (val { programStatus = status })
      return ()

getProgram :: Doc -> CouchMonad Program
getProgram progId = do
  r <- getDoc dbPrograms progId
  case r of
    Just (_,_,val) -> return val
    Nothing -> fail $ "non-existant program: " ++ show progId

getTestSuite :: Doc -- ^test suite id
             -> CouchMonad TestSuite
getTestSuite testId = do
  r <- getDoc dbSuites testId
  case r of
    Just (_,_,val) -> return val
    Nothing -> fail "non-existant test suite"

getUserProgs :: Doc -- ^username
             -> Doc -- ^assignment id
             -> Integer -- ^since
             -> CouchMonad [ProgramInfo]
getUserProgs userId asgnId since = do
  r <- queryView dbPrograms (doc "programs") (doc "byuser")
         [("key", JSArray [showJSON userId, showJSON asgnId])]
  return (map snd r)

getUserTestSuites :: String -- ^username
                  -> Doc -- ^assignment id
                  -> CouchMonad [TestSuite]
getUserTestSuites userId asgnId = do
  r <- queryView dbSuites (doc "suites") (doc "byuser")
       [ ("key",JSArray [showJSON userId,showJSON asgnId]) ]
  return (map snd r)
  
getAvailableTests :: Doc -- ^assignment id
                  -- | test id and submission id list
                  -> CouchMonad [(Doc,Doc)]
getAvailableTests asgnId = do
 r <- queryView dbSuites (doc "suites") (doc "availableTests")
        [("key",showJSON asgnId)]
 return $ map (\(id,subid) -> (id,subid)) r

getTestsForApproval :: Doc -- ^assignment id
                    -> CouchMonad [(Doc,TestSuite)]
getTestsForApproval asgnId = do
  r <- queryView dbSuites (doc "suites") (doc "submittedTests" )
         [("key", showJSON asgnId)]
  return $ map (\(id,val) -> (id,val)) r

