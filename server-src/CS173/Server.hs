module CS173.Server where

import Mail
import Control.Monad
import System.Log.Logger
import Network.WebServer
import Text.JSON
import Database.CouchDB
import Database.CouchDB.Safety
import CS173.Data
import CS173.Messages
import CS173.Tourney
import CS173.Config
import Sessions
import qualified CS173.Actions as Action
import CS173.Actions (LoginResult (..))

import Control.Exception
import Control.Monad.Trans (MonadIO,liftIO,lift)
import Control.Concurrent
import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified Data.ByteString as ByteString

couchIO = liftIO.runCouchDB'


singleton x = [x]

userService :: (Monad m, MonadIO m, FromData a, SessionInfo m) 
     => String
     -> (String -> a -> WebT m Response)
     -> ServerPartT m Response
userService path f = dir path 
  [ methodSP POST $  withData $ \val -> 
     [ requireSession $ \userId ->  do
         liftIO $ infoM "tourney.user" (userId ++ " requested " ++ path)
         r <- f userId val 
         return (setHeader "Cache-Control" "no-store, must-revalidate" r)
     ]
  ]

adminService :: (Monad m, MonadIO m, FromData a, SessionInfo m) 
             => String
             -> (String -> a -> WebT m Response)
             -> ServerPartT m Response
adminService path f = dir path 
  [ methodSP POST $  withData $ \val -> 
      [ requireAdminSession $ \userId -> do
          liftIO $ infoM "tourney.admin" (userId ++ " requested " ++ path)
          r <- f userId val 
          return (setHeader "Cache-Control" "no-store, must-revalidate" r)
      ]
  ]

loginService = withData $ \(Login username password) -> 
  [ method POST $ do
      sessionLength <- lift $ getSessionLength
      r <- couchIO $ Action.login username password
      case r of
        LoginFailed -> do
          jsonResponse False "invalid username or password"
        LoginUser -> do 
          sid <- setSession username
          jsonResponse True sid
        LoginAdmin -> do 
          sid <- setAdminSession username 
          jsonResponse True sid
  ]



newUserService = withData $ \(Login username password) ->
  [ method POST $ do
      r <- couchIO $ Action.newLogin username password
      case r of
        True -> jsonResponse True ""
        False -> jsonResponse False "account exists or is disabled"
  ]

assignments userId () = do
  r <- couchIO $ Action.activeAssignments
  jsonResponse True r

newTest userId (NewTest body asgnId) = do
  couchIO $ do
    subId <- Action.newSubmission body 
    now <- Action.getTime
    let testSuite = TestSuite userId asgnId subId TestSuiteSubmitted now
    Action.newTest testSuite
  jsonResponse True "test received; processing"


newProgram userId (NewProgram asgnId body) = do
  r <- couchIO $ do
    asgn <- Action.getAssignment asgnId
    case assignmentEnabled asgn of
      True -> do
        submissionId <- Action.newSubmission body
        now <- Action.getTime
        let prog = Program userId asgnId now TestPending submissionId
        programId <- Action.newProgram prog
        return (True,programId)
      False -> return (False,"assignment not enabled")
  jsonResponse (fst r) (snd r)

testSuites userId (ForAssignment asgnId) = do
    r <- couchIO $ Action.getUserTestSuites userId asgnId
    jsonResponse True r

programs userId (ForAssignment asgnId) = do
    now <- Action.getTime
    r <- couchIO $ Action.getUserProgs userId asgnId now
    jsonResponse True r

pendingApproval userId (ForAssignment asgnId) = do
    r <- couchIO $ Action.getTestsForApproval asgnId
    jsonResponse True r

setTestStatus adminId 
              (ToggleStatus testId userId asgnId status singleSuite) = do
    let disable existingTestId = do
          getAndUpdateDoc (db "suites")  (doc existingTestId) $ \ts -> case tsStatus ts of
            TestSuiteTACheckOK -> ts { tsStatus = TestSuiteSuperseded }
            otherwise -> ts
    r <- couchIO $ do
           when singleSuite $ do
             existingTests <- queryViewKeys (db "suites") (doc "suites")
                                (doc "byuser")
                                [("key", showJSON [ userId, asgnId ])]
             mapM_ disable existingTests
           Action.setTestSuiteStatus testId status
    jsonResponse r ""

getSubmissionBody userId (ForId id) = do
    r <- couchIO $ Action.getSubmission id
    case r of
      Just body -> jsonResponse True body
      Nothing -> jsonResponse False "submission does not exist"

changePass userId (ChangePassword currentPass newPass) = do
    (isSucess,value) <- couchIO $ do
      r <- getDoc (db "users") (doc userId)
      case r of
        Nothing -> do
          liftIO $ criticalM "tourney.admin" 
            ("Could not find user document for " ++ userId 
             ++ " in an authenticated session")
          return (False,"user does not exist") -- wtf?
        Just (id,rev,user) | currentPass /= userPassword user -> do
          return (False,"incorrect password")
        Just (id,rev,user) -> do
          r <- updateDoc (db "users") (id,rev) (user { userPassword = newPass })
          case r of
            Just _ -> return (True,"password successfully changed")
            Nothing -> do
              liftIO $ infoM "tourney.admin"
                ("Update conflict changing the password for " ++ userId)
              -- Well, not really, but are we really going to report "update
              -- conflict" to the user?
              return (False,"incorrect password")
    jsonResponse isSucess value  

reportAPICallError =  withRequest $ \request -> do
  let path =  concat $ L.intersperse "/" (rqPaths request)
  let method = show $ rqMethod request
  let cookies = concat $ L.intersperse ", " (map fst $ rqCookies request)
  let inputs = concat $ L.intersperse ", " (map fst $ rqCookies request)
  let rs = method ++ " " ++ path ++ " (Cookies: " ++ cookies 
           ++ "; Inputs: " ++ inputs ++ ")"
  liftIO $ warningM "cs173.api" ("Invalid /api request: " ++ rs)
  jsonResponse False  "not logged in or invalid request"

getGold userId (ForId id) = do
    (isSuccess,value) <- couchIO $ do
      r <- getDoc (db "assignment") (doc id)
      case r of
        Nothing -> do
          liftIO $ errorM "tourney.admin" 
            ("attempt by " ++ userId ++ " to get the solution for " ++ id ++
             ", which is a non-existant assignment")
          return (False,"that assignment does not exist")
        Just (_,_,asgn) -> do
          r <- getDoc (db "submissions") (doc $ assignmentSolutionId asgn)
          case r of
            Nothing -> do
              liftIO $ errorM "tourney.admin"
                ("the gold solution for " ++ id ++ " does not exist " ++
                 "(detected while " ++ userId ++ " tried to see it)")
              return (False,"no solution exists")
            Just (_,_,Submission sol) -> return (True,sol)
    jsonResponse isSuccess value

updateGold userId (UpdateGold asgnId sol) = do
    liftIO $ criticalM "tourney.admin" 
      (userId ++ " is changing the solution for " ++ asgnId ++ ".  I wonder "
       ++ " how many students just got screwed?")
    (isSuccess,value) <- couchIO $ do
      r <- getDoc (db "assignment") (doc asgnId)
      case r of
        Nothing -> do
          liftIO $ errorM "tourney.admin" 
            ("attempt by " ++ userId ++ " to get the solution for " ++ 
             asgnId ++ ", which is a non-existant assignment")
          return (False,"that assignment does not exist")
        Just (_,_,asgn) -> do
          r <- getAndUpdateDoc (db "submissions") 
                               (doc $ assignmentSolutionId asgn)
                 (const $ Submission sol)
          case r of
            Nothing -> do
              liftIO $ errorM "tourney.admin"
                ("the gold solution for " ++ asgnId ++ " does not exist " ++
                 "(detected while " ++ userId ++ " tried to change it)")
              return (False,"no solution exists")
            Just _ -> return (True,"solution updated")
    jsonResponse isSuccess value

allAsgns userId () = do
  let getAsgn id = do
         -- not a security risk and avoid ambiguous (JSON a)
        r <- getDocPrim (db "assignment") (doc id)
        case r of
          Nothing -> do
            liftIO $ errorM " tourney.admin"
              ("assignment " ++ id ++ " not found (servicing /allasgns)")
            return Nothing
          Just (_,_,asgn) -> return (Just $ JSObject $ toJSObject asgn)
  value <- couchIO $ do
    ids <- getAllDocIds (db "assignment")
    mapM getAsgn ids
  jsonResponse True (JSArray $ catMaybes value)

numTestSuites userId (ForId asgnId) = do
  r <- couchIO $ queryView (db "suites") (doc "byasgn") (doc "numtests")
                           [("key",showJSON asgnId)]
  case r of
    [(_,val::Int)] -> jsonResponse True val
    otherwise -> jsonResponse True (0::Int) -- no tests, no view!
  

newAsgn userId (NewAsgn goldSoln asgn) = do
    (isSuccess,value) <- couchIO $ do
      (solnId,_) <- newDoc (db "submissions") (Submission goldSoln)
      r <- newNamedDoc (db "assignment")
                       (doc $ assignmentId asgn) 
                       (asgn { assignmentSolutionId = fromJSString solnId })
      case r of
        Left _ -> 
          return (False,"an assignment with that short name already exists")
        Right _ ->
          return (True,"assignment created")
    jsonResponse isSuccess value

isAsgnEnabled userId (ForId asgnId) = do
    (isSuccess,value) <- couchIO $ do
      r <- getDoc (db "assignment") (doc asgnId)
      case r of
        Nothing -> do
          liftIO $ errorM "tourney.admin" (asgnId ++ " assignment does " ++
                     " not exist (checking if enabled)")
          return (False,False)
        Just (_,_,asgn) -> do
          return (True,assignmentEnabled asgn)
    jsonResponse isSuccess value

setAsgnEnabled userId (EnableAsgn asgnId enable) = do
    isSuccess <- couchIO $ do
      r <- getAndUpdateDoc (db "assignment") (doc asgnId)
             (\asgn -> asgn { assignmentEnabled = enable })
      case r of
        Nothing -> do
          liftIO $ errorM "tourney.admin"
            ("could not enable/disable the assignment " ++ asgnId)
          return False
        Just _ -> return True
    jsonResponse isSuccess False

currentTests userId (ForId asgnId) = do
  r <- couchIO $ do
    testIds <- queryViewKeys (db "suites") (doc "suites")
                 (doc "availableTests") [("key",JSString $ toJSString asgnId)]
    tests <- mapM Action.getTestSuite testIds
    return (True,zip testIds tests)
  jsonResponse (fst r) (snd r) 
    

forgotPassword :: ServerPartT ServerM Response
forgotPassword = withData $ \(ForId userId) -> singleton $ method POST $ do
  r <- couchIO $ getDoc (db "users") (doc userId)
  case r of
    -- don't leak existence of the account
    Nothing -> jsonResponse True ""
    Just (_,_,user) -> do
      liftIO $ mailFrom "cs173tas@cs.brown.edu" [userId]
                 "[testfest] password reminder"
                 ("Your password is: " ++ userPassword user)
      jsonResponse True ""
    

maintainThread n threadM = do
  mvar <- newEmptyMVar
  forkOS (threadM `finally` putMVar mvar ())
  forkOS $ do takeMVar mvar
              criticalM "tourney.tester" "Tester thread died!"
              threadDelay (60 * 1000000)
              maintainThread (n+1) threadM
  return ()

server p config = do
  maintainThread 0 (runConfig config (testSuiteTesterThread config))
  simpleHTTP nullConf { port=p } $ runServerParts config
    [ dir "api" [ dir "ping" [ anyRequest $ ok (toResponse "pong") ]
                , dir "login" [ loginService]
                , dir "logout" [ logoutSession ]
                -- , dir "newuser" [ newUserService ]
                , dir "forgotpassword" [ forgotPassword ]
                , userService "changepass" changePass
                , userService "assignments" assignments
                , adminService "allasgns" allAsgns 
                , userService "newtest" newTest
                , userService "newprog" newProgram
                , userService "testsuites" testSuites 
                , userService "programs" programs
                , userService "numtests" numTestSuites
                , dir "isadmin" [ isAdminSession ]
                , adminService "pendingapproval" pendingApproval
                , adminService "setteststatus" setTestStatus
                , adminService "code"  getSubmissionBody 
                , adminService "newasgn"  newAsgn 
                , adminService "updategold"  updateGold 
                , adminService "getgold"  getGold 
                , adminService "isasgnenabled"  isAsgnEnabled 
                , adminService "setasgnenabled"  setAsgnEnabled 
                , adminService "currenttests" currentTests
                , reportAPICallError ]
    , fileServe ["index.html"] (configStaticRoot config)
    ]
