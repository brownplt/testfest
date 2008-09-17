module CS173.Server where

import Mail
import Control.Monad
import System.Log.Logger
import Network.WebServer
import Text.JSON
import Database.CouchDB
import CS173.Data
import CS173.Tourney
import CS173.Config
import Network.WebServer.Sessions
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

jsonHeader response = setHeader "Content-Type" "application/json" response

jsonResponse :: (JSON a, Monad m)
             => Bool -> a -> ServerT m Response
jsonResponse isSuccess val = do
  let body = encode $ toJSObject [ ("success", JSBool isSuccess)
                                 , ("value", showJSON val) ]
  return $ ok $ jsonHeader $ toResponse body


userService :: (Monad m, MonadIO m, SessionInfo m)
            => String -> (String -> ServerT m Response) 
            -> ServerT m Response
userService path f = dir path $ do
  method POST
  userId <- requireSession
  liftIO $ infoM "tourney.user" (userId ++ " requested " ++ path)
  r <- f userId
  return (setHeader "Cache-Control" "no-store, must-revalidate" r)

adminService :: (Monad m, MonadIO m, SessionInfo m)
             => String -> (String -> ServerT m Response) 
             -> ServerT m Response
adminService path f = dir path $ do
  method POST
  userId <- requireAdminSession
  liftIO $ infoM "tourney.admin" (userId ++ " requested " ++ path)
  r <- f userId
  return (setHeader "Cache-Control" "no-store, must-revalidate" r)

loginService = do
  username <- jsonInput "username" -- TODO: why the different formats?
  password <- stringInput "password"
  method POST
  sessionLength <- lift $ getSessionLength
  r <- couchIO $ Action.login username password
  case r of
    LoginFailed -> do
      jsonResponse False "invalid username or password"
    LoginUser -> do 
      f <- setSession (show username)
      r <- jsonResponse True ""
      return (f r)
    LoginAdmin -> do 
      f <- setAdminSession (show username)
      r <- jsonResponse True ""
      return (f r)


{-
newUserService = withData $ \(Login username password) ->
  [ method POST $ do
      r <- couchIO $ Action.newLogin username password
      case r of
        True -> jsonResponse True ""
        False -> jsonResponse False "account exists or is disabled"
  ]
-}

assignments userId = do
  r <- couchIO $ Action.activeAssignments
  jsonResponse True r

newTest userId = do
  body <- stringInput "test"
  asgnId <- jsonInput "assignment"
  couchIO $ do
    subId <- Action.newSubmission body 
    now <- Action.getTime
    let testSuite = TestSuite (doc userId) asgnId subId TestSuiteSubmitted now
    Action.newTest testSuite
  jsonResponse True "test received; processing"


newProgram userId = do
  asgnId <- jsonInput "asgnid"
  body <- stringInput "prog"
  r <- couchIO $ do
    asgn <- Action.getAssignment asgnId
    case assignmentEnabled asgn of
      True -> do
        submissionId <- Action.newSubmission body
        now <- Action.getTime
        let prog = Program (doc userId) asgnId now TestPending submissionId
        programId <- Action.newProgram prog
        return (True,show programId)
      False -> return (False,"assignment not enabled")
  jsonResponse (fst r) (snd r)

testSuites userId = do
    asgnId <- jsonInput "asgnid"
    r <- couchIO $ Action.getUserTestSuites userId asgnId
    jsonResponse True r

programs userId = do 
    asgnId <- jsonInput "asgnid"
    now <- Action.getTime
    r <- couchIO $ Action.getUserProgs (doc userId) asgnId now
    jsonResponse True r

pendingApproval userId  = do
    asgnId <- jsonInput "asgnid"
    r <- couchIO $ Action.getTestsForApproval asgnId
    jsonResponse True r

setTestStatus adminId  = do
  testId <- stringInput "id"
  status <- jsonInput "status"
  userId <- stringInput "userid"
  asgnId <- stringInput "asgnid"
  singleSuite <- jsonInput "singlesuite"
  let disable existingTestId = do
        getAndUpdateDoc (db "suites") existingTestId $ \ts -> 
          case tsStatus ts of
            TestSuiteTACheckOK -> ts { tsStatus = TestSuiteSuperseded }
            otherwise -> ts
  r <- couchIO $ do
         when singleSuite $ do
           existingTests <- queryViewKeys (db "suites") (doc "suites")
                              (doc "byuser")
                              [("key", showJSON [ userId, asgnId ])]
           mapM_ disable existingTests
         Action.setTestSuiteStatus (doc testId) status
  jsonResponse r ""

getSubmissionBody userId  = do
    id <- jsonInput "id"
    r <- couchIO $ Action.getSubmission id
    case r of
      Just body -> jsonResponse True body
      Nothing -> jsonResponse False "submission does not exist"

changePass userId = do 
  currentPass <- stringInput "oldpass"
  newPass <- stringInput "newpass"
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

reportAPICallError =  do
  request <- getRequest
  let path =  concat $ L.intersperse "/" (rqPaths request)
  let method = show $ rqMethod request
  let cookies = concat $ L.intersperse ", " (map fst $ rqCookies request)
  let inputs = concat $ L.intersperse ", " (map fst $ rqCookies request)
  let rs = method ++ " " ++ path ++ " (Cookies: " ++ cookies 
           ++ "; Inputs: " ++ inputs ++ ")"
  liftIO $ warningM "cs173.api" ("Invalid /api request: " ++ rs)
  jsonResponse False  "not logged in or invalid request"

getGold userId  = do
    id <- jsonInput "id"
    (isSuccess,value) <- couchIO $ do
      r <- getDoc (db "assignment") (doc id)
      case r of
        Nothing -> do
          liftIO $ errorM "tourney.admin" 
            ("attempt by " ++ userId ++ " to get the solution for " ++ id ++
             ", which is a non-existant assignment")
          return (False,"that assignment does not exist")
        Just (_,_,asgn) -> do
          r <- getDoc (db "submissions") (assignmentSolutionId asgn)
          case r of
            Nothing -> do
              liftIO $ errorM "tourney.admin"
                ("the gold solution for " ++ id ++ " does not exist " ++
                 "(detected while " ++ userId ++ " tried to see it)")
              return (False,"no solution exists")
            Just (_,_,Submission sol) -> return (True,sol)
    jsonResponse isSuccess value

updateGold userId = do
  sol <- stringInput "gold"
  asgnId <- stringInput "asgnid"
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
                             (assignmentSolutionId asgn)
               (const $ Submission sol)
        case r of
          Nothing -> do
            liftIO $ errorM "tourney.admin"
              ("the gold solution for " ++ asgnId ++ " does not exist " ++
               "(detected while " ++ userId ++ " tried to change it)")
            return (False,"no solution exists")
          Just _ -> return (True,"solution updated")
  jsonResponse isSuccess value

allAsgns userId = do
  let getAsgn id = do
         -- not a security risk and avoid ambiguous (JSON a)
        r <- getDocPrim (db "assignment") id
        case r of
          Nothing -> do
            liftIO $ errorM " tourney.admin"
              ("assignment " ++ show id ++ " not found (servicing /allasgns)")
            return Nothing
          Just (_,_,asgn) -> return (Just $ JSObject $ toJSObject asgn)
  value <- couchIO $ do
    ids <- getAllDocIds (db "assignment")
    mapM getAsgn ids
  jsonResponse True (JSArray $ catMaybes value)

numTestSuites userId  = do
  asgnId::Int <- jsonInput "id"
  r <- couchIO $ queryView (db "suites") (doc "byasgn") (doc "numtests")
                           [("key",showJSON asgnId)]
  case r of
    [(_,val::Int)] -> jsonResponse True val
    otherwise -> jsonResponse True (0::Int) -- no tests, no view!
  

newAsgn userId = do
  goldSoln <- stringInput "gold"
  asgn <- jsonInput "asgn"
  (isSuccess,value) <- couchIO $ do
    (solnId,_) <- newDoc (db "submissions") (Submission goldSoln)
    r <- newNamedDoc (db "assignment")
                     (assignmentId asgn) 
                     (asgn { assignmentSolutionId = solnId })
    case r of
      Left _ -> 
        return (False,"an assignment with that short name already exists")
      Right _ ->
        return (True,"assignment created")
  jsonResponse isSuccess value

isAsgnEnabled userId  = do
    asgnId <- jsonInput "id"
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

jsonInput :: (Monad m, JSON a) => String -> ServerT m a
jsonInput fieldName = do
  raw <- stringInput fieldName
  case decode raw of
    Text.JSON.Ok a -> return a
    otherwise -> fail $ "could not parse " ++ fieldName ++ " as a JSON string"

setAsgnEnabled userId = do
  asgnId <- stringInput "id"
  enable <- jsonInput "enable"
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

currentTests userId  = do
  asgnId <- jsonInput "id"
  r <- couchIO $ do
    testIds <- queryViewKeys (db "suites") (doc "suites")
                 (doc "availableTests") [("key",JSString $ toJSString asgnId)]
    tests <- mapM Action.getTestSuite testIds
    return (True,zip testIds tests)
  jsonResponse (fst r) (snd r) 
    

forgotPassword = do
  userId <- jsonInput "id"
  method POST
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
  runServer p $ run173Server config (dir "api" api `mplus` files config)

api = anyOf
  [ dir "ping" $ (return $ ok $ toResponse "pong")
  , dir "login" loginService
  , dir "logout" $ return (logoutSession "")
  -- , dir "newuser"  newUserService 
  , dir "forgotpassword"  forgotPassword 
  , userService "changepass" changePass
  , userService "assignments" assignments
  , adminService "allasgns" allAsgns 
  , userService "newtest" newTest
  , userService "newprog" newProgram
  , userService "testsuites" testSuites 
  , userService "programs" programs
  , userService "numtests" numTestSuites
  , dir "isadmin"  $ isAdminSession  >>= \r -> jsonResponse r r
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

files config =  serveFiles ["index.html"] (configStaticRoot config)
