module CS173.Data where

import Database.CouchDB
import Text.JSON
import Database.CouchDB.JSON
import Data.Maybe (isNothing,fromJust)

data Assignment = Assignment
  { assignmentId :: Doc
  , assignmentDesc :: String
  , assignmentEnabled :: Bool
  , assignmentEnd :: Maybe Integer
  , asgnTestCmd :: String -- ^the command to run when testing tests
  , asgnSolnCmd :: String -- ^the command to run when testing solutions
  , asgnTestLang :: String -- ^the '#lang' of tests, if any
  , asgnSolnLang :: String -- ^the '#lang' of solutions, if any
  , asgnSingleTestSuite :: Bool -- ^single test suite assignments only
                                -- have a single test suite per user
                                -- active at a time
  , assignmentSolutionId :: Doc
  } deriving (Show)

data User = User
  { userId :: Doc
  , userEnabled :: Bool
  , userPassword :: String
  , userAdmin :: Bool
  } deriving (Show)

-- |Submitted tests and solutions
data Submission = Submission String

data TestSuite = TestSuite
  { testSuiteUserId :: Doc
  , testSuiteAssignmentId :: Doc
  , testSuiteSubmissionId :: Doc
  , tsStatus :: TestSuiteStatus
  , testSuiteTime :: Integer
  } deriving (Show)


data TestStatus = TestPending | TestOK | TestError String deriving (Show)

data Program = Program
  { programUserId :: Doc
  , programAssignmentId :: Doc
  , programTime :: Integer
  , programStatus :: TestStatus
  , programSubmissionId :: Doc
  }

data Report = Report
  { repTestId :: Doc
  , repProgId :: Doc
  , repAsgnId :: Doc
  , repNumErrors :: Int
  , repMsg :: String
  , repTime :: Integer
  , repProgUserId :: Doc
  , repTestUserId :: Doc
  , repTestTime :: Integer
  , repProgTime :: Integer
  } 

instance JSON Report where

  readJSON val = do
    obj <- jsonObject val
    testId <- jsonField "testid" obj
    progId <- jsonField "progid" obj
    asgnId <- jsonField "asgnid" obj
    numErrors <- jsonField "numerrors" obj
    repMsg <- jsonField "msgs" obj
    time <- jsonField "time" obj
    progUser <- jsonField "proguser" obj
    testUser <- jsonField "testuser" obj
    testTime <- jsonField "testtime" obj
    progTime <- jsonField "progtime" obj
    return (Report testId progId asgnId numErrors repMsg time 
                   progUser testUser testTime progTime)

  showJSON (Report testId progId asgnId numErrors msg time 
                   progUser testUser testTime progTime) =
    JSObject $ toJSObject
      [ ("testid",showJSON testId)
      , ("progid",showJSON progId)
      , ("asgnid",showJSON asgnId)
      , ("numerrors",showJSON numErrors)
      , ("msg",showJSON msg)
      , ("time",showJSON time)
      , ("proguser",showJSON progUser)
      , ("testuser",showJSON testUser)
      , ("testtime",showJSON testTime)
      , ("progtime",showJSON progTime)
      ]

instance JSON Program where

  readJSON val = do
    obj <- jsonObject val
    userId <- jsonField "userid" obj
    asgnId <- jsonField "asgnid" obj
    time <- jsonField "time" obj
    status <- jsonField "status" obj
    submissionId <- jsonField "subid" obj
    return (Program userId asgnId time status submissionId)

  showJSON (Program userid asgnid time status subid) =
    JSObject $ toJSObject
      [ ("userid", showJSON userid)
      , ("asgnid", showJSON asgnid)
      , ("time", showJSON time)
      , ("status", showJSON status)
      , ("subid", showJSON subid)
      ]

isActiveAssignment :: Integer -> Assignment -> Bool
isActiveAssignment now asgn =
  assignmentEnabled asgn && 
  (isNothing (assignmentEnd asgn) || fromJust (assignmentEnd asgn) <= now)

newAssignment id desc = Assignment id desc False Nothing ""

instance JSON Submission where
  readJSON val = do
    obj <- jsonObject val
    body <- jsonField "body" obj
    return (Submission body)

  showJSON (Submission body) =
    JSObject $ toJSObject [("body",showJSON body)]

instance JSON User where
  
  readJSON val = do
    obj <- jsonObject val
    id <- jsonField "id" obj
    password <- jsonField "password" obj
    enabled <- jsonField "enabled" obj
    admin <- jsonField "admin" obj
    return (User id enabled password admin)

  showJSON (User id enabled password admin) =
    JSObject $ toJSObject
      [ ("id", showJSON id)
      , ("password",showJSON password)
      , ("enabled",JSBool enabled)
      , ("admin", showJSON admin)
      ]

data TestSuiteStatus
  = TestSuiteSubmitted
  | TestSuiteMachineCheckOK
  | TestSuiteMachineCheckError String
  | TestSuiteTACheckOK -- ^ this is the *only* state in which a test may be
                       -- used against submissions
  | TestSuiteTACheckError String
  | TestSuiteSuperseded
  | TestSuiteRetracted
  deriving (Show)

instance JSON TestSuiteStatus where
  readJSON val = do
    obj <- jsonObject val
    status <- jsonField "stat" obj
    case status of
      "Submitted" -> return TestSuiteSubmitted
      "MachineCheckOK" -> return TestSuiteMachineCheckOK
      "MachineCheckError" -> do
        msg <- jsonField "message" obj
        return (TestSuiteMachineCheckError msg)
      "TACheckOK" -> return TestSuiteTACheckOK
      "TACheckError" -> do
        msg <- jsonField "message" obj
        return (TestSuiteTACheckError msg)
      "Superseded" -> do
        return TestSuiteSuperseded
      "Retracted" -> return TestSuiteRetracted
      otherwise -> fail $ "invalid test suite status: " ++ status

  showJSON val = 
    JSObject (toJSObject $ ("stat",showJSON $ status val)
                           :(msg val)) where
      msg (TestSuiteMachineCheckError s) = [("message",showJSON s)]
      msg (TestSuiteTACheckError s) = [("message",showJSON s)]
      msg _ = []
      status TestSuiteSubmitted = "Submitted"
      status TestSuiteMachineCheckOK = "MachineCheckOK"
      status (TestSuiteMachineCheckError _) = "MachineCheckError"
      status TestSuiteTACheckOK = "TACheckOK"
      status (TestSuiteTACheckError _) = "TACheckError"
      status TestSuiteSuperseded = "Superseded"
      status TestSuiteRetracted = "Retracted"
  

instance JSON TestStatus where
  readJSON (JSBool True) = return TestOK
  readJSON (JSBool False) = return TestPending
  readJSON (JSString s) = return (TestError (fromJSString s))
  readJSON v = fail $ "invalid test status: " ++ show v

  showJSON TestPending = showJSON False
  showJSON TestOK = showJSON True
  showJSON (TestError reason) = showJSON reason

instance JSON Assignment where

  readJSON val = do
    obj <- jsonObject val
    id <- jsonField "id" obj
    desc <- jsonField "desc" obj
    enabled <- jsonField "enabled" obj
    end <- jsonField "endtime" obj
    let end' = if end == 0 then Nothing else (Just end)
    testCmd <- jsonField "testcmd" obj
    solnCmd <- jsonField "solncmd" obj
    testLang <- jsonField "testlang" obj
    solnLang <- jsonField "solnlang" obj
    singleTestSuite <- jsonField "singletestsuite" obj
    solnId <- jsonField "solnid" obj
    return (Assignment id desc enabled end' testCmd solnCmd testLang
                       solnLang singleTestSuite solnId)

  showJSON (Assignment id desc enabled end testCmd solnCmd 
                       testLang solnLang singleTestSuite solnId) =
    JSObject $ toJSObject
      [ ("id", showJSON id)
      , ("desc", showJSON desc)
      , ("enabled", JSBool enabled)
      , ("endtime", JSRational True end')
      , ("testcmd", showJSON testCmd)
      , ("solncmd", showJSON solnCmd)
      , ("testlang", showJSON testLang)
      , ("solnlang", showJSON solnLang)
      , ("singletestsuite", showJSON singleTestSuite)
      , ("solnid", showJSON solnId)
      ] where end' = case end of
                Nothing -> 0 -- JavaScript for false
                Just n -> fromIntegral n

data ProgramInfo = ProgramInfo 
  { pid :: Doc -- ^test suite id
  , piStatus :: TestStatus -- ^test suite status
  , piTime :: Integer
  }

instance JSON ProgramInfo where
  
  readJSON val = do
    obj <- jsonObject val
    id <- jsonField "id" obj
    status <- jsonField "status" obj
    time <- jsonField "time" obj
    return (ProgramInfo id status time)

  showJSON (ProgramInfo id status time) = 
    JSObject $ toJSObject
      [ ("id",showJSON id)
      , ("status", showJSON status)
      , ("time", showJSON time)
      ]

instance JSON TestSuite where

  readJSON val = do
    obj <- jsonObject val
    userId <- jsonField "userid" obj
    asgnId <- jsonField "asgnid" obj
    body <- jsonField "body" obj
    status <- jsonField "status" obj
    time <- jsonField "time" obj
    return (TestSuite userId asgnId body status time)

  showJSON (TestSuite userId asgnId body status time) = 
    JSObject $ toJSObject
      [ ("userid", showJSON userId)
      , ("asgnid", showJSON asgnId)
      , ("body", showJSON body)
      , ("status", showJSON status)
      , ("time", showJSON time)
      ]
