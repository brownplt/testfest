module CS173.Tourney where

import Data.Maybe (fromJust)
import qualified Data.Maybe as Y
import System.Log.Logger
import Text.JSON
import System.Directory
import System.Process
import System.Timeout
import System.IO
import Control.Concurrent
import Database.CouchDB
import Database.CouchDB.Safety
import System.Exit
import Control.Monad.Trans
import Control.Exception (finally)
import System.FilePath (takeFileName)

import CS173.Data
import CS173.Actions
import CS173.Config
import CS173.NormalizeSubmissions

numTests s = case readsPrec 1 s of
  (n,_):rest -> n
  [] -> 1

substCommand :: String -- ^assignment's command string
             -> String -- ^PLAI root path (collects/plai)
             -> String -- ^path to solution
             -> String -- ^path to test suite
             -> String
substCommand cmd plai solution testSuite = 
  unesc . (subst 'p' plai) . (subst 's' solution) . (subst 't' testSuite) $ cmd
  where subst ch sub [] = []
        subst ch sub ('%':ch':rest)
          | ch == ch' = sub ++ rest
          | otherwise = '%':ch':(subst ch sub rest)
        subst ch sub (x:xs) = x:(subst ch sub xs)
        unesc ('%':'%':rest) = '%':(unesc rest)
        unesc (x:xs) = x:(unesc xs)
        unesc [] = []

runCommandTimed :: String
                -> Int -- ^time limit
                -> IO (Maybe (Int,String,String))
runCommandTimed commandString timeLimit = do
  infoM "tourney" ("Running command: " ++ commandString)
  (stdin,stdout,stderr,process) <- runInteractiveCommand commandString
  let getResult = do
        code <- waitForProcess process
        hSetBinaryMode stdout False
        hSetBinaryMode stderr False
        errs <- hGetContents stderr
        outs <- hGetContents stdout
        hClose stdin
        length errs `seq` hClose stderr -- hGetContents leaks memory
        length outs `seq` hClose stdout
        return (outs,errs)
  result <- timeout (timeLimit * 1000000) getResult
  case result of
    Nothing -> do terminateProcess process
                  return Nothing
    Just (out,err) -> do
      code <- getProcessExitCode process
      case code of
        Just ExitSuccess -> return $ Just (0,out,err) -- UNIX!!!
        Just (ExitFailure n) -> return $ Just (n,out,err)
        Nothing -> fail $ "cannot get exit code"

collectErrors :: [TestStatus]
              -> String
collectErrors results = concatMap f results where
  f (TestError s) = s ++ "\n\n"
  f _ = ""

withTempFile :: FilePath -> FilePath -> (FilePath-> Handle -> IO a) -> IO a
withTempFile dir template action = do
  (path,handle) <- openTempFile dir template
  action path handle `finally` (hClose handle  >> removeFile path)

runTest :: String -- ^test command
        -> String -- ^ test suite
        -> String -- ^ program
        -> String -- ^test id
        -> String -- ^soln id
        -> ServerM TestStatus
runTest testCommand testText progText testId progId = do
  plaiTest <- plaiTestPath
  now <- getTime
  r <- liftIO $ do
    dir <- getTemporaryDirectory
    withTempFile dir "solution.ss" $ \solutionPath hSolution -> do
      withTempFile dir "test-suite.ss" $ \testSuitePath hTestSuite -> do
        hPutStr hSolution progText
        hFlush hSolution
        hClose hSolution
        hPutStr hTestSuite testText
        hFlush hTestSuite
        hClose hTestSuite
        setCurrentDirectory dir
        -- Run for at most 60 seconds of real time.  The test script is expected
        -- to limit the CPU time and memory consumption.
        runCommandTimed (substCommand testCommand plaiTest 
                                      (takeFileName solutionPath)
                                      (takeFileName testSuitePath))
                        60
  lift $ runCouchDB' $ do
    test <- getTestSuite testId
    prog <- getProgram progId
    let asgnId = programAssignmentId prog  
    let mkRep s n = Report testId progId asgnId n s now (testSuiteUserId test)
                           (programUserId prog) (programTime prog) 
                           (testSuiteTime test)
    case r of
      Nothing -> do
        newDoc dbRep (mkRep "Your test suite took too long." (-1))
        return (TestError "running the test took too long.")  
      Just (0,out,err) -> do
        newDoc dbRep (mkRep "" 0)
        return TestOK
      Just (_,outs,errs) -> do
        newDoc dbRep (mkRep errs (numTests errs))
        return (TestError errs)
    
checkTestSuite :: String -> ServerM ()
checkTestSuite testId = do
  plaiTest <- plaiTestPath
  liftIO $ infoM "tourney.tester" ("running on test suite " ++ testId)
  liftIO $ runCouchDB' $ do
    now <- getTime
    testSuite <- getTestSuite testId
    assignment <- getAssignment (testSuiteAssignmentId testSuite)
    (Just testSuiteText') <- getSubmission (testSuiteSubmissionId testSuite)
    
    let testSuiteText = case asgnTestLang assignment of
          "" -> testSuiteText'
          lang -> standardizeLang lang testSuiteText'
    (Just goldSolutionText) <- getSubmission (assignmentSolutionId assignment)
    r <- liftIO $ do
      dir <- getTemporaryDirectory
      withTempFile dir "solution.ss" $ \solutionPath hSolution -> do
        withTempFile dir "test-suite.ss" $ \testSuitePath hTestSuite -> do
          hPutStr hSolution goldSolutionText
          hFlush hSolution
          hClose hSolution
          hPutStr hTestSuite testSuiteText
          hFlush hTestSuite
          hClose hTestSuite
          setCurrentDirectory dir
          let cmd = substCommand (asgnTestCmd assignment) plaiTest
                     (takeFileName solutionPath) 
                     (takeFileName testSuitePath)
          runCommandTimed cmd 60 -- at most 60 seconds of real time
    case r of
      Nothing ->  do
        updateTestSuiteStatus (const $ TestSuiteMachineCheckError 
                                       "Your test suite took too long.")
                              testId
      Just (0,outs,errs) -> do
        updateTestSuiteStatus (const TestSuiteMachineCheckOK) testId
      Just (_,outs,errs) ->  do
        updateTestSuiteStatus (const $ TestSuiteMachineCheckError errs) testId
    return ()

testSuiteTesterThread :: ServerM ()
testSuiteTesterThread = do
  liftIO $ debugM "tourney.tester" "Checking for new jobs..."
  lst <- liftIO $ runCouchDB' $ queryViewKeys dbSuites (doc "suites") 
                                              (doc "pending") []
  
  mapM_ checkTestSuite lst
  lst <- liftIO $ runCouchDB' $ queryViewKeys dbPrograms 
                                  (doc "programs") (doc "pending") []
  mapM_ checkProgram lst
  liftIO $ threadDelay (20 * 1000000)
  testSuiteTesterThread

getTestBody (_,subId) = do
  (Just testBody) <- getSubmission subId
  return testBody


checkProgram :: String -> ServerM ()
checkProgram progId = do
  liftIO $ infoM "tourney.tester" ("running on solution " ++ progId)
  (prog,progBody,tests,testBodies,testCmd) <- liftIO $ runCouchDB' $ do
    prog <- getProgram progId
    asgn <- getAssignment (programAssignmentId prog)
    let testCmd = asgnSolnCmd asgn
    (Just progBody') <- getSubmission (programSubmissionId prog)
    let progBody = standardizeLang (asgnSolnLang asgn) progBody'
    tests <- getAvailableTests (programAssignmentId prog)
    testBodies <- mapM getTestBody tests
    let testIds = map fst tests
    return (prog,progBody,tests,zip testBodies testIds,testCmd)
  testResults <- mapM (\(testBody,testId) -> 
                        runTest testCmd testBody progBody testId progId) 
                      testBodies
  let testErrors = collectErrors testResults
  liftIO $ runCouchDB' $  updateProgramStatus progId $ case testErrors of
    "" -> TestOK
    otherwise -> TestError testErrors
