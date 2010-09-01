module CS173.Tourney where

import Paths_CS173Tourney -- created by Cabal
import Data.IORef
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
  mv <- newEmptyMVar
  let doRun = do
        (stdin,stdout,stderr,process) <- runInteractiveCommand commandString
        code <- waitForProcess process
        hSetBinaryMode stdout False
        hSetBinaryMode stderr False
        errs <- hGetContents stderr
        outs <- hGetContents stdout
        hClose stdin
        length errs `seq` hClose stderr -- hGetContents leaks memory
        length outs `seq` hClose stdout
        putMVar mv (code,outs,errs)
  forkOS doRun
  (code,out,err) <- takeMVar mv
  case code of
    ExitSuccess -> return $ Just (0,out,err) -- UNIX!!!
    ExitFailure n -> return $ Just (n,out,err)

collectErrors :: [TestStatus]
              -> String
collectErrors results = concatMap f results where
  f (TestError s) = s ++ "\n\n"
  f _ = ""

withTempFile :: FilePath -> FilePath -> (FilePath-> Handle -> IO a) -> IO a
withTempFile dir template action = do
  (path,handle) <- openTempFile dir template
  action path handle `finally` (hClose handle  >> removeFile path)


executeTest :: String -- ^test command
            -> String -- ^solution body
            -> String -- ^test suite body
            -> IO (Either String ()) -- ^ report for student on errors
executeTest testCmd solutionBody testBody = do
  plaiTest <- getDataDir
  dir <- getTemporaryDirectory
  result <- withTempFile dir "solution.rkt" $ \solutionPath hSolution -> do
    withTempFile dir "test-suite.rkt" $ \testSuitePath hTestSuite -> do
      let cmd = substCommand testCmd plaiTest (takeFileName solutionPath)
                             (takeFileName testSuitePath)
      hPutStr hSolution solutionBody >> hFlush hSolution >> hClose hSolution
      hPutStr hTestSuite testBody >> hFlush hTestSuite >> hClose hTestSuite
      setCurrentDirectory dir
      -- Run for at most 60 seconds of real time.  The test script is expected
      -- to limit the CPU time and memory consumption.
      runCommandTimed  cmd 60
  case result of
    Nothing -> return (Left "Took too much time (> 60 seconds of real time)")
    Just (0,stdoutStr,stderrStr) -> return (Right ())
    Just (_,stdoutStr,stderrStr) -> return (Left stdoutStr)
  


runTest :: String -- ^test command
        -> String -- ^ test suite
        -> String -- ^ program
        -> Doc -- ^test id
        -> Doc -- ^soln id
        -> ServerM TestStatus
runTest testCommand testText progText testId progId = do
  plaiTest <- plaiTestPath
  now <- getTime
  r <- liftIO $ do
    dir <- getTemporaryDirectory
    withTempFile dir "solution.rkt" $ \solutionPath hSolution -> do
      withTempFile dir "test-suite.rkt" $ \testSuitePath hTestSuite -> do
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
        newDoc dbRep (mkRep outs (numTests outs))
        return (TestError errs)
    
checkTestSuite :: Doc -> ServerM ()
checkTestSuite testId = do
  plaiTest <- plaiTestPath
  liftIO $ infoM "tourney.tester" ("running on test suite " ++ show testId)
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
      withTempFile dir "solution.rkt" $ \solutionPath hSolution -> do
        withTempFile dir "test-suite.rkt" $ \testSuitePath hTestSuite -> do
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
        liftIO $ infoM "tourney.tester" (show testId ++ " took too long.")
        updateTestSuiteStatus (const $ TestSuiteMachineCheckError 
                                       "Your test suite took too long.")
                              testId
      Just (0,outs,errs) -> do
        liftIO $ infoM "tourney.tester" (show testId ++ " passed gold.")
        updateTestSuiteStatus (const TestSuiteMachineCheckOK) testId
      Just (code,outs,errs) ->  do
        liftIO $ infoM "tourney.tester" $ show testId ++ 
          " raised errors (exit code: " ++ show code ++ ")"
        updateTestSuiteStatus (const $ TestSuiteMachineCheckError outs) testId
    return ()

testSuiteTesterThread config = do
  liftIO $ debugM "tourney.tester" "Checking for new jobs..."
  lst <- liftIO $ runCouchDB' $ queryViewKeys dbSuites (doc "suites") 
                                              (doc "pending") []
  
  thisDoc <- liftIO $ newIORef Nothing
  waitVar <- liftIO $ newEmptyMVar
  let checkTestSuites = do
        mapM_ (\testId -> do writeIORef thisDoc (Just testId)
                             runConfig config $ checkTestSuite testId) lst
        writeIORef thisDoc Nothing
        
  liftIO $ do 
    forkOS (checkTestSuites `finally` putMVar waitVar ())
    takeMVar waitVar
    failed <- readIORef thisDoc
    case failed of
      Nothing -> return ()
      Just testId -> do
        errorM "tourney.tester" $ "GHC assplosion on " ++ show testId
        runCouchDB' $ updateTestSuiteStatus 
          (const $ TestSuiteMachineCheckError "out of time/memory") testId
        errorM "tourney.tester" $ "Successfully killed " ++ (show testId)
  lst <- liftIO $ runCouchDB' $ queryViewKeys dbPrograms 
                                  (doc "programs") (doc "pending") []
  mapM_ checkProgram lst
  liftIO $ threadDelay (20 * 1000000)
  testSuiteTesterThread config

getTestBody (_,subId) = do
  (Just testBody) <- getSubmission subId
  return testBody


checkProgram :: Doc -> ServerM ()
checkProgram progId = do
  liftIO $ infoM "tourney.tester" ("running on solution " ++ show progId)
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
