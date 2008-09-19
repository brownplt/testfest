module Main where

import CS173.Data
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Control.Monad
import qualified Control.Exception as E
import System.Environment
import Database.CouchDB
import CS173.Actions
import CS173.Tourney (executeTest)
import qualified Data.Maybe as Y
import qualified Data.List as L
import System.Exit
import Control.Monad.Trans

data Flag
  = Help
  deriving (Ord,Eq)

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"] (NoArg Help)
      "display this help message"
  ]

usage = usageInfo "173finalround ASGNID SOLUTION-FILE" options

checkHelp :: [Flag] -> IO ()
checkHelp (Help:_) = do
  putStrLn usage
  exitFailure
checkHelp _ = return ()

main = do
  args <- getArgs
  let (opts,cmds,errs) = getOpt Permute options args
  opts <- return $ L.sort opts
  unless (null errs) $ fail $ concat $ L.intersperse "\n" (map show errs)
  checkHelp opts
  case cmds of
    [asgnId,solutionName] -> runFinalRound (doc asgnId) solutionName
    otherwise -> putStrLn usage >> exitFailure

getSubdirectories :: FilePath -> IO [FilePath]
getSubdirectories path = do
  let excludeDotFiles "." = False
      excludeDotFiles ".." = False
      excludeDotFiles _ = True
  files <- getDirectoryContents path
  all <- filterM doesDirectoryExist files
  return (filter excludeDotFiles all)

assertSolutionExists :: FilePath -> FilePath -> IO ()
assertSolutionExists solutionName path = do
  exists <- doesFileExist (path</>solutionName)
  unless exists $ do
    putStrLn $ show path</>solutionName ++ " not found; aborting."
    exitFailure
  return ()

getSubmission' :: Doc -> CouchMonad String
getSubmission' subId = do
  r <- getSubmission subId
  case r of
    Nothing -> do
      liftIO $ putStrLn $ "Submission " ++ show subId ++ " not found"
      liftIO exitFailure              
    Just v -> return v

runFinalRound :: Doc ->  FilePath -> IO ()
runFinalRound asgnId solutionName = do
  students <- getSubdirectories "."
  mapM_ (assertSolutionExists solutionName) students
  runCouchDB' $ do
    asgn <- getAssignment asgnId
    tests <- getAvailableTests asgnId
    let testSubmissionIds = map snd tests
    testBodies <- mapM getSubmission' testSubmissionIds

    let runTests studentId = do
          solutionBody <- liftIO $ readFile (studentId</>solutionName)
          results <- mapM (executeTest (asgnSolnCmd asgn)  solutionBody)
                           testBodies
          let processResult (Right (),testId,testBody) = return ()
              processResult (Left msg,testId,testBody) = do
                writeFile (studentId</>testId `addExtension` "ss") testBody
                writeFile (studentId</>testId `addExtension` "txt") msg
          mapM_ processResult (zip3 results (map (show.fst) tests) testBodies)

    liftIO $ mapM_ runTests students
    
    
