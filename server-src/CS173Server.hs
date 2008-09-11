module Main where

import Paths_CS173Tourney -- created by Cabal
import Logging
import System.Log.Logger
import System.Log.Handler.Simple
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Data.List (sort,intersperse)
import CS173.Server (server)
import CS173.Sample (makeSample)
import CS173.Config (Config (..))
import Data.Char (ord,chr)
import Codec.Utils (Octet)
import qualified System.FilePath as Path

data Flag
  = Help
  | Port String
  | SessionSecret String
  | DebugMsgs
  | ErrorLogEmail String
  | LogFile String
  deriving (Ord,Eq)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help)
      "display this help message"
  , Option [] ["listen-port"] (ReqArg Port "PORT")
      "server port (defaults to 8080)"
  , Option [] ["session-secret"] (ReqArg SessionSecret "SECRET")
      "secret phrase for sessions"
  , Option [] ["error-log-email"] (ReqArg ErrorLogEmail "EMAIL")
      "email address to send all errors"
  , Option ['d'] ["debug-messages"] (NoArg DebugMsgs)
      "print debug messages to the terminal"
  , Option ['l'] ["log-file"] (ReqArg LogFile "PATH")
      "log messages to the given file"
  ]

string2words :: String -> [Octet]
string2words = map (fromIntegral . ord) -- TODO : ensure range check

dropIndex "index.html" = []
dropIndex (x:xs) = x:(dropIndex xs)
dropIndex [] = error "index.html not found"

main = do
  args <- getArgs
  let (opts,cmds,errs) = getOpt Permute options args
  unless (null errs) $
    fail (concat errs)
  opts <- return $ sort opts
  checkHelp opts
  dataDir <- getDataDir
  let staticRoot = Path.joinPath [ dataDir, "web" ]
  let plaiTestPath = dataDir
  
  (port,opts) <- getPort opts
  (sessionSecret,opts) <- getSessionSecret opts
  (maybeLogEmail,opts) <- getLogEmail opts
  (logLevel,opts) <- getLogLevel opts
  (maybeLogFile,opts) <- getLogFile opts
  let sessionLength = 15 * 60 -- in seconds
  let cfg = Config staticRoot plaiTestPath sessionLength
                   (string2words sessionSecret)
  setupLogging maybeLogEmail maybeLogFile logLevel
  case cmds of
    [] -> do noticeM "tourney" "Starting tourney-server."
             server port cfg 
    ["server"] -> do noticeM "tourney" "Starting tourney-server..."
                     server port cfg
    ["sample"] -> do noticeM "tourney" "Creating sample setup..."
                     makeSample cfg
    otherwise -> putStrLn $ "Unrecognized commands: "
                            ++ concat (intersperse " " cmds)

setupLogging :: Maybe String -> Maybe String -> Priority -> IO ()
setupLogging maybeLogEmail maybeLogFile priority = do
  log <- getRootLogger
  log <- return $ setLevel priority log
  log <- case maybeLogFile of
    Nothing -> do
      stderrLogger <- makeStderrLogger priority
      return (setHandlers [stderrLogger] log)
    Just path -> do fileLogger <- makeFileLogger path priority
                    return (setHandlers [fileLogger] log)
  log <- case maybeLogEmail of
    Nothing -> return log
    Just email -> return (addHandler (MailLogger [email] ERROR "[tourney]") log)
  saveGlobalLogger log

checkHelp :: [Flag] -> IO ()
checkHelp flags | null flags || head flags == Help = do
  putStrLn (usageInfo  "tourney-server -s PATH:" options)
  exitFailure
checkHelp _ = return ()

getPort ((Port p):rest) = return (read p,rest)
getPort rest = do
  putStrLn "No port specified, listening on 8080"
  return (8080,rest)

getSessionSecret :: [Flag] -> IO (String,[Flag])
getSessionSecret ((SessionSecret s):rest) = return (s,rest)
getSessionSecret rest = do
  putStrLn "No session secret specified!  Using 'redbull64', which is the \
           \password Arjun uses on random sites like nytimes.com."
  return ("redbull64",rest)

getLogEmail :: [Flag] -> IO (Maybe String,[Flag])
getLogEmail ((ErrorLogEmail s):rest) = return (Just s,rest)
getLogEmail flags = return (Nothing,flags)

getLogLevel :: [Flag] -> IO (Priority,[Flag])
getLogLevel (DebugMsgs:rest) = return (DEBUG,rest)
getLogLevel flags = return (INFO,flags)

getLogFile :: [Flag] -> IO (Maybe String,[Flag])
getLogFile ((LogFile path):rest) = return (Just path,rest)
getLogFile flags = return (Nothing,flags)
