module Logging 
  ( 
  -- * Logging via email
    MailLogger (..)
  -- * Logging to files
  -- $fileLogger
  , makeFileLogger
  , makeStdoutLogger
  , makeStderrLogger
  ) where

import Mail
import Control.Concurrent.QSem
import qualified Control.Exception as E
import qualified Data.Time as Time
import System.Locale (defaultTimeLocale)
import System.Log
import System.Log.Handler
import System.Exit (ExitCode(..))
import System.IO
import System.Process (runInteractiveProcess,waitForProcess)

data MailLogger = MailLogger 
  { mailLoggerTo :: [String]
  , mailLoggerPriority :: Priority
  , mailLoggerSubjectPrefix :: String
  }
  
instance LogHandler MailLogger where
  setLevel ml priority = ml { mailLoggerPriority = priority }

  getLevel ml = mailLoggerPriority ml

  emit (MailLogger to _ prefix) (priority,subject) body = do
    mailFrom 
           "noreply@cs173.cs.brown.edu"
           to
           (prefix ++ ": " ++ subject ++ " (" ++ show priority ++ ")")
           body
    return ()

  close _ = return ()

--
-- $fileLogger
-- Send log messages to a file
--

data FileLogger = FileLogger
  { flHandle :: Handle
  , flSem :: QSem
  , flName :: Maybe String -- ^name of the file, 'Nothing' for stdout / stderr
  , flPriority :: Priority
  }

makeFileLogger' :: Handle -> Maybe FilePath -> Priority -> IO FileLogger
makeFileLogger' handle yPath priority = do
  sem <- newQSem 1
  return (FileLogger handle sem yPath priority)
  

makeFileLogger :: FilePath -> Priority -> IO FileLogger
makeFileLogger path priority = do
  handle <- openFile path AppendMode
  makeFileLogger' handle (Just path) priority

makeStdoutLogger :: Priority -> IO FileLogger
makeStdoutLogger priority = do
  makeFileLogger' stdout Nothing priority

makeStderrLogger :: Priority -> IO FileLogger
makeStderrLogger priority = do
  makeFileLogger' stderr Nothing priority


instance LogHandler FileLogger where
  setLevel fl p = fl { flPriority = p }

  getLevel fl = flPriority fl

  emit (FileLogger handle sem name _) (priority,body) prefix =  do
    let put = hPutStr handle
    let emit' = do
          time <- Time.getZonedTime
          put (Time.formatTime defaultTimeLocale "%X %F" time)
          put " [ "
          put prefix
          put " ]: "
          put body
          put "\n"
          hFlush handle
    waitQSem sem
    emit' `E.finally` signalQSem sem
  
  close (FileLogger _ _ Nothing _) = return () -- don't close stdout / stderr
  close (FileLogger handle _ (Just _) _) = hClose handle
