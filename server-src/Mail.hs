module Mail
  ( mailFrom
  ) where

import Data.List (intersperse)
import System.Log.Logger (infoM,errorM)
import System.Exit
import System.IO
import Control.Exception (finally)
import System.Process

-- |Assumes /usr/sbin/sendmail
mailFrom :: String -- ^sender
         -> [String] -- ^recipients
         -> String -- ^subject
         -> String -- ^body
         -> IO Bool
mailFrom fromAddr toAddrs subject body = do
  (stdin,stdout,stderr,process) <- 
    runInteractiveCommand "/usr/sbin/sendmail -t"
  let sendMail = do
        hSetBinaryMode stdin False
        hPutStrLn stdin $ "To: " ++ (concat $ intersperse ", " toAddrs)
        hPutStrLn stdin $ "From: " ++ fromAddr
        hPutStrLn stdin $ "Subject: " ++ subject
        hPutStrLn stdin $ ""
        hPutStrLn stdin $ body
        hFlush stdin
        hClose stdin
        exitCode <- waitForProcess process
        case exitCode of
          ExitSuccess -> do
            infoM "mail" $ "message to " ++ show toAddrs ++ " from " ++
              fromAddr ++ "; subject: " ++ subject
            return True
          ExitFailure _ -> do
            errorM "mail" $ "/usr/sbin/sendmail failed; message to " ++ 
              show toAddrs ++ " from " ++ fromAddr ++ "; subject: " ++ subject
            return False
  sendMail `finally` (mapM_ hClose [stdin,stdout,stderr])

