module Main where

import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit
import Database.CouchDB
import System.Random
import Control.Monad
import Data.List (sort)
import Mail
import Logging

import CS173.Data

data Flag
  = Help
  | Suffix String
  | Create
  | Notify
  deriving (Eq,Ord)


options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help)
      "display this help message"
  , Option [] ["suffix"] (ReqArg Suffix "@domain")
      "suffix to append onto email addresses, if they don't have @"
  , Option ['c'] ["create-accounts"] (NoArg Create)
      "create accounts"
  , Option ['n'] ["notify-users"] (NoArg Notify)
      "notify users of their passwords"
  ]

main = do
  args <- getArgs
  let (opts,cmds,errs) = getOpt RequireOrder options args
  unless (null errs) $
    fail (concat errs)
  opts <- return $ sort opts
  checkHelp opts
  (maybeSuffix,opts) <- maybeSuffix opts
  case opts of
    [Create] -> createAccounts maybeSuffix cmds
    [Notify] -> notifyUsers maybeSuffix cmds
    otherwise -> fail "Invalid arguments, type -h for help."


createAccounts maybeSuffix users = mapM_ (createAccount maybeSuffix)  users

notifyUsers maybeSuffix users = mapM_ (notifyUser maybeSuffix) users

createAccount ms u = do
  let userId = applySuffix ms u
  password <- mapM (const $ randomRIO ('A','Z')) [1..10] -- 10 char password
  r <- runCouchDB' $ newNamedDoc (db "users") (doc userId)
                                 (User (doc userId) True password False)
  case r of
    Left _ -> putStrLn $ userId ++ " already exists, moving on."
    Right _ -> return ()
  
notifyUser ms u = do
  let userId = applySuffix ms u
  r <- runCouchDB' $ getDoc (db "users") (doc userId)
  case r of
    Nothing -> putStrLn $ userId ++ " does not exist; moving on."
    Just (_,_,user) -> do
      r <- mailFrom "cs173tas@cs.brown.edu" [userId] 
             "[testfest]: account password"  $
             "Welcome to testfest.  The system is accessible at: \n\
             \\n\
             \http://cs173.cs.brown.edu\n\
             \\n\
             \Your username is:\n\n"
             ++ userId ++
             "\n\nYour password is:\n\n" ++ userPassword user ++
             "\n\nIf you have any problems, please email the TAs\n\
             \at cs173tas@cs.brown.edu."
      case r of
        True -> do putStrLn $ "Notified " ++ userId
                   return ()
        False -> putStrLn $ "ERROR emailing " ++ userId ++ "; moving on."
 
 
  
checkHelp :: [Flag] -> IO ()
checkHelp flags | null flags || head flags == Help = do
  putStrLn (usageInfo  "173tourney-accounts " options)
  exitFailure
checkHelp _ = return ()

maybeSuffix ((Suffix suffix):rest) = return (Just suffix,rest)
maybeSuffix lst = return (Nothing,lst)

applySuffix Nothing user = user
applySuffix (Just suffix) user | '@' `elem` user = user
                               | otherwise = user ++ suffix
