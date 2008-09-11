module CS173.Messages where

import Control.Monad.Trans
import Text.JSON
import Database.CouchDB.JSON
import Network.WebServer
import System.Log.Logger
import CS173.Data


data EnableAsgn = EnableAsgn String Bool

data Login = Login String String

data NewProgram = NewProgram
  { newProgramAsgnId :: String
  , newProgramBody :: String
  }

data NewTest = NewTest String String

data ForAssignment = ForAssignment String

data ForId = ForId String

instance FromData () where
  fromData = return ()

instance FromData ForId where
  fromData = do
    id <- look "id"
    return (ForId id)

instance FromData ForAssignment where
  fromData = do
    asgnid <- look "asgnid"
    return (ForAssignment asgnid)

instance FromData NewProgram where
  fromData = do
    asgnId <- look "asgnid"
    body <- look "prog"
    return (NewProgram asgnId body)

instance FromData NewTest where
  fromData = do
    body <- look "test"
    assignment <- look "assignment"
    return (NewTest body assignment)

instance FromData Login where
  fromData = do
    username <- look "username"
    password <- look "password"
    return (Login username password)

data ChangePassword 
  = ChangePassword String -- ^current password
                   String -- ^new password

instance FromData ChangePassword where
  fromData = do
    currentPass <- look "oldpass"
    newPass <- look "newpass"
    return (ChangePassword currentPass newPass)

data NewAsgn 
  = NewAsgn String -- ^ solution
            Assignment -- ^assignment, solution id is ignored

instance FromData NewAsgn where
  fromData = do
    soln <- look "gold"
    asgn <- lookJSON "asgn"
    return (NewAsgn soln asgn)

data UpdateGold
  = UpdateGold String -- ^asgn id
               String -- ^solution

instance FromData UpdateGold where
  fromData = do
    soln <- look "gold"
    asgnid <- look "asgnid"
    return (UpdateGold asgnid soln)

instance FromData EnableAsgn where
  fromData = do
    id <- look "id"
    enable <- lookJSON "enable"
    return (EnableAsgn id enable)

data EnableTest = EnableTest String Bool Bool


lookJSON :: (JSON a) => String -> RqData a 
lookJSON field = do
  raw <- look field
  case decode raw of
    Text.JSON.Ok a -> return a
    otherwise -> fail "could not read"

instance FromData EnableTest where
  fromData = do
    id <- look "id"
    enable <- lookJSON "enable"
    disable <- lookJSON "disable"
    return (EnableTest id enable disable)

data ToggleStatus = ToggleStatus String String String TestSuiteStatus Bool

instance FromData ToggleStatus where
  fromData = do
    id <- look "id"
    status <- lookJSON "status"
    user <- look "userid"
    asgn <- look "asgnid"
    singleSuite <- lookJSON "singlesuite"
    return (ToggleStatus id user asgn status singleSuite)


jsonResponse :: (JSON a, Monad m)
             => Bool -> a -> WebT m Response
jsonResponse isSuccess val = do
  let body = encode $ toJSObject [ ("success", JSBool isSuccess)
                                 , ("value", showJSON val) ]
  ok $ jsonHeader $ toResponse body


jsonHeader response = setHeader "Content-Type" "application/json" response
