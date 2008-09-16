module CS173.Config 
  ( Config (..)
  , staticRoot
  , plaiTestPath
  , run173Server
  , runConfig
  , ServerM
  ) where

import Control.Monad.State
import Data.Char (ord,chr)
import Codec.Utils (Octet)
import Control.Monad.Trans
import Network.WebServer hiding (runServer)
import Network.WebServer.Sessions

data Config = Config
  { configStaticRoot :: String
  , configPLAITestPath :: String
  , configSessionLength :: Int
  , configSessionSecret :: [Octet]
  }


instance SessionInfo (StateT Config IO) where
 getSessionLength = do
  Config{configSessionLength=r} <- get
  return r

 getSessionSecret = do 
  Config{configSessionSecret=r} <- get
  return r

instance SessionInfo m => SessionInfo (ServerT m) where
  getSessionLength = lift getSessionLength
  getSessionSecret = lift getSessionSecret


type ServerM = StateT Config IO

staticRoot :: ServerM String
staticRoot = do
  Config{configStaticRoot=r} <- get
  return r

plaiTestPath :: ServerM String
plaiTestPath = do
  Config{configPLAITestPath=r} <- get
  return r


runConfig :: Config -> ServerM a -> IO a
runConfig config m = evalStateT m config

run173Server :: Config -> ServerT ServerM a -> ServerT IO a
run173Server config (ServerT f) = ServerT $ \request ->
  evalStateT (f request) config

