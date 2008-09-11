module CS173.Config 
  ( Config (..)
  , staticRoot
  , plaiTestPath
  , runServer
  , runServerParts
  , runConfig
  , ServerM
  ) where

import Control.Monad.State
import Data.Char (ord,chr)
import Codec.Utils (Octet)
import Control.Monad.Trans
import Sessions
import Network.WebServer

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

runServer :: Config -> ServerPartT ServerM a -> ServerPartT IO a
runServer config (ServerPartT f) = ServerPartT $ \request ->
  WebT (evalStateT (unWebT $ f request) config)

runServerParts :: Config -> [ServerPartT ServerM a] -> [ServerPartT IO a]
runServerParts config parts = map (runServer config) parts
