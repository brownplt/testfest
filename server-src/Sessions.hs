module Sessions
  ( SessionInfo (..)
  , initSessions
  , requireSession
  , setSession
  , setAdminSession
  , requireAdminSession
  , isAdminSession
  , logoutSession
  ) where

import qualified Codec.Binary.Base64.String as Base64
import qualified Data.ByteString as BS
import Data.HMAC
import Codec.Utils
import Data.Char (ord,chr)
import Control.Monad.Trans
import Control.Exception
import System.IO
import System.Log.Logger
import System.Time (ClockTime (..), getClockTime)
import Network.WebServer -- alternatively, use HAppS.Server
import Control.Monad.Trans
import Text.JSON

jsonHeader response = setHeader "Content-Type" "application/json" response

jsonResponse :: (JSON a, Monad m)
             => Bool -> a -> WebT m Response
jsonResponse isSuccess val = do
  let body = encode $ toJSObject [ ("success", JSBool isSuccess)
                                 , ("value", showJSON val) ]
  ok $ jsonHeader $ toResponse body

-- |Two bits of information that are necessary for sessions.  Simply define
-- the monad which uses sessions as instances of this class.
class Monad m => SessionInfo m where
  getSessionSecret :: m [Octet]
  getSessionLength :: m Int 


-- |Create the session database.  All existing sessions are lost.
initSessions :: IO ()
initSessions = return ()

data Session = Session
  { sessionUser :: String
  , sessionEnd :: Int
  , sessionAdmin :: Bool
  , sessionHash :: [Octet]
  } deriving Show

type Key = [Octet]
type Digest = [Octet]

string2words :: String -> [Octet]
string2words = map (fromIntegral . ord) -- TODO : ensure range check

words2string :: [Octet] -> [Char]
words2string = map (chr.fromIntegral)

-- Write a session to a stream of bytes.
encodeSession' :: String -> Int -> Bool -> [Octet]
encodeSession' userId endTime isAdmin = 
  let userId' = string2words userId
      endTime' = i2osp 4 endTime -- an Int consumes 4 bytes, pad to 4
      isAdmin' = if isAdmin then 65 else 66
      userIdLen' = case length userId' of
                     x | x < 255 -> fromIntegral x
                       | otherwise -> error "userId of the session is too long"

    in userIdLen' : isAdmin' : (endTime' ++ userId')


-- |Returned session may not be valid.  You have to check the MAC.
decodeSession' :: Monad m => [Octet] -> m Session
decodeSession' bs = case bs of
  (userIdLen:isAdmin:t1:t2:t3:t4:rest) -> do
    let userIdLen' = fromIntegral userIdLen
    isAdmin' <- case isAdmin of
      65 -> return True
      66 -> return False
      otherwise -> fail "admin field invalid"
    let endTime' = fromOctets 256 [t1,t2,t3,t4]
    let restLen = length rest
    (userId',mac') <- case restLen of
      _ | restLen >= userIdLen' -> return $ splitAt userIdLen' rest
      otherwise -> fail "user id field too short"
    return $ Session (map (chr.fromIntegral) userId') endTime' isAdmin' mac'
  otherwise -> fail "string too short"
                 
encodeSession :: String -> Int -> Bool -> Key -> [Octet]
encodeSession userId endTime isAdmin secretKey = 
  let bytes = encodeSession' userId endTime isAdmin
    in bytes ++ (hmac_md5 secretKey bytes) 

decodeSession :: Monad m => [Octet] -> Key -> m Session
decodeSession bs secretKey = do
  s@(Session userId endTime isAdmin hash) <- decodeSession' bs
  if hash == hmac_md5 secretKey (encodeSession' userId endTime isAdmin)
    then return s
    else fail "bad MAC" -- bad MAC

data RequestSession = RequestSession String
                    | NoRequestSession

-- |Relies on the session field in POST data to determine the session.  If
-- a cookie is specified, ensure that it has the same value.  This allows us
-- to resume sessions after a page refresh.
instance FromData RequestSession where
  fromData = do
    sessionCookie <- lookMaybeCookie "session"
    sessionId <- look "session"
    case sessionCookie of
      Nothing -> return (RequestSession sessionId)
      Just cookie 
        | cookieValue cookie == sessionId -> return (RequestSession sessionId)
        | otherwise -> return NoRequestSession

checkSession :: (MonadIO m, SessionInfo m)
             => String
             -> m (Maybe (String,Bool,String))
checkSession sessionBase64 = do
  let sessionBytes = Base64.decode sessionBase64
  sessionSecret <- getSessionSecret
  sessionLength <- getSessionLength
  case decodeSession (string2words sessionBytes) sessionSecret of
    Just (Session userId end admin hash)  -> do
      (TOD now _ ) <- liftIO getClockTime
      if fromIntegral now <= fromIntegral end
        then let bytes = encodeSession userId (fromIntegral now + sessionLength)
                                       admin sessionSecret
                 base64 = Base64.encode (words2string bytes)
               in return (Just (userId,admin,base64))
        else return Nothing
    Nothing -> do
      liftIO $ infoM "tourney.sessions" ("Decoding the session failed")
      return Nothing 

logoutSession :: MonadIO m => ServerPartT m Response
logoutSession = withData $ \s -> case s of
  NoRequestSession -> [ anyRequest $ ok (toResponse "true") ]
  RequestSession sid -> [ anyRequest $ do
      addCookie 0 (mkCookie "session" "")
      ok (toResponse "true")
    ]
    



-- |Runs the provided 'ServerPartT' with the username of the current
-- session.
requireSession :: (MonadIO m, SessionInfo m)
               => (String -> WebT m Response) -- ^takes username
               -> ServerPartT m Response
requireSession guarded = withData $ \s -> case s of
  NoRequestSession -> [ anyRequest $ do
      liftIO $ infoM "sessions" ("not logged in / session expired")
      unauthorized $ toResponse "Not Logged In"
    ]
  RequestSession sid -> [ anyRequest $ do
    result <- lift $ checkSession sid
    sessionLength <- lift $ getSessionLength
    case result of
      Nothing -> do
        liftIO $ infoM "sessions" ("session expired")
        addCookie 0 (mkCookie "session" "")
        unauthorized $ toResponse "Session Expired"
      Just (s,_,cookie) -> do
        r <- guarded s
        addCookie sessionLength (mkCookie "session" cookie)
        return r
    ]

requireAdminSession :: (MonadIO m, SessionInfo m)
                    => (String -> WebT m Response) -- ^takes username
                    -> ServerPartT m Response
requireAdminSession guarded = withData $ \s -> case s of
  NoRequestSession ->  [
    anyRequest $ do
      liftIO $ infoM "sessions" ("not logged in / session expired")
      unauthorized $ toResponse "Not Logged In"
    ]
  RequestSession sid -> [ anyRequest $ do
    result <- lift $ checkSession sid
    case result of
      Nothing -> do
        liftIO $ infoM "sessions" ("session expired")
        addCookie 0 (mkCookie "session" "")
        unauthorized $ toResponse "Session Expired"
      Just (s,True,cookie) -> do
        r <- guarded s
        sessionLength <- lift $ getSessionLength
        addCookie sessionLength (mkCookie "session" cookie)
        return r
      Just (s,False,_) -> do
        liftIO $ criticalM "sessions" ("unauthorized access attempt by " ++ s)
        addCookie 0 (mkCookie "session" "")
        unauthorized $ toResponse "Unauthorized Operation"
    ]


setSession' :: (Monad m, MonadIO m, SessionInfo m)
            => Bool -- ^admin?
            -> String -- ^username
            -> WebT m String -- ^returns the session token
setSession' admin username  = do
  sessionLength <- lift getSessionLength 
  sessionSecret <- lift getSessionSecret
  (TOD now _) <- liftIO getClockTime
  let expiry = fromIntegral now + sessionLength
  let bytes = encodeSession username expiry admin sessionSecret
  let base64 = Base64.encode (words2string bytes)
  addCookie sessionLength (mkCookie "session" base64)
  return base64

setSession :: (Monad m, MonadIO m, SessionInfo m)
           => String -- ^username
           -> WebT m String -- ^returns the session token
setSession = setSession' False

setAdminSession :: (Monad m, MonadIO m, SessionInfo m)
                => String -- ^username
                -> WebT m String -- ^returns the session token
setAdminSession = setSession' True

isAdminSession :: (Monad m, MonadIO m, SessionInfo m)
               => ServerPartT m Response
isAdminSession = withData $ \s -> case s of
  NoRequestSession ->  [
    anyRequest $ do
      liftIO $ infoM "sessions" ("not logged in / session expired")
      unauthorized $ toResponse "not logged in"
    ]
  RequestSession sid -> [ do
    result <- anyRequest $ lift $ checkSession sid
    case result of
      Nothing -> anyRequest $ do
        liftIO $ infoM "sessions" ("session expired")
        addCookie 0 (mkCookie "session" "")
        unauthorized $ toResponse "session expired"
      Just (s,True,_) -> anyRequest $ ok (toResponse "true")
      Just (_,False,_) -> anyRequest $ ok (toResponse "false")
    ]
  
