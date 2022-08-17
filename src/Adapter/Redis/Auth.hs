module Adapter.Redis.Auth where

import ClassyPrelude
import Data.Has
import qualified Database.Redis as R
import qualified Domain.Auth as D
import Text.StringRandom

type State = R.Connection

-- | create state from redis url string.
-- format: redis://user:pass@host:port/db
-- sample: redis://abc:def@localhost:6379/0
withState :: String -> (State -> IO a) -> IO a
withState connUrl action = do
  case R.parseConnectInfo connUrl of
    Left err -> throwString "Invalid Redis Conn URL" <> show err
    Right connectInfo -> do
      conn <- R.connect connectInfo
      action conn

type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action

newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
  sessionId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result <- withConn $ R.set (emcodeUtf8 sessionId) (fromString . show $ userId)
  case result of
    Right R.Ok -> return sessionId
    err -> throwString $ "Unexpected redis error: " <> show err
