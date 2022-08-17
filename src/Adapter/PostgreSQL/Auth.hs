module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Data.Has
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import qualified Domain.Auth as D
import Katip
import Text.StringRandom

type State = Pool Connection

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError e -> throwString e
    _ -> return ()
  where
    cmds =
      [ MigrationInitialization,
        MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
      ]

data Config = Config
  { configUrl :: ByteString,
    configStripeCount :: Int,
    configMaxOpenConnPerStripe :: Int,
    configIdleConnTimeout :: NominalDiffTime
  }

withPool :: Config -> (State -> IO a) -> IO a
withPool config action = do
  bracket initPool cleanPool action
  where
    initPool =
      createPool
        openConn
        closeConn
        (configStripeCount config)
        (configIdleConnTimeout config)
        (configMaxOpenConnPerStripe config)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl config)
    closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState config action =
  withPool config $ \state -> do
    migrate state
    action state

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

addAuth ::
  PG r m =>
  D.Auth ->
  m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth email pass) = do
  let rawEmail = D.rawEmail email
      rawPassW = D.rawPassWord pass
  -- generate vCode
  vCode <- liftIO $ do
    r <- stringRandomIO "[A-Za-z0-9]{16}"
    return $ tshow rawEmail <> "_" <> r <> "_" <> r
  -- issue query
  result <- withConn $ \conn ->
    try $ query conn qry (rawEmail, rawPassW, vCode)
  -- interpret result
  case result of
    Right [Only uid] -> return $ Right (uid, vCode)
    Right _ -> throwString "SHould not happen: PG doesn't return userId"
    Left err@SqlError {sqlState = state, sqlErrorMsg = msg} ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg
        then return $ Left D.RegistrationErrorEmailTaken
        else throwString $ "Unhandled PG exception: " <> show err
  where
    qry =
      "insert into auths \
      \(email, pass, email_verification_code, is_email_verified) \
      \values (?, crypt(?, gen_salt('bf')), ?, 'f') \
      \returning id"

setEmailAsVerified ::
  PG r m =>
  D.VerificationCode ->
  m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  result <- withConn $ \conn -> query conn qry (Only vCode)
  case result of
    [(uid, mail)] -> case D.mkEmail mail of
      Right email -> return $ Right (uid, email)
      _ -> throwString "SHould not happen: PG returns invalid email: " <> unpack mail
    _ -> rerturn $ Left D.EmailVerificationErrorInvalidCode
  where
    qry =
      "update auths \
      \set is_email_verified = 't' \
      \where email_verification_code = ? \
      \returning id, cast (email as text)"

findUserByAuth ::
  PG r m =>
  D.Auth ->
  m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth email pass) = do
  let rawEmail = D.rawEmail email
      rawPassW = D.rawPassWord pass
  result <- withConn $ \conn -> query conn qry (rawEmail, rawPassW)
  return $ case result of
    [(uid, isVerified)] -> Just (uid, isVerified)
    _ -> Nothing
  where
    qry =
      "select id, is_email_verified \
      \from auths \
      \where email = ? and pass = crypt(?, pass)"

findEmailFromUserId ::
  PG r m =>
  D.UserId ->
  m (Maybe D.Email)
findEmailFromUserId uid = do
  result <- withConn $ \conn -> query conn qry (Only uid)
  return $ case result of
    [Only mail] -> case D.mkEmail mail of
      Right email -> Just email
      _ -> throwString "Should not happen: PG returns invalid email: " <> unpack mail
    _ -> Nothing
  where
    qry =
      "select cast (email as text) \
      \from auths \
      \where id = ?"
