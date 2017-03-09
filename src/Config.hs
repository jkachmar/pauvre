{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Exception                    (throwIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (ReaderT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Database.Persist.Postgresql
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant
import           System.Environment                   (lookupEnv)

--------------------------------------------------------------------------------

-- | HandlerM type encapsulating the context the API will operate within
-- The standard 'ExceptT ServantErr IO' stack is wrapped in a 'ReaderT' to
-- provide Environment and Database information throughout the app.
type HandlerM = ReaderT Config Handler

--------------------------------------------------------------------------------

-- | Handler configuration consists of a DB connection pool and the environment flag
data Config
  = Config
  { getPool :: ConnectionPool
  , getEnv  :: Environment
  }

-- | Distinguish between Development, Test, and Production environments
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

-- | Returns a 'Wai' logging middleware based on the environment
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

--------------------------------------------------------------------------------

-- | Create a connection pool for a given environment
makePool :: Environment -> IO ConnectionPool
makePool Test        = runNoLoggingT (createPostgresqlPool testConnStr 1)
makePool Development = runStdoutLoggingT (createPostgresqlPool devConnStr 1)
makePool Production  = do
  pool <- runMaybeT $ do
    let keys = [ "host="
               , " port="
               , " user="
               , " password="
               , " dbname="
               ]
        envs = [ "PGHOST"
               , "PGPORT"
               , "PGUSER"
               , "PGPASS"
               , "PGDATABASE"
               ]
    prodConnStr <- mconcat . Prelude.zipWith (<>) keys . fmap BS.pack
                   <$> traverse (MaybeT . lookupEnv) envs
    runStdoutLoggingT $ createPostgresqlPool prodConnStr (envPool Production)

  case pool of
    Nothing ->
      throwIO $ userError "Database configuration variables not present in the environment."
    Just a ->
      pure a

--------------------------------------------------------------------------------

-- | The number of connections within a pool for a given environment
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 5

-- | Simple connection string for development work
devConnStr :: ConnectionString
devConnStr = "host=localhost dbname=pauvre user=jkachmar password= port=5432"

-- | Simple connection string for testing
testConnStr :: ConnectionString
testConnStr = "host=localhost dbname=pauvre-test user=test password=test port=5432"
