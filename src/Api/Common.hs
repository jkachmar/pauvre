{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Api.Common where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Reader      (MonadIO, MonadReader)
import           Data.Int                  (Int64)
import           Database.Persist.Sql      (Entity, Filter, PersistEntity,
                                            PersistEntityBackend, SqlBackend,
                                            ToBackendKey, get, selectList,
                                            toSqlKey)
import           Servant

import           Config                    (Config)
import           Model                     (runDbRead)

--------------------------------------------------------------------------------

type SelectAll record =
  Get '[JSON] [Entity record]

selectAll
  :: ( MonadIO m
     , MonadReader Config m
     , PersistEntity record
     , PersistEntityBackend record ~ SqlBackend
     )
  => m [Entity record]
selectAll = runDbRead $ selectList [] []

--------------------------------------------------------------------------------

type SelectById field record =
     Capture field Int64
  :> Get '[JSON] record

selectById
  :: ( MonadIO m
     , MonadError ServantErr m
     , MonadReader Config m
     , PersistEntityBackend record ~ SqlBackend
     , ToBackendKey SqlBackend record
     )
  => Int64
  -> m record
selectById ident = do
  result <- runDbRead $ get (toSqlKey ident)
  case result of
    Nothing -> throwError err404
    Just r  -> pure r
