{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Api.Common where

import           Control.Monad.Reader (MonadIO, MonadReader)
import           Database.Persist.Sql (Entity, Filter, PersistEntity,
                                       PersistEntityBackend, SqlBackend,
                                       selectList)
import           Servant

import           Config
import           Model

--------------------------------------------------------------------------------

type SelectAll key =
  Get '[JSON] [Entity key]

selectAll ::
  ( MonadIO m
  , MonadReader Config m
  , PersistEntity key
  , PersistEntityBackend key ~ SqlBackend
  ) => m [Entity key]
selectAll = runDbRead $ selectList ([] :: [Filter key]) []
