{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import           Data.Text            (Text)
import           Database.Persist.Sql (ConnectionPool, SqlReadT, printMigration,
                                       runMigration, runSqlPool)
import           Database.Persist.TH

import           Config

--------------------------------------------------------------------------------

-- | This is an arbitrarily contrived schema for a hypothetical apartment
-- management system
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Region json sql=regions
    name Text sqltype=text

Building json sql=buildings
    name      Text           sqltype=text
    address   Text           sqltype=text
    city      Text           sqltype=text
    state     Text           sqltype=text
    latitude  Double
    longitude Double
    regionId  RegionId Maybe

Tenant json sql=tenants
    name       Text             sqltype=text
    buildingId BuildingId Maybe
|]

--------------------------------------------------------------------------------

doMigrations :: ConnectionPool -> IO ()
doMigrations = runSqlPool (runMigration migrateAll)

printMigrations :: ConnectionPool -> IO ()
printMigrations = runSqlPool (printMigration migrateAll)

--------------------------------------------------------------------------------

runDbRead
  :: ( MonadReader Config m
     , MonadIO m
     )
  => SqlReadT IO b
  -> m b
runDbRead query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
