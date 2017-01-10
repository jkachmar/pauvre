{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Building where

import           Data.Aeson                  (ToJSON)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql
import           GHC.Generics                (Generic)
import           Servant

import           Api.Common                  (SelectById, SelectAll, selectAll, selectById)
import           Config                      (HandlerM)
import           Model

--------------------------------------------------------------------------------

type BuildingApi =
  "building" :> SelectAll Building
  :<|>
  "building" :> SelectById "building_id" Building
  :<|>
  "building" :> "tenant"
             :> Capture "building_id" BuildingParamId
             :> Get '[JSON] [Entity Tenant]

--------------------------------------------------------------------------------

newtype BuildingParamId = BuildingParamId Int64
  deriving (Eq, Ord, Show, Generic, FromHttpApiData, ToJSON)

--------------------------------------------------------------------------------

buildingServer :: ServerT BuildingApi HandlerM
buildingServer =
       selectAll
  :<|> selectById
  :<|> tenantsFromBuildingId

--------------------------------------------------------------------------------

tenantsFromBuildingId :: BuildingParamId -> HandlerM [Entity Tenant]
tenantsFromBuildingId (BuildingParamId buildingId) = do
  tenants <-
    runDbRead (selectList
                [TenantBuildingId ==. (Just $ toSqlKey buildingId)]
                [])

  case tenants of
    [] -> throwError err404
    _  -> pure tenants
