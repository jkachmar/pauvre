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

import           Config
import           Model

--------------------------------------------------------------------------------

type BuildingApi = "building" :> BuildingApi'

type BuildingApi' =
  Get '[JSON] [Entity Building]

  :<|> Capture "building_id" BuildingParamId
    :> Get '[JSON] [Entity Tenant]

--------------------------------------------------------------------------------

newtype BuildingParamId = BuildingParamId Int64
  deriving (Eq, Ord, Show, Generic, FromHttpApiData, ToJSON)

--------------------------------------------------------------------------------

buildingServer :: ServerT BuildingApi HandlerM
buildingServer =
       allBuildings
  :<|> tenantsFromBuildingId

allBuildings :: HandlerM [Entity Building]
allBuildings = runDbRead (selectList [] [])

tenantsFromBuildingId :: BuildingParamId -> HandlerM [Entity Tenant]
tenantsFromBuildingId (BuildingParamId buildingId) = do
  tenants <- runDbRead (selectList
                        [TenantBuildingId ==. (Just $ toSqlKey buildingId)]
                        [])

  if (null tenants) then throwError err404 else pure tenants
