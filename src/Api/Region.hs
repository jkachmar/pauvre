{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Region where

import           Data.Aeson                  (ToJSON)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql
import           GHC.Generics                (Generic)
import           Servant

import           Config
import           Model

--------------------------------------------------------------------------------

type RegionApi = "region" :> RegionApi'

type RegionApi' =
  Get '[JSON] [Entity Region]

  :<|> Capture "region_id" RegionParamId
    :> Get '[JSON] [Entity Building]

--------------------------------------------------------------------------------

newtype RegionParamId = RegionParamId Int64
  deriving (Eq, Ord, Show, Generic, FromHttpApiData, ToJSON)

--------------------------------------------------------------------------------

regionServer :: ServerT RegionApi HandlerM
regionServer =
       allRegions
  :<|> buildingsFromRegionId

allRegions :: HandlerM [Entity Region]
allRegions = runReadOnly (selectList [] [])

buildingsFromRegionId :: RegionParamId -> HandlerM [Entity Building]
buildingsFromRegionId (RegionParamId regionId) = do
  buildings <- runReadOnly (selectList
                            [BuildingRegionId ==. (Just $ toSqlKey regionId)]
                            [])

  if (null buildings) then throwError err404 else pure buildings
