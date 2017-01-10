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

import           Api.Common
import           Config                      (HandlerM)
import           Model

--------------------------------------------------------------------------------

type RegionApi =
  "region" :> SelectAll Region
  :<|>
  "region" :> SelectById "region_id" Region
  :<|>
  "region" :> Capture "region_id" RegionParamId
           :> Get '[JSON] [Entity Building]

--------------------------------------------------------------------------------

newtype RegionParamId = RegionParamId Int64
  deriving (Eq, Ord, Show, Generic, FromHttpApiData, ToJSON)

--------------------------------------------------------------------------------

regionServer :: ServerT RegionApi HandlerM
regionServer =
       selectAll
  :<|> selectById
  :<|> buildingsFromRegionId

--------------------------------------------------------------------------------

buildingsFromRegionId :: RegionParamId -> HandlerM [Entity Building]
buildingsFromRegionId (RegionParamId regionId) = do
  buildings <-
    runDbRead (selectList
                [BuildingRegionId ==. (Just $ toSqlKey regionId)]
                [])

  case buildings of
    [] -> throwError err404
    _  -> pure buildings
