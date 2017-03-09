{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Model.Building where

import           Composite.Aeson   (JsonFormat, defaultJsonFormatRec,
                                    jsonFormatWithIso, parseJsonWithFormat',
                                    recJsonFormat, toJsonWithFormat)
import           Composite.Opaleye (defaultRecTable)
import           Control.Lens      (_Wrapped)
import           Control.Lens.TH   (makeWrapped)
import           Data.Aeson        (FromJSON (parseJSON), ToJSON (toJSON))
import           Frames            (Record)
import           Opaleye           (Table (Table))

import           Types

--------------------------------------------------------------------------------

type ApiBuilding =
  '[ FBuildingName
   , FBuildingAddress
   , FBuildingState
   , FBuildingLatitude
   , FBuildingLongitude
   , FBuildingRegionId
   ]

type DBBuildingColumns =
  '[ CBuildingId
   , CBuildingName
   , CBuildingAddress
   , CBuildingState
   , CBuildingLatitude
   , CBuildingLongitude
   , CBuildingRegionId
   ]

type DBBuilding =
  '[ FBuildingId
   , FBuildingName
   , FBuildingAddress
   , FBuildingState
   , FBuildingLatitude
   , FBuildingLongitude
   , FBuildingRegionId
   ]

--------------------------------------------------------------------------------

newtype ApiBuildingJson = ApiBuildingJson { unApiBuildingJson :: Record ApiBuilding }
makeWrapped ''ApiBuildingJson

apiBuildingJsonFormat :: JsonFormat e ApiBuildingJson
apiBuildingJsonFormat =
  jsonFormatWithIso _Wrapped (recJsonFormat defaultJsonFormatRec)

instance ToJSON ApiBuildingJson where
  toJSON = toJsonWithFormat apiBuildingJsonFormat

instance FromJSON ApiBuildingJson where
  parseJSON = parseJsonWithFormat' apiBuildingJsonFormat

--------------------------------------------------------------------------------

buildingTable :: Table (Record DBBuildingColumns) (Record DBBuildingColumns)
buildingTable = Table "building" defaultRecTable
