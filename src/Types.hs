{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Types where

import           ClassyPrelude
import           Composite.TH      (withProxies)
import           Frames            ((:->))
import           Opaleye           (Column, Nullable, PGInt8, PGText)

--------------------------------------------------------------------------------

withProxies [d|
  type FRegionId   = "id"   :-> Int64
  type CRegionId   = "id"   :-> Column PGInt8
  type FRegionName = "name" :-> Text
  type CRegionName = "name" :-> Column PGText

  type FBuildingId        = "id"        :-> Int64
  type CBuildingId        = "id"        :-> Column PGInt8
  type FBuildingName      = "name"      :-> Text
  type CBuildingName      = "name"      :-> Column PGText
  type FBuildingAddress   = "address"   :-> Text
  type CBuildingAddress   = "address"   :-> Column PGText
  type FBuildingState     = "state"     :-> Text
  type CBuildingState     = "state"     :-> Column PGText
  type FBuildingLatitude  = "latitude"  :-> Text
  type CBuildingLatitude  = "latitude"  :-> Column PGText
  type FBuildingLongitude = "longitude" :-> Text
  type CBuildingLongitude = "longitude" :-> Column PGText
  type FBuildingRegionId  = "region_id" :-> Int64
  type CBuildingRegionId  = "region_id" :-> Column (Nullable PGText)

  type FTenantId         = "id"          :-> Int64
  type CTenantId         = "id"          :-> Column PGInt8
  type FTenantName       = "name"        :-> Text
  type CTenantName       = "name"        :-> Column PGText
  type FTenantBuildingId = "building_id" :-> Int64
  type CTenantBuildingId = "building_id" :-> Column (Nullable PGText)
  |]
