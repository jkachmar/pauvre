{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Model.Region where

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

type ApiRegion       = '[ FRegionName ]
type DBRegionColumns = '[ CRegionId, CRegionName ]
type DBRegion        = '[ FRegionId, FRegionName ]

--------------------------------------------------------------------------------

newtype ApiRegionJson = ApiRegionJson { unApiRegionJson :: Record ApiRegion }
makeWrapped ''ApiRegionJson

apiRegionJsonFormat :: JsonFormat e ApiRegionJson
apiRegionJsonFormat =
  jsonFormatWithIso _Wrapped (recJsonFormat defaultJsonFormatRec)

instance ToJSON ApiRegionJson where
  toJSON = toJsonWithFormat apiRegionJsonFormat

instance FromJSON ApiRegionJson where
  parseJSON = parseJsonWithFormat' apiRegionJsonFormat

--------------------------------------------------------------------------------

regionTable :: Table (Record DBRegionColumns) (Record DBRegionColumns)
regionTable = Table "region" defaultRecTable
