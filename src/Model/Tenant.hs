{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Model.Tenant where

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

type ApiTenant =
  '[ FTenantName
   , FTenantBuildingId
   ]

type DBTenantColumns =
  '[ CTenantId
   , CTenantName
   , CTenantBuildingId
   ]

type DBTenant =
  '[ FTenantId
   , FTenantName
   , FTenantBuildingId
   ]

--------------------------------------------------------------------------------

newtype ApiTenantJson = ApiTenantJson { unApiTenantJson :: Record ApiTenant }
makeWrapped ''ApiTenantJson

apiTenantJsonFormat :: JsonFormat e ApiTenantJson
apiTenantJsonFormat =
  jsonFormatWithIso _Wrapped (recJsonFormat defaultJsonFormatRec)

instance ToJSON ApiTenantJson where
  toJSON = toJsonWithFormat apiTenantJsonFormat

instance FromJSON ApiTenantJson where
  parseJSON = parseJsonWithFormat' apiTenantJsonFormat

--------------------------------------------------------------------------------

tenantTable :: Table (Record DBTenantColumns) (Record DBTenantColumns)
tenantTable = Table "tenant" defaultRecTable
