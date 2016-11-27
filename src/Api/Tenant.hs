{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Tenant where

import           Data.Aeson                  (ToJSON)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql
import           GHC.Generics                (Generic)
import           Servant

import           Config
import           Model

--------------------------------------------------------------------------------

type TenantApi = "tenant" :> TenantApi'

type TenantApi' =
  Get '[JSON] [Entity Tenant]

  :<|> Capture "tenant_id" TenantParamId
    :> Get '[JSON] (Entity Tenant)

--------------------------------------------------------------------------------

newtype TenantParamId = TenantParamId Int64
  deriving (Eq, Ord, Show, Generic, FromHttpApiData, ToJSON)

--------------------------------------------------------------------------------

tenantServer :: ServerT TenantApi HandlerM
tenantServer =
       allTenants
  :<|> tenantFromId

allTenants :: HandlerM [Entity Tenant]
allTenants = runReadOnly (selectList [] [])

tenantFromId :: TenantParamId -> HandlerM (Entity Tenant)
tenantFromId (TenantParamId tenantId) = do
  tenant <-  runReadOnly (selectFirst
                          [TenantId ==. toSqlKey tenantId]
                          [])
  maybe (throwError err404) (pure) tenant
