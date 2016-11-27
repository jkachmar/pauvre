{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Servant

import           Api.Building
import           Api.Region
import           Api.Tenant

import           Config

--------------------------------------------------------------------------------

type Api =
       BuildingApi
  :<|> RegionApi
  :<|> TenantApi

api :: Proxy Api
api = Proxy

apiServer :: ServerT Api HandlerM
apiServer =
       buildingServer
  :<|> regionServer
  :<|> tenantServer

--------------------------------------------------------------------------------

enterServer :: Config -> Server Api
enterServer cfg = enter (runReaderTNat cfg) apiServer

app :: Config -> Application
app cfg = serve api $ enterServer cfg
