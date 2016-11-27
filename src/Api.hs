{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Servant

import           Api.Building
import           Api.Region

import           Config

--------------------------------------------------------------------------------

type Api =
       BuildingApi
  :<|> RegionApi

api :: Proxy Api
api = Proxy

apiServer :: ServerT Api HandlerM
apiServer =
       buildingServer
  :<|> regionServer

--------------------------------------------------------------------------------

enterServer :: Config -> Server Api
enterServer cfg = enter (runReaderTNat cfg) apiServer

app :: Config -> Application
app cfg = serve api $ enterServer cfg
