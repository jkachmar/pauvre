{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import Servant

import Api.Building

import Config

--------------------------------------------------------------------------------

type Api =
  BuildingApi

api :: Proxy Api
api = Proxy

apiServer :: ServerT Api HandlerM
apiServer =
  buildingServer

--------------------------------------------------------------------------------

enterServer :: Config -> Server Api
enterServer cfg = enter (runReaderTNat cfg) apiServer

app :: Config -> Application
app cfg = serve api $ enterServer cfg
