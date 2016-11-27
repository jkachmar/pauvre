module Main where

import           Network.Wai.Handler.Warp (run)
import           Safe                     (readMay)
import           System.Environment       (lookupEnv)

import           Api                      (app)
import           Config

main :: IO ()
main = do
  env  <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8081
  pool <- makePool env

  let cfg = Config { getPool = pool, getEnv = env }
      logger = setLogger env

  run port $ logger $ app cfg

-- | Helper function to handle looking up env vars
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      pure def
    Just str ->
      maybe (handleFailedRead str) pure (readMay str)

  where
    handleFailedRead str =
      error $ mconcat
          [ "Failed to read [["
          , str
          , "]] for environment variable "
          , env
          ]
