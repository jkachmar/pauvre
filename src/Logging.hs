{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Logging where

import           ClassyPrelude
import           Control.Monad.Logger  (Loc (loc_filename, loc_module, loc_package, loc_start),
                                        LogLevel, LogSource,
                                        LoggingT (runLoggingT))
import           Data.ByteString.Char8 (hPutStrLn)
import           System.IO             (stderr)
import           System.Log.FastLogger (LogStr, ToLogStr (toLogStr), fromLogStr)

--------------------------------------------------------------------------------

-- | An alias for the long type that LoggingT requires to log messages
type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Create a logging context from a couple of logging settings
withLogger :: (MonadMask m, MonadIO m) => LoggingT m a -> m a
withLogger action = runLoggingT action logMsg

-- | Create a logging context with an existing `LogFunction`
withLoggingFunc :: LogFunction -> LoggingT m a -> m a
withLoggingFunc = flip runLoggingT

logMsg :: LogFunction
logMsg loc _ level msg = do
  dateLogStr <- nowLogString
  hPutStrLn stderr . fromLogStr $ dateLogStr <> " [" <> (toLogStr . show) level <> "] " <> msg <> " @(" <> locLogString loc <> ")"

locLogString :: Loc -> LogStr
locLogString loc = p <> ":" <> m <> " " <> f <> ":" <> l <> ":" <> c
  where p = toLogStr . loc_package $ loc
        m = toLogStr . loc_module $ loc
        f = toLogStr . loc_filename $ loc
        l = toLogStr . show . fst . loc_start $ loc
        c = toLogStr . show . snd . loc_start $ loc

nowLogString :: IO LogStr
nowLogString = do
  now <- getCurrentTime
  pure . toLogStr $ formatTime defaultTimeLocale "%Y-%m-%d %T%Q" now
