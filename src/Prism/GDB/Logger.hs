{-# LANGUAGE OverloadedStrings #-}

module Prism.GDB.Logger where

import Control.Monad.Logger
import Control.Monad.Trans (MonadIO, liftIO)

import qualified Data.ByteString.Char8 as S8
import Data.Time.Format
import qualified Data.Time.Clock.POSIX as PTime
import qualified Data.Text as T

import System.IO (FilePath, stdout, Handle, withFile, IOMode(..))
import System.Log.FastLogger (fromLogStr)

-------------------------------------------------------------------------------

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

makeLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> LogStr
makeLogStr loc src level msg =
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "]" `mappend`
    (if isDefaultLoc loc
        then " : "
        else
            " (" `mappend`
            toLogStr (S8.pack fileLocStr) `mappend`
            ") : ")
    `mappend` msg `mappend` "\n"
  where
    fileLocStr = (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

logToStdout :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
logToStdout h loc src level msg = do
    date <- PTime.getCurrentTime
    let str = formatTime defaultTimeLocale "%H:%M:%S " date
    S8.hPutStr h $ S8.pack str <> ls
  where
    ls = fromLogStr $ makeLogStr loc src level msg

runMyLoggingT :: MonadIO m => LogLevel -> LoggingT m a -> m a
runMyLoggingT logLevel = (`runLoggingT` logToStdout stdout) . filterLogger filterL
    where
        filterL _ level = logLevel == level

-------------------------------------------------------------------------------
