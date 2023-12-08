module Prism.GDB (
        ------------------------------------------------------
        GDBServer (..)
        , GDBState (..)
        , runServer
        , gdbThread
        ------------------------------------------------------
    ) where

import Data.Maybe (maybe)

import Control.Monad.State
import Control.Monad.Logger (LogLevel)

import System.IO (Handle, stdout)

import Prism.GDB.Types
import Prism.GDB.Server
import Prism.GDB.Protocol
import Prism.GDB.Logger

-------------------------------------------------------------------------------

gdbThread :: LogLevel -> Maybe Handle -> String -> Int -> GDBState -> IO ()
gdbThread logLevel logFile address port state = do
    runStateT (runMyLoggingT logLevel logHandle (runGDB $ runServer address port)) state
    return ()
    where
        logHandle = maybe stdout id logFile

-------------------------------------------------------------------------------
