module Prism.GDB (
        ------------------------------------------------------
        GDBServer (..)
        , GDBState (..)
        , runServer
        , gdbThread
        ------------------------------------------------------
    ) where

import Control.Monad.State
import Control.Monad.Logger (LogLevel)

import Prism.GDB.Types
import Prism.GDB.Server
import Prism.GDB.Protocol
import Prism.GDB.Logger

-------------------------------------------------------------------------------

gdbThread :: LogLevel -> GDBState -> IO ()
gdbThread logLevel state = do
    runStateT (runMyLoggingT logLevel (runGDB $ runServer "127.0.0.1" 20001)) state
    return ()

-------------------------------------------------------------------------------
