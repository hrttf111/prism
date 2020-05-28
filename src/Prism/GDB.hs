module Prism.GDB (
        ------------------------------------------------------
        GDBServer (..)
        , GDBState (..)
        , runServer
        , gdbThread
        ------------------------------------------------------
    ) where

import Control.Monad.State

import Prism.GDB.Types
import Prism.GDB.Server
import Prism.GDB.Protocol
import Prism.GDB.Logger

-------------------------------------------------------------------------------

gdbThread :: GDBState -> IO ()
gdbThread state = do
    runStateT (runMyLoggingT (runGDB $ runServer "127.0.0.1" 20001)) state
    return ()

-------------------------------------------------------------------------------
