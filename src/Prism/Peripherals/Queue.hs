{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Prism.Peripherals.Queue where

import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Exception (throwIO)

import Prism.Cpu
import Prism.Peripherals.Types

-------------------------------------------------------------------------------

data IOCmdType = IOMemType | IOPortType deriving (Show)

data IOCmd = IOCmdRead8 IOCmdType IOHandlerIndex MemOffset
             | IOCmdRead16 IOCmdType IOHandlerIndex MemOffset 
             | IOCmdWrite8 IOCmdType IOHandlerIndex MemOffset Uint8 
             | IOCmdWrite16 IOCmdType IOHandlerIndex MemOffset Uint16 
             deriving (Show)

data IOCmdData = IOCmdData8 Uint8
                 | IOCmdData16 Uint16

data IOQueue = IOQueue {
        ioQueueReq :: TQueue IOCmd,
        ioQueueRsp :: TQueue IOCmdData
    }

createIOQueue = IOQueue <$> newTQueueIO <*> newTQueueIO

-------------------------------------------------------------------------------

class (OperandVal a) => IOValRemote a where
    ioValRemoteRead :: (MonadIO m) => IOQueue -> IOCmdType -> IOHandlerIndex -> MemOffset -> m a
    ioValRemoteWrite :: (MonadIO m) => IOQueue -> IOCmdType -> IOHandlerIndex -> MemOffset -> a -> m ()
    ioValRemoteRespond :: (MonadIO m) => IOQueue -> a -> m ()

-------------------------------------------------------------------------------

instance IOValRemote Uint8 where
    ioValRemoteRead (IOQueue req rsp) cmdType handler offset = liftIO $ do
        atomically $ writeTQueue req $ IOCmdRead8 cmdType handler offset
        val <- atomically $ readTQueue rsp
        case val of
            IOCmdData8 d -> return d
            _ -> throwIO IOCtxException
    ioValRemoteWrite (IOQueue req _) cmdType handler offset val = liftIO $ do
        atomically $ writeTQueue req $ IOCmdWrite8 cmdType handler offset val
    ioValRemoteRespond (IOQueue _ rsp) val = liftIO $ do
        atomically $ writeTQueue rsp $ IOCmdData8 val

instance IOValRemote Uint16 where
    ioValRemoteRead (IOQueue req rsp) cmdType handler offset = liftIO $ do
        atomically $ writeTQueue req $ IOCmdRead16 cmdType handler offset
        val <- atomically $ readTQueue rsp
        case val of
            IOCmdData16 d -> return d
            _ -> throwIO IOCtxException
    ioValRemoteWrite (IOQueue req _) cmdType handler offset val = liftIO $ do
        atomically $ writeTQueue req $ IOCmdWrite16 cmdType handler offset val
    ioValRemoteRespond (IOQueue _ rsp) val = liftIO $ do
        atomically $ writeTQueue rsp $ IOCmdData16 val

-------------------------------------------------------------------------------
