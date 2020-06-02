{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Peripherals.Remote where

import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM.TQueue

import qualified Data.Array as Array

import Prism.Cpu
import Prism.Peripherals.Types
import Prism.Peripherals.Queue

-------------------------------------------------------------------------------

data PeripheralsRemote p = PeripheralsRemote {
        remotePeripheralPort :: PeripheralArray (PeripheralHandlerPort (RemoteTrans p)),
        remotePeripheralMem :: PeripheralArray (PeripheralHandlerMem (RemoteTrans p)),
        remoteIOQueue :: IOQueue,
        remotePeripherals :: p
    }

newtype RemoteTransM s m a = RemoteTransM {
    runRemote :: (StateT s m) a
} deriving (Monad, Applicative, Functor, MonadState s)

instance MonadTrans (RemoteTransM s) where
    lift = RemoteTransM . lift

instance MonadIO m => MonadIO (RemoteTransM s m) where
    liftIO = lift . liftIO

type RemoteTrans p = RemoteTransM (PeripheralsRemote p) IO

-------------------------------------------------------------------------------

runRemotePeripherals :: IOQueue -> Peripheral (RemoteTrans p) p -> RemoteTrans p () -> IO ()
runRemotePeripherals queue peripherals actions = do
    putStrLn "Tread start"
    ((runStateT . runRemote $ actions) remote)
    putStrLn "Thread end"
    where
        remote = PeripheralsRemote (peripheralPort peripherals)
                                   (peripheralMem peripherals)
                                   queue
                                   (peripheralDevices peripherals)

execPeripheralsOnce :: RemoteTrans p ()
execPeripheralsOnce = do
    queue@(IOQueue req _) <- remoteIOQueue <$> get
    msg <- liftIO $ atomically $ readTQueue req
    liftIO $ putStrLn $ "Got message " ++ (show msg)
    case msg of
        IOCmdRead8 IOMemType handlerIndex memOffset -> do
            val <- readOp $ MMIOInternal8 (handlerIndex, memOffset)
            ioValRemoteRespond queue val
        IOCmdRead16 IOMemType handlerIndex memOffset -> do
            val <- readOp $ MMIOInternal16 (handlerIndex, memOffset)
            ioValRemoteRespond queue val
        IOCmdWrite8 IOMemType handlerIndex memOffset val ->
            writeOp (MMIOInternal8 (handlerIndex, memOffset)) val
        IOCmdWrite16 IOMemType handlerIndex memOffset val ->
            writeOp (MMIOInternal16 (handlerIndex, memOffset)) val
        IOCmdRead8 IOPortType handlerIndex memOffset -> do
            val <- readOp $ PortInternal8 (handlerIndex, fromIntegral memOffset)
            ioValRemoteRespond queue val
        IOCmdRead16 IOPortType handlerIndex memOffset -> do
            val <- readOp $ PortInternal16 (handlerIndex, fromIntegral memOffset)
            ioValRemoteRespond queue val
        IOCmdWrite8 IOPortType handlerIndex memOffset val -> do
            writeOp (PortInternal8 (handlerIndex, fromIntegral memOffset)) val
        IOCmdWrite16 IOPortType handlerIndex memOffset val -> do
            writeOp (PortInternal16 (handlerIndex, fromIntegral memOffset)) val
    return ()

-------------------------------------------------------------------------------

instance Operand MMIOInternal8 (RemoteTrans p) Uint8 where
    readOp (MMIOInternal8 (handlerIndex, offset)) = do
        peripherals <- get
        let handler = (remotePeripheralMem peripherals) Array.! handlerIndex
        (peripheralMemRead8 handler) offset
    writeOp (MMIOInternal8 (handlerIndex, offset)) val = do
        peripherals <- get
        let handler = (remotePeripheralMem peripherals) Array.! handlerIndex
        (peripheralMemWrite8 handler) offset val

instance Operand MMIOInternal16 (RemoteTrans p) Uint16 where
    readOp (MMIOInternal16 (handlerIndex, offset)) = do
        peripherals <- get
        let handler = (remotePeripheralMem peripherals) Array.! handlerIndex
        (peripheralMemRead16 handler) offset
    writeOp (MMIOInternal16 (handlerIndex, offset)) val = do
        peripherals <- get
        let handler = (remotePeripheralMem peripherals) Array.! handlerIndex
        (peripheralMemWrite16 handler) offset val

-------------------------------------------------------------------------------

instance Operand PortInternal8 (RemoteTrans p) Uint8 where
    readOp (PortInternal8 (handlerIndex, port)) = do
        peripherals <- get
        let handler = (remotePeripheralPort peripherals) Array.! handlerIndex
        (peripheralPortRead8 handler) port
    writeOp (PortInternal8 (handlerIndex, port)) val = do
        peripherals <- get
        let handler = (remotePeripheralPort peripherals) Array.! handlerIndex
        (peripheralPortWrite8 handler) port val

instance Operand PortInternal16 (RemoteTrans p) Uint16 where
    readOp (PortInternal16 (handlerIndex, port)) = do
        peripherals <- get
        let handler = (remotePeripheralPort peripherals) Array.! handlerIndex
        (peripheralPortRead16 handler) port
    writeOp (PortInternal16 (handlerIndex, port)) val = do
        peripherals <- get
        let handler = (remotePeripheralPort peripherals) Array.! handlerIndex
        (peripheralPortWrite16 handler) port val

-------------------------------------------------------------------------------
