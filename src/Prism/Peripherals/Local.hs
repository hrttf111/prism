{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Peripherals.Local where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import qualified Data.Array as Array

import Prism.Cpu

import Prism.Peripherals.Types
import Prism.Peripherals.Builder
import Prism.Peripherals.Queue
import Prism.Peripherals.Scheduler

-------------------------------------------------------------------------------

data PeripheralsLocal p = PeripheralsLocal {
        localMaxPort :: IOHandlerIndex,
        localMaxMem :: IOHandlerIndex,
        localPeripheralPort :: PeripheralArray (PeripheralHandlerPort (LocalTrans p)),
        localPeripheralMem :: PeripheralArray (PeripheralHandlerMem (LocalTrans p)),
        localIOQueue :: IOQueue,
        localPeripherals :: p
    }

-------------------------------------------------------------------------------

newtype LocalTransM s m a = LocalTransM {
    runLocal :: (StateT s m) a
} deriving (Monad, Applicative, Functor, MonadState s)

instance MonadTrans (LocalTransM s) where
    lift = LocalTransM . lift

instance MonadIO m => MonadIO (LocalTransM s m) where
    liftIO = lift . liftIO

{-
instance RunPeripheralsM (PeripheralsLocal p) (LocalTransM (PeripheralsLocal p) IO) PrismM where
    runPeripheralsM ctx c = do
        c1 <- ctxIO <$> get
        (res, iCtx) <- liftIO $ ((runStateT . runLocal $ c) ctx)
        let ioCtx = IOCtx iCtx
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
        modify $ (\s -> s { ctxIO = ioCtx } )
        return res
-}

type LocalTrans p = LocalTransM (PeripheralsLocal p) IO

-------------------------------------------------------------------------------

instance Operand MMIOInternal8 (LocalTrans p) Uint8 where
    readOp (MMIOInternal8 (handlerIndex, offset)) = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemRead8 handler) offset
    writeOp (MMIOInternal8 (handlerIndex, offset)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemWrite8 handler) offset val

instance Operand MMIOInternal16 (LocalTrans p) Uint16 where
    readOp (MMIOInternal16 (handlerIndex, offset)) = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemRead16 handler) offset
    writeOp (MMIOInternal16 (handlerIndex, offset)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemWrite16 handler) offset val

-------------------------------------------------------------------------------

instance Operand PortInternal8 (LocalTrans p) Uint8 where
    readOp (PortInternal8 (handlerIndex, port)) = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortRead8 handler) port
    writeOp (PortInternal8 (handlerIndex, port)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortWrite8 handler) port val

instance Operand PortInternal16 (LocalTrans p) Uint16 where
    readOp (PortInternal16 (handlerIndex, port)) = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortRead16 handler) port
    writeOp (PortInternal16 (handlerIndex, port)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortWrite16 handler) port val

-------------------------------------------------------------------------------
