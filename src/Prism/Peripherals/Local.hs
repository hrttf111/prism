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
        localPeripheralPort :: PeripheralArray (PeripheralHandlerPort p),
        localPeripheralMem :: PeripheralArray (PeripheralHandlerMem p),
        localIOQueue :: IOQueue,
        localScheduler :: Scheduler (PeripheralsLocal p),
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
                    devices = localPeripherals peripherals
                (devices_, val) <- liftIO $ (peripheralMemRead8 handler) devices offset
                modify (\s -> s { localPeripherals = devices_ } )
                return val
    writeOp (MMIOInternal8 (handlerIndex, offset)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                devices_ <- liftIO $ (peripheralMemWrite8 handler) devices offset val
                modify (\s -> s { localPeripherals = devices_ } )

instance Operand MMIOInternal16 (LocalTrans p) Uint16 where
    readOp (MMIOInternal16 (handlerIndex, offset)) = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                (devices_, val) <- liftIO $ (peripheralMemRead16 handler) devices offset
                modify (\s -> s { localPeripherals = devices_ } )
                return val
    writeOp (MMIOInternal16 (handlerIndex, offset)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                devices_ <- liftIO $ (peripheralMemWrite16 handler) devices offset val
                modify (\s -> s { localPeripherals = devices_ } )

-------------------------------------------------------------------------------

instance Operand PortInternal8 (LocalTrans p) Uint8 where
    readOp (PortInternal8 (handlerIndex, port)) = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                (devices_, val) <- liftIO $ (peripheralPortRead8 handler) devices port
                modify (\s -> s { localPeripherals = devices_ } )
                return val
    writeOp (PortInternal8 (handlerIndex, port)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                devices_ <- liftIO $ (peripheralPortWrite8 handler) devices port val
                modify (\s -> s { localPeripherals = devices_ } )

instance Operand PortInternal16 (LocalTrans p) Uint16 where
    readOp (PortInternal16 (handlerIndex, port)) = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                (devices_, val) <- liftIO $ (peripheralPortRead16 handler) devices port
                modify (\s -> s { localPeripherals = devices_ } )
                return val
    writeOp (PortInternal16 (handlerIndex, port)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                    devices = localPeripherals peripherals
                devices_ <- liftIO $ (peripheralPortWrite16 handler) devices port val
                modify (\s -> s { localPeripherals = devices_ } )

-------------------------------------------------------------------------------
