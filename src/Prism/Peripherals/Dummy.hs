{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Peripherals.Dummy where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Prism.Cpu

import Prism.Peripherals.Types
import Prism.Peripherals.Builder

-------------------------------------------------------------------------------

data DummyCtx = DummyCtx {
        dummyVal :: Int
    } deriving (Show)

-------------------------------------------------------------------------------

newtype DummyTransM s m a = DummyTransM {
    runDummy :: (StateT s m) a
} deriving (Monad, Applicative, Functor, MonadState s)

instance MonadTrans (DummyTransM s) where
    lift = DummyTransM . lift

instance MonadIO m => MonadIO (DummyTransM s m) where
    liftIO = lift . liftIO

instance RunPeripheralsM DummyCtx (DummyTransM DummyCtx IO) PrismM where
    runPeripheralsM ctx c = do
        c1 <- ctxIO <$> get
        (res, iCtx) <- liftIO $ ((runStateT . runDummy $ c) ctx)
        let v = (dummyVal iCtx) + 1
        let ioCtx = IOCtx (iCtx { dummyVal = v })
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
        modify $ (\s -> s { ctxIO = ioCtx } )
        return res

type DummyTrans = DummyTransM DummyCtx IO

-------------------------------------------------------------------------------

instance Operand PortInternal8 DummyTrans Uint8 where
    readOp (PortInternal8 (index, port)) = return 0
    writeOp (PortInternal8 (index, port)) _ = return ()

instance Operand PortInternal16 DummyTrans Uint16 where
    readOp (PortInternal16 (index, port)) = return 0
    writeOp (PortInternal16 (index, port)) _ = return ()

-------------------------------------------------------------------------------

instance Operand MMIOInternal8 DummyTrans Uint8 where
    readOp (MMIOInternal8 (index, port)) = return 0
    writeOp (MMIOInternal8 (index, port)) _ = return ()

instance Operand MMIOInternal16 DummyTrans Uint16 where
    readOp (MMIOInternal16 (index, port)) = return 0
    writeOp (MMIOInternal16 (index, port)) _ = return ()

-------------------------------------------------------------------------------

instance InterruptDispatcher DummyTrans where
    dispatchIrqUp _ = return False
    dispatchIrqDown _ = return False
    ackIrq = return $ PrismInt 0

-------------------------------------------------------------------------------

instance PeripheralsMonad DummyTrans where
    runPeripherals = return ()

-------------------------------------------------------------------------------

makeDummyIO :: Int -> p -> IO (IOCtx, Peripheral IO p)
makeDummyIO memSize devices = do
    let peripheral = makeEmptyPeripherals memSize devices
        ioCtx = IOCtx (DummyCtx 0)
                      (peripheralMemRegion peripheral)
                      (peripheralPortRegion peripheral)
    return (ioCtx, peripheral)

-------------------------------------------------------------------------------
