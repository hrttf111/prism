{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Peripherals.Local where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import qualified Data.Array as Array

import Prism.Cpu

import Prism.Peripherals.Types
import Prism.Peripherals.Builder
import Prism.Peripherals.Queue

-------------------------------------------------------------------------------

data PeripheralsLocal p = PeripheralsLocal {
        localMaxPort :: IOHandlerIndex,
        localMaxMem :: IOHandlerIndex,
        localPeripheralPort :: PeripheralArray (PeripheralHandlerPort p),
        localPeripheralMem :: PeripheralArray (PeripheralHandlerMem p),
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

{-instance RunPeripheralsM LocalCtx (LocalTransM LocalCtx IO) PrismM where
    runPeripheralsM ctx c = do
        c1 <- ctxIO <$> get
        (res, iCtx) <- liftIO $ ((runStateT . runLocal $ c) ctx)
        let v = (dummyVal iCtx) + 1
        let ioCtx = IOCtx (iCtx { dummyVal = v })
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
        modify $ (\s -> s { ctxIO = ioCtx } )
        return res
        -}

type LocalTrans p = LocalTransM (PeripheralsLocal p) IO

-------------------------------------------------------------------------------
{-
instance Operand MMIOInternal8 (PeripheralsLocal p) Uint8 where
    readOp (MMIOInternal8 (index, port)) =
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else liftIO $ do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                devices <- readIORef $ localPeripherals peripherals
                (devices_, val) <- ioValMemRead devices handler offset
                writeIORef (localPeripherals peripherals) devices_
                return val
    writeOp (MMIOInternal8 (index, port)) _ = return ()

instance Operand MMIOInternal16 DummyTrans Uint16 where
    readOp (MMIOInternal16 (index, port)) = return 0
    writeOp (MMIOInternal16 (index, port)) _ = return ()
-}
-------------------------------------------------------------------------------
