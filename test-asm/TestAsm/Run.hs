{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module TestAsm.Run where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Control.Concurrent (forkIO, forkFinally, takeMVar, putMVar, newEmptyMVar, ThreadId)

import Data.Text (Text)
import qualified Data.ByteString as B

import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Array (allocaArray, callocArray, pokeArray)
import Foreign.Marshal.Utils (fillBytes)

import Test.Hspec

import Prism.Cpu
import Prism.Decoder
import Prism.Run
import Prism.Command
import Prism.Peripherals
import Prism.Instructions (internalInstrList, x86InstrList)

import TestAsm.Common
import Assembler

import qualified Qemu
import qualified ExecPrism as Ep
import qualified ExecNative

-------------------------------------------------------------------------------

data TestDev = TestDev deriving (Show)

type PeripheralsTest1 = PeripheralsLocal TestDev
type PeripheralsTest = LocalTrans TestDev

instance InterruptDispatcher PeripheralsTest where
    dispatchIrqUp int = return False
    dispatchIrqDown int = return False
    ackIrq = return $ PrismInt 7

instance PeripheralsMonad PeripheralsTest where
    runPeripherals = return ()

instance RunPeripheralsM PeripheralsTest1 PeripheralsTest PrismM where
    runPeripheralsM ctx c = do
        c1 <- ctxIO <$> get
        (res, iCtx) <- liftIO $ ((runStateT . runLocal $ c) ctx)
        let ioCtx = IOCtx iCtx
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
        modify $ (\s -> s { ctxIO = ioCtx } )
        return res

instance RunPeripheralsDirect PeripheralsTest1 PrismM where
    runPeripheralsDirect ctx command = return ()

class PeripheralsTestCreator m p | p -> m where
    createTestPeripherals :: PeripheralLocal m p -> IOQueue -> IOCtx

instance PeripheralsTestCreator PeripheralsTest TestDev where
    createTestPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue =
        IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue emptyScheduler devices) memRegion portRegion

-------------------------------------------------------------------------------

data PrismEnvMaker = PrismEnvMaker

instance TestEnvMaker PrismEnvMaker (TestEnv1 Ep.ExecutorPrism) where
    makeTestEnv _ = do
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList runner
        return $ TestEnv1 Nothing makeAsmStr prismExec
        where
            runner = decodeMemIp

data PrismEnvHaltMaker = PrismEnvHaltMaker

instance TestEnvMaker PrismEnvHaltMaker (TestEnv1 Ep.ExecutorPrism) where
    makeTestEnv _ = do
        comm <- newPrismComm False
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList (runner comm)
        return $ TestEnv1 Nothing makeAsmStr prismExec
        where
            runner comm decoder _ = decodeHaltCpu decoder comm

data QemuEnvMaker = QemuEnvMaker

instance TestEnvMaker QemuEnvMaker (TestEnv1 Qemu.ExecutorQemu) where
    makeTestEnv _ = do
        return $ TestEnv1 Nothing Qemu.assembleQemu Qemu.ExecutorQemu

data PrismQemuEnvMaker = PrismQemuEnvMaker

instance TestEnvMaker PrismQemuEnvMaker (TestEnv2 Ep.ExecutorPrism Qemu.ExecutorQemu) where
    makeTestEnv _ = do
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList runner
        return $ TestEnv2 makeAsmStr prismExec Qemu.assembleQemu Qemu.ExecutorQemu
        where
            runner = decodeMemIp

data PrismNativeEnvMaker = PrismNativeEnvMaker

instance TestEnvMaker PrismNativeEnvMaker (TestEnv2 Ep.ExecutorPrism ExecNative.ExecutorNative) where
    makeTestEnv _ = do
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList runner
        return $ TestEnv2 makeAsmStr prismExec ExecNative.assembleNative ExecNative.ExecutorNative
        where
            runner = decodeMemIp

-------------------------------------------------------------------------------

data PrismEnvPeripheralsMaker devRemote devLocal memLocal = PrismEnvPeripheralsMaker {
    prismEnvPeriphDevRemote :: devRemote,
    prismEnvPeriphpPortsRemote :: [PeripheralPort (RemoteTrans devRemote)],
    prismEnvPeriphpMemRemote :: [PeripheralMem (RemoteTrans devRemote)],
    prismEnvPeriphpDevLocal :: devLocal,
    prismEnvPeriphpPortsLocal :: [PeripheralPort memLocal],
    prismEnvPeriphpMemLocal :: [PeripheralMem memLocal],
    prismEnvPeriphpInterrupts :: [InterruptHandlerLocation],
    prismEnvPeriphpPreStartAction :: PrismM (),
    prismEnvPeriphpComm :: Maybe PrismComm
}

instance (PeripheralsTestCreator mL dL) => TestEnvMaker (PrismEnvPeripheralsMaker dR dL mL) (TestEnv1 Ep.ExecutorPrism) where
    makeTestEnv (PrismEnvPeripheralsMaker devR portsR memsR devL portsL memsL intList preStartAction maybeComm) = do
        comm <- (case maybeComm of
            Just c -> return c
            Nothing -> liftIO $ newPrismComm False
            )
        queue <- liftIO $ createIOQueue
        let ioCtx = createTestPeripherals peripheralL queue
        mvar <- liftIO $ newEmptyMVar
        threadId <- liftIO $ forkFinally (do
            runRemotePeripherals queue peripheralR execPeripheralsOnce
            ) (\_ -> putMVar mvar ())
        prismExec <- Ep.createPrismExecutor ioCtx x86InstrList intList debugCtx (runner comm)
        return $ TestEnv1 (Just $ PeripheralThread threadId mvar) makeAsmStr prismExec
        where
            debugCtx = DebugCtx (\_ _ _ -> return ()) (\_ _ -> False)
            runner comm decoder _ = do
                preStartAction
                decodeHaltCpu decoder comm
            memSize = 1024 * 1024
            pageSize = 1024
            (peripheralR, peripheralL) = createPeripheralsLR devR
                                                             devL
                                                             memSize
                                                             pageSize
                                                             portsR
                                                             memsR
                                                             portsL
                                                             memsL

defaultPrismEnvPeriphMaker = PrismEnvPeripheralsMaker TestDev
                                                      []
                                                      []
                                                      TestDev
                                                      []
                                                      []
                                                      []
                                                      (return ())
                                                      Nothing

-------------------------------------------------------------------------------
