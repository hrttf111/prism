{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Infra (
    runTest
    , ShouldEq(..), ShouldEqSources(..) 
    , GetVal(..)
    , MemRange(..), MemRangeDisp(..)
    , MemDisp8(..), MemDisp16(..)
    , MemRangeRes(..)
    , AllRegs
    , showAllRegs, showAllRegsL, showAllRegsR
    , shouldEqSourcesAllFlags
-------------------------------------------------------------------------------
    , PeripheralsTestCreator(..)
-------------------------------------------------------------------------------
    , PrismEnvMaker(..), PrismEnvPeripheralsMaker(..)
    , PrismQemuEnvMaker(..)
    , PrismNativeEnvMaker(..)
    , defaultPrismEnvPeriphMaker
-------------------------------------------------------------------------------
    , QemuEnvMaker(..)
) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Control.Concurrent (forkIO, forkFinally, takeMVar, putMVar, newEmptyMVar, ThreadId)

import Data.Text as T
import qualified Data.ByteString as B

import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Array (allocaArray, callocArray, pokeArray)
import Foreign.Marshal.Utils (fillBytes)

import Test.Hspec

import NeatInterpolation

import Prism.Cpu
import Prism.Decoder
import Prism.Run
import Prism.Command
import Prism.Peripherals
import Prism.Instructions (internalInstrList, x86InstrList)

import Infra.Types
import Infra.Assembler

import qualified Infra.ExecQemu as ExecQemu
import qualified Infra.ExecPrism as ExecPrism
import qualified Infra.ExecNative as ExecNative

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
        IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue emptyScheduler debugCtx devices) memRegion portRegion
        where
            debugCtx = DebugCtx (\_ _ _ -> return ()) (\_ _ -> False)

-------------------------------------------------------------------------------

makePrismExec :: IO (ExecPrism.ExecutorPrism, Text -> IO B.ByteString)
makePrismExec = do
    comm <- newPrismComm False
    prismExec <- ExecPrism.createPrismExecutorNoIO x86InstrList (runner comm)
    return (prismExec, asmFunc)
    where
        asmFooter = [untrimming|
            hlt
        |]
        asmFunc t = makeAsmStr $ T.append t asmFooter
        runner comm decoder _ = decodeHaltCpu decoder comm

data PrismEnvMaker = PrismEnvMaker

instance TestEnvMaker PrismEnvMaker (TestEnv1 ExecPrism.ExecutorPrism) where
    makeTestEnv _ = do
        (exec, func) <- makePrismExec
        return $ TestEnv1 Nothing func exec

data QemuEnvMaker = QemuEnvMaker

instance TestEnvMaker QemuEnvMaker (TestEnv1 ExecQemu.ExecutorQemu) where
    makeTestEnv _ = do
        return $ TestEnv1 Nothing ExecQemu.assembleQemu ExecQemu.ExecutorQemu

data PrismQemuEnvMaker = PrismQemuEnvMaker

instance TestEnvMaker PrismQemuEnvMaker (TestEnv2 ExecPrism.ExecutorPrism ExecQemu.ExecutorQemu) where
    makeTestEnv _ = do
        (exec, func) <- makePrismExec
        return $ TestEnv2 func exec ExecQemu.assembleQemu ExecQemu.ExecutorQemu

data PrismNativeEnvMaker = PrismNativeEnvMaker

instance TestEnvMaker PrismNativeEnvMaker (TestEnv2 ExecPrism.ExecutorPrism ExecNative.ExecutorNative) where
    makeTestEnv _ = do
        (exec, func) <- makePrismExec
        return $ TestEnv2 func exec ExecNative.assembleNative ExecNative.ExecutorNative

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

instance (PeripheralsTestCreator mL dL) => TestEnvMaker (PrismEnvPeripheralsMaker dR dL mL) (TestEnv1 ExecPrism.ExecutorPrism) where
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
        prismExec <- ExecPrism.createPrismExecutor ioCtx x86InstrList intList debugCtx (runner comm)
        return $ TestEnv1 (Just $ PeripheralThread threadId mvar) asmFunc prismExec
        where
            asmFooter = [untrimming|
                hlt
            |]
            asmFunc t = makeAsmStr $ T.append t asmFooter
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
