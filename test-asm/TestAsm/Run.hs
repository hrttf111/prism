{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module TestAsm.Run where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Control.Concurrent

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

------------------------------------------------------------------------------

createTestEnv1 :: MonadIO m =>
                  IOCtx ->
                  Maybe ThreadId ->
                  [PrismInstruction] ->
                  [InterruptHandlerLocation] ->
                  PrismM () ->
                  m TestEnv
createTestEnv1 ioCtx threadId instrList intList preStartAction = liftIO $ do
    ptrA <- allocMemRegRaw
    memReg <- allocMemReg
    memMain <- allocMemMain memSize
    intM <- configureInterrupts memMain intHandlersOffset intList
    let decoder = makeDecoderList (instrList ++ (internalInstrList intM))
    asmTest <- makeAsmTest
    return $ TestEnv 
                threadId
                makeAsmStr
                makeAsmStr16
                (execNative asmTest ptrA)
                (execP memReg memMain decoder)
    where
        debugCtx = DebugCtx (\_ _ _ -> return ()) (\_ _ -> False)
        memSize = 65000
        codeStart = 12000 :: Uint16
        intHandlersOffset = 60000
        execNative asmTest ptrA mainCode =
            MemReg <$> execCode asmTest mainCode ptrA
        execP memReg memMain decoder mainCode runner = do
            let ctx = makeCtx memReg memMain ioCtx debugCtx
                instrEnd = fromIntegral codeStart + B.length mainCode
            runPrismM ctx $ func1 decoder mainCode instrEnd runner
        func1 decoder mainCode instrEnd runner = do
            clearRegs
            copyMainMem (fromIntegral codeStart) mainCode
            writeOp ss 1000
            writeOp sp 640
            writeOp ds (div 8000 16)
            writeOp cs (div codeStart 16)
            preStartAction
            runner decoder instrEnd

createPeripheralsTestEnv1 :: (MonadIO m, PeripheralsTestCreator mL pL) =>
                            [PrismInstruction] ->
                            pR ->
                            [PeripheralPort (RemoteTrans pR)] ->
                            [PeripheralMem (RemoteTrans pR)] ->
                            pL ->
                            [PeripheralPort mL] ->
                            [PeripheralMem mL] ->
                            [InterruptHandlerLocation] ->
                            PrismM () ->
                            m TestEnv
createPeripheralsTestEnv1 instrList devR portsR memsR devL portsL memsL intList preStartAction = do
    queue <- liftIO $ createIOQueue
    let ioCtx = createTestPeripherals peripheralL queue
    threadId <- liftIO . forkIO $ runRemotePeripherals queue peripheralR execPeripheralsOnce
    createTestEnv1 ioCtx (Just threadId) instrList intList preStartAction
    where
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

createPeripheralsTestEnv :: (MonadIO m, PeripheralsTestCreator mL pL) =>
                            [PrismInstruction] ->
                            pR ->
                            [PeripheralPort (RemoteTrans pR)] ->
                            [PeripheralMem (RemoteTrans pR)] ->
                            pL ->
                            [PeripheralPort mL] ->
                            [PeripheralMem mL] ->
                            [InterruptHandlerLocation] ->
                            m TestEnv
createPeripheralsTestEnv instrList devR portsR memsR devL portsL memsL intList =
    createPeripheralsTestEnv1 instrList devR portsR memsR devL portsL memsL intList (return ())

-------------------------------------------------------------------------------

type RegEqFunc = MemReg -> Expectation

execPrismHalt :: (HasCallStack) => [RegEqFunc] -> TestEnv -> PrismComm -> Text -> Expectation
execPrismHalt regs env comm cd = do
    code16 <- (assembleNative16 env) cd
    ctx <- (executePrism env) code16
        (\decoder _ -> decodeHaltCpu decoder comm)
    let memRegP = ctxReg ctx
    mapM_ (\f -> f memRegP) regs

-------------------------------------------------------------------------------

data PrismEnvMaker = PrismEnvMaker

instance TestEnvMaker PrismEnvMaker (TestEnv1 Ep.ExecutorPrism) where
    makeTestEnv _ = do
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList runner
        return $ TestEnv1 Nothing makeAsmStr16 prismExec
        where
            runner = decodeMemIp

data PrismEnvHaltMaker = PrismEnvHaltMaker

instance TestEnvMaker PrismEnvHaltMaker (TestEnv1 Ep.ExecutorPrism) where
    makeTestEnv _ = do
        comm <- newPrismComm False
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList (runner comm)
        return $ TestEnv1 Nothing makeAsmStr16 prismExec
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
        return $ TestEnv2 makeAsmStr16 prismExec Qemu.assembleQemu Qemu.ExecutorQemu
        where
            runner = decodeMemIp

data PrismNativeEnvMaker = PrismNativeEnvMaker

instance TestEnvMaker PrismNativeEnvMaker (TestEnv2 Ep.ExecutorPrism ExecNative.ExecutorNative) where
    makeTestEnv _ = do
        prismExec <- Ep.createPrismExecutorNoIO x86InstrList runner
        return $ TestEnv2 makeAsmStr16 prismExec ExecNative.assembleNative ExecNative.ExecutorNative
        where
            runner = decodeMemIp

-------------------------------------------------------------------------------

makePrismPeripheralsEnv :: (MonadIO m, PeripheralsTestCreator mL pL) =>
                            pR ->
                            [PeripheralPort (RemoteTrans pR)] ->
                            [PeripheralMem (RemoteTrans pR)] ->
                            pL ->
                            [PeripheralPort mL] ->
                            [PeripheralMem mL] ->
                            [InterruptHandlerLocation] ->
                            PrismM () ->
                            m (TestEnv1 Ep.ExecutorPrism)
makePrismPeripheralsEnv devR portsR memsR devL portsL memsL intList preStartAction = do
    comm <- liftIO $ newPrismComm False
    queue <- liftIO $ createIOQueue
    let ioCtx = createTestPeripherals peripheralL queue
    threadId <- liftIO . forkIO $ runRemotePeripherals queue peripheralR execPeripheralsOnce
    prismExec <- Ep.createPrismExecutor ioCtx x86InstrList intList debugCtx (runner comm)
    return $ TestEnv1 (Just threadId) makeAsmStr16 prismExec
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

-------------------------------------------------------------------------------
