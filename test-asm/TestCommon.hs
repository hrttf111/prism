{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module TestCommon where

import Test.Hspec
import Test.Hspec.Expectations

import NeatInterpolation
import Data.Text (Text)
import qualified Data.ByteString as B

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent

import Data.IORef
import Data.Word (Word8, Word16)
import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Array (allocaArray, callocArray, pokeArray)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Utils (fillBytes)

import Prism
import PrismCpu
import PrismDecoder
import PrismPeripheral
import PrismInterrupt
import PrismCommand
import PrismRun

import Peripherals.Remote
import Peripherals.Local
import Instruction.Processor

import Assembler

-------------------------------------------------------------------------------

data TestDev = TestDev deriving (Show)

type PeripheralsTest = PeripheralsLocal TestDev

instance InterruptDispatcher PeripheralsTest where
    dispatchInterruptUp peripherals int = return (peripherals, False)
    dispatchInterruptDown peripherals int = return (peripherals, False)
    ackInterrupt peripherals = return (peripherals, PrismInt 7)

instance PeripheralRunner PeripheralsTest where
    runPeripherals ctx peripherals = return (ctx, peripherals)
    peripheralCycles peripherals = return 99999999
    needUpdate peripherals = return False


class PeripheralsTestCreator p where
    createTestPeripherals :: PeripheralLocal p -> IOQueue -> IO IOCtx

instance PeripheralsTestCreator TestDev where
    createTestPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue = do
        ref <- newIORef devices
        return $ IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue ref) memRegion portRegion

-------------------------------------------------------------------------------

type CodeExecutor = (Text -> IO MemReg)
type PrismRunner = PrismDecoder -> Int -> Ctx -> PrismCtx IO Ctx

data TestEnv = TestEnv {
        peripheralThreadId :: Maybe ThreadId,
        assembleNative :: (Text -> IO B.ByteString),
        assembleNative16 :: (Text -> IO B.ByteString),
        executeNative :: (B.ByteString -> IO MemReg),
        executePrism :: (B.ByteString -> PrismRunner -> IO Ctx)
    }

createTestEnv1 :: MonadIO m =>
                  IOCtx ->
                  Maybe ThreadId ->
                  [PrismInstruction] ->
                  [InterruptHandlerLocation] ->
                  m TestEnv
createTestEnv1 ioCtx threadId instrList intList = liftIO $ do
    ptrReg <- callocBytes 64
    ptrMem <- callocBytes memSize
    intM <- configureInterrups (MemMain ptrMem) intHandlersOffset intList
    let decoder = makeDecoderList (instrList ++ (internalInstrList intM))
    asmTest <- makeAsmTest
    ptrA <- callocArray 64
    return $ TestEnv 
                threadId
                makeAsmStr
                makeAsmStr16
                (execNative asmTest ptrA)
                (execP ptrReg ptrMem decoder)
    where
        memSize = 65000
        codeStart = 12000
        intHandlersOffset = 60000
        execNative asmTest ptrA mainCode = MemReg <$> execCode asmTest mainCode ptrA
        execP ptrReg ptrMem decoder mainCode runner = do
            let codeLen = B.length mainCode
                ctx = makePrismCtx (MemReg ptrReg) (MemMain ptrMem) ioCtx
                array = B.unpack mainCode
                f (MemMain p) = p
                f1 (MemReg p) = p
            fillBytes (f1 $ ctxReg ctx) 0 64
            pokeArray (flip plusPtr (fromIntegral codeStart) $ f $ ctxMem ctx) array
            runPrism $ execPrism decoder codeLen ctx runner
        execPrism decoder codeLen ctx runner = do
            writeSeg (ctxReg ctx) ss 1000
            writeReg16 (ctxReg ctx) sp 640
            writeSeg (ctxReg ctx) ds 8000
            writeSeg (ctxReg ctx) cs (div codeStart 16)
            runner decoder (fromIntegral codeStart + codeLen) ctx

createTestEnv :: MonadIO m => [PrismInstruction] -> m TestEnv
createTestEnv instrList = do
    (ioCtx, _) <- liftIO $ makeEmptyIO (1024*1024) devicesStub
    createTestEnv1 ioCtx Nothing combinedList []
    where
        combinedList = instrList ++ (segmentInstrList instrList)
        devicesStub = 0 :: Int


createPeripheralsTestEnv :: (MonadIO m, PeripheralsTestCreator p) => 
                            [PrismInstruction] -> 
                            pR ->
                            [PeripheralPort pR] ->
                            [PeripheralMem pR] ->
                            p ->
                            [PeripheralPort p] ->
                            [PeripheralMem p] ->
                            [InterruptHandlerLocation] ->
                            m TestEnv
createPeripheralsTestEnv instrList devR portsR memsR devL portsL memsL intList = do
    queue <- liftIO $ createIOQueue
    ioCtx <- liftIO $ createTestPeripherals peripheralL queue
    threadId <- liftIO . forkIO $ execPeripheralsOnce queue peripheralR
    createTestEnv1 ioCtx (Just threadId) combinedList intList
    where
        memSize = 1024 * 1024
        pageSize = 1024
        combinedList = instrList ++ (segmentInstrList instrList)
        (peripheralR, peripheralL) = createPeripheralsLR devR devL memSize pageSize portsR memsR portsL memsL


class RegTest a where
    shouldEq :: (HasCallStack) => a -> Int -> MemReg -> Expectation
    shouldEqReg :: (HasCallStack) => a -> MemReg -> MemReg -> Expectation

instance RegTest Reg8 where
    shouldEq reg valExp memReg = do
        val <- readReg8 memReg reg
        val `shouldBe` (fromIntegral valExp)
    shouldEqReg reg memReg1 memReg2 = do
        val1 <- readReg8 memReg1 reg
        val2 <- readReg8 memReg2 reg
        val1 `shouldBe` val2

instance RegTest Reg16 where
    shouldEq reg valExp memReg = do
        val <- readReg16 memReg reg
        val `shouldBe` (fromIntegral valExp)
    shouldEqReg reg memReg1 memReg2 = do
        val1 <- readReg16 memReg1 reg
        val2 <- readReg16 memReg2 reg
        val1 `shouldBe` val2

flagsShouldEq :: (HasCallStack) => Flags -> MemReg -> Expectation
flagsShouldEq flags memReg = do
    (flagsN, _) <- readFlags memReg
    flags `shouldBe` flagsN

type RegEqFunc = MemReg -> Expectation

execPrism :: (HasCallStack) => [RegEqFunc] -> TestEnv -> Text -> Expectation
execPrism regs env cd = do
    code16 <- (assembleNative16 env) cd
    ctx <- (executePrism env) code16 decodeMemIp
    let memRegP = ctxReg ctx
    mapM_ (\f -> f memRegP) regs

execPrismHalt :: (HasCallStack) => [RegEqFunc] -> TestEnv -> PrismComm -> Text -> Expectation
execPrismHalt regs env comm cd = do
    code16 <- (assembleNative16 env) cd
    ctx <- (executePrism env) code16
        (\decoder _ ctx -> decodeHaltCpu decoder comm ctx)
    let memRegP = ctxReg ctx
    mapM_ (\f -> f memRegP) regs

execAndCmp :: (HasCallStack, RegTest a) => [a] -> TestEnv -> Text -> Expectation
execAndCmp regs env cd = do
    code <- (assembleNative env) cd
    code16 <- (assembleNative16 env) cd
    memRegN <- (executeNative env) code
    ctx <- (executePrism env) code16 decodeMemIp
    let memRegP = ctxReg ctx
    mapM_ (\r -> r `shouldEqReg` memRegP $ memRegN) regs
    (ctxFlags ctx) `flagsShouldEq` memRegN

execAndCmpNF :: (HasCallStack, RegTest a) => [a] -> TestEnv -> Text -> Expectation
execAndCmpNF regs env cd = do
    code <- (assembleNative env) cd
    code16 <- (assembleNative16 env) cd
    memRegN <- (executeNative env) code
    ctx <- (executePrism env) code16 decodeMemIp
    let memRegP = ctxReg ctx
    mapM_ (\r -> r `shouldEqReg` memRegP $ memRegN) regs

execCodeTest :: MonadIO m => AsmTest -> MemReg -> Text -> m MemReg
execCodeTest asmTest (MemReg ptrA) code = liftIO $ do
    mainCode <- makeAsmStr code
    execCode asmTest mainCode ptrA
    return $ MemReg ptrA
