{-# LANGUAGE FlexibleContexts #-}

module TestAsm.Run where

import Control.Monad.Trans (MonadIO, liftIO)

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

import TestAsm.Common
import Assembler

-------------------------------------------------------------------------------

createTestEnv1 :: MonadIO m =>
                  --IOCtx ->
                  Maybe ThreadId ->
                  [PrismInstruction] ->
                  --[InterruptHandlerLocation] ->
                  m TestEnv
createTestEnv1 threadId instrList = liftIO $ do
    ptrA <- allocMemRegRaw
    memReg <- allocMemReg
    memMain <- allocMemMain memSize
    --intM <- configureInterrups (MemMain ptrMem) intHandlersOffset intList
    --let decoder = makeDecoderList (instrList ++ (internalInstrList intM))
    let decoder = makeDecoderList instrList
    asmTest <- makeAsmTest
    return $ TestEnv 
                threadId
                makeAsmStr
                makeAsmStr16
                (execNative asmTest ptrA)
                (execP memReg memMain decoder)
    where
        memSize = 65000
        codeStart = 12000 :: Uint16
        intHandlersOffset = 60000
        execNative asmTest ptrA mainCode =
            MemReg <$> execCode asmTest mainCode ptrA
        execP memReg memMain decoder mainCode runner = do
            let ctx = makeCtx memReg memMain --ioCtx
                instrEnd = fromIntegral codeStart + B.length mainCode
            runPrismM ctx $ func1 decoder mainCode instrEnd runner
        func1 decoder mainCode instrEnd runner = do
            clearRegs
            copyMainMem (fromIntegral codeStart) mainCode
            writeOp ss 1000
            writeOp sp 640
            writeOp ds (div 8000 16)
            writeOp cs (div codeStart 16)
            runner decoder instrEnd

createTestEnv :: MonadIO m => [PrismInstruction] -> m TestEnv
createTestEnv instrList = do
    --(ioCtx, _) <- liftIO $ makeEmptyIO (1024*1024) devicesStub
    createTestEnv1 Nothing combinedList
    where
        combinedList = instrList -- ++ (segmentInstrList instrList)
        --devicesStub = 0 :: Int

-------------------------------------------------------------------------------

type RegEqFunc = MemReg -> Expectation

execPrism :: (HasCallStack) => [RegEqFunc] -> TestEnv -> Text -> Expectation
execPrism regs env cd = do
    code16 <- (assembleNative16 env) cd
    ctx <- (executePrism env) code16 decodeMemIp
    let memRegP = ctxReg ctx
    mapM_ (\f -> f memRegP) regs

{-
execPrismHalt :: (HasCallStack) => [RegEqFunc] -> TestEnv -> PrismComm -> Text -> Expectation
execPrismHalt regs env comm cd = do
    code16 <- (assembleNative16 env) cd
    ctx <- (executePrism env) code16
        (\decoder _ ctx -> decodeHaltCpu decoder comm ctx)
    let memRegP = ctxReg ctx
    mapM_ (\f -> f memRegP) regs
    -}

execAndCmp :: (HasCallStack, RegTest a v) => [a] -> TestEnv -> Text -> Expectation
execAndCmp regs env cd = do
    code <- (assembleNative env) cd
    code16 <- (assembleNative16 env) cd
    memRegN <- (executeNative env) code
    ctx <- (executePrism env) code16 decodeMemIp
    let memRegP = ctxReg ctx
    mapM_ (\r -> r `shouldEqReg` memRegP $ memRegN) regs
    (ctxFlags ctx) `flagsShouldEq` memRegN

execAndCmpNF :: (HasCallStack, RegTest a v) => [a] -> TestEnv -> Text -> Expectation
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

-------------------------------------------------------------------------------
