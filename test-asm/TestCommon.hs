module TestCommon where

import Test.Hspec
import Test.Hspec.Expectations

import NeatInterpolation
import Data.Text (Text)
import qualified Data.ByteString as B

import Control.Monad.Trans (MonadIO, liftIO)

import Data.Word (Word8, Word16)
import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Array (allocaArray, callocArray, pokeArray)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Utils (fillBytes)

import Prism
import PrismCpu
import PrismDecoder
import PrismPeripheral

import Instruction.Processor

import Assembler

-------------------------------------------------------------------------------

type CodeExecutor = (Text -> IO MemReg)

data TestEnv = TestEnv {
        assembleNative :: (Text -> IO B.ByteString),
        assembleNative16 :: (Text -> IO B.ByteString),
        executeNative :: (B.ByteString -> IO MemReg),
        executePrism :: (B.ByteString -> IO Ctx)
    }

createTestEnv1 :: MonadIO m => PrismDecoder -> m TestEnv
createTestEnv1 decoder = liftIO $ do
    ptrReg <- callocBytes 64
    ptrMem <- callocBytes memSize
    asmTest <- makeAsmTest
    ptrA <- callocArray 64
    return $ TestEnv makeAsmStr makeAsmStr16 (execNative asmTest ptrA) (execP ptrReg ptrMem decoder)
    where
        memSize = 65000
        codeStart = 12000
        execNative asmTest ptrA mainCode = MemReg <$> execCode asmTest mainCode ptrA
        execP ptrReg ptrMem decoder mainCode = do
            (ioCtx, peripheral) <- makeEmptyIO (1024*1024)
            let codeLen = B.length mainCode
                ctx = makePrismCtx (MemReg ptrReg) (MemMain ptrMem) ioCtx
                array = B.unpack mainCode
                f (MemMain p) = p
                f1 (MemReg p) = p
            fillBytes (f1 $ ctxReg ctx) 0 64
            pokeArray (flip plusPtr (fromIntegral codeStart) $ f $ ctxMem ctx) array
            runPrism $ execPrism decoder codeLen ctx
        execPrism decoder codeLen ctx = do
            writeSeg (ctxReg ctx) ss 1000
            writeReg16 (ctxReg ctx) sp 640
            writeSeg (ctxReg ctx) ds 8000
            writeSeg (ctxReg ctx) cs (div codeStart 16)
            decodeMemIp decoder (fromIntegral codeStart + codeLen) ctx

createTestEnv :: MonadIO m => [PrismInstruction] -> m TestEnv
createTestEnv instrList = createTestEnv1 $ makeDecoderList combinedList
    where
        combinedList = instrList ++ (segmentInstrList instrList)

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
    ctx <- (executePrism env) code16
    let memRegP = ctxReg ctx
    mapM_ (\f -> f memRegP) regs

execAndCmp :: (HasCallStack, RegTest a) => [a] -> TestEnv -> Text -> Expectation
execAndCmp regs env cd = do
    code <- (assembleNative env) cd
    code16 <- (assembleNative16 env) cd
    memRegN <- (executeNative env) code
    ctx <- (executePrism env) code16
    let memRegP = ctxReg ctx
    mapM_ (\r -> r `shouldEqReg` memRegP $ memRegN) regs
    (ctxFlags ctx) `flagsShouldEq` memRegN

execAndCmpNF :: (HasCallStack, RegTest a) => [a] -> TestEnv -> Text -> Expectation
execAndCmpNF regs env cd = do
    code <- (assembleNative env) cd
    code16 <- (assembleNative16 env) cd
    memRegN <- (executeNative env) code
    ctx <- (executePrism env) code16
    let memRegP = ctxReg ctx
    mapM_ (\r -> r `shouldEqReg` memRegP $ memRegN) regs

execCodeTest :: MonadIO m => AsmTest -> MemReg -> Text -> m MemReg
execCodeTest asmTest (MemReg ptrA) code = liftIO $ do
    mainCode <- makeAsmStr code
    execCode asmTest mainCode ptrA
    return $ MemReg ptrA
