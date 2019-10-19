module TestCommon where

import Prism
import PrismCpu

import Assembler

import Test.Hspec
import Test.Hspec.Expectations

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)

import Data.Word (Word8, Word16)
import Foreign.Marshal.Array (allocaArray, callocArray)

type CodeExecutor = (Text -> IO MemReg)

createTestEnv :: MonadIO m => m CodeExecutor
createTestEnv = liftIO $ do
    asmTest <- makeAsmTest
    ptrA <- callocArray 100
    return $ execCodeTest asmTest (MemReg ptrA)


class RegTest a where
    shouldEq :: a -> Int -> MemReg -> Expectation
    shouldEqReg :: a -> MemReg -> MemReg -> Expectation


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

execCodeTest :: MonadIO m => AsmTest -> MemReg -> Text -> m MemReg
execCodeTest asmTest (MemReg ptrA) code = liftIO $ do
    mainCode <- makeAsmStr code
    execCode asmTest mainCode ptrA
    return $ MemReg ptrA
