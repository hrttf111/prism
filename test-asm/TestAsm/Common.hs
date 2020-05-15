{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestAsm.Common where

import Test.Hspec
import Test.Hspec.Expectations

import Control.Concurrent

import Data.Text (Text)
import qualified Data.ByteString as B

import Prism.Cpu
import Prism.Decoder (PrismDecoder)

import Assembler

-------------------------------------------------------------------------------

type PrismRunner = PrismDecoder -> Int -> PrismM ()

data TestEnv = TestEnv {
        peripheralThreadId :: Maybe ThreadId,
        assembleNative :: (Text -> IO B.ByteString),
        assembleNative16 :: (Text -> IO B.ByteString),
        executeNative :: (B.ByteString -> IO MemReg),
        executePrism :: (B.ByteString -> PrismRunner -> IO Ctx)
    }

-------------------------------------------------------------------------------

class (MemRegManipulator a MemReg v) => RegTest a v | a -> v where
    shouldEq :: (HasCallStack) => a -> Int -> MemReg -> Expectation
    shouldEqReg :: (HasCallStack) => a -> MemReg -> MemReg -> Expectation

instance RegTest Reg8 Uint8 where
    shouldEq reg valExp memReg = do
        val <- readRegRaw memReg reg
        val `shouldBe` (fromIntegral valExp)
    shouldEqReg reg memReg1 memReg2 = do
        val1 <- readRegRaw memReg1 reg
        val2 <- readRegRaw memReg2 reg
        val1 `shouldBe` val2

instance RegTest Reg16 Uint16 where
    shouldEq reg valExp memReg = do
        val <- readRegRaw memReg reg
        val `shouldBe` (fromIntegral valExp)
    shouldEqReg reg memReg1 memReg2 = do
        val1 <- readRegRaw memReg1 reg
        val2 <- readRegRaw memReg2 reg
        val1 `shouldBe` val2

flagsShouldEq :: (HasCallStack) => Flags -> MemReg -> Expectation
flagsShouldEq flags memReg = do
    (flagsN, _) <- readRegRaw memReg flagsInternal
    flags `shouldBe` flagsN

-------------------------------------------------------------------------------
