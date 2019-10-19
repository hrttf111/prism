{-# LANGUAGE QuasiQuotes #-}

module TestFlags where

import Test.Hspec

import Assembler
import TestCommon

import PrismCpu

import NeatInterpolation
import Data.Text (Text)

import Data.Word (Word8, Word16)
import Foreign.Ptr
import Foreign.Storable (peekByteOff)

testFlagsZF execC = 
    describe "Flags ZF" $ do
        memReg <- runIO $ execC [text|
            mov ax, 1
            mov cx, 2
        |]
        it "ZF set" $ do
            al `shouldEq` 1 $ memReg
            ah `shouldEq` 0 $ memReg
            ax `shouldEq` 1 $ memReg
            dl `shouldEqReg` memReg $ memReg
