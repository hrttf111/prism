{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Assembler
import TestCommon
import TestFlags

import PrismCpu

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)


testMov execC =
    describe "MOV" $ do
        it "AX and CX" $! do
            memReg <- execC [text|
                mov ax, WORD 199
                mov bx, 34
                mov cx, 43
                mov dx, 131
                add bx, 123
            |]
            al `shouldEq` 199 $ memReg
            cl `shouldEq` 43 $ memReg

testAdd execC =
    describe "ADD" $ do
        it "Add imm8 to AL" $! do
            memReg <- execC [text|
                mov ax, 0
                mov bx, 5
                add ax, 4
                add ax, bx
            |]
            al `shouldEq` 9 $ memReg


main :: IO ()
main = do
    execC <- createTestEnv 
    hspec $ do
        testMov execC
        testAdd execC
        testFlagsZF execC
