{-# LANGUAGE QuasiQuotes #-}

module TestArithmetic where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testAdd env =
    describe "Test add8" $ do
        it "Add8" $ do
            code <- (assembleNative env) $ [text|
                mov al, 0xFF
                add al, 1
            |]
            memRegN <- (executeNative env) code
            al `shouldEq` 0 $ memRegN
            --ctx <- (executePrism env) code
            --let memRegP = ctxReg ctx
            --ax `shouldEqReg` memRegN $ memRegP
            --bx `shouldEqReg` memRegN $ memRegP
