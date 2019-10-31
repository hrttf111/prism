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
                mov al, 1
                add al, 2
            |]
            memRegN <- (executeNative env) code
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            al `shouldEq` 3 $ memRegN
            al `shouldEqReg` memRegN $ memRegP
        it "Add8 neg" $ do
            code <- (assembleNative env) $ [text|
                mov al, 1
                add al, -1
            |]
            memRegN <- (executeNative env) code
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            al `shouldEq` 0 $ memRegN
            al `shouldEqReg` memRegN $ memRegP
