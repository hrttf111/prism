{-# LANGUAGE QuasiQuotes #-}

module TestArithmetic where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testAdd env = do
    describe "ADD [8] ACC REG <- IMM" $ do
        it "Simple add" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, 2
            |]
        it "Add negative CF ZF" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, -1
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [al] env $ [text|
                mov al, -127
                add al, -120
            |]
    describe "ADD [16] ACC REG <- IMM" $ do
        it "Simple add" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                add ax, 2
            |]
        it "Add negative CF ZF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                add ax, -1
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -32123
                add ax, -31234
            |]
    describe "ADD [8] REG <- REG" $ do
        it "Simple add" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                mov bx, 2
                add ax, bx
            |]
