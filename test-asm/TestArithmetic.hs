{-# LANGUAGE QuasiQuotes #-}

module TestArithmetic where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testAdd env = do
    describe "Test add" $ do
        it "Add8" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, 2
            |]
        it "Add8 neg" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, -1
            |]
        it "Add8 neg2" $ do
            execAndCmp [al] env $ [text|
                mov al, -127
                add al, -120
            |]
    describe "Test add16" $ do
        it "Add16" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                add ax, 2
            |]
        it "Add16 neg" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                add ax, -1
            |]
        it "Add16 neg2" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -32123
                add ax, -31234
            |]
