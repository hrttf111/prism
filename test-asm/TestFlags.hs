{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestFlags where

import Test.Hspec

import Prism.Cpu
import Prism.Instructions

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

-------------------------------------------------------------------------------

testFlagsZF env = 
    describe "Flags ZF" $ do
        --todo
        {-it "ZF set" $ do
            runTest env ([untrimming|
                mov ax, 0
            |]) $ do
                --shouldEq ZF True
                shouldEqSources ZF-}
        it "ZF cleared" $ do
            runTest env ([untrimming|
                mov al, 0
                add al, 1
            |]) $ do
                shouldEq ZF False
                shouldEqSources ZF

testFlagsCF env = 
    describe "Flags CF" $ do
        it "CF cleared" $ do
            runTest env ([untrimming|
                mov al, 10
                add al, 245
            |]) $ do
                shouldEq CF False
                shouldEq OF False
                --shouldEqSources [CF, OF]
        it "CF cleared sub" $ do
            runTest env ([untrimming|
                mov al, 0x80
                sub al, 1
            |]) $ do
                showAllRegsL
                showAllRegsR
                shouldEq al 127
                shouldEq CF False
                shouldEq OF True
                shouldEqSources CF
                shouldEqSources OF
                --shouldEqSources [CF, OF]
                shouldEqSources al
        it "CF set" $ do
            runTest env ([untrimming|
                mov al, 1
                add al, 255
            |]) $ do
                shouldEq CF True
                shouldEqSources CF
                shouldEqSources al
        it "CF set negative" $ do
            runTest env ([untrimming|
                mov al, 127
                add al, -120
            |]) $ do
                shouldEq CF True
                shouldEq OF False
                shouldEqSources [CF, OF]
        it "CF set sub" $ do
            runTest env ([untrimming|
                mov al, 1
                sub al, 2
            |]) $ do
                shouldEq CF True
                shouldEq OF False
                shouldEq al 255
                shouldEqSources [CF, OF]
                shouldEqSources al
        it "CF set mul" $ do
            runTest env ([untrimming|
                mov bl, 100
                mov al, 10
                mul bl
            |]) $ do
                shouldEq CF True
                shouldEq OF True
                shouldEq ax 1000
                shouldEqSources [CF, OF]
                shouldEqSources ax

testFlagsOF env = 
    describe "Flags OF" $ do
        it "OF cleared" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, 255
            |]) $ do
                shouldEq al (-128)
                shouldEq OF False
                shouldEq CF True
        it "OF set when ADD" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, 127
            |]) $ do
                shouldEq OF False
                shouldEq CF True
        it "OF set when ADD -2" $ do
            runTest env ([untrimming|
                mov al, -2
                add al, -2
            |]) $ do
                shouldEq OF False
                shouldEq CF True
        it "OF set when ADD negative" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, -120
            |]) $ do
                shouldEq OF True
                shouldEq CF True
                shouldEq al 9
        it "OF set when SUB" $ do
            runTest env ([untrimming|
                mov al, -127
                sub al, 120
            |]) $ do
                shouldEq OF True
                shouldEq CF False
                shouldEq al 9
        it "OF set when SUB negative" $ do
            runTest env ([untrimming|
                mov al, -127
                sub al, -120
            |]) $ do
                shouldEq OF False
                shouldEq CF True
