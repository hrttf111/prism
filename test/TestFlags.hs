module TestFlags where

import Test.Hspec

import Prism
import PrismCpu

testFlagsCF =
    describe "Flags CF" $ do
        it "CF carry8" $ do
            (calcCFCarry8 10 255) `shouldBe` False -- 10 + 245 = 255
            (calcCFCarry8 1 0) `shouldBe` True -- 1 + 255 = 256
            (calcCFCarry8 255 254) `shouldBe` True -- 255 + 255 = 254
            (calcCFCarry8 1 1) `shouldBe` False -- 1 + 0 = 1
        it "CF borrow8" $ do
            (calcCFBorrow8 5 1) `shouldBe` False -- 5 - 4 = 1
            (calcCFBorrow8 128 127) `shouldBe` False -- 128 - 1 = 127
            (calcCFBorrow8 1 255) `shouldBe` True -- 1 - 2 = 255
            (calcCFBorrow8 255 0) `shouldBe` False -- 255 - 255 = 0
            (calcCFBorrow8 1 1) `shouldBe` False -- 1 + 0 = 1

testFlagsSF =
    describe "Flags SF" $ do
        it "SF 8" $ do
            (calcSF8 0x80) `shouldBe` True
            (calcSF8 0x7F) `shouldBe` False
            (calcSF8 0) `shouldBe` False
            (calcSF8 0xFF) `shouldBe` True

testFlagsZF =
    describe "Flags ZF" $ do
        it "ZF 8" $ do
            (calcZF8 0) `shouldBe` True
            (calcZF8 1) `shouldBe` False
            (calcZF8 255) `shouldBe` False

testFlagsPF =
    describe "Flags PF" $ do
        it "PZ 8" $ do
            (calcPF8 0) `shouldBe` False
            (calcPF8 1) `shouldBe` True
            (calcPF8 0xFF) `shouldBe` False
            (calcPF8 0x11) `shouldBe` False
            (calcPF8 0x31) `shouldBe` True

testFlagsAF =
    describe "Flags AF" $ do
        it "AF carry8" $ do
            (calcAFCarry8 0x09 0x05) `shouldBe` True -- 9 + 6 = 5
            (calcAFCarry8 0x01 0x01) `shouldBe` False -- 1 + 0 = 1
            (calcAFCarry8 0x01 0x02) `shouldBe` False -- 1 + 1 = 2
        it "AF borrow8" $ do
            (calcAFBorrow8 0x09 0x05) `shouldBe` False -- 9 - 4 = 5
            (calcAFBorrow8 0x01 0x01) `shouldBe` False -- 1 - 0 = 1
            (calcAFBorrow8 0x01 0x08) `shouldBe` True -- 1 - 9 = 8
