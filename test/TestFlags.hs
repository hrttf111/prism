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
        it "CF carry16" $ do
            (calcCFCarry16 10 0xFFFF) `shouldBe` False
            (calcCFCarry16 1 0) `shouldBe` True -- 1 + 0xFFFF = 0
            (calcCFCarry16 0xFFFF 0xFFFE) `shouldBe` True -- 0xFFFF + 0xFFFF
            (calcCFCarry16 1 1) `shouldBe` False
        it "CF borrow16" $ do
            (calcCFBorrow16 5 1) `shouldBe` False -- 5 - 4 = 1
            (calcCFBorrow16 0xFFF 0xFFE) `shouldBe` False
            (calcCFBorrow16 1 0xFFFF) `shouldBe` True
            (calcCFBorrow16 0xFFFF 0) `shouldBe` False
            (calcCFBorrow16 1 1) `shouldBe` False

testFlagsSF =
    describe "Flags SF" $ do
        it "SF 8" $ do
            (calcSF8 0x80) `shouldBe` True
            (calcSF8 0x7F) `shouldBe` False
            (calcSF8 0) `shouldBe` False
            (calcSF8 0xFF) `shouldBe` True
        it "SF 16" $ do
            (calcSF16 0x8000) `shouldBe` True
            (calcSF16 0x7FFF) `shouldBe` False
            (calcSF16 0) `shouldBe` False
            (calcSF16 0xFFFF) `shouldBe` True

testFlagsZF =
    describe "Flags ZF" $ do
        it "ZF 8" $ do
            (calcZF8 0) `shouldBe` True
            (calcZF8 1) `shouldBe` False
            (calcZF8 255) `shouldBe` False
        it "ZF 16" $ do
            (calcZF16 0) `shouldBe` True
            (calcZF16 1) `shouldBe` False
            (calcZF16 0xFFFF) `shouldBe` False

testFlagsPF =
    describe "Flags PF" $ do
        it "PZ 8" $ do
            (calcPF8 0) `shouldBe` True
            (calcPF8 1) `shouldBe` False
            (calcPF8 0xFF) `shouldBe` True
            (calcPF8 0x11) `shouldBe` True
            (calcPF8 0x31) `shouldBe` False
        it "PZ 16" $ do
            (calcPF16 0) `shouldBe` True
            (calcPF16 1) `shouldBe` False
            (calcPF16 0xFF) `shouldBe` True
            (calcPF16 0x11) `shouldBe` True
            (calcPF16 0x31) `shouldBe` False
            (calcPF16 0xFF00) `shouldBe` True
            (calcPF16 0xFF01) `shouldBe` False
            (calcPF16 0x0101) `shouldBe` False
            (calcPF16 0x1101) `shouldBe` False

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
        it "AF carry16" $ do
            (calcAFCarry16 0x09 0x05) `shouldBe` True -- 9 + 6 = 5
            (calcAFCarry16 0x01 0x01) `shouldBe` False -- 1 + 0 = 1
            (calcAFCarry16 0x01 0x02) `shouldBe` False -- 1 + 1 = 2
        it "AF borrow16" $ do
            (calcAFBorrow16 0x09 0x05) `shouldBe` False -- 9 - 4 = 5
            (calcAFBorrow16 0x01 0x01) `shouldBe` False -- 1 - 0 = 1
            (calcAFBorrow16 0x01 0x08) `shouldBe` True -- 1 - 9 = 8

testFlagsOF =
    describe "Flags OF" $ do
        it "OF add8" $ do
            (calcOFAdd8i 1 1 2) `shouldBe` False
            (calcOFAdd8i 127 1 (-128)) `shouldBe` True
            (calcOFAdd8i (-2) (-2) (-4)) `shouldBe` False
            (calcOFAdd8i (-127) (-1) (-128)) `shouldBe` False
            (calcOFAdd8i (-127) (-120) 9) `shouldBe` True
            (calcOFAdd8 1 1 2) `shouldBe` False
            (calcOFAdd8 0x7F 1 0x80) `shouldBe` True
            (calcOFAdd8 0xFE 0xFE 0xFC) `shouldBe` False
            (calcOFAdd8 0x81 0xFF 0x80) `shouldBe` False
            (calcOFAdd8 0x81 0x88 9) `shouldBe` True
        it "OF sub8" $ do
            (calcOFSub8i 1 1 0) `shouldBe` False
            (calcOFSub8i 127 (-1) (-128)) `shouldBe` True
            (calcOFSub8i (-128) 1 127) `shouldBe` True
            (calcOFSub8i (-127) 120 9) `shouldBe` True
            (calcOFSub8 1 1 0) `shouldBe` False
            (calcOFSub8 127 0xFF 0x80) `shouldBe` True
            (calcOFSub8 0x80 1 127) `shouldBe` True
            (calcOFSub8 0x81 120 9) `shouldBe` True
        it "OF add16" $ do
            (calcOFAdd16 1 1 2) `shouldBe` False
            (calcOFAdd16 0x7FFF 1 0x8000) `shouldBe` True
            (calcOFAdd16 0xFFFE 0xFFFE 0xFFFC) `shouldBe` False
            (calcOFAdd16 0x8001 0xFFFF 0x8000) `shouldBe` False
            (calcOFAdd16 0x8001 0x8008 9) `shouldBe` True
        it "OF sub16" $ do
            (calcOFSub16 1 1 0) `shouldBe` False
            (calcOFSub16 0x7FFF 0xFFFF 0x8000) `shouldBe` True
            (calcOFSub16 0x8000 1 0x7FFF) `shouldBe` True
            (calcOFSub16 0x8001 120 9) `shouldBe` True
