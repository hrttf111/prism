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
