module TestFlags where

import Test.Hspec

import Prism
import PrismCpu

testFlagsCF =
    describe "Flags CF" $ do
        it "CF cleared" $ do
            (calcCFCarry8 1 1) `shouldBe` False
        it "CF set" $ do
            (calcCFCarry8 255 1) `shouldBe` True
        it "CF borrow" $ do
            (calcCFBorrow8 1 5) `shouldBe` True
            (calcCFBorrow8 5 1) `shouldBe` False
