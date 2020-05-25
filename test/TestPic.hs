module TestPic where

import Test.Hspec

import Peripherals.Pic

-------------------------------------------------------------------------------

testPic = do
    describe "Test PIC" $ do
        it "Test mask" $ do
            (getISRMask 7 0x80) `shouldBe` 0x7F
            (getISRMask 7 0x01) `shouldBe` 0x00
            (getISRMask 4 0x01) `shouldBe` 0xE0
        it "Find highest" $ do
            (picFindHighest 7 0x0F) `shouldBe` (Just 0)
            (picFindHighest 7 0xF0) `shouldBe` (Just 4)
            (picFindHighest 4 0x0F) `shouldBe` (Just 0)
            (picFindHighest 4 0x00) `shouldBe` (Nothing)

-------------------------------------------------------------------------------
