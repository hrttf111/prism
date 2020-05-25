module TestCpu where

import Test.Hspec

import Data.Int
import Data.Tuple
import Foreign.Ptr

import Prism.Cpu
import Prism.Instructions.Arithmetic (div8)

-------------------------------------------------------------------------------

testDiv = do
    describe "DIV" $ do
        it "Unt8" $ do
            (snd $ div8 clearFlags 1000 50) `shouldBe` 20

testSign = do
    describe "Convert unsigned to singed (complement 2)" $ do
        it "Uint8 to Int8" $ do
            (toSignedCompl2 (0x01 :: Uint8)) `shouldBe` (0x01 :: Int8)
            (toSignedCompl2 (0x7F :: Uint8)) `shouldBe` (0x7F :: Int8)
            (toSignedCompl2 (0x80 :: Uint8)) `shouldBe` (-128 :: Int8)
            (toSignedCompl2 (0xFF :: Uint8)) `shouldBe` (-1 :: Int8)
        it "Uint16 to Int16" $ do
            (toSignedCompl2 (0x01 :: Uint16)) `shouldBe` (0x01 :: Int16)
            (toSignedCompl2 (0x7FFF :: Uint16)) `shouldBe` (0x7FFF :: Int16)
            (toSignedCompl2 (0x8000 :: Uint16)) `shouldBe` (-32768 :: Int16)
            (toSignedCompl2 (0xFFFF :: Uint16)) `shouldBe` (-1 :: Int16)
    describe "Convert signed to unsinged (complement 2)" $ do
        it "Int8 to Uint8" $ do
            (toSignedCompl2 (0x01 :: Int8)) `shouldBe` (0x01 :: Uint8)
            (toSignedCompl2 (0x7F :: Int8)) `shouldBe` (0x7F :: Uint8)
            (toSignedCompl2 (-128 :: Int8)) `shouldBe` (0x80 :: Uint8)
            (toSignedCompl2 (-1 :: Int8)) `shouldBe` (0xFF :: Uint8)
        it "Int16 to Uint16" $ do
            (toSignedCompl2 (0x01 :: Int16)) `shouldBe` (0x01 :: Uint16)
            (toSignedCompl2 (0x7FFF :: Int16)) `shouldBe` (0x7FFF :: Uint16)
            (toSignedCompl2 (-32768 :: Int16)) `shouldBe` (0x8000 :: Uint16)
            (toSignedCompl2 (-1 :: Int16)) `shouldBe` (0xFFFF :: Uint16)
    describe "Signed operation with unsinged arg (complement 2)" $ do
        it "Mul Uint8" $ do
            let op = (*) :: Int8 -> Int8 -> Int8 
            (signedOp op (0x01 :: Uint8) (0x01 :: Uint8)) `shouldBe` (0x01 :: Uint8)
            (signedOp op (0xFF :: Uint8) (0x01 :: Uint8)) `shouldBe` (0xFF :: Uint8)
            (signedOp op (0x08 :: Uint8) (0x08 :: Uint8)) `shouldBe` (0x40 :: Uint8)
            (signedOp op (0xFF :: Uint8) (0x0A :: Uint8)) `shouldBe` ((0xFF - 9) :: Uint8)
        it "Mul Uint16" $ do
            let op = (*) :: Int8 -> Int8 -> Int8 
            (signedOp op (0x0001 :: Uint16) (0x0001 :: Uint16)) `shouldBe` (0x0001 :: Uint16)
            (signedOp op (0xFFFF :: Uint16) (0x0001 :: Uint16)) `shouldBe` (0xFFFF :: Uint16)
            (signedOp op (0x08 :: Uint16) (0x08 :: Uint16)) `shouldBe` (0x0040 :: Uint16)
            (signedOp op (0xFFFF :: Uint16) (0x000A :: Uint16)) `shouldBe` ((0xFFFF - 9) :: Uint16)

-------------------------------------------------------------------------------
