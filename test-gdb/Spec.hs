import Test.Hspec

import Text.Parsec
import Text.Parsec.Text

import Data.Text
import Data.Word

import Prism.GDB.Types
import Prism.GDB.Protocol

-------------------------------------------------------------------------------

testHaltReason = do
    describe "Parse HaltReason - '?'" $ do
        it "Success" $ do
            (parse gdbParser "" (pack "?")) `shouldBe` (Right GHaltReason)
        it "Fail" $ do
            (parse gdbParser "" (pack "dsdsc")) `shouldBe` (Right GUknownCommand)
    describe "Parse Cont - 'c'" $ do
        it "Success" $
            (parse gdbParser "" (pack "c")) `shouldBe` (Right (GCont Nothing))
        it "Success addr" $
            (parse gdbParser "" (pack "c5555")) `shouldBe` (Right (GCont (Just 5555)))
    describe "Parse ReadMem - 'm'" $ do
        it "Success" $
            (parse gdbParser "" (pack "m1,2")) `shouldBe` (Right (GReadMem (1,2)))
    describe "Parse WriteMem - 'M'" $ do
        it "Success" $
            (parse gdbParser "" (pack "M1,2:0102")) `shouldBe` (Right (GWriteMem (1,[1,2])))
    describe "hexConvert" $ do
        it "toHex Word32" $ do
            (assembleHex (0xabcd::Word32) 0) `shouldBe` "cdab0000"
            (assembleHex (0x0bcd::Word32) 0) `shouldBe` "cd0b0000"
            (assembleHex (0x0d::Word32) 0) `shouldBe` "0d000000"
            (assembleHex (0x00::Word32) 0) `shouldBe` "00000000"
        it "toHex Word16" $ do
            (assembleHex (0xabcd::Word16) 0) `shouldBe` "cdab"
            (assembleHex (0x0bcd::Word16) 0) `shouldBe` "cd0b"
            (assembleHex (0x0d::Word16) 0) `shouldBe` "0d00"
            (assembleHex (0x00::Word16) 0) `shouldBe` "0000"
        it "toHex Word8" $ do
            (assembleHex (0xab::Word8) 0) `shouldBe` "ab"
            (assembleHex (0x0b::Word8) 0) `shouldBe` "0b"
        it "fromHex Word32" $ do
            (parse hexWordParser "" (pack "cdab0000")) `shouldBe` (Right (0xabcd :: Word32))
            (parse hexWordParser "" (pack "00000000")) `shouldBe` (Right (0 :: Word32))
            (parse hexWordParser "" (pack "000000")) `shouldBe` (Right (0 :: Word32))

main :: IO ()
main = do
    hspec testHaltReason

-------------------------------------------------------------------------------
