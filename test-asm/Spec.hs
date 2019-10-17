{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Assembler

import NeatInterpolation
import Data.Text (Text)

import Data.Word (Word8, Word16)
import Foreign.Ptr
import Foreign.Marshal.Array (allocaArray, callocArray)
import Foreign.Storable (peekByteOff)


execCodeTest :: AsmTest -> Ptr Word8 -> Text -> IO (Ptr Word8)
execCodeTest asmTest ptrA code = do
    mainCode <- makeAsmStr code
    execCode asmTest mainCode ptrA


testReg str ptrA index val = do
    a <- runIO $ (peekByteOff ptrA index :: IO Word16)
    it str $
        a `shouldBe` val


testMov execC =
    describe "MOV" $ do
        ptrA <- runIO $ execC [text|
            mov ax, WORD 199
            mov bx, 34
            mov cx, 43
            mov dx, 131
            add bx, 123
        |]
        testReg "MOV imm8 to AX" ptrA 0 199
        testReg "MOV imm8 to CX" ptrA 2 43


testAdd execC =
    describe "ADD" $ do
        ptrA <- runIO $ execC [text|
            mov ax, 0
            mov bx, 5
            add ax, 4
            add ax, bx
        |]
        testReg "ADD imm8 and reg to AX" ptrA 0 9


main :: IO ()
main = hspec $ do
  asmTest <- runIO $ makeAsmTest
  ptrA <- runIO $ callocArray 100
  let execC = execCodeTest asmTest ptrA
  testMov execC
  testAdd execC
