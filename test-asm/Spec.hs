{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Assembler
import TestCommon
import TestFlags

import PrismCpu

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)

import Data.Word (Word8, Word16)
import Foreign.Ptr
import Foreign.Marshal.Array (allocaArray, callocArray)
import Foreign.Storable (peekByteOff)



testMov execC =
    describe "MOV" $ do
        memReg <- runIO $ execC [text|
            mov ax, WORD 199
            mov bx, 34
            mov cx, 43
            mov dx, 131
            add bx, 123
        |]
        it "AX and CX" $ do
            ax `shouldEq` 199 $ memReg
            cx `shouldEq` 43 $ memReg

{-
testAdd execC =
    describe "ADD" $ do
        ptrA <- runIO $ execC [text|
            mov ax, 0
            mov bx, 5
            add ax, 4
            add ax, bx
        |]
        testReg16 "Add imm8 to AL" memReg al 9
-}

main :: IO ()
main = do
    execC <- createTestEnv 
    hspec $ do
        testMov execC
        testFlagsZF execC
