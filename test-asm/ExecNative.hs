{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ExecNative (
    assembleNative
    , ExecutorNative(..), ExecutorNativeRes
) where

import NeatInterpolation

import qualified Control.Exception as E
import Control.Monad.Trans (MonadIO, liftIO)

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (free)

import Data.Word (Word8)
import Data.List (intercalate)

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B

import Prism.Cpu

import Assembler (makeAsm)
import TestAsm.Common

-------------------------------------------------------------------------------

foreign import ccall unsafe "mmap.h do_mmap"
    do_mmap :: CInt
            -> IO (Ptr a)

foreign import ccall unsafe "mmap.h do_munmap"
    do_munmap :: Ptr ()
              -> CInt
              -> IO ()

type IntFunction = Ptr Word8 -> IO CInt
foreign import ccall "dynamic"
  mkFun :: FunPtr IntFunction -> IntFunction

-------------------------------------------------------------------------------

asmHeader = [untrimming|
    BITS 64

    %macro save_regs 0
        push rax
        push rbx
        push rcx
        push rdx
        push rdi
        push rsi
        push rbp
        push rsp
    %endmacro

    %macro load_regs 0
        pop rsp
        pop rbp
        pop rsi
        pop rdi
        pop rdx
        pop rcx
        pop rbx
        pop rax
    %endmacro

    %macro set_regs 0
        pushf
        mov WORD [rdi], ax
        mov WORD [rdi+2], cx
        mov WORD [rdi+4], dx
        mov WORD [rdi+6], bx
        mov WORD [rdi+8], sp
        mov WORD [rdi+10], bp
        mov WORD [rdi+12], si
        mov WORD [rdi+14], di
        ;mov WORD [rdi+32], es
        mov WORD [rdi+34], cs
        ;mov WORD [rdi+36], ss
        ;mov WORD [rdi+38], ds
        pop rax
        mov WORD [rdi+42], ax
    %endmacro

    save_regs
    push rdi
    mov rbp, rsp
|]

asmFooter = [untrimming|
    pop rdi
    set_regs
    load_regs
    ret
|]

-------------------------------------------------------------------------------

assembleNative :: T.Text -> IO B.ByteString
assembleNative programText =
    makeAsm $ encodeUtf8 fullText
    where
        fullText = asmHeader `T.append` programText `T.append` asmFooter

-------------------------------------------------------------------------------

data ExecutorNativeRes = ExecutorNativeRes {
    enrMemReg :: MemReg
}

instance ExecutorRes ExecutorNativeRes where
    freeRes (ExecutorNativeRes (MemReg p1)) = free p1

data ExecutorNative = ExecutorNative

instance (MonadIO m) => ProgramExecutor ExecutorNative ExecutorNativeRes m where
    execProgram ep mainCode = liftIO $ do
        ptrA <- allocMemRegRaw
        E.bracket openMMap closeMMap (\ptr -> do
            let array = B.unpack mainCode
                offset = 0
                ptrN = plusPtr ptr offset
                ptrF = castPtrToFunPtr ptrN
            pokeArray ptrN array
            mkFun ptrF ptrA
            return ptrA
            )
        return $ ExecutorNativeRes $ MemReg ptrA
        where
            len = (fromIntegral $ B.length mainCode) :: CInt
            openMMap = do_mmap len
            closeMMap ptr = do_munmap ptr len 

-------------------------------------------------------------------------------

instance (MonadIO m) => OperandSupport ExecutorNativeRes Reg8 Uint8 m where
    readSourceOp epr reg =
        readOpRaw (enrMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorNativeRes Reg16 Uint16 m where
    readSourceOp epr reg =
        readOpRaw (enrMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorNativeRes AllRegs String m where
    readSourceOp epr _ = do
        sr <- (intercalate "\n") <$> (printRegs $ enrMemReg epr)
        sf <- (intercalate "\n") <$> (printFlags $ enrMemReg epr)
        return $ sr ++ "\n" ++ sf

instance (MonadIO m) => OperandSupport ExecutorNativeRes Flag Bool m where
    readSourceOp epr reg =
        readOpRaw (enrMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorNativeRes EFlag Bool m where
    readSourceOp epr reg =
        readOpRaw (enrMemReg epr) reg

-------------------------------------------------------------------------------
