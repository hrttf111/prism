{-# LANGUAGE QuasiQuotes #-}

module Assembler
    (
      AsmTest,
      makeAsm,
      makeAsmStr,
      makeAsmTest,
      execApp,
      execCode
    ) where

import Foreign.C.Types (CInt(..))
import System.IO (hFlush, hClose, withFile, IOMode( ReadMode ) )

import NeatInterpolation
import Data.Text (Text, append, unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import System.Posix.Process (executeFile, forkProcess, getProcessStatus)
import System.Posix.Temp(mkstemp)
import qualified Control.Exception as E
import System.Directory (removeFile)

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)

import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

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
asmMacros = [text|
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
|]
-------------------------------------------------------------------------------
asmHeader = [text|
    save_regs
    push rdi
    mov rbp, rsp
|]

asmFooter = [text|
    pop rdi
    set_regs
    load_regs
    ret
|]
-------------------------------------------------------------------------------

data AsmTest = AsmTest {
    codeHeader :: B.ByteString,
    codeFooter :: B.ByteString
}

makeAsmTest :: IO AsmTest
makeAsmTest = do
    codeH <- makeAsm . encodeUtf8 $ asmMacros `append` asmHeader
    codeF <- makeAsm . encodeUtf8 $ asmMacros `append` asmFooter
    return $ AsmTest codeH codeF

-------------------------------------------------------------------------------

makeAsmStr :: Text -> IO B.ByteString
makeAsmStr text = makeAsm $  encodeUtf8 (prefix `append` text)
    where
        prefix = pack "BITS 64\n"

-------------------------------------------------------------------------------

makeAsm :: BC.ByteString -> IO B.ByteString
makeAsm input = 
    E.bracket mkLibFile removeFile
        (\f1 -> E.bracket mkTempFile closeAndRemove $ m f1)
    where
        closeAndRemove (f, h) = hClose h >> removeFile f
        mkLibFile = return "/tmp/asmhs_lib"
        mkTempFile = mkstemp "/tmp/asmhs_temp_"
        m pathLib (path, h) = do
            B.hPutStr h input >> hFlush h
            (forkProcess $ openAsm path pathLib)
                >>= getProcessStatus True True
                >>= processStatus pathLib
        processStatus path (Just _) = withFile path ReadMode B.hGetContents
        processStatus _ Nothing = return BC.empty
        openAsm path pathLib = executeFile "yasm" True (opts path pathLib) Nothing
        opts path pathLib = ["-f", "bin", "-p", "nasm", path, "-o", pathLib]

-------------------------------------------------------------------------------

execApp :: B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
execApp bs ptrA = E.bracket openMMap closeMMap (\ptr -> do
        let array = B.unpack bs
            offset = 0
            ptrN = plusPtr ptr offset
            ptrF = castPtrToFunPtr ptrN
        pokeArray ptrN array
        mkFun ptrF ptrA
        return ptrA
    )
    where
        len = (fromIntegral $ B.length bs) :: CInt
        openMMap = do_mmap len
        closeMMap ptr = do_munmap ptr len 

-------------------------------------------------------------------------------

execCode :: AsmTest -> B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
execCode (AsmTest header footer) mainCode ptrA = do
        execApp code ptrA
    where
        code = header `B.append` mainCode `B.append` footer

-------------------------------------------------------------------------------
