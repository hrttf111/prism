{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Qemu (
    execQemu, execQemuBin
    , execCode
    , assembleQemu
    , ExecutorQemu(..), ExecutorQemuRes(..)
) where

import NeatInterpolation

import Control.Concurrent (threadDelay)

import Control.Monad.Trans (MonadIO, liftIO)

import System.IO (withFile, IOMode( ReadMode, WriteMode ) )
import System.Posix.IO (stdOutput, stdError, dupTo, openFd, OpenMode(WriteOnly), defaultFileFlags)
import System.Posix.Process (ProcessStatus( Exited ), executeFile, forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Directory (removeFile)

import Foreign.Ptr (plusPtr, castPtr)

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import Data.Either (either)
import Data.List (intercalate)
import Numeric (showHex)

import Control.Exception (Exception, throwIO)

import Prism.Cpu

import Assembler (makeAsm)
import TestAsm.Common

-------------------------------------------------------------------------------

asmBiosHeader = [untrimming|
    BITS 16
    CPU 8086

    ;------------------------------------------------------------------------------;
    ;Macros and defines

    %macro save_regs 1
        mov WORD [%1], ax
        mov WORD [%1+2], cx
        mov WORD [%1+4], dx
        mov WORD [%1+6], bx
        mov WORD [%1+8], sp
        mov WORD [%1+10], bp
        mov WORD [%1+12], si
        mov WORD [%1+14], di
        mov WORD [%1+32], es
        mov WORD [%1+34], cs
        mov WORD [%1+36], ss
        mov WORD [%1+38], ds
        ;mov WORD[%1+40], ip
        pushf
        pop dx
        mov WORD[%1+42], dx
    %endmacro

    %macro detect_disk 1
        mov dl, 0 ; diskette 0
        mov ah, 8 ; read params
        int 0x13
        ;
        cmp cl, 36
        jne OFF18
        mov [%1], WORD offset1_36
        jmp DETECT_DONE
        OFF18:
        mov [%1], WORD offset1_18
        DETECT_DONE:
    %endmacro

    %macro read_disk 3
        mov ax, %2
        mov es, ax
        mov ah, 2  ; read
        mov bx, WORD [%1]
        mov al, [bx+BOOTLOADER_START+disk_offset.num_sect]
        mov ch, [bx+BOOTLOADER_START+disk_offset.track]
        mov cl, [bx+BOOTLOADER_START+disk_offset.sector]
        mov dh, [bx+BOOTLOADER_START+disk_offset.head]
        mov dl, 0   ; drive
        mov bx, %3
        int 0x13
        mov [disk_res], al
        mov [disk_res+1], ah
    %endmacro

    BOOTLOADER_START equ 0x7C00
    REG_AREA equ 0x7E10
    MAGIC_1 equ 0xEE88
    MAGIC_2 equ 0xFF99

    struc disk_offset
        .track: resb 1
        .sector: resb 1
        .head: resb 1
        .num_sect resb 1
    endstruc

    ;------------------------------------------------------------------------------;
    ;Bootloader

    SECTION bootloader start=0
    mov ax, 0
    mov ss, ax
    mov sp, BOOTLOADER_START
    sti

    ;read test to memory
    detect_disk ptr_offset1
    read_disk ptr_offset1, 0x1000, 0

    ;start test
    cli
    ;clear flags
    mov dx, 0
    push dx
    popf
    jmp 1000h:START

    STOP:
    mov WORD [magic_num], MAGIC_1
    save_regs REG_AREA
    mov WORD [magic_num+2], MAGIC_2

    FINISHED:
        sti
        hlt
        jmp FINISHED

    ;------------------------------------------------------------------------------;
    ;Constants

    offset1_18:
        istruc disk_offset
            at disk_offset.track,    db 3
            at disk_offset.sector,   db 3
            at disk_offset.head,     db 1
            at disk_offset.num_sect, db 3
        iend
    offset1_36:
        istruc disk_offset
            at disk_offset.track,    db 1
            at disk_offset.sector,   db 21
            at disk_offset.head,     db 1
            at disk_offset.num_sect, db 3
        iend
    times 510-($-$$$) db 0
    dw 0xAA55

    ;------------------------------------------------------------------------------;
    ;Vars

    absolute 0x7E00
    magic_num resw 2
    disk_res resb 2
    ptr_offset1 resw 1

    ;------------------------------------------------------------------------------;
    ;Test section

    SECTION .text start=10000h
    START:
|]

asmBiosFooter = [untrimming|
    ;return to bootloader
    jmp 0:(STOP+BOOTLOADER_START)
|]

-------------------------------------------------------------------------------

execQemu :: String -> String -> IO (Maybe B.ByteString)
execQemu binPath memFile = do
    childId <- forkProcess execQemu_
    threadDelay sleepTime
    signalProcess sigTERM childId
    threadDelay waitEndTime
    childStatus <- getProcessStatus True True childId
    case childStatus of
        Just st -> do
            memData <- withFile memFile ReadMode B.hGetContents
            removeFile memFile
            return $ Just memData
        Nothing ->
            return Nothing
    where
        sleepTime = 200000 -- 200ms
        waitEndTime = 100000 -- 100ms
        execQemu_ = do
            nullFd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
            dupTo nullFd stdOutput
            dupTo nullFd stdError
            executeFile "qemu-system-i386" True qemuOpts Nothing
        qemuOpts = ["-fda", binPath,
                     "-m", "1M",
                     "-object", "memory-backend-file,id=pc.ram,size=1M,mem-path="++memFile++",prealloc=on,share=on",
                     "-machine", "memory-backend=pc.ram",
                     "-display", "none"
                     ]

execQemuBin :: B.ByteString -> IO (Maybe B.ByteString)
execQemuBin bin = do
    withFile binPath WriteMode (\h -> do
        B.hPut h bin
        )
    res <- execQemu binPath memPath
    removeFile binPath
    return res
    where
        binPath = "test.bin"
        memPath = "mem.data"

-------------------------------------------------------------------------------

data ExecutorQemuRes = ExecutorQemuRes {
    eqrMemReg :: B.ByteString,
    eqrMemMain :: B.ByteString,
    eqrMemMainOffset :: Int
} deriving (Show)

instance ExecutorRes ExecutorQemuRes where
    freeRes _ = return ()

convMemAddress :: ExecutorQemuRes -> Int -> Maybe Int
convMemAddress res address | address < (eqrMemMainOffset res) = Nothing
convMemAddress res address | (address - (eqrMemMainOffset res)) > (B.length $ eqrMemMain res) = Nothing
convMemAddress res address = Just $ address - (eqrMemMainOffset res)

data QemuAddressException = QemuAddressException Int Int Int

instance Exception QemuAddressException
instance Show QemuAddressException where
    show (QemuAddressException lower addr upper) =
        "QemuAddressException(" ++ (showHex lower "") ++ "," ++ (showHex addr "") ++ "," ++ (showHex upper "") ++ ")"

convAddr :: (MonadIO m) => ExecutorQemuRes -> Int -> m Int
convAddr res address =
    case convMemAddress res address of
        Just addr -> return addr
        Nothing -> liftIO . throwIO $ QemuAddressException (eqrMemMainOffset res) address (eqrMemMainOffset res + B.length (eqrMemMain res))

data ExecutorQemu = ExecutorQemu

assembleQemu :: T.Text -> IO B.ByteString
assembleQemu programText =
    makeAsm $ encodeUtf8 fullText
    where
        fullText = asmBiosHeader `T.append` programText `T.append` asmBiosFooter

execCode :: B.ByteString -> IO (Either String ExecutorQemuRes)
execCode mainCode = do
    resMem <- execQemuBin mainCode
    case resMem of
        Just mem -> makeRes mem
        Nothing -> return $ Left "Could not execute QEMU"
    where
        regAreaStart = 0x7E10
        regAreaSize = 64
        magic1Offset = 0x7E00
        magic2Offset = 0x7E02
        programDataOffset = 0x9800
        programDataSize = 0x1000
        minMemSize = 0x10000
        validateMemoryRes mem =
            validateMemorySize >> mapM_ validateMemoryMagic [(magic1Offset, 0x88), (magic1Offset+1, 0xee), (magic2Offset, 0x99), (magic2Offset+1, 0xff)]
            where
                validateMemorySize =
                    if B.length mem < minMemSize then
                        Left $ "Memory is truncated: " ++ (show $ B.length mem)
                                                       ++ " < "
                                                       ++ (show minMemSize)
                        else
                            Right ()
                validateMemoryMagic (offset, magic) =
                    if (B.index mem offset) /= magic then
                        Left $ "Magic is wrong on offset " ++ (show offset)
                                                     ++ ": "
                                                     ++ (show $ B.index mem offset)
                                                     ++ " != "
                                                     ++ (show magic)
                        else
                            Right ()
        makeRes mem = do
            let validationRes = validateMemoryRes mem
            case validateMemoryRes mem of
                Right _ ->
                    B.useAsCStringLen mem (\(ptr, len) -> do
                        let regPtr = plusPtr ptr regAreaStart
                            dataPtr = plusPtr ptr programDataOffset
                        regs <- B.packCStringLen (regPtr, regAreaSize)
                        progData <- B.packCStringLen (dataPtr, programDataSize)
                        return $ Right $ ExecutorQemuRes regs progData programDataOffset
                        )
                Left err -> return $ Left err

instance (MonadIO m) => ProgramExecutor ExecutorQemu ExecutorQemuRes m where
    execProgram _ mainCode = liftIO $ do
        res <- execCode mainCode
        return $ either (\_ -> ExecutorQemuRes B.empty B.empty 0) id res

-------------------------------------------------------------------------------

instance (MonadIO m) => OperandSupport ExecutorQemuRes Reg8 Uint8 m where
    readSourceOp er reg = liftIO $
        B.useAsCStringLen (eqrMemReg er) (\(ptr, len) ->
            readOpRaw (MemReg $ castPtr ptr) reg
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes Reg16 Uint16 m where
    readSourceOp er reg = liftIO $
        B.useAsCStringLen (eqrMemReg er) (\(ptr, len) ->
            readOpRaw (MemReg $ castPtr ptr) reg
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes RegSeg Uint16 m where
    readSourceOp er reg = liftIO $
        B.useAsCStringLen (eqrMemReg er) (\(ptr, len) ->
            readOpRaw (MemReg $ castPtr ptr) reg
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes MemPhy8 Uint8 m where
    readSourceOp er (MemPhy8 offset) = liftIO $
        B.useAsCStringLen (eqrMemMain er) (\(ptr, len) ->
            convAddr er offset >>= readOpRaw (MemMain $ castPtr ptr) . MemPhy8
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes MemPhy16 Uint16 m where
    readSourceOp er (MemPhy16 offset) = liftIO $
        B.useAsCStringLen (eqrMemMain er) (\(ptr, len) ->
            convAddr er offset >>= readOpRaw (MemMain $ castPtr ptr) . MemPhy16
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes MemRange MemRangeRes m where
    readSourceOp epr (MemRange start end) =
        MemRangeRes <$> mapM (\mem -> convAddr epr (fromIntegral mem) >>= readSourceOp epr . MemPhy8) [start..end]

instance (MonadIO m) => OperandSupport ExecutorQemuRes AllRegs String m where
    readSourceOp er _ = liftIO $
        B.useAsCStringLen (eqrMemReg er) (\(ptr, len) -> do
            sr <- (intercalate "\n") <$> (printRegs $ MemReg $ castPtr ptr)
            sf <- (intercalate "\n") <$> (printFlags $ MemReg $ castPtr ptr)
            return $ sr ++ "\n" ++ sf
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes Flag Bool m where
    readSourceOp er flag = liftIO $
        B.useAsCStringLen (eqrMemReg er) (\(ptr, len) ->
            readOpRaw (MemReg $ castPtr ptr) flag
            )

instance (MonadIO m) => OperandSupport ExecutorQemuRes EFlag Bool m where
    readSourceOp er flag = liftIO $
        B.useAsCStringLen (eqrMemReg er) (\(ptr, len) ->
            readOpRaw (MemReg $ castPtr ptr) flag
            )

-------------------------------------------------------------------------------
