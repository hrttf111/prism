{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Cpu.Registers (
        Reg8, Reg16, RegSeg
        , readReg8, writeReg8
        , readReg16, writeReg16
        , readSeg, writeSeg
        , al, cl, dl, bl, ah, ch, dh, bh
        , ax, cx, dx, bx, sp, bp, si, di
        , cs, ds, es, ss
        , ip, flagsInternal
        , printRegs, readRegFlags 
    ) where

import Control.Monad.State.Strict (get)
import Control.Monad.Trans (MonadIO, liftIO)

import Numeric (showHex)

import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism.Cpu.Types
import Prism.Cpu.Monad

-------------------------------------------------------------------------------

al = Reg8 0
cl = Reg8 1
dl = Reg8 2
bl = Reg8 3
ah = Reg8 4
ch = Reg8 5
dh = Reg8 6
bh = Reg8 7

instance Show Reg8 where
    show (Reg8 0) = "AL"
    show (Reg8 1) = "CL"
    show (Reg8 2) = "DL"
    show (Reg8 3) = "BL"
    show (Reg8 4) = "AH"
    show (Reg8 5) = "CH"
    show (Reg8 6) = "DH"
    show (Reg8 7) = "BH"

ax = Reg16 0
cx = Reg16 1
dx = Reg16 2
bx = Reg16 3
sp = Reg16 4
bp = Reg16 5
si = Reg16 6
di = Reg16 7

instance Show Reg16 where
    show (Reg16 0) = "AX"
    show (Reg16 1) = "CX"
    show (Reg16 2) = "DX"
    show (Reg16 3) = "BX"
    show (Reg16 4) = "SP"
    show (Reg16 5) = "BP"
    show (Reg16 6) = "SI"
    show (Reg16 7) = "DI"

es = RegSeg 0
cs = RegSeg 1
ss = RegSeg 2
ds = RegSeg 3

instance Show RegSpec where
    show (RegSpec 0) = "IP"
    show (RegSpec 1) = "Flags[Internal]"

ip = RegSpec 0
flagsInternal = RegSpec 1

-------------------------------------------------------------------------------

instance Operand Reg8 CpuTrans Uint8 where
    readOp reg = do
        memReg <- ctxReg <$> get
        readReg8 memReg reg
    writeOp reg val = do
        memReg <- ctxReg <$> get
        writeReg8 memReg reg val

instance Operand Reg16 CpuTrans Uint16 where
    readOp reg = do
        memReg <- ctxReg <$> get
        readReg16 memReg reg
    writeOp reg val = do
        memReg <- ctxReg <$> get
        writeReg16 memReg reg val

instance Operand RegSeg CpuTrans Uint16 where
    readOp reg = do
        memReg <- ctxReg <$> get
        readSeg memReg reg
    writeOp reg val = do
        memReg <- ctxReg <$> get
        writeSeg memReg reg val

instance Operand RegSpec CpuTrans Uint16 where
    readOp (RegSpec 0) = do
        memReg <- ctxReg <$> get
        readRegIP memReg
    readOp (RegSpec 1) = do
        memReg <- ctxReg <$> get
        readRegFlags memReg
    writeOp (RegSpec 0) val = do
        memReg <- ctxReg <$> get
        writeRegIP memReg val
    writeOp (RegSpec 1) val = do
        memReg <- ctxReg <$> get
        writeRegFlags memReg val

-------------------------------------------------------------------------------

readReg8 :: MonadIO m => MemReg -> Reg8 -> m Uint8
readReg8 (MemReg mr) (Reg8 ii) | ii < 4 =
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr (i * 2)
readReg8 (MemReg mr) (Reg8 ii) = 
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr ((i - 4) * 2 + 1)

{-# SPECIALISE INLINE readReg8 :: MemReg -> Reg8 -> CpuTrans Uint8 #-}

writeReg8 :: MonadIO m => MemReg -> Reg8 -> Uint8 -> m ()
writeReg8 (MemReg mr) (Reg8 ii) val | ii < 4 =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr (i * 2) val
writeReg8 (MemReg mr) (Reg8 ii) val =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr ((i - 4) * 2 + 1) val

{-# SPECIALISE INLINE writeReg8 :: MemReg -> Reg8 -> Uint8 -> CpuTrans () #-}

-------------------------------------------------------------------------------

readReg16 :: MonadIO m => MemReg -> Reg16 -> m Uint16
readReg16 (MemReg mr) (Reg16 ii) =
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr (i * 2)

{-# SPECIALISE INLINE readReg16 :: MemReg -> Reg16 -> CpuTrans Uint16 #-}

writeReg16 :: MonadIO m => MemReg -> Reg16 -> Uint16 -> m ()
writeReg16 (MemReg mr) (Reg16 ii) val =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr (i * 2) val

{-# SPECIALISE INLINE writeReg16 :: MemReg -> Reg16 -> Uint16 -> CpuTrans () #-}

-------------------------------------------------------------------------------

readSeg :: MonadIO m => MemReg -> RegSeg -> m Uint16
readSeg (MemReg mr) (RegSeg ii) =
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr (32 + i * 2)

{-# SPECIALISE INLINE readSeg :: MemReg -> RegSeg -> CpuTrans Uint16 #-}

writeSeg :: MonadIO m => MemReg -> RegSeg -> Uint16 -> m ()
writeSeg (MemReg mr) (RegSeg ii) val =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr (32 + i * 2) val

{-# SPECIALISE INLINE writeSeg :: MemReg -> RegSeg -> Uint16 -> CpuTrans () #-}

-------------------------------------------------------------------------------

readRegIP :: MonadIO m => MemReg -> m Uint16
readRegIP (MemReg mr) =
    liftIO $ peekByteOff mr 40

{-# SPECIALISE INLINE readRegIP :: MemReg -> CpuTrans Uint16 #-}

writeRegIP :: MonadIO m => MemReg -> Uint16 -> m ()
writeRegIP (MemReg mr) val =
    liftIO $ pokeByteOff mr 40 val

{-# SPECIALISE INLINE writeRegIP :: MemReg -> Uint16 -> CpuTrans () #-}

readRegFlags :: MonadIO m => MemReg -> m Uint16
readRegFlags (MemReg mr) = do
    liftIO $ peekByteOff mr 42

{-# SPECIALISE INLINE readRegFlags :: MemReg -> CpuTrans Uint16 #-}

writeRegFlags :: MonadIO m => MemReg -> Uint16 -> m ()
writeRegFlags (MemReg mr) val =
    liftIO $ pokeByteOff mr 42 val

{-# SPECIALISE INLINE writeRegFlags :: MemReg -> Uint16 -> CpuTrans () #-}

-------------------------------------------------------------------------------

showReg8 :: MonadIO m => MemReg -> Reg8 -> m String
showReg8 memReg reg = do
    val <- readReg8 memReg reg
    return $ show reg ++ " = 0x" ++ showHex val ""

showReg16 :: MonadIO m => MemReg -> Reg16 -> m String
showReg16 memReg reg = do
    val <- readReg16 memReg reg
    return $ show reg ++ " = 0x" ++ showHex val ""

showRegSeg :: MonadIO m => MemReg -> RegSeg -> m String
showRegSeg memReg reg = do
    val <- readSeg memReg reg
    return $ show reg ++ " = 0x" ++ showHex val ""

showRegs3 :: MonadIO m => MemReg -> Reg8 -> Reg8 -> Reg16 -> m String
showRegs3 memReg reg1 reg2 reg3 = do
    s1 <- showReg8 memReg reg1
    s2 <- showReg8 memReg reg2
    s3 <- showReg16 memReg reg3
    return $ s1 ++ " " ++ s2 ++ " " ++ s3

showRegIP :: MonadIO m => MemReg -> m String
showRegIP memReg = do
    val <- readRegIP memReg
    return $ "IP = 0x" ++ showHex val ""

printRegs :: MonadIO m => MemReg -> m [String]
printRegs memReg =
    mapM (\a -> a) [
        showRegs3 memReg al ah ax,
        showRegs3 memReg bl bh bx,
        showRegs3 memReg cl ch cx,
        showRegs3 memReg dl dh dx,
        showReg16 memReg sp,
        showReg16 memReg bp,
        showReg16 memReg si,
        showReg16 memReg di,
        showRegSeg memReg es,
        showRegSeg memReg cs,
        showRegSeg memReg ss,
        showRegSeg memReg ds,
        showRegIP memReg
        ]

-------------------------------------------------------------------------------

instance MemRegManipulator Reg8 MemReg Uint8 where
    readRegRaw = readReg8

instance MemRegManipulator Reg16 MemReg Uint16 where
    readRegRaw = readReg16

instance MemOpManipulator Reg8 MemReg Uint8 where
    readOpRaw = readReg8

instance MemOpManipulator Reg16 MemReg Uint16 where
    readOpRaw = readReg16

instance MemOpManipulator RegSeg MemReg Uint16 where
    readOpRaw = readSeg

-------------------------------------------------------------------------------
