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
    ) where

import Control.Monad.State.Strict (get)
import Control.Monad.Trans (MonadIO, liftIO)

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
