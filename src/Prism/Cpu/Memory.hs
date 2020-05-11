{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Cpu.Memory (
        MemSeg8, MemSeg16, MemPhy8, MemPhy16
    ) where

import Control.Monad.State.Strict (get)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Maybe (fromMaybe)
import Data.Bits (shiftL)

import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Registers

-------------------------------------------------------------------------------

instance Operand MemSeg8 CpuTrans Uint8 where
    readOp (MemSeg8 memSeg) = do
        s <- get
        offset <- getMemOffset s memSeg
        readOp $ MemPhy8 offset
    writeOp (MemSeg8 memSeg) val = do
        s <- get
        offset <- getMemOffset s memSeg
        writeOp (MemPhy8 offset) val

instance Operand MemSeg16 CpuTrans Uint16 where
    readOp (MemSeg16 memSeg) = do
        s <- get
        offset <- getMemOffset s memSeg
        readOp $ MemPhy16 offset
    writeOp (MemSeg16 memSeg) val = do
        s <- get
        offset <- getMemOffset s memSeg
        writeOp (MemPhy16 offset) val

-------------------------------------------------------------------------------

instance Operand MemPhy8 CpuTrans Uint8 where
    readOp (MemPhy8 offset) = do
        (MemMain mm) <- ctxMem <$> get
        liftIO $ peekByteOff mm offset
    writeOp (MemPhy8 offset) val = do
        (MemMain mm) <- ctxMem <$> get
        liftIO $ pokeByteOff mm offset val

instance Operand MemPhy16 CpuTrans Uint16 where
    readOp (MemPhy16 offset) = do
        (MemMain mm) <- ctxMem <$> get
        liftIO $ peekByteOff mm offset
    writeOp (MemPhy16 offset) val = do
        (MemMain mm) <- ctxMem <$> get
        liftIO $ pokeByteOff mm offset val

-------------------------------------------------------------------------------

getMemOffset :: (MonadIO m) => Ctx -> MemSeg -> m MemOffset
getMemOffset ctx mem = getMemOffsetI (ctxReg ctx) (ctxReplaceSeg ctx) mem

{-# SPECIALISE INLINE getMemOffset :: Ctx -> MemSeg -> CpuTrans MemOffset #-}

getMemOffsetI :: (MonadIO m) => MemReg -> Maybe RegSeg -> MemSeg -> m MemOffset
getMemOffsetI memReg regSeg (MemBxSi disp) =
    getMemReg3 memReg bx si (findRegSeg1 ds regSeg) disp
getMemOffsetI memReg regSeg (MemBxDi disp) =
    getMemReg3 memReg bx di (findRegSeg1 ds regSeg) disp
getMemOffsetI memReg regSeg (MemBpSi disp) =
    getMemReg3 memReg bp si (findRegSeg1 ss regSeg) disp
getMemOffsetI memReg regSeg (MemBpDi disp) =
    getMemReg3 memReg bp di (findRegSeg1 ss regSeg) disp
getMemOffsetI memReg regSeg (MemSi disp) =
    getMemReg2 memReg si (findRegSeg1 ds regSeg) disp
getMemOffsetI memReg regSeg (MemDi disp) =
    getMemReg2 memReg di (findRegSeg1 ds regSeg) disp
getMemOffsetI memReg regSeg (MemBp disp) =
    getMemReg2 memReg bp (findRegSeg1 ss regSeg) disp
getMemOffsetI memReg regSeg (MemBx disp) =
    getMemReg2 memReg bx (findRegSeg1 ds regSeg) disp
getMemOffsetI memReg regSeg (MemDirect disp) =
    getMemReg1 memReg (findRegSeg1 ds regSeg) disp

{-# SPECIALISE INLINE getMemOffsetI :: MemReg -> Maybe RegSeg -> MemSeg -> CpuTrans MemOffset #-}

findRegSeg1 :: RegSeg -> Maybe RegSeg -> RegSeg
findRegSeg1 = fromMaybe

{-# INLINE findRegSeg1 #-}

getMemReg3 :: (MonadIO m) => MemReg -> Reg16 -> Reg16 -> RegSeg -> Disp -> m MemOffset
getMemReg3 memReg reg1 reg2 regSeg disp = do
    valR1 <- fromIntegral <$> readReg16 memReg reg1
    valR2 <- fromIntegral <$> readReg16 memReg reg2
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let disp32 = fromIntegral disp :: MemOffset
    return $ (shiftL valSeg 4) + valR1 + valR2 + disp32

{-# SPECIALISE INLINE getMemReg3 :: MemReg -> Reg16 -> Reg16 -> RegSeg -> Disp -> CpuTrans MemOffset #-}

getMemReg2 :: (MonadIO m) => MemReg -> Reg16 -> RegSeg -> Disp -> m MemOffset
getMemReg2 memReg reg1 regSeg disp = do
    valR1 <- fromIntegral <$> readReg16 memReg reg1
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let disp32 = fromIntegral disp :: MemOffset
    return $ (shiftL valSeg 4) + valR1 + disp32

{-# SPECIALISE INLINE getMemReg2 :: MemReg -> Reg16 -> RegSeg -> Disp -> CpuTrans MemOffset #-}

getMemReg1 :: (MonadIO m) => MemReg -> RegSeg -> Disp -> m MemOffset
getMemReg1 memReg regSeg disp = do
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let disp32 = fromIntegral disp :: MemOffset
    return $ (shiftL valSeg 4) + disp32

{-# SPECIALISE INLINE getMemReg1 :: MemReg -> RegSeg -> Disp -> CpuTrans MemOffset #-}

-------------------------------------------------------------------------------
