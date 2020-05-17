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

import Numeric (showHex)

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

instance MemAddress MemSeg8 CpuTrans Uint8 where
    getEA (MemSeg8 memSeg) = do
        s <- get
        getMemEA s memSeg
    getPA (MemSeg8 memSeg) = do
        s <- get
        getMemOffset s memSeg

instance MemAddress MemSeg16 CpuTrans Uint16 where
    getEA (MemSeg16 memSeg) = do
        s <- get
        getMemEA s memSeg
    getPA (MemSeg16 memSeg) = do
        s <- get
        getMemOffset s memSeg

-------------------------------------------------------------------------------

instance MemAddress MemPhy8 CpuTrans Uint8 where
    getEA (MemPhy8 offset) = return 0
    getPA (MemPhy8 offset) = return offset

instance MemAddress MemPhy16 CpuTrans Uint16 where
    getEA (MemPhy16 offset) = return 0
    getPA (MemPhy16 offset) = return offset

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
getMemOffsetI memReg regSeg MemSp =
    getMemReg2 memReg sp ss 0

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

getMemEA :: (MonadIO m) => Ctx -> MemSeg -> m EA
getMemEA ctx mem = getEAI (ctxReg ctx) mem

getEA3 :: MonadIO m => MemReg -> Reg16 -> Reg16 -> Disp -> m EA
getEA3 memReg reg1 reg2 disp = do
    valR1 <- fromIntegral <$> readReg16 memReg reg1
    valR2 <- fromIntegral <$> readReg16 memReg reg2
    return $ valR1 + valR2 + disp

getEA2 :: MonadIO m => MemReg -> Reg16 -> Disp -> m EA
getEA2 memReg reg1 disp = do
    valR1 <- readReg16 memReg reg1
    return $ valR1 + disp

getEAI :: MonadIO m => MemReg -> MemSeg -> m EA
getEAI memReg (MemBxSi disp) = getEA3 memReg bx si disp
getEAI memReg (MemBxDi disp) = getEA3 memReg bx di disp
getEAI memReg (MemBpSi disp) = getEA3 memReg bp si disp
getEAI memReg (MemBpDi disp) = getEA3 memReg bp di disp
getEAI memReg (MemSi disp) = getEA2 memReg si disp
getEAI memReg (MemDi disp) = getEA2 memReg di disp
getEAI memReg (MemBp disp) = getEA2 memReg bp disp
getEAI memReg (MemBx disp) = getEA2 memReg bx disp
getEAI memReg (MemDirect disp) = return disp
getEAI memReg MemSp = return 0

-------------------------------------------------------------------------------

showMem3 :: Reg16 -> Reg16 -> RegSeg -> Disp -> String
showMem3 reg1 reg2 regSeg@(RegSeg 3) 0 =
    "[" ++ (show reg1) ++ " + " ++ (show reg2) ++ "]"
showMem3 reg1 reg2 regSeg@(RegSeg 3) disp =
    "[" ++ (show reg1) ++ " + " ++ (show reg2) ++ " + 0x" ++ (showHex disp "]")
showMem3 reg1 reg2 regSeg 0 =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ " + " ++ (show reg2) ++ "]"
showMem3 reg1 reg2 regSeg disp =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ " + " ++ (show reg2) ++ " + 0x" ++ (showHex disp "]")

showMem2 :: Reg16 -> RegSeg -> Disp -> String
showMem2 reg1 regSeg@(RegSeg 3) 0 =
    "[" ++ (show reg1) ++  "]"
showMem2 reg1 regSeg@(RegSeg 3) disp =
    "[" ++ (show reg1) ++ " + 0x" ++ (showHex disp "]")
showMem2 reg1 regSeg 0 =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ "]"
showMem2 reg1 regSeg disp =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ " + 0x" ++ (showHex disp "]")

showMem1 :: RegSeg -> Disp -> String
showMem1 regSeg@(RegSeg 3) disp =
    "[0x" ++ (showHex disp "]")
showMem1 regSeg disp =
    "[" ++ (show regSeg) ++ ":0x" ++ (showHex disp "]")

instance Show MemSeg where
    show (MemBxSi disp) = showMem3 bx si ds disp
    show (MemBxDi disp) = showMem3 bx di ds disp
    show (MemBpSi disp) = showMem3 bp si ss disp
    show (MemBpDi disp) = showMem3 bp di ss disp
    show (MemSi disp) = showMem2 si ds disp
    show (MemDi disp) = showMem2 di ds disp
    show (MemBp disp) = showMem2 bp ss disp
    show (MemBx disp) = showMem2 bx ds disp
    show (MemDirect disp) = showMem1 ds disp
    show MemSp = showMem2 sp ss 0

instance Show MemSeg8 where
    show (MemSeg8 mem) = show mem

instance Show MemSeg16 where
    show (MemSeg16 mem) = show mem

instance Show MemPhy8 where
    show (MemPhy8 mem) = show mem

instance Show MemPhy16 where
    show (MemPhy16 mem) = show mem

-------------------------------------------------------------------------------
