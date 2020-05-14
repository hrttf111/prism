{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Cpu.Types where

import Data.Word (Word8, Word16, Word32)
import Data.Bits (FiniteBits, Bits)

import Foreign.Storable

-------------------------------------------------------------------------------

type OperandVal a =
    (Storable a, Bounded a, Integral a, FiniteBits a, Num a, Bits a)

class (Monad m, OperandVal v) => Operand a m v | a m -> v where
    readOp :: a -> m v
    writeOp :: a -> v -> m ()

-------------------------------------------------------------------------------

class (Monad m) => CpuFlag a m where
    getFlag :: a -> m Bool
    setFlag :: a -> Bool -> m ()

class (Monad m) => CpuFlags a m where
    getFlags :: m a
    setFlags :: a -> m ()

-------------------------------------------------------------------------------

type Imm8 = Uint8
type Imm16 = Uint16

type Uint8 = Word8
type Uint16 = Word16
type Uint32 = Word32

-------------------------------------------------------------------------------

newtype Reg8 = Reg8 Word8 deriving (Eq)
newtype Reg16 = Reg16 Word8 deriving (Eq)
newtype RegSeg = RegSeg Word8 deriving (Eq)
newtype RegSpec = RegSpec Word8 deriving (Eq)

class RegDecoder a where
    decodeReg :: Word8 -> a
    decodeRegVal :: a -> Word8

type OperandReg a m v = (RegDecoder a, Operand a m v)

instance Show RegSeg where
    show (RegSeg 0) = "ES"
    show (RegSeg 1) = "CS"
    show (RegSeg 2) = "SS"
    show (RegSeg 3) = "DS"

-------------------------------------------------------------------------------

type EA = Uint16 -- effective address
type Disp = Uint16
type MemOffset = Int
type MemSegType = Word8

data MemSeg = MemBxSi Disp |
              MemBxDi Disp |
              MemBpSi Disp |
              MemBpDi Disp |
              MemSi Disp |
              MemDi Disp |
              MemBp Disp |
              MemBx Disp |
              MemDirect Disp |
              MemSp deriving (Eq)

newtype MemSeg8 = MemSeg8 MemSeg deriving (Eq)
newtype MemSeg16 = MemSeg16 MemSeg deriving (Eq)

newtype MemPhy8 = MemPhy8 MemOffset deriving (Eq)
newtype MemPhy16 = MemPhy16 MemOffset deriving (Eq)

class MemDecoder a where
    decodeMemSeg :: MemSegType -> Disp -> a
    decodeMemDirect :: Disp -> a
    unwrapMemSeg :: a -> MemSeg
    wrapMemSeg :: MemSeg -> a

class (Operand a m v) => MemAddress a m v where
    getEA :: a -> m EA
    getPA :: a -> m MemOffset

type OperandMem a m v = (MemDecoder a, Operand a m v, MemAddress a m v)

-------------------------------------------------------------------------------

data Flag = CF | PF | AF | ZF | SF | OF deriving (Eq, Show)

data Flags = Flags {
        flagCF :: Bool,
        flagPF :: Bool,
        flagAF :: Bool,
        flagZF :: Bool,
        flagSF :: Bool,
        flagOF :: Bool
    } deriving (Show, Eq)

data EFlag = TF | IF | DF deriving (Eq, Show)

data EFlags = EFlags {
        eflagTF :: Bool,
        eflagIF :: Bool,
        eflagDF :: Bool
    } deriving (Show, Eq)

-------------------------------------------------------------------------------

newtype PrismInt = PrismInt Uint8 deriving (Eq)
newtype PrismIRQ = PrismIRQ Uint8 deriving (Eq)

-------------------------------------------------------------------------------

class Monad m => RunCpu c s m where
    runCpu :: s -> c -> m s

class ( Monad m
      , Operand Reg8 m Uint8
      , Operand Reg16 m Uint16
      , Operand RegSeg m Uint16
      , Operand RegSpec m Uint16
      , Operand MemSeg8 m Uint8
      , Operand MemSeg16 m Uint16
      , Operand MemPhy8 m Uint8
      , Operand MemPhy16 m Uint16
      , CpuFlag Flag m
      , CpuFlags Flags m
      , CpuFlag EFlag m
      , CpuFlags EFlags m
      ) => CpuMonad m where
    halt :: m ()
    incCycles :: m ()
    updateIP :: Uint16 -> m ()
    instrAddress :: m MemOffset
    needStop :: m Bool

-------------------------------------------------------------------------------
