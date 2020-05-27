{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Cpu.Types where

import Control.Monad.Trans (MonadIO)

import Data.Word (Word8, Word16, Word32)
import Data.Bits (FiniteBits, Bits)

import Foreign.Storable

-------------------------------------------------------------------------------

type OperandVal a =
    (Storable a, Bounded a, Integral a, FiniteBits a, Num a, Bits a, Show a)

class (Show a, Monad m, OperandVal v) => Operand a m v | a m -> v where
    readOp :: a -> m v
    writeOp :: a -> v -> m ()

-------------------------------------------------------------------------------

class (Show a, Monad m) => CpuFlag a m where
    getFlag :: a -> m Bool
    setFlag :: a -> Bool -> m ()

class (Show a, Monad m) => CpuFlags a m where
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

class MemRegManipulator a p v | a p -> v where
    readRegRaw :: (MonadIO m) => p -> a -> m v

type OperandReg a m v = (RegDecoder a, Operand a m v)

-------------------------------------------------------------------------------

type EA = Uint16 -- effective address
type Disp = Uint16
type MemOffset = Int
type MemSegType = Word8

class MemArithmetics a where
    mapMem :: (Disp -> Disp) -> a -> a

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

newtype MemSegExp8 = MemSegExp8 (RegSeg, MemSeg) deriving (Eq)
newtype MemSegExp16 = MemSegExp16 (RegSeg, MemSeg) deriving (Eq)

newtype MemPhy8 = MemPhy8 MemOffset deriving (Eq)
newtype MemPhy16 = MemPhy16 MemOffset deriving (Eq)

class MemSegWrapper a where
    unwrapMemSeg :: a -> MemSeg
    wrapMemSeg :: MemSeg -> a

class (MemSegWrapper a) => MemDecoder a where
    decodeMemSeg :: MemSegType -> Disp -> a
    decodeMemDirect :: Disp -> a

class (Operand a m v) => MemAddress a m v where
    getEA :: a -> m EA
    getPA :: a -> m MemOffset

type OperandMem a m v = ( MemDecoder a
                        , MemArithmetics a
                        , Operand a m v
                        , MemAddress a m v )

-------------------------------------------------------------------------------

newtype Port8 = Port8 Uint16 deriving (Eq)
newtype Port16 = Port16 Uint16 deriving (Eq)

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

class (Monad m) => InterruptRun m where
    interruptActive :: m Bool
    retInterrupt :: m ()
    processInterrupts :: m ()
    raiseInterrupt :: PrismInt -> m ()

class (Monad m) => InterruptDispatcher m where
    dispatchIrqUp :: PrismIRQ -> m Bool
    dispatchIrqDown :: PrismIRQ -> m Bool
    ackIrq :: m PrismInt

-------------------------------------------------------------------------------

class (Monad m) => CpuDebug m where
    cpuLog :: String -> m ()

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
      , Operand MemSegExp8 m Uint8
      , Operand MemSegExp16 m Uint16
      , Operand MemPhy8 m Uint8
      , Operand MemPhy16 m Uint16
      , MemAddress MemSeg8 m Uint8
      , MemAddress MemSeg16 m Uint16
      , Operand Port8 m Uint8
      , Operand Port16 m Uint16
      , CpuFlag Flag m
      , CpuFlags Flags m
      , CpuFlag EFlag m
      , CpuFlags EFlags m
      , InterruptDispatcher m
      , InterruptRun m
      , CpuDebug m
      ) => CpuMonad m where
    cpuHalt :: m ()
    cpuOverrideSegment :: Maybe RegSeg -> m ()
    cpuUpdateIP :: Uint16 -> m ()
    cpuInstrAddress :: m MemOffset
    cpuNextInstrByte :: m Uint8

-------------------------------------------------------------------------------
