{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Prism where

import Control.Monad.Trans

import Data.Word (Word8, Word16, Word32)
import Data.Bits (FiniteBits, Bits)
import Data.Maybe (Maybe)

import Foreign.Ptr
import GHC.Generics
import Data.Array.Unboxed

-------------------------------------------------------------------------------

type Imm8 = Uint8
type Imm16 = Uint16

type Uint8 = Word8
type Uint16 = Word16
type Uint32 = Word32

type Disp = Word16

type EA = Uint16
type MemOffset = Int

class Operand a b | a -> b where
    readOp :: MonadIO m => Ctx -> a -> m b
    writeOp :: MonadIO m => Ctx -> a -> b -> m ()

type OperandVal a = (Bounded a, Integral a, FiniteBits a, Num a, Bits a)

class ImmDecoder a where
    decodeImm :: Uint8 -> Uint8 -> a
    immLength :: a -> Uint16

-------------------------------------------------------------------------------

newtype Reg8 = Reg8 Word8 deriving (Eq)
newtype Reg16 = Reg16 Word8 deriving (Eq)
newtype RegSeg = RegSeg Word8 deriving (Eq)

class RegDecoder a where
    decodeReg :: Word8 -> a
    decodeRegVal :: a -> Word8

type OperandReg a b = (RegDecoder a, Operand a b)

instance Show RegSeg where
    show (RegSeg 0) = "ES"
    show (RegSeg 1) = "CS"
    show (RegSeg 2) = "SS"
    show (RegSeg 3) = "DS"

-------------------------------------------------------------------------------

data Mem = MemBxSi Disp |
           MemBxDi Disp |
           MemBpSi Disp |
           MemBpDi Disp |
           MemSi Disp |
           MemDi Disp |
           MemBp Disp |
           MemBx Disp |
           MemDirect Disp deriving (Eq)

newtype Mem8 = Mem8 Mem deriving (Eq)
newtype Mem16 = Mem16 Mem deriving (Eq)

class MemDecoder a where
    decodeMem1 :: Word8 -> Disp -> a
    decodeMemDirect :: Disp -> a
    unwrapMem :: a -> Mem
    wrapMem :: Mem -> a

class (MemDecoder a, Operand a b) => OperandMem a b | a -> b where
    readMemOp :: MonadIO m => Ctx -> RegSeg -> a -> m b
    writeMemOp :: MonadIO m => Ctx -> RegSeg -> a -> b -> m ()

-------------------------------------------------------------------------------

newtype MemIO = MemIO (UArray MemOffset Bool) deriving (Show)

class MemIOHandler a where
    memIORead8 :: MonadIO m => a -> Ctx -> MemOffset -> m Uint8
    memIOWrite8 :: MonadIO m => a -> Ctx -> MemOffset -> Uint8 -> m ()
    memIORead16 :: MonadIO m => a -> Ctx -> MemOffset -> m Uint16
    memIOWrite16 :: MonadIO m => a -> Ctx -> MemOffset -> Uint16 -> m ()

data EmptyMemIOH = EmptyMemIOH deriving (Show)

instance MemIOHandler EmptyMemIOH where
    memIORead8 _ _ _ = return 0
    memIOWrite8 _ _ _ _ = return ()
    memIORead16 _ _ _ = return 0
    memIOWrite16 _ _ _ _ = return ()

data MemIOCtx = MemIOCtx {
        memIOHandler :: EmptyMemIOH,
        memIOPageSize :: Int,
        memIORegion :: MemIO
    }

instance Show MemIOCtx where
    show c = "MemIOCtx " ++ (show $ memIORegion c)

-------------------------------------------------------------------------------

-- Note: Max size of instruction is 6 bytes
type InstrBytes = (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8)

newtype MemReg = MemReg (Ptr Word8) deriving (Show)
newtype MemMain = MemMain (Ptr Word8) deriving (Show)

data Flags = Flags {
        flagCF :: Bool,
        flagPF :: Bool,
        flagAF :: Bool,
        flagZF :: Bool,
        flagSF :: Bool,
        flagOF :: Bool
    } deriving (Show, Eq)

data EFlags = EFlags {
        eflagTF :: Bool,
        eflagIF :: Bool,
        eflagDF :: Bool
    } deriving (Show)

clearFlags :: Flags
clearFlags = Flags False False False False False False

clearEFlags :: EFlags
clearEFlags = EFlags False False False

newtype PrismInt = PrismInt Uint8 deriving (Eq)

instance Show PrismInt where
    show (PrismInt val) = "Int " ++ (show val)

data PrismInterrupts = PrismInterrupts {
        intListHigh :: [PrismInt],
        intListNmi :: [PrismInt],
        intListIntr :: [PrismInt],
        intSingleStep :: [PrismInt],
        intInterruptUp :: Bool
    } deriving (Show)

emptyPrismInterrupts = PrismInterrupts [] [] [] [] False
emptyMemIO = MemIO $ array (0, 0) [(0, False)]
emptyMemIOCtx = MemIOCtx EmptyMemIOH (1024 * 1024) emptyMemIO
makePrismCtx memReg memMain = Ctx memReg memMain clearFlags clearEFlags noReplaceSeg noStop emptyMemIOCtx emptyPrismInterrupts

noReplaceSeg ::  Maybe RegSeg
noReplaceSeg = Nothing

noStop = False

data Ctx = Ctx {
        ctxReg :: MemReg,
        ctxMem :: MemMain,
        ctxFlags :: Flags,
        ctxEFlags :: EFlags,
        ctxReplaceSeg :: Maybe RegSeg,
        ctxStop :: Bool,
        ctxMemIO :: MemIOCtx,
        ctxInterrupts :: PrismInterrupts
    } deriving (Show)

newtype PrismCtx m a = PrismCtx {
    runPrism :: m a
} deriving (Monad, Applicative, Functor)

type PrismM = PrismCtx IO Ctx

instance MonadTrans PrismCtx where
    lift = PrismCtx

instance MonadIO m => MonadIO (PrismCtx m) where
    liftIO = lift . liftIO
