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
import Control.Concurrent.STM.TQueue

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

data IOCmdType = IOMemType | IOPortType

data IOCmd = IOCmdRead8 IOCmdType MemOffset
             | IOCmdRead16 IOCmdType MemOffset 
             | IOCmdWrite8 IOCmdType MemOffset Uint8 
             | IOCmdWrite16 IOCmdType MemOffset Uint16 

data IOCmdData = IOCmdData8 Uint8
                 | IOCmdData16 Uint16

data IOQueue = IOQueue {
        ioQueueReq :: TQueue IOCmd,
        ioQueueRsp :: TQueue IOCmdData
    }

class (OperandVal a) => IOVal a where
    ioValRead :: MonadIO m => IOQueue -> IOCmdType -> MemOffset -> m a
    ioValWrite :: MonadIO m => IOQueue -> IOCmdType -> MemOffset -> a -> m ()
    ioValRespond :: MonadIO m => IOQueue -> a -> m ()

class IOMem a where
    ioMemRead :: (MonadIO m, IOVal b, OperandVal b) => a -> Int -> m b
    ioMemWrite :: (MonadIO m, IOVal b, OperandVal b) => a -> Int -> b -> m ()

class IOPort a where
    ioPortRead :: (MonadIO m, IOVal b, OperandVal b) => a -> Uint16 -> m b
    ioPortWrite :: (MonadIO m, IOVal b, OperandVal b) => a -> Uint16 -> b -> m ()

newtype MemIORegion = MemIORegion (UArray MemOffset Bool) deriving (Show)

data IOCtx = IOCtx {
        ioCtxQueue :: IOQueue,
        ioCtxPageSize :: Int,
        ioCtxMemRegion :: MemIORegion
    }

instance Show IOCtx where
    show c = "IOCtx " ++ (show $ ioCtxMemRegion c)

instance IOMem IOCtx where
    ioMemRead ctx offset = ioValRead (ioCtxQueue ctx) IOMemType offset
    ioMemWrite ctx offset val = ioValWrite (ioCtxQueue ctx) IOMemType offset val

instance IOPort IOCtx where
    ioPortRead ctx offset = ioValRead (ioCtxQueue ctx) IOPortType (fromIntegral offset)
    ioPortWrite ctx offset val = ioValWrite (ioCtxQueue ctx) IOPortType (fromIntegral offset) val

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

createIOQueue = IOQueue <$> newTQueueIO <*> newTQueueIO
emptyPrismInterrupts = PrismInterrupts [] [] [] [] False
emptyMemIORegion = MemIORegion $ array (0, 0) [(0, False)]
emptyIOCtx queue = IOCtx queue (1024 * 1024) emptyMemIORegion
makePrismCtx memReg memMain ioCtx =
    Ctx memReg memMain clearFlags clearEFlags noReplaceSeg noStop ioCtx emptyPrismInterrupts

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
        ctxIO :: IOCtx,
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
