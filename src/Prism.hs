{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE ExistentialQuantification #-}
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

import Data.IORef
import Foreign.Storable
import Foreign.Ptr
import GHC.Generics
import Data.Array.Unboxed (UArray, array)
import Data.Array (Array)

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

type OperandVal a = (Storable a, Bounded a, Integral a, FiniteBits a, Num a, Bits a)

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

type IOHandlerIndex = Uint16
type IOPageIndex = Int -- div offset pageSize (or) shiftR n $ offset .&. mask
type IOPageOffset = Int -- mod offset pageSize (or) offset - IOPageIndex

data MemLocation = MemLocation {
        memLocationStart :: MemOffset,
        memLocationEnd :: MemOffset
    } deriving (Eq)

newtype IOPage = IOPage (UArray IOPageOffset IOHandlerIndex) deriving (Show, Eq)

newtype PortIORegion = PortIORegion (UArray Uint16 IOHandlerIndex) deriving (Show, Eq)

data MemIORegion = MemIORegion {
        ioPageSize :: Int,
        ioRegionL1 :: UArray Int IOPageIndex,
        ioRegionL2 :: Array IOPageIndex IOPage
    } deriving (Show)

-- fill 3 arrays:
-- 1. L1 which is Int -> Int
--    is fixed size == memorySize/pageSize
--    unused indexes are occupied by emptyPage
-- 2. L2 which is Int -> IOPage
--    is variable size, depends on mapped pages count
--    indexes for pages are generated in run-time
-- 3. IOPage which is Uint16 -> Uint16
--    is fixed size
--    unused indexes are occupied by emptyHandler

data PeripheralHandlerMem p = PeripheralHandlerMem {
        peripheralMemWrite8 :: p -> MemOffset -> Uint8 -> IO p,
        peripheralMemWrite16 :: p -> MemOffset -> Uint16 -> IO p,
        peripheralMemRead8 :: p -> MemOffset -> IO (p, Uint8),
        peripheralMemRead16 :: p -> MemOffset -> IO (p, Uint16)
    }

data PeripheralHandlerPort p = PeripheralHandlerPort {
        peripheralPortWrite8 :: p -> Uint16 -> Uint8 -> IO p,
        peripheralPortWrite16 :: p -> Uint16 -> Uint16 -> IO p,
        peripheralPortRead8 :: p -> Uint16 -> IO (p, Uint8),
        peripheralPortRead16 :: p -> Uint16 -> IO (p, Uint16)
    }

type IOVal a = (IOValMem a, IOValPort a, IOValRemote a)

class (OperandVal a) => IOValRemote a where
    ioValRemoteRead :: MonadIO m => IOQueue -> IOCmdType -> IOHandlerIndex -> MemOffset -> m a
    ioValRemoteWrite :: MonadIO m => IOQueue -> IOCmdType -> IOHandlerIndex -> MemOffset -> a -> m ()
    ioValRemoteRespond :: MonadIO m => IOQueue -> a -> m ()

class (OperandVal a) => IOValMem a where
    ioValMemRead :: MonadIO m => p -> PeripheralHandlerMem p -> MemOffset -> m (p, a)
    ioValMemWrite :: MonadIO m => p -> PeripheralHandlerMem p -> MemOffset -> a -> m p

class (OperandVal a) => IOValPort a where
    ioValPortRead :: MonadIO m => p -> PeripheralHandlerPort p -> Uint16 -> m (p, a)
    ioValPortWrite :: MonadIO m => p -> PeripheralHandlerPort p -> Uint16 -> a -> m p

class IOMem a where
    ioMemRead :: (MonadIO m, IOVal b) => a -> IOHandlerIndex -> MemOffset -> m b
    ioMemWrite :: (MonadIO m, IOVal b) => a -> IOHandlerIndex -> MemOffset -> b -> m ()

class IOPort a where
    ioPortRead :: (MonadIO m, IOVal b) => a -> IOHandlerIndex -> Uint16 -> m b
    ioPortWrite :: (MonadIO m, IOVal b) => a -> IOHandlerIndex -> Uint16 -> b -> m ()

-------------------------------------------------------------------------------

class InterruptDispatcher s where
    dispatchInterruptUp :: s -> PrismInt -> IO (s, Bool)
    dispatchInterruptDown :: s -> PrismInt -> IO (s, Bool)
    ackInterrupt :: s -> IO (s, PrismInt)

class PeripheralRunner s where
    runPeripherals :: Ctx -> s -> IO (Ctx, s)
    needUpdate :: s -> IO Bool
    peripheralCycles :: s -> IO Int

-------------------------------------------------------------------------------

data IOCmdType = IOMemType | IOPortType deriving (Show)

data IOCmd = IOCmdRead8 IOCmdType IOHandlerIndex MemOffset
             | IOCmdRead16 IOCmdType IOHandlerIndex MemOffset 
             | IOCmdWrite8 IOCmdType IOHandlerIndex MemOffset Uint8 
             | IOCmdWrite16 IOCmdType IOHandlerIndex MemOffset Uint16 
             deriving (Show)

data IOCmdData = IOCmdData8 Uint8
                 | IOCmdData16 Uint16

data IOQueue = IOQueue {
        ioQueueReq :: TQueue IOCmd,
        ioQueueRsp :: TQueue IOCmdData
    }

data IOCtx = forall a . (IOCtxInternal a) => IOCtx {
        ioCtxInternal :: a,
        ioCtxMemRegion :: MemIORegion,
        ioCtxPortRegion :: PortIORegion
    }

instance Show IOCtx where
    show c = "IOCtx " ++ (show $ ioCtxMemRegion c)

type IOCtxInternal a = (IOMem a, IOPort a, InterruptDispatcher a, PeripheralRunner a)

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
        intIntrOn :: Bool,
        intSingleStep :: [PrismInt],
        intInterruptUp :: Bool
    } deriving (Show)

emptyPrismInterrupts = PrismInterrupts [] [] False [] False
makePrismCtx memReg memMain ioCtx =
    Ctx memReg memMain clearFlags clearEFlags noReplaceSeg noStop maxCycles ioCtx emptyPrismInterrupts

noReplaceSeg ::  Maybe RegSeg
noReplaceSeg = Nothing

noStop = False
maxCycles = 999999999

data Ctx = Ctx {
        ctxReg :: MemReg,
        ctxMem :: MemMain,
        ctxFlags :: Flags,
        ctxEFlags :: EFlags,
        ctxReplaceSeg :: Maybe RegSeg,
        ctxStop :: Bool,
        ctxCycles :: Int,
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
