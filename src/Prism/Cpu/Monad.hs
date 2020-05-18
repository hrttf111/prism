{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Cpu.Monad (
          Ctx (..)
        , MemReg (..), MemMain (..)
        , RunCpu (..)
        , CpuTransM (..), CpuTrans (..)
        , allocMemRegRaw, allocMemReg, allocMemMain
        , clearRegs, copyMainMem 
        , makeCtx, makeTransM
        , peekFirstByte
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict --(modify, MonadState, StateT)

import Data.Word (Word8)
import qualified Data.ByteString as B

import Foreign.Ptr
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Storable (peekByteOff)

import Prism.Cpu.Types
import Prism.Cpu.Peripherals

-------------------------------------------------------------------------------

newtype MemReg = MemReg (Ptr Word8) deriving (Show)
newtype MemMain = MemMain (Ptr Word8) deriving (Show)

-------------------------------------------------------------------------------

data PrismInterrupts = PrismInterrupts {
        intListHigh :: [PrismInt],
        intListNmi :: [PrismInt],
        intIntrOn :: Bool,
        intSingleStep :: [PrismInt],
        intInterruptUp :: Bool
    } deriving (Show)

instance Show PrismInt where
    show (PrismInt val) = "INT " ++ (show val)

instance Show PrismIRQ where
    show (PrismIRQ val) = "IRQ " ++ (show val)

--data IOCtx = forall a . (IOCtxInternal a) => IOCtx {
data IOCtx = IOCtx {
        --ioCtxInternal :: a,
        ioCtxMemRegion :: MemIORegion,
        ioCtxPortRegion :: PortIORegion
    }

instance Show IOCtx where
    show c = "IOCtx " ++ (show $ ioCtxMemRegion c)

-------------------------------------------------------------------------------

data Ctx = Ctx {
        ctxReg :: MemReg,
        ctxMem :: MemMain,
        ctxFlags :: Flags,
        ctxEFlags :: EFlags,
        ctxReplaceSeg :: Maybe RegSeg,
        ctxStop :: Bool,
        ctxCycles :: Int,
        --ctxIO :: IOCtx,
        ctxInterrupts :: PrismInterrupts
    } deriving (Show)

-------------------------------------------------------------------------------

instance Show RegSeg where
    show (RegSeg 0) = "ES"
    show (RegSeg 1) = "CS"
    show (RegSeg 2) = "SS"
    show (RegSeg 3) = "DS"

-------------------------------------------------------------------------------

newtype CpuTransM s m a = CpuTransM {
    runCpuE :: (StateT s m) a
} deriving (Monad, Applicative, Functor, MonadState s)

instance MonadTrans (CpuTransM s) where
    lift = CpuTransM . lift

instance MonadIO m => MonadIO (CpuTransM s m) where
    liftIO = lift . liftIO

instance RunCpu (CpuTransM Ctx IO a) Ctx IO where
    runCpu ctx c = do
        s <- ((runStateT . runCpuE $ c) ctx)
        return $ snd s

type CpuTrans = CpuTransM Ctx IO

instance CpuDebug CpuTrans where
    cpuLog = liftIO . putStrLn

-------------------------------------------------------------------------------

memRegSize = 64

allocMemRegRaw :: MonadIO m => m (Ptr Word8)
allocMemRegRaw = liftIO $ callocBytes memRegSize

allocMemReg :: MonadIO m => m MemReg
allocMemReg = MemReg <$> allocMemRegRaw

allocMemMain :: MonadIO m => Int -> m MemMain
allocMemMain size = liftIO $ MemMain <$> callocBytes size

-------------------------------------------------------------------------------

clearRegs :: CpuTrans ()
clearRegs = do
    (MemReg ptr) <- ctxReg <$> get
    liftIO $ fillBytes ptr 0 memRegSize

copyMainMem :: Int -> B.ByteString -> CpuTrans ()
copyMainMem start memArray = do
    (MemMain ptr) <- ctxMem <$> get
    liftIO $ pokeArray (flip plusPtr (fromIntegral start) $ ptr) $ B.unpack memArray

-------------------------------------------------------------------------------

emptyPrismInterrupts = PrismInterrupts [] [] False [] False

clearFlags :: Flags
clearFlags = Flags False False False False False False

clearEFlags :: EFlags
clearEFlags = EFlags False False False

noReplaceSeg ::  Maybe RegSeg
noReplaceSeg = Nothing

noStop = False
maxCycles = 999999999

makeCtx :: MemReg -> MemMain -> Ctx
makeCtx memReg memMain =
    Ctx memReg
        memMain
        clearFlags
        clearEFlags
        noReplaceSeg
        noStop
        maxCycles
        emptyPrismInterrupts

makeTransM :: Ctx -> CpuTrans ()
makeTransM ctx = put ctx

-------------------------------------------------------------------------------

peekFirstByte :: (MonadIO m) => MemMain -> Int -> m Uint8
peekFirstByte (MemMain ptr) offset = liftIO $ peekByteOff ptr offset

-------------------------------------------------------------------------------
