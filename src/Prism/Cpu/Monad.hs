{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Cpu.Monad (
          Ctx (..), IOCtx (..), DebugCtx(..)
        , MemReg (..), MemMain (..)
        , RunCpu (..)
        , PrismInterrupts (..)
        , CpuTransM (..), CpuTrans (..)
        , allocMemRegRaw, allocMemReg, allocMemMain
        , clearFlags, clearRegs, copyMainMem 
        , makeCtx, makeTransM
        , peekFirstByte
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

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

data IOCtx = forall a m . ( PeripheralsMonad m
                          , RunPeripheralsM a m CpuTrans
                          , RunPeripheralsDirect a CpuTrans
                          ) => IOCtx {
        ioCtxInternal :: a,
        ioCtxMemRegion :: MemIORegion,
        ioCtxPortRegion :: PortIORegion
    }

instance Show IOCtx where
    --show c = "IOCtx " ++ (show $ ioCtxMemRegion c) ++ (show $ ioCtxPortRegion c)
    show c = "IOCtx " ++ (show $ ioCtxMemRegion c) -- ++ (show $ ioCtxPortRegion c)

-------------------------------------------------------------------------------

data DebugCtx = DebugCtx {
        debugCtxPrint :: Int -> Int -> String -> IO (), -- level -> feature -> msg
        debugCtxLevelEnable :: Int -> Int -> Bool -- level -> feature
    }

instance Show DebugCtx where
    show _ = "DebugCtx"

printLog :: (Enum level) => level -> LogFeature -> String -> CpuTrans ()
printLog level (LogFeature index) msg = do
    (DebugCtx printF _) <- ctxDebug <$> get
    liftIO $ printF levelIndex index msg
    where
        levelIndex = fromEnum level

execDebugAction :: (Enum level) => level -> LogFeature -> CpuTrans () -> CpuTrans ()
execDebugAction level (LogFeature index) a = do
    (DebugCtx _ enabledF) <- ctxDebug <$> get
    if enabledF levelIndex index then
        a
        else
            return ()
    where
        levelIndex = fromEnum level

ignoreLog _ _ _ = return ()
ignoreAction _ _ _ = return ()

-------------------------------------------------------------------------------

data Ctx = Ctx {
        ctxReg :: MemReg,
        ctxMem :: MemMain,
        ctxFlags :: Flags,
        ctxEFlags :: EFlags,
        ctxReplaceSeg :: Maybe RegSeg,
        ctxStop :: Bool,
        ctxCycles :: CpuCycles,
        ctxCyclesP :: CpuCyclesDelta,
        ctxInterrupts :: PrismInterrupts,
        ctxIO :: IOCtx,
        ctxDebug :: DebugCtx
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

-------------------------------------------------------------------------------

instance CpuDebugM CpuTrans Trace where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM CpuTrans Debug where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM CpuTrans Info where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM CpuTrans Warning where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM CpuTrans Error where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

-------------------------------------------------------------------------------

instance InterruptDispatcher CpuTrans where
    dispatchIrqUp irq =
        ctxIO <$> get >>= f
        where
            f (IOCtx s _ _) = runPeripheralsM s $ dispatchIrqUp irq
    dispatchIrqDown irq =
        ctxIO <$> get >>= f
        where
            f (IOCtx s _ _) = runPeripheralsM s $ dispatchIrqDown irq
    ackIrq =
        ctxIO <$> get >>= f
        where
            f (IOCtx s _ _) = runPeripheralsM s ackIrq

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
minCycles = CpuCycles 0
maxCycles = maxBound :: CpuCyclesDelta

makeCtx :: MemReg -> MemMain -> IOCtx -> DebugCtx -> Ctx
makeCtx memReg memMain ioCtx debugCtx =
    Ctx memReg
        memMain
        clearFlags
        clearEFlags
        noReplaceSeg
        noStop
        minCycles
        maxCycles
        emptyPrismInterrupts
        ioCtx
        debugCtx

makeTransM :: Ctx -> CpuTrans ()
makeTransM ctx = put ctx

-------------------------------------------------------------------------------

peekFirstByte :: (MonadIO m) => MemMain -> Int -> m Uint8
peekFirstByte (MemMain ptr) offset = liftIO $ peekByteOff ptr offset

-------------------------------------------------------------------------------
