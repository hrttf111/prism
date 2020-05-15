{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Cpu.Monad (
          Ctx (..)
        , MemReg (..), MemMain (..)
        , RunCpu (..)
        , CpuTransM (..), CpuTrans (..)
        , allocMemRegRaw, allocMemReg, allocMemMain
        , makeCtx, makeTransM
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict --(modify, MonadState, StateT)

import Data.Word (Word8)

import Foreign.Ptr
import Foreign.Marshal.Alloc (callocBytes)

import Prism.Cpu.Types

-------------------------------------------------------------------------------

newtype MemReg = MemReg (Ptr Word8) deriving (Show)
newtype MemMain = MemMain (Ptr Word8) deriving (Show)

data Ctx = Ctx {
        ctxReg :: MemReg,
        ctxMem :: MemMain,
        ctxFlags :: Flags,
        ctxEFlags :: EFlags,
        ctxReplaceSeg :: Maybe RegSeg,
        ctxStop :: Bool,
        ctxCycles :: Int
    } deriving (Show)

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

memRegSize = 64

allocMemRegRaw :: MonadIO m => m (Ptr Word8)
allocMemRegRaw = liftIO $ callocBytes memRegSize

allocMemReg :: MonadIO m => m MemReg
allocMemReg = MemReg <$> allocMemRegRaw

allocMemMain :: MonadIO m => Int -> m MemMain
allocMemMain size = liftIO $ MemMain <$> callocBytes size

-------------------------------------------------------------------------------

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
    Ctx memReg memMain clearFlags clearEFlags noReplaceSeg noStop maxCycles

makeTransM :: Ctx -> CpuTrans ()
makeTransM ctx = put ctx

-------------------------------------------------------------------------------
