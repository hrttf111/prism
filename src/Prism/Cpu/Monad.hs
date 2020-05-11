{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Cpu.Monad where

import Control.Monad.Trans
import Control.Monad.State.Strict --(modify, MonadState, StateT)

import Data.Word (Word8)

import Foreign.Ptr

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
