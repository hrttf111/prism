{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism where

import Data.Word (Word8, Word16, Word32)

import Foreign.Ptr

import Control.Monad.Trans

import GHC.Generics

type Imm8 = Uint8
type Imm16 = Uint16

type Uint8 = Word8
type Uint16 = Word16

type Disp = Word16

--type MemOffset = Word32
type MemOffset = Int

newtype Reg8 = Reg8 Word8
newtype Reg16 = Reg16 Word8
newtype RegSeg = RegSeg Word8

data Mem = MemBxSi Disp |
           MemBxDi Disp |
           MemBpSi Disp |
           MemBpDi Disp |
           MemSi Disp |
           MemDi Disp |
           MemBp Disp |
           MemBx Disp |
           MemDirect Disp

-- Note: Max size of instruction is 6 bytes
type InstrBytes = (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8)

newtype MemReg = MemReg (Ptr Word8) deriving (Show)
newtype MemMain = MemMain (Ptr Word8) deriving (Show)

data Ctx = Ctx {
        ctxReg :: MemReg,
        ctxMem :: MemMain
    } deriving (Show)

newtype PrismCtx m a = PrismCtx {
    runPrism :: m a
} deriving (Monad, Applicative, Functor)

instance MonadTrans PrismCtx where
    lift = PrismCtx

instance MonadIO m => MonadIO (PrismCtx m) where
    liftIO = lift . liftIO
