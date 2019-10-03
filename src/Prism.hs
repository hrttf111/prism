{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism where

import Data.Word (Word8, Word16)

import Foreign.Ptr

import Control.Monad.Trans

import GHC.Generics

type Imm8 = Uint8
type Imm16 = Uint16

type Uint8 = Word8
type Uint16 = Word16

newtype Reg8 = Reg8 Word8 deriving (Show)
newtype Reg16 = Reg16 Word8 deriving (Show)
newtype RegSeg = RegSeg Word8 deriving (Show) 

-- Note: Max size of instruction is 6 bytes
type InstrBytes = (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8)

data Ctx = Ctx {
        ctxMem :: Ptr Word8,
        ctxReg :: Ptr Word8
    } deriving (Show)

newtype PrismCtx m a = PrismCtx {
    runPrism :: m a
} deriving (Monad, Applicative, Functor)

instance MonadTrans PrismCtx where
    lift = PrismCtx

instance MonadIO m => MonadIO (PrismCtx m) where
    liftIO = lift . liftIO
