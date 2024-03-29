{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Prism.GDB.Types where

import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import qualified Control.Monad.State.Class as SC
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Control
import Control.Monad.Logger

import Control.Exception.Lifted (bracket)

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T

import Prism.Command

-------------------------------------------------------------------------------

data GDBState = GDBState {
    gdbAckEnabled :: Bool,
    gdbPacketSize :: Int,
    gdbNumBreakpoints :: Int,
    gdbCmdQueue :: PrismCmdQueue,
    gdbRspQueue :: PrismRspQueue
}

-------------------------------------------------------------------------------

newtype GDBServerM m a = GDBServerM {
    runGDB :: LoggingT (StateT GDBState m) a
} deriving (Monad, Applicative, Functor, SC.MonadState GDBState, MonadLogger)

instance MonadTrans GDBServerM where
    lift = GDBServerM . lift . lift

instance (MonadBase b m) => MonadBase b (GDBServerM m) where
    liftBase = liftBaseDefault

instance MonadIO m => MonadIO (GDBServerM m) where
    liftIO = lift . liftIO

instance MonadTransControl GDBServerM where
    type StT GDBServerM a = StT (StateT GDBState) (StT LoggingT a)
    liftWith = defaultLiftWith2 GDBServerM runGDB
    restoreT = defaultRestoreT2 GDBServerM

instance MonadBaseControl b m => MonadBaseControl b (GDBServerM m) where
    type StM (GDBServerM m) a = ComposeSt GDBServerM m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type GDBServer = GDBServerM IO ()

-------------------------------------------------------------------------------
