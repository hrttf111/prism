{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Prism.Cpu.Trans where

import Control.Monad.Trans
import Control.Monad.State.Strict --(modify, MonadState, StateT)

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Flags
import Prism.Cpu.Memory
import Prism.Cpu.Registers

-------------------------------------------------------------------------------

instance CpuMonad CpuTrans where
    halt = modify (\s -> s { ctxStop = True } )

-------------------------------------------------------------------------------
