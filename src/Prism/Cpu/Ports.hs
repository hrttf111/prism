{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Cpu.Ports where

import Control.Monad.State.Strict (get)

import Prism.Cpu.Types
import Prism.Cpu.Monad

-------------------------------------------------------------------------------

instance Operand Port8 CpuTrans Uint8 where
    readOp (Port8 port) = return 0
    writeOp (Port8 port) _ = return ()

instance Operand Port16 CpuTrans Uint16 where
    readOp (Port16 port) = return 0
    writeOp (Port16 port) _ = return ()

instance Show Port8 where
    show (Port8 port) = "Port8 = " ++ show port

instance Show Port16 where
    show (Port16 port) = "Port16 = " ++ show port

-------------------------------------------------------------------------------
