{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Cpu.Ports where

import Control.Monad.State.Strict (get)

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Peripherals

-------------------------------------------------------------------------------

instance Operand Port8 CpuTrans Uint8 where
    readOp (Port8 port) = do
        c <- ctxIO <$> get
        let index = findPortIndex (ioCtxPortRegion c) port
        readPort8 c $ PortInternal8 (index, port)
    writeOp (Port8 port) val = do
        c <- ctxIO <$> get
        let index = findPortIndex (ioCtxPortRegion c) port
        writePort8 c val $ PortInternal8 (index, port)

instance Operand Port16 CpuTrans Uint16 where
    readOp (Port16 port) = return 0
    writeOp (Port16 port) _ = return ()

instance Show Port8 where
    show (Port8 port) = "Port8 = " ++ show port

instance Show Port16 where
    show (Port16 port) = "Port16 = " ++ show port

-------------------------------------------------------------------------------

readPort8 :: IOCtx -> PortInternal8 -> CpuTrans Uint8
readPort8 (IOCtx s _ _) port = 
    runPeripheralsM s $ readOp port

writePort8 :: IOCtx -> Uint8 -> PortInternal8 -> CpuTrans ()
writePort8 (IOCtx s _ _) val port = 
    runPeripheralsM s $ writeOp port val

-------------------------------------------------------------------------------
