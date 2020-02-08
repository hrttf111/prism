{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Peripherals.PC where

import Control.Monad.Trans

import Data.Bits

import Prism
import PrismPeripheral
import Peripherals.Local
import Peripherals.Pic

-------------------------------------------------------------------------------

data PC = PC {
        pcCycles :: Int,
        pcPicMaster :: Pic,
        pcPicSlave :: Pic
    } deriving (Show)

type PeripheralsPC = PeripheralsLocal PC

instance IOMem PeripheralsPC where
    ioMemRead peripherals handler offset =
        ioValRemoteRead (localIOQueue peripherals) IOMemType handler offset
    ioMemWrite peripherals handler offset val =
        ioValRemoteWrite (localIOQueue peripherals) IOMemType handler offset val

instance IOPort PeripheralsPC where
    ioPortRead peripherals handler offset = 
        ioValRemoteRead (localIOQueue peripherals) IOPortType handler (fromIntegral offset)
    ioPortWrite peripherals handler offset val =
        ioValRemoteWrite (localIOQueue peripherals) IOPortType handler (fromIntegral offset) val

instance InterruptDispatcher PeripheralsPC where
    dispatchInterruptUp peripherals int = return (peripherals, False)
    dispatchInterruptDown peripherals int = return (peripherals, False)
    ackInterrupt peripherals = return (peripherals, PrismInt 7)

instance PeripheralRunner PeripheralsPC where
    runPeripherals ctx peripherals = return (ctx, peripherals)
    peripheralCycles peripherals = 99999999

-------------------------------------------------------------------------------
