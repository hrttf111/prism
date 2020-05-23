module Prism.Peripherals (
        ------------------------------------------------------
        PeripheralMem (..), PeripheralPort (..)
        , PeripheralHandlerMem (..)
        , PeripheralHandlerPort (..)
        ------------------------------------------------------
        , InterruptHandler 
        , InterruptHandlerLocation 
        , intInternal
        ------------------------------------------------------
        , configureInterrups
        ------------------------------------------------------
        , makeDummyIO
        ------------------------------------------------------
    ) where

import Prism.Peripherals.Types
import Prism.Peripherals.Builder
import Prism.Peripherals.Dummy
import Prism.Peripherals.Interrupt
import Prism.Peripherals.Queue

-------------------------------------------------------------------------------
