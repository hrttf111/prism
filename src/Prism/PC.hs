module Prism.PC (
        ------------------------------------------------------
        PC (..), PeripheralsPC
        , createPC, pcPorts
        , getPC, putPC
        ------------------------------------------------------
        , mkBiosInterrupts
        , getPcBiosSharedState
        , PcKey (..), PcKeyFlags (..)
        , emptyKeyFlags
        , SharedKeyboardState (..)
    ) where

import Prism.PC.PC
import Prism.PC.Pic
import Prism.PC.Bios

-------------------------------------------------------------------------------
