module Prism.PC (
        ------------------------------------------------------
        PC (..), PeripheralsPC
        , createPC, pcPorts
        , getPC, putPC
        ------------------------------------------------------
        , mkBiosInterrupts
    ) where

import Prism.PC.PC
import Prism.PC.Pic
import Prism.PC.Bios

-------------------------------------------------------------------------------
