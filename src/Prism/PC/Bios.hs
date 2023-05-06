module Prism.PC.Bios where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Data.Bits

import Prism.Cpu
import Prism.Peripherals

-------------------------------------------------------------------------------

data PcKey = PcKey {
} deriving (Show)

data SharedKeyboardState = SharedKeyboardState {
} deriving (Show)

data PcKeyboard = PcKeyboard {
    pcKeyboardShared :: SharedKeyboardState,
    pcKeyboardList :: [PcKey]
} deriving (Show)

data PcBios = PcBios {
    pcBiosKeyboard :: PcKeyboard
} deriving (Show)

mkBios = PcBios (PcKeyboard SharedKeyboardState [])

-------------------------------------------------------------------------------

biosInterrupt :: InterruptHandler
biosInterrupt int =
    cpuRunDirect $ DirectCommandU8 int

mkBiosInterrupts :: [InterruptHandlerLocation]
mkBiosInterrupts = [(PrismInt 0x10, biosInterrupt)]

-------------------------------------------------------------------------------

processBios :: PcBios -> Uint8 -> PrismM PcBios
processBios bios int = do
    valAh <- readOp ah
    liftIO $ putStrLn $ "BIOS interrupt: " ++ (show int)
    liftIO $ putStrLn $ "BIOS AH: " ++ (show valAh)
    writeOp al 89
    return bios
