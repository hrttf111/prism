module Prism.PC.Bios where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM

import Data.Bits

import Prism.Cpu
import Prism.Peripherals

-------------------------------------------------------------------------------

data PcKey = PcKey {
    pcKeyMain :: Uint8,
    pcKeyAux :: Uint8
} deriving (Show)

data SharedKeyboardState = SharedKeyboardState {
} deriving (Show)

data PcKeyboard = PcKeyboard {
    pcKeyboardShared :: TVar SharedKeyboardState,
    pcKeyboardList :: [PcKey]
}

instance Show PcKeyboard where
    show _ = "PcKeyboard"

data PcBios = PcBios {
    pcBiosKeyboard :: PcKeyboard
} deriving (Show)

mkBios :: (MonadIO m) => m PcBios
mkBios = do
    keyboardState <- liftIO $ newTVarIO SharedKeyboardState
    return $ PcBios (PcKeyboard keyboardState [])

-------------------------------------------------------------------------------

biosInterruptKeyboardInternal :: InterruptHandler
biosInterruptKeyboardInternal _ =
    cpuRunDirect $ DirectCommandU8 9

biosInterruptVideo :: InterruptHandler
biosInterruptVideo _ =
    cpuRunDirect $ DirectCommandU8 0x10

biosInterruptKeyboard :: InterruptHandler
biosInterruptKeyboard _ =
    cpuRunDirect $ DirectCommandU8 0x16

biosInterruptClock :: InterruptHandler
biosInterruptClock _ =
    cpuRunDirect $ DirectCommandU8 0x1a

biosInterruptTest :: InterruptHandler
biosInterruptTest _ =
    cpuRunDirect $ DirectCommandU8 0xef

mkBiosInterrupts :: [InterruptHandlerLocation]
mkBiosInterrupts = [ (PrismInt 9, biosInterruptKeyboardInternal)
                   , (PrismInt 0x10, biosInterruptVideo)
                   , (PrismInt 0x16, biosInterruptKeyboard)
                   , (PrismInt 0x1a, biosInterruptClock)
                   , (PrismInt 0x1f, biosInterruptTest) -- todo: use higher interrupt vector
                   ]

-------------------------------------------------------------------------------

processBiosKeyboardInternal :: PcBios -> PrismM PcBios
processBiosKeyboardInternal bios = do
    return bios

-------------------------------------------------------------------------------

processBiosKeyboard :: PcBios -> PrismM PcBios
processBiosKeyboard bios = do
    valAh <- readOp ah
    case valAh of
        0 -> return () -- Get key
        1 -> return () -- Check key
        2 -> return () -- Check shift
        12 -> return () -- Check shift
        _ -> liftIO $ putStrLn "Unsupported"
    return bios

processBiosVideo :: PcBios -> PrismM PcBios
processBiosVideo bios = do
    valAh <- readOp ah
    case valAh of
        0 -> return () -- Set video mode
        1 -> return () -- Set cursor shape
        2 -> return () -- Set cursor pos
        3 -> return () -- Get cursor pos
        6 -> return () -- Scroll up
        7 -> return () -- Scroll down
        8 -> return () -- Get char
        9 -> return () -- Write char + attr
        0xe -> return () -- Write char
        0xf -> return () -- Get video mode
        _ -> liftIO $ putStrLn "Unsupported"
    return bios

processBiosClock :: PcBios -> PrismM PcBios
processBiosClock bios = do
    valAh <- readOp ah
    case valAh of
        0 -> return () -- Get ticks
        2 -> return () -- Get time
        4 -> return () -- Get date
        0xf -> return () -- Init
        _ -> liftIO $ putStrLn "Unsupported"
    return bios

processBiosTest :: PcBios -> PrismM PcBios
processBiosTest bios = do
    valAh <- readOp ah
    liftIO $ putStrLn "BIOS test interrupt"
    liftIO $ putStrLn $ "BIOS AH: " ++ (show valAh)
    writeOp al 89
    return bios

processBios :: PcBios -> Uint8 -> PrismM PcBios
processBios bios 9 = processBiosKeyboardInternal bios
processBios bios 0x10 = processBiosVideo bios
processBios bios 0x16 = processBiosKeyboard bios
processBios bios 0x1a = processBiosClock bios
processBios bios _ = processBiosTest bios
