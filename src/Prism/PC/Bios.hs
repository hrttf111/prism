module Prism.PC.Bios where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM

import Data.Bits
import Data.List (uncons)

import Prism.Cpu
import Prism.Peripherals

-------------------------------------------------------------------------------

data PcKey = PcKey {
    pcKeyMain :: Uint8, -- ASCII code
    pcKeyAux :: Uint8   -- Scan code
} deriving (Show)

data PcKeyFlags = PcKeyFlags {
    pcKeyFlagInsert :: Bool,
    pcKeyFlagCapsLock :: Bool,
    pcKeyFlagNumLock :: Bool,
    pcKeyFlagScrollLock :: Bool,
    pcKeyFlagAlt :: Bool,
    pcKeyFlagCtrl :: Bool,
    pcKeyFlagLeftShift :: Bool,
    pcKeyFlagRightShift :: Bool,
    pcKeyFlagSysReq :: Bool,
    pcKeyFlagRightAlt :: Bool,
    pcKeyFlagLeftAlt :: Bool,
    pcKeyFlagRightCtrl :: Bool,
    pcKeyFlagLeftCtrl :: Bool
} deriving (Show)

emptyKeyFlags = PcKeyFlags False False False False False False False False False False False False False

boolToBit :: Int -> Bool -> Uint8
boolToBit bitNum True = setBit 1 bitNum
boolToBit _ _ = 0

toShiftFlags :: PcKeyFlags -> Uint8
toShiftFlags flags =
    (boolToBit 7 (pcKeyFlagInsert flags)) .&.
    (boolToBit 6 (pcKeyFlagCapsLock flags)) .&.
    (boolToBit 5 (pcKeyFlagNumLock flags)) .&.
    (boolToBit 4 (pcKeyFlagScrollLock flags)) .&.
    (boolToBit 3 (pcKeyFlagAlt flags)) .&.
    (boolToBit 2 (pcKeyFlagCtrl flags)) .&.
    (boolToBit 1 (pcKeyFlagLeftShift flags)) .&.
    (boolToBit 0 (pcKeyFlagRightShift flags))

toExtShiftFlags :: PcKeyFlags -> Uint8
toExtShiftFlags flags =
    (boolToBit 7 (pcKeyFlagSysReq flags)) .&.
    (boolToBit 6 (pcKeyFlagCapsLock flags)) .&.
    (boolToBit 5 (pcKeyFlagNumLock flags)) .&.
    (boolToBit 4 (pcKeyFlagScrollLock flags)) .&.
    (boolToBit 3 (pcKeyFlagRightAlt flags)) .&.
    (boolToBit 2 (pcKeyFlagRightCtrl flags)) .&.
    (boolToBit 1 (pcKeyFlagLeftAlt flags)) .&.
    (boolToBit 0 (pcKeyFlagLeftCtrl flags))

data SharedKeyboardState = SharedKeyboardState {
    sharedFlags :: PcKeyFlags,
    sharedKeys :: [PcKey]
} deriving (Show)

data PcKeyboard = PcKeyboard {
    pcKeyboardShared :: TVar SharedKeyboardState,
    pcKeyboardFlags :: PcKeyFlags,
    pcKeyboardList :: [PcKey]
}

instance Show PcKeyboard where
    show _ = "PcKeyboard"

data PcBios = PcBios {
    pcBiosKeyboard :: PcKeyboard
} deriving (Show)

mkBios :: (MonadIO m) => m PcBios
mkBios = do
    keyboardState <- liftIO $ newTVarIO $ SharedKeyboardState emptyKeyFlags []
    return $ PcBios (PcKeyboard keyboardState emptyKeyFlags [])

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
        0 -> do -- Get key
            let keys' = uncons $ pcKeyboardList $ pcBiosKeyboard bios
            case keys' of
                Just (key, keyList') -> do
                    writeOp al $ pcKeyMain key
                    writeOp ah $ pcKeyAux key
                    return $ bios { pcBiosKeyboard = (pcBiosKeyboard bios) {pcKeyboardList = keyList'} }
                Nothing -> do
                    -- TODO: suspend
                    return bios
        1 -> do -- Check key
            let keys' = uncons $ pcKeyboardList $ pcBiosKeyboard bios
            case keys' of
                Just (key, keyList') -> do
                    setFlag ZF True
                    writeOp al $ pcKeyMain key
                    writeOp ah $ pcKeyAux key
                    return bios
                Nothing -> do
                    setFlag ZF False
                    return bios
        2 -> do -- Check shift flags
            let valAl = toShiftFlags $ pcKeyboardFlags $ pcBiosKeyboard bios
            writeOp al valAl
            return bios
        0x12 -> do -- Check ext shift flags
            let valAl = toShiftFlags $ pcKeyboardFlags $ pcBiosKeyboard bios
                valAh = toExtShiftFlags $ pcKeyboardFlags $ pcBiosKeyboard bios
            writeOp al valAl
            writeOp ah valAh
            return bios
        _ -> do
            liftIO $ putStrLn "Unsupported"
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
