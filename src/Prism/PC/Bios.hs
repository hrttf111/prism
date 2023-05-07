module Prism.PC.Bios where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM

import Data.Bits
import Data.List (uncons)

import System.CPUTime (getCPUTime)

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
boolToBit bitNum True = bit bitNum
boolToBit _ _ = 0

toShiftFlags :: PcKeyFlags -> Uint8
toShiftFlags flags =
    (boolToBit 7 (pcKeyFlagInsert flags)) .|.
    (boolToBit 6 (pcKeyFlagCapsLock flags)) .|.
    (boolToBit 5 (pcKeyFlagNumLock flags)) .|.
    (boolToBit 4 (pcKeyFlagScrollLock flags)) .|.
    (boolToBit 3 (pcKeyFlagAlt flags)) .|.
    (boolToBit 2 (pcKeyFlagCtrl flags)) .|.
    (boolToBit 1 (pcKeyFlagLeftShift flags)) .|.
    (boolToBit 0 (pcKeyFlagRightShift flags))

toExtShiftFlags :: PcKeyFlags -> Uint8
toExtShiftFlags flags =
    (boolToBit 7 (pcKeyFlagSysReq flags)) .|.
    (boolToBit 6 (pcKeyFlagCapsLock flags)) .|.
    (boolToBit 5 (pcKeyFlagNumLock flags)) .|.
    (boolToBit 4 (pcKeyFlagScrollLock flags)) .|.
    (boolToBit 3 (pcKeyFlagRightAlt flags)) .|.
    (boolToBit 2 (pcKeyFlagRightCtrl flags)) .|.
    (boolToBit 1 (pcKeyFlagLeftAlt flags)) .|.
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

data PcTimer = PcTimer {
    pcTimerLastInt8 :: Uint64, -- in us
    pcTimerTicks :: Uint64
} deriving (Show)

emptyTimer = PcTimer 0 0

data PcBios = PcBios {
    pcBiosKeyboard :: PcKeyboard,
    pcTimer :: PcTimer
} deriving (Show)

mkBios :: (MonadIO m) => m PcBios
mkBios = do
    keyboardState <- liftIO $ newTVarIO $ SharedKeyboardState emptyKeyFlags []
    return $ PcBios (PcKeyboard keyboardState emptyKeyFlags []) emptyTimer

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
mkBiosInterrupts = [ (PrismInt 8, \_ -> cpuRunDirect $ DirectCommandU8 8)
                   , (PrismInt 9, biosInterruptKeyboardInternal)
                   , (PrismInt 0x10, biosInterruptVideo)
                   , (PrismInt 0x16, biosInterruptKeyboard)
                   , (PrismInt 0x1a, biosInterruptClock)
                   , (PrismInt 0x1f, biosInterruptTest) -- todo: use higher interrupt vector
                   ]

-------------------------------------------------------------------------------

processBiosTimerISR :: PcBios -> PrismM PcBios
processBiosTimerISR bios = do
    liftIO $ putStrLn "Int 8 ISR"
    ticks <- liftIO getCPUTime
    let timerPeriod = 54900 -- 54.9 ms ~ 18.2 Hz
        ticksUs = fromIntegral $ div ticks 1000000 -- ps -> us
        diff = ticksUs - (pcTimerLastInt8 $ pcTimer bios)
        timerTicks = pcTimerTicks $ pcTimer bios
        timerTicks' = timerTicks + 1
    liftIO $ putStrLn $ "Ticks = " ++ (show timerTicks')
    liftIO $ putStrLn $ "Diff = " ++ (show diff)
    {-if diff > timerPeriod then
        raiseInterrupt $ PrismInt 0x1c
        else
            return ()-}
    return $ bios { pcTimer = PcTimer ticksUs timerTicks' }

processBiosKeyboardISR :: PcBios -> PrismM PcBios
processBiosKeyboardISR bios = do
    --liftIO $ putStrLn "Int 9 ISR"
    let keyboard = pcBiosKeyboard bios
        shared = pcKeyboardShared keyboard
    res <- liftIO $ atomically $
        swapTVar shared $ SharedKeyboardState emptyKeyFlags []
    let keys' = (pcKeyboardList keyboard) ++ (sharedKeys res)
        keys'' = drop ((length keys') - 16) keys'
        keyboard' = PcKeyboard shared (sharedFlags res) keys''
    return $ bios { pcBiosKeyboard = keyboard' }

-------------------------------------------------------------------------------

saveInterruptCtx :: PrismM ()
saveInterruptCtx = do
    flags <- getFlags
    eflags <- getFlags
    pushV $ flagsToVal flags $ eflagsToVal eflags 0
    pushP cs
    pushP ip

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
                    -- TODO: suspend, may check shared state periodically
                    return bios
        1 -> do -- Check key
            let keys' = uncons $ pcKeyboardList $ pcBiosKeyboard bios
            case keys' of
                Just (key, keyList') -> do
                    retInterrupt
                    setFlag ZF False
                    writeOp al $ pcKeyMain key
                    writeOp ah $ pcKeyAux key
                    saveInterruptCtx
                    return bios
                Nothing -> do
                    retInterrupt
                    setFlag ZF True
                    saveInterruptCtx
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
        0 -> do -- Get ticks
            let ticks = pcTimerTicks $ pcTimer bios
                highTicks = fromIntegral $ shiftR (ticks .&. 0xFFFF0000) 16
                lowTicks = fromIntegral $ ticks .&. 0xFFFF
            writeOp al 0
            writeOp cx highTicks
            writeOp dx lowTicks
            return ()
        2 -> do -- Get time
            let hours = hexToBcd8 0
                minutes = hexToBcd8 0
                seconds = hexToBcd8 0
            writeOp al hours
            writeOp ch hours
            writeOp cl minutes
            writeOp dh seconds
            writeOp dl 0
            return ()
        4 -> do -- Get date
            let century = hexToBcd8 20
                year = hexToBcd8 0
                month = hexToBcd8 0
                day = hexToBcd8 0
            writeOp ch century
            writeOp cl year
            writeOp dh month
            writeOp dl day
            return ()
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
processBios bios 8 = processBiosTimerISR bios
processBios bios 9 = processBiosKeyboardISR bios
processBios bios 0x10 = processBiosVideo bios
processBios bios 0x16 = processBiosKeyboard bios
processBios bios 0x1a = processBiosClock bios
processBios bios _ = processBiosTest bios
