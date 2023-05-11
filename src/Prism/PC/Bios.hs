{-# LANGUAGE FlexibleContexts #-}

module Prism.PC.Bios where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM

import Data.Bits
import Data.List (uncons)
import Data.Time (getCurrentTime, timeToTimeOfDay, UTCTime(..), TimeOfDay(..))
import qualified Data.ByteString as B

import Foreign.Ptr
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Storable (peekByteOff, pokeByteOff)

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

data VideoCursor = VideoCursor {
    videoCursorColumn :: Int, -- horizontal
    videoCursorRow :: Int, -- vertical
    videoCursorEnabled :: Bool
} deriving (Show)

data SharedVideoState = SharedVideoState {
    --videoMemory :: B.ByteString,
    videoMemory :: Ptr Uint8,
    videoCursor :: VideoCursor,
    videoScrollPos :: Int
} deriving (Show)

data PcVideo = PcVideo {
    pcVideoShared :: TVar SharedVideoState
}

instance Show PcVideo where
    show _ = "PcVideo"

data PcBios = PcBios {
    pcBiosKeyboard :: PcKeyboard,
    pcVideoState :: PcVideo,
    pcTimer :: PcTimer
} deriving (Show)

mkBios :: (MonadIO m) => m PcBios
mkBios = do
    let memSize = 65536
    videoMem <- liftIO $ callocBytes memSize
    keyboardState <- liftIO $ newTVarIO $ SharedKeyboardState emptyKeyFlags []
    videoState <- liftIO $ newTVarIO $ SharedVideoState videoMem (VideoCursor 0 0 True) 0
    return $ PcBios (PcKeyboard keyboardState emptyKeyFlags []) (PcVideo videoState) emptyTimer

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
    --liftIO $ putStrLn $ "Ticks = " ++ (show timerTicks')
    --liftIO $ putStrLn $ "Diff = " ++ (show diff)
    if diff > timerPeriod then do
        --liftIO $ putStrLn "Raise interrupt 0x1c"
        raiseInterrupt $ PrismInt 0x1c
        else
            return ()
    --liftIO $ putStrLn "Continue"
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
        0 -> do -- Set video mode
            writeOp al 0x30
            return ()
        1 -> do -- Set cursor shape
            valCh <- readOp ch
            valCl <- readOp cl
            let enableCursor = ((valCh .&. 0x10) /= 0) && ((valCl .&. 0x30) /= 0)
                vs = pcVideoShared $ pcVideoState bios
            liftIO $ atomically $ do
                s <- readTVar vs
                let c = videoCursor s
                writeTVar vs $ s { videoCursor = c { videoCursorEnabled = enableCursor } }
            return ()
        2 -> do -- Set cursor pos
            --valBh <- readOp bh
            valDh <- fromIntegral <$> readOp dh -- row
            valDl <- fromIntegral <$> readOp dl -- column
            let vs = pcVideoShared $ pcVideoState bios
            liftIO $ atomically $ do
                s <- readTVar vs
                let c = videoCursor s
                writeTVar vs $ s { videoCursor = c { videoCursorColumn = valDl, videoCursorRow = valDh } }
            writeOp al 0
            return ()
        3 -> do -- Get cursor pos
            --valBh <- readOp bh
            let vs = pcVideoShared $ pcVideoState bios
            cursor <- liftIO $ atomically $ videoCursor <$> readTVar vs
            writeOp ch 0
            writeOp cl 0
            writeOp dh $ fromIntegral $ videoCursorColumn cursor
            writeOp dl $ fromIntegral $ videoCursorRow cursor
            return ()
        6 -> do -- Scroll up
            doScroll True
            return ()
        7 -> do -- Scroll down
            doScroll False
            return ()
        8 -> do -- Get char
            --valBh <- readOp bh
            let vs = pcVideoShared $ pcVideoState bios
            (ptr, offset) <- liftIO $ atomically $ do
                s <- readTVar vs
                let offset = videoOffset s
                return (videoMemory s, offset)
            (code, attr) <- liftIO $ readChar ptr offset
            writeOp al code -- ASCII char
            writeOp ah attr -- attr
            return ()
        9 -> do -- Write char + attr
            valAl <- readOp al -- ASCII code
            valBl <- readOp bl -- attr
            --valBh <- readOp bh
            valCx <- readOp cx -- repeat
            let vs = pcVideoShared $ pcVideoState bios
            (ptr, offset) <- liftIO $ atomically $ do
                s <- readTVar vs
                let offset = videoOffset s
                return (videoMemory s, offset)
            liftIO $ writeChar ptr offset valAl valBl
            return ()
        0xe -> do -- Write char and update cursor
            valAl <- readOp al -- ASCII code
            valBl <- readOp bl -- FG color
            --valBh <- readOp bh
            let vs = pcVideoShared $ pcVideoState bios
            (ptr, offset) <- liftIO $ atomically $ do
                s <- readTVar vs
                let offset = videoOffset s
                    c' = updateCursorPos $ videoCursor s
                writeTVar vs $ s { videoCursor = c' }
                return (videoMemory s, offset)
            liftIO $ writeChar ptr offset valAl valBl
            return ()
        0xf -> do -- Get video mode
            writeOp al 0 -- mode
            writeOp ah $ fromIntegral videoColumns -- number of columns
            writeOp bh 0 -- page
            return ()
        _ -> liftIO $ putStrLn "Unsupported"
    return bios
    where
        videoColumns = 80
        videoRows = 100
        updateCursorPos c = if (h+1) < videoColumns then
            c { videoCursorRow = v, videoCursorColumn = (h + 1) }
            else if v < videoRows then
                c { videoCursorRow = (v + 1), videoCursorColumn = 0 }
                else
                    c { videoCursorRow = v, videoCursorColumn = (videoColumns - 1) }
            where
                v = videoCursorRow c
                h = videoCursorColumn c
        videoOffset state = ((sPos + v) * videoColumns + h) * 2
            where
                charSize = 2
                c = videoCursor state
                (v, h) = (videoCursorRow c, videoCursorColumn c)
                sPos = videoScrollPos state
        writeChar ptr off code attr = do
            pokeByteOff ptr off code
            pokeByteOff ptr (off+1) attr
        readChar ptr off =
            (,) <$> peekByteOff ptr off <*> peekByteOff ptr (off+1)
        doScroll doUp = do
            valAl <- readOp al -- scroll distance in rows
            valBh <- readOp bh -- attr for blank lines
            valCh <- readOp ch -- top row scroll window
            valCl <- readOp cl -- left column scroll window
            valDh <- readOp dh -- bottom row scroll window
            valDl <- readOp dl -- right column scroll window
            --TODO
            return ()

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
            utcTime <- liftIO getCurrentTime
            let tm = timeToTimeOfDay $ utctDayTime utcTime
                hours = hexToBcd8 $ fromIntegral $ todHour tm
                minutes = hexToBcd8 $ fromIntegral $ todMin tm
                seconds = hexToBcd8 $ fromIntegral $ div (fromEnum $ todSec tm) 1000000000000
            writeOp al hours
            writeOp ch hours
            writeOp cl minutes
            writeOp dh seconds
            writeOp dl 0
            return ()
        4 -> do -- Get date
            let century = hexToBcd8 19
                year = hexToBcd8 80
                month = hexToBcd8 1
                day = hexToBcd8 1
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
