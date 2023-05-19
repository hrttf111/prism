{-# LANGUAGE FlexibleContexts #-}

module Prism.PC.Bios where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM

import Data.Bits
import Data.List (uncons)
import Data.Time (getCurrentTime, timeToTimeOfDay, UTCTime(..), TimeOfDay(..))
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.ByteString.Internal as BI

import Foreign.Ptr
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Marshal.Array (pokeArray, copyArray)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.ForeignPtr (withForeignPtr)

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

data VideoCommand = VideoFullDraw
                  | VideoDrawChar Uint8 Uint8
                  | VideoUpdateCursor
                  deriving (Show)

data SharedVideoState = SharedVideoState {
    videoMemory :: Ptr Uint8,
    videoCursor :: VideoCursor,
    videoScrollPos :: Int,
    videoCommands :: [VideoCommand]
} deriving (Show)

data PcVideo = PcVideo {
    pcVideoShared :: TVar SharedVideoState
}

instance Show PcVideo where
    show _ = "PcVideo"

data PcChs = PcChs {
    pcChsCylinder :: Uint16,
    pcChsHead :: Uint16,
    pcChsSector :: Uint16
} deriving (Show)

chsToOffset :: PcDisk -> PcChs -> Int
chsToOffset disk (PcChs c' h' s') = (c * n_heads + h) * n_sectors + (s - 1)
    where
        dp = pcDiskParams disk
        n_heads = fromIntegral $ pcChsHead dp
        n_sectors = fromIntegral $ pcChsSector dp
        c = fromIntegral c'
        h = fromIntegral h'
        s = fromIntegral s'

sectorsToInt :: Uint8 -> Int
sectorsToInt val = 512 * fromIntegral val

data PcDiskIndex = PcDiskFloppy Uint8
                 | PcDiskHdd Uint8
                 deriving (Show, Ord, Eq)

intToDiskIndex :: Uint8 -> PcDiskIndex
intToDiskIndex i | i >= 0x80 = PcDiskHdd $ i - 0x80
intToDiskIndex i = PcDiskFloppy i

data DiskOpReq = DiskOpReq {
    diskOpReqDiskOffset :: Int, -- byte offset on disk
    diskOpReqDataLen :: Int, -- byte length of data to read/write
    diskOpReqMemPtr :: Ptr Uint8, -- ptr in main memory
    diskOpReqNumSector :: Uint8,
    diskOpReqDrive :: PcDisk
}

data PcDisk = PcDisk {
    pcDiskStatus :: Int,
    pcDiskParams :: PcChs,
    --pcDiskRead :: Offset -> Len -> Data
    pcDiskRead :: Int -> Int -> IO B.ByteString,
    --pcDiskWrite :: Offset -> Data -> ()
    pcDiskWrite :: Int -> B.ByteString -> IO ()
}

instance Show PcDisk where
    show _ = "PcDisk"

pcDiskDummyRead :: Int -> Int -> IO B.ByteString
pcDiskDummyRead _ _ = return B.empty

pcDiskDummyWrite :: Int -> B.ByteString -> IO ()
pcDiskDummyWrite _ _ = return ()

emptyDisk = PcDisk 0 (PcChs 0 0 0) pcDiskDummyRead pcDiskDummyWrite

data PcBios = PcBios {
    pcBiosKeyboard :: PcKeyboard,
    pcVideoState :: PcVideo,
    pcTimer :: PcTimer,
    pcDisks :: Map.Map PcDiskIndex PcDisk
} deriving (Show)

mkBios :: (MonadIO m) => m PcBios
mkBios = do
    let memSize = 65536
    videoMem <- liftIO $ callocBytes memSize
    keyboardState <- liftIO $ newTVarIO $ SharedKeyboardState emptyKeyFlags []
    videoState <- liftIO $ newTVarIO $ SharedVideoState videoMem (VideoCursor 0 0 True) 0 []
    return $ PcBios (PcKeyboard keyboardState emptyKeyFlags []) (PcVideo videoState) emptyTimer Map.empty

-------------------------------------------------------------------------------

biosInterruptTest :: InterruptHandler
biosInterruptTest _ =
    cpuRunDirect $ DirectCommandU8 0xef

mkBiosInterrupts :: [InterruptHandlerLocation]
mkBiosInterrupts = [ (PrismInt 8, \_ -> cpuRunDirect $ DirectCommandU8 8)
                   , (PrismInt 9, \_ -> cpuRunDirect $ DirectCommandU8 9)
                   , (PrismInt 0x10, \_ -> cpuRunDirect $ DirectCommandU8 0x10)
                   , (PrismInt 0x11, \_ -> cpuRunDirect $ DirectCommandU8 0x11)
                   , (PrismInt 0x12, \_ -> cpuRunDirect $ DirectCommandU8 0x12)
                   , (PrismInt 0x13, \_ -> cpuRunDirect $ DirectCommandU8 0x13)
                   , (PrismInt 0x16, \_ -> cpuRunDirect $ DirectCommandU8 0x16)
                   , (PrismInt 0x1a, \_ -> cpuRunDirect $ DirectCommandU8 0x1a)
                   , (PrismInt 0x1f, biosInterruptTest) -- todo: use higher interrupt vector
                   ]

-------------------------------------------------------------------------------

processBiosTimerISR :: PcBios -> PrismM PcBios
processBiosTimerISR bios = do
    --liftIO $ putStrLn "Int 8 ISR"
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
        return $ bios { pcTimer = PcTimer ticksUs timerTicks' }
        else do
            --liftIO $ putStrLn "Continue"
            return $ bios { pcTimer = PcTimer (pcTimerLastInt8 $ pcTimer bios) timerTicks' }

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
            writeOp al 0x30 -- text mode is set
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
                    commands = (videoCommands s) ++ [VideoUpdateCursor]
                writeTVar vs $ s { videoCursor = c { videoCursorColumn = valDl, videoCursorRow = valDh }, videoCommands = commands }
            writeOp al 0
            return ()
        3 -> do -- Get cursor pos
            --valBh <- readOp bh
            let vs = pcVideoShared $ pcVideoState bios
            cursor <- liftIO $ atomically $ videoCursor <$> readTVar vs
            writeOp ch 0 -- starting cursor scan line
            writeOp cl 0 -- ending cursor scan line
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
            liftIO $ atomically $ modifyTVar vs $ addVideoCommands [VideoDrawChar valAl valBl]
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
            liftIO $ atomically $ modifyTVar vs $ addVideoCommands [(VideoDrawChar valAl valBl), VideoUpdateCursor]
            return ()
        0xf -> do -- Get video mode
            writeOp al 3 -- mode (CGA test)
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
        addVideoCommands commands s =
            s { videoCommands = commands' }
            where
                commands' = (videoCommands s) ++ commands
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

getDiskOpReq :: PcBios -> PrismM (Maybe DiskOpReq)
getDiskOpReq bios = do
    valAl <- readOp al -- number of sectors
    valCh <- readOp ch -- track number
    valCl <- readOp cl -- sector number
    valDh <- readOp dh -- head number
    valDl <- readOp dl -- drive number
    let trackNum = (shiftL (fromIntegral valCl :: Uint16) 2) .&. 0x30
        chs = PcChs trackNum (fromIntegral valDh :: Uint16) (fromIntegral (valCl .&. 0x3F) :: Uint16)
        driveIndex = intToDiskIndex valDl
        drive = Map.lookup driveIndex (pcDisks bios)
        lenBytesSectors = sectorsToInt valAl
        numSectors = valAl
    --es:bx -- input buffer
    case drive of
        Just d -> do
            valBx <- readOp bx
            valEs <- readOp es
            s <- get
            let memOffset = (shiftL (fromIntegral valEs) 4) + (fromIntegral valBx)
                (MemMain memPtr) = ctxMem s
                memPtr' = plusPtr memPtr memOffset
                offset = chsToOffset d chs
            return $ Just $ DiskOpReq offset lenBytesSectors memPtr' numSectors d
        _ -> do
            return Nothing

processBiosDisk :: PcBios -> PrismM PcBios
processBiosDisk bios = do
    valAh <- readOp ah
    case valAh of
        2 -> do -- read sectors
            req <- getDiskOpReq bios
            case req of
                Just (DiskOpReq diskOffset dataLen memPtr numSectors drive) -> do
                    bs <- liftIO $ (pcDiskRead drive) diskOffset dataLen
                    let (srcPtr, srcLen) = BI.toForeignPtr0 bs
                    liftIO $ withForeignPtr srcPtr (\ srcPtr -> do
                        copyArray memPtr srcPtr srcLen)
                    writeOp al numSectors -- sectors read
                    return ()
                _ -> do
                    writeOp al 0 -- sectors read
                    writeOp ah 15 -- no drive
                    return ()
        3 -> do -- write sectors
            req <- getDiskOpReq bios
            case req of
                Just (DiskOpReq diskOffset dataLen memPtr numSectors drive) -> do
                    memData <- liftIO $ BI.create dataLen (\ dataPtr ->
                        copyArray dataPtr memPtr dataLen
                        )
                    liftIO $ (pcDiskWrite drive) diskOffset memData
                    writeOp al numSectors -- sectors read
                    return ()
                _ -> do
                    writeOp al 0 -- sectors read
                    writeOp ah 15 -- no drive
                    return ()
            return ()
        4 -> do -- verify sectors
            writeOp ah 0
            return ()
        5 -> do -- format track
            writeOp ah 0
            return ()
        8 -> do -- read params
            valDl <- readOp dl -- drive number
            writeOp ax 0 -- always 0
            writeOp bh 0 -- always 0
            writeOp bl 4 -- 3.5", 1.44 MB
            writeOp ch 0 -- tracks
            writeOp cl 0 -- sectors
            writeOp dh 0 -- heads, always 1 when CMOS valid
            writeOp dl 0 -- number of diskette drives
            -- CF = 0 when no error
            -- es:di pointer to param table
            return ()
        0xc -> do -- seek
            writeOp ah 0
            return ()
        0x10 -> do -- check ready
            -- TODO: check docs
            writeOp ah 0
            return ()
        0x15 -> do -- read type
            valDl <- readOp dl -- drive number
            writeOp ah 0x03 -- fixed disk
            return ()
        0x16 -> do -- detect change
            writeOp ah 0
            return ()
        _ -> liftIO $ putStrLn "Unsupported"
    return bios

processBiosEquipment :: PcBios -> PrismM PcBios
processBiosEquipment bios = do
    let val = 0x0021 -- 80x25 color mode + 1 diskette
    writeOp ax val
    return bios

processBiosGetMemorySize :: PcBios -> PrismM PcBios
processBiosGetMemorySize bios = do
    writeOp ax 0x280 -- 640K
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
processBios bios 0x11 = processBiosGetMemorySize bios
processBios bios 0x12 = processBiosEquipment bios
processBios bios 0x13 = processBiosDisk bios
processBios bios 0x16 = processBiosKeyboard bios
processBios bios 0x1a = processBiosClock bios
processBios bios _ = processBiosTest bios
