{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (foldM, when)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Semigroup ((<>))
import Data.Maybe (maybeToList, maybe)
import qualified Data.Map.Strict (fromList)
import Data.List (intercalate)

import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Time.Clock.POSIX as PTime

import Control.Monad.Logger (LogLevel(..))
import Control.Monad.Trans.State
import Control.Concurrent.STM
import Control.Concurrent

import Data.Bits (shiftR, (.&.))
import Data.Char (toLower)
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Array (Array, (!), bounds)
import System.IO (FilePath, Handle, openFile, hClose, hSetBuffering, BufferMode(..), hPutStrLn, hSeek, hFileSize, IOMode(..), SeekMode(..))
import System.Exit (ExitCode(ExitFailure))
import System.Posix.Process (exitImmediately)

import Options.Applicative
import Options.Applicative.Types

import Foreign.Storable (peekByteOff)
import Foreign.Marshal.Array (pokeArray, peekArray)
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Vty
import Graphics.Vty.Image

import Prism.Cpu
import Prism.Decoder
import Prism.Peripherals
import Prism.Command
import Prism.Run
import Prism.Instructions
import Prism.GDB
import Prism.PC
import qualified Prism.Log as Log
import Prism.Log ((.=), (..=))

-------------------------------------------------------------------------------

maxMemorySize = 1024 * 1024
bootloaderStart = 0x7C00

-------------------------------------------------------------------------------

convertKeyToKeycode :: Key -> Uint8
convertKeyToKeycode key = case key of
    KEsc -> 0x01
    KEnter -> 0x1C
    KLeft -> 0x4B
    KRight -> 0x4D
    KUp -> 0x48
    KDown -> 0x50
    KFun n | n <= 10 -> 0x3A + (fromIntegral n)
    KFun 11 -> 0x57
    KFun 12 -> 0x58
    KBackTab -> 0x0F
    --KPrtScr -> 0
    --KPause -> 0
    KIns -> 0x52
    KHome -> 0x47
    KPageUp -> 0x49
    KDel -> 0x53
    KEnd -> 0x4F
    KPageDown -> 0x51
    KBS -> 0x0E -- backspace
    KChar c -> case toLower c of
        '`' -> 0x29
        '~' -> 0x29
        '1' -> 0x02
        '!' -> 0x02
        '2' -> 0x03
        '@' -> 0x03
        '3' -> 0x04
        '#' -> 0x04
        '4' -> 0x05
        '$' -> 0x05
        '5' -> 0x06
        '%' -> 0x06
        '6' -> 0x07
        '^' -> 0x07
        '7' -> 0x08
        '&' -> 0x08
        '8' -> 0x09
        '*' -> 0x09
        '9' -> 0x0A
        '(' -> 0x0A
        '0' -> 0x0B
        ')' -> 0x0B
        '-' -> 0x0C
        '_' -> 0x0C
        '=' -> 0x0D
        '+' -> 0x0D
        -- Backspace 0x0E/0x08
        -- Tab 0x0F/0x09
        'q' -> 0x10
        'w' -> 0x11
        'e' -> 0x12
        'r' -> 0x13
        't' -> 0x14
        'y' -> 0x15
        'u' -> 0x16
        'i' -> 0x17
        'o' -> 0x18
        'p' -> 0x19
        '[' -> 0x1A
        '{' -> 0x1A
        ']' -> 0x1B
        '}' -> 0x1B
        '|' -> 0x2B
        '\\' -> 0x2B
        -- Capslock 0x3A
        'a' -> 0x1E
        's' -> 0x1F
        'd' -> 0x20
        'f' -> 0x21
        'g' -> 0x22
        'h' -> 0x23
        'j' -> 0x24
        'k' -> 0x25
        'l' -> 0x26
        ';' -> 0x27
        ':' -> 0x27
        '\'' -> 0x28
        '"' -> 0x28
        -- Enter 0x1C/0x0D
        -- L Shift 0x2A
        'z' -> 0x2C
        'x' -> 0x2D
        'c' -> 0x2E
        'v' -> 0x2F
        'b' -> 0x30
        'n' -> 0x31
        'm' -> 0x32
        ',' -> 0x33
        '.' -> 0x34
        '/' -> 0x35
        -- R Shift 0x36
        -- L Ctrl 0x1D
        -- L Alt 0x38
        ' ' -> 0x39
        -- R Alt 0x38
        -- R Ctrl 0x1D
        -- Insert 0x52
        -- Delete 0x53
        -- Left 0x4B
        -- Home 0x47
        -- End 0x4F
        -- Up 0x48
        -- Down 0x50
        -- Page Up 0x49
        -- Page Down 0x51
        -- Right 0x4D
        -- Num Lock 0x45
        -- 7/Home
        -- 4/Left
        -- 1/End
        -- /
        -- 8/Up
        -- 5
        -- 2/Down
        -- 0/Ins
        -- *
        -- 9/PgUp
        -- 6/Right
        -- 3/PgDown
        -- Del
        -- -
        -- +
        -- Enter
        -- Esc 0x01
        -- F1-F12 0x3B-0x44 0x57 0x58
        -- SysReq
        -- Print Screen 0x2A
        -- Scroll Lock 0x46
        -- Pause
        _ -> 0
    _ -> 0


convertKeyToAscii :: Key -> Uint8
convertKeyToAscii (KChar c) = fromIntegral $ fromEnum c
convertKeyToAscii key = case key of
    KEsc -> 0x01
    KEnter -> 0x0D
    KBackTab -> 0x09
    KBS -> 0x08 -- backspace
    _ -> 0


convertUint8ToChar :: Uint8 -> Char
convertUint8ToChar = toEnum . fromEnum

isBadChar :: Uint8 -> Bool
isBadChar 0x8 = True
isBadChar 0xa = True
isBadChar 0xd = True
isBadChar _ = False

filterChar :: Uint8 -> Uint8
filterChar c | isBadChar c = 0x20
filterChar c = c

convertUint8ToAttr :: Uint8 -> Attr
convertUint8ToAttr val = defAttr `withForeColor` fg `withBackColor` bg
    where
        fg = convColor $ val .&. 0xf
        bg = convColor $ shiftR val 4
        convColor :: Uint8 -> Color
        convColor 0 = black
        convColor 1 = blue
        convColor 2 = green
        convColor 3 = cyan
        convColor 4 = red
        convColor 5 = magenta
        convColor 6 = black -- brown
        convColor 7 = white
        convColor 8 = brightBlack
        convColor 9 = brightBlue
        convColor 10 = brightGreen
        convColor 11 = brightCyan
        convColor 12 = brightRed
        convColor 13 = brightMagenta
        convColor 14 = yellow
        convColor 15 = brightWhite

peekVideoRow :: (VideoConsole c) => c -> Ptr Uint8 -> Int -> IO [(Uint8, Uint8)]
peekVideoRow console videoMem row =
    splitPairs [] <$> peekArray rowLengthBytes rowPtr
    where
        rowLengthBytes = (videoConsoleCharSize console) * (videoConsoleColumns console)
        rowPtr = plusPtr videoMem $ videoConsoleMemOffset console row 0
        splitPairs :: [(Uint8, Uint8)] -> [Uint8] -> [(Uint8, Uint8)]
        splitPairs res [] = res
        splitPairs res (_:[]) = res
        splitPairs res (a:(b:c)) = splitPairs (res ++ [(a,b)]) c


peripheralThread :: Vty -> PrismCmdQueue -> TVar Int -> TMVar SharedKeyboardState -> TMVar SharedVideoState -> DebugCtx -> IO ()
peripheralThread vty (PrismCmdQueue queue) wdVar keyboard video debugCtx = do
    runP (picForImage $ resize (videoConsoleColumns console) (videoConsoleRows console) emptyImage) 0 0
    where
        console = VideoConsole80x25
        drawVideoScreen :: Ptr Uint8 -> Int -> IO Image
        drawVideoScreen mem scroll =
            ((crop (videoConsoleColumns console) (videoConsoleRows console)) . vertCat)
                <$> mapM drawRow (videoConsoleRowsRange console)
            where
                drawRowNoAttr rowN =
                    (string defAttr . map (convertUint8ToChar . filterChar . fst)) <$> peekVideoRow console mem rowN
                drawRow rowN = do
                    horizCat <$> mapM (drawChar rowN) (videoConsoleColumnsRange console)
                drawChar rowN columnN = do
                    (valChar, valAttr) <- peekVideoChar console mem rowN columnN
                    return $ char (convertUint8ToAttr valAttr) (convertUint8ToChar $ filterChar valChar)
        execVideo pic VideoFullDraw = do
            s <- atomically $ readTMVar video
            img <- drawVideoScreen (videoMemory s) (videoScrollPos s)
            let cursor = videoCursor s
                pic' = picForImage img
                pic'' = pic' { picCursor = (Cursor (videoCursorColumn cursor) (videoCursorRow cursor)) }
            update vty pic''
            return pic''
        execVideo pic (VideoDrawChar char attr cursor1) = do
            s <- atomically $ readTMVar video
            let cursor = videoCursor s
                c = convertUint8ToChar char
                pic' = addToTop pic $ translate (videoCursorColumn cursor1) (videoCursorRow cursor1) $ string defAttr [c]
                pic'' = pic' { picCursor = (Cursor (videoCursorColumn cursor) (videoCursorRow cursor)) }
            update vty pic''
            return pic''
        execVideo pic VideoUpdateCursor = do
            s <- atomically $ readTMVar video
            let cursor = videoCursor s
                pic' = pic { picCursor = (Cursor (videoCursorColumn cursor) (videoCursorRow cursor)) }
            update vty pic'
            return pic'
        readKey = do
            event <- nextEventNonblocking vty
            case event of
                Just (EvKey (KChar 'c') [MCtrl]) -> do
                    atomically $ writeTQueue queue PCmdStop
                    threadDelay 5000000 -- 5s
                    doLog debugCtx Error "Forced exit"
                    exitImmediately $ ExitFailure 1
                    return ()
                Just (EvKey key mods) -> do
                    atomically $ do
                        ks <- takeTMVar keyboard
                        let keyCode = convertKeyToKeycode key
                            keyAscii = convertKeyToAscii key
                            pcKey = PcKey keyAscii keyCode
                            ks' = SharedKeyboardState (sharedFlags ks) ((sharedKeys ks) ++ [pcKey])
                        putTMVar keyboard ks'
                        writeTQueue queue $ PCmdInterruptUp (PrismIRQ 1)
                    return ()
                _ -> return ()
        getCommands =
            atomically $ do
                s <- takeTMVar video
                let cmd = videoCommands s
                putTMVar video $ s { videoCommands = [] }
                return cmd
        runP pic n k = do
            doLog debugCtx Trace "Get commands"
            videoCommands <- getCommands
            doLog debugCtx Trace $ "Exec commands: " ++ (show $ length videoCommands)
            pic' <- if ((length videoCommands) > 3) || ((length $ picLayers pic) > 20) then
                execVideo pic VideoFullDraw
                else
                    foldM execVideo pic videoCommands
            doLog debugCtx Trace "Read key"
            readKey
            doLog debugCtx Trace "Delay"
            threadDelay 10000
            atomically $ modifyTVar wdVar (+1)
            when (n > 10) $ do
                doLog debugCtx Info $ "Tick=" ++ show k
                runP pic' 0 (k+1)
            runP pic' (n+1) k

-------------------------------------------------------------------------------

doLog debugCtx level msg = do
    let fid = Log.getFeatureId Log.PrismCommon
    (debugCtxPrint debugCtx) (fromEnum level) fid $ msg

watchdogExec :: TVar Int -> DebugCtx -> IO ()
watchdogExec idPeripheral debugCtx =
    doExec (0, 0) 0
    where
        doExec (idPeripheralLast, nLast) n = do
            threadDelay 1000000 -- 1s
            idCurrent <- atomically $ readTVar idPeripheral
            if idCurrent > idPeripheralLast then
                doExec (idCurrent, n) (n+1)
                else if (n - nLast) > 3 then do
                    doLog debugCtx Error "Forced exit WD"
                    exitImmediately $ ExitFailure 1
                    else do
                        doLog debugCtx Warning  $ "Watchdog: n="
                                                ++ (show n) ++ ", nL=" ++ (show nLast)
                                                ++ ", var=" ++ (show idPeripheralLast)
                        doExec (idCurrent, nLast) (n+1)

-------------------------------------------------------------------------------

buildFloppy :: Handle -> Uint32 -> IO (Maybe PcDisk)
buildFloppy handle loc =
    (diskFloppySizeToChs . fromIntegral <$> hFileSize handle) >>= mapM (\ chs ->
            return $ PcDisk 0 loc chs (readFloppy handle) (writeFloppy handle))

readFloppy :: Handle -> Int -> Int -> IO BS.ByteString
readFloppy handle offset length = do
    if (maxLength <= 0) || (offset < 0) || (length <= 0) then
        return BS.empty
        else do
            hSeek handle AbsoluteSeek $ fromIntegral offset
            dt <- BS.hGetSome handle normLength
            let lenDiff = normLength - (BS.length dt)
            if lenDiff > 0 then
                return $ BS.append dt $ BS.pack [0 | _ <- [0..lenDiff]]
                else
                    return dt
    where
        maxLength = maxFloppySize - offset
        normLength = min length maxLength

writeFloppy :: Handle -> Int -> BS.ByteString -> IO ()
writeFloppy handle offset dt =
    if (offset < 0) || (offset > maxFloppySize) || (BS.null dt) then
        return ()
        else do
            diskSize <- fromIntegral <$> hFileSize handle
            let maxLength = diskSize - offset
            if maxLength <= 0 then
                return ()
                else do
                    let normLength = min maxLength (BS.length dt)
                    hSeek handle AbsoluteSeek $ fromIntegral offset
                    BS.hPut handle $ BS.take normLength dt

-------------------------------------------------------------------------------

readCodeToPtr :: MonadIO m => FilePath -> Ptr Word8 -> Int -> m (Ptr Word8, Int)
readCodeToPtr filePath ptr offset = liftIO $ do
    bs <- BS.readFile filePath
    let array = BS.unpack bs
        ptrN = plusPtr ptr offset
    pokeArray ptrN array
    return (ptr, BS.length bs)

data AppOpts = AppOpts {
        binPath :: !FilePath,
        enableGDB :: !Bool,
        pauseGDB :: !Bool,
        disableVty :: !Bool,
        floppyMode :: !Bool,
        logLevelGDB :: !String,
        externalLog :: !Bool
    }

buildPC :: (MonadIO m) => AppOpts -> DebugCtx -> m (PC, IOCtx, [InterruptHandlerLocation], (TMVar SharedKeyboardState, TMVar SharedVideoState))
buildPC opts debugCtx = do
    queue <- liftIO $ createIOQueue
    disks <- liftIO $ if floppyMode opts then do
        handle <- openFile (binPath opts) ReadWriteMode
        map (PcDiskFloppy 0,) . maybeToList <$> buildFloppy handle 0xE0100
        else
            return []
    pc <- createPcWithDisks disks
    let states = getPcBiosSharedState pc
    return $ (pc, mkIOCtx pc queue, intList, states)
    where
        intList = mkBiosInterrupts
        pageSize = 1024
        portEntries = pcPorts
        memEntries = pcMemory
        mkIOCtx pc queue =
            let (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) =
                    createPeripheralsL pc maxMemorySize pageSize portEntries memEntries
            in
                IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue emptyScheduler debugCtx devices) memRegion portRegion

debugCtx logFile = DebugCtx (maybe debugPrint debugPrintToFile logFile) fEnable
    where
        featureArray =
            Log.makeFeatureArray $
            Log.BiosTimer .= Error
            <> Log.PrismCommon .= Warning
            -- <> Log.CpuJmpIntra .= Trace
            -- <> Log.CpuCallIntra .= Trace
            -- <> Log.CpuHalt .= Debug
            -- <> Log.CpuJmpInter .= Trace
            -- <> Log.CpuCallInter .= Trace
            -- <> Log.CpuInt .= Trace
            <> Log.BiosKeyboard .= Warning
            <> Log.BiosVideo .= Info
            -- <> Log.BiosDisk .= Debug
            -- <> Log.PrismCommand .= Debug
            <> Log.PrismPc .= Warning
            -- <> Log.PrismRun .= Trace
            -- <> Log.CpuStrings .= Trace
        formatStr level feature msg = do
            date <- PTime.getCurrentTime
            let timeStr = formatTime defaultTimeLocale "%H:%M:%S.%q" date
            return $ timeStr ++ " "
                    ++ (Log.intToLevelName level) ++ " "
                    ++ (Log.intToFeatureName feature) ++ " -- "
                    ++ msg
        debugPrint level feature msg =
            if fEnable level feature then
                formatStr level feature msg >>= putStrLn
                else
                    return ()
        debugPrintToFile handle = \level feature msg ->
            if fEnable level feature then
                formatStr level feature msg >>= hPutStrLn handle
                else
                    return ()
        fEnable level feature =
            (featureArray ! feature) <= level

runBinary :: AppOpts -> IO ()
runBinary opts = do
    let binPath_ = binPath opts
        enableGDB_ = enableGDB opts
        logPath = "./prism-log"
    logFile <- if (externalLog opts) then
        Just <$> do
            h <- openFile logPath AppendMode
            hSetBuffering h LineBuffering
            hPutStrLn h "Start log"
            return h
        else
            return Nothing
    comm <- newPrismComm enableGDB_
    when enableGDB_ (do
        when (pauseGDB opts) (do
            let (PrismCmdQueue queue) = commCmdQueue comm
            atomically $ writeTQueue queue PCmdPause
            )
        let logLevel = case logLevelGDB opts of
                            "debug" -> LevelDebug
                            "info" -> LevelInfo
                            "warn" -> LevelWarn
                            "error" -> LevelError
                            "no" -> LevelOther $ T.pack "no"
                            _ -> LevelError
            gdbAddress = "127.0.0.1"
            gdbPort = 20001
        forkIO $ gdbThread logLevel logFile gdbAddress gdbPort $ GDBState True 1000 0 (commCmdQueue comm) (commRspQueue comm)
        return ()
        )
    memReg <- allocMemReg
    memMain <- allocMemMain maxMemorySize
    let (MemMain ptrMem) = memMain
    (pc, ioCtx, intList, states) <- buildPC opts (debugCtx logFile)
    vty <- startVtyThread (commCmdQueue comm) (fst states) (snd states) (debugCtx logFile)
    when (not $ floppyMode opts) $ do
        readCodeToPtr binPath_ ptrMem 0
        return ()
    let ctx = makeCtx memReg memMain ioCtx (debugCtx logFile)
    intM <- configureInterrupts memMain 0xFF000 intList
    ctxNew <- runPrismM ctx $ do
        clearRegs
        writeOp ip bootloaderStart
        writeOp cs 0
        writeOp ss 0
        writeOp sp 0x7C00
        setPcMemory pc
        initPicPit
        when (floppyMode opts) $ rebootPc pc
        decodeHaltCpu (decoder intM) comm
    case vty of
        Just v -> do
            shutdown v
        _ -> return ()
    liftIO . putStrLn . show $ ctxNew
    putStrLn =<< ((intercalate "\n") <$> (printRegs $ ctxReg ctxNew))
    mapM_ hClose logFile
    where
        doRunVty = not $ disableVty opts
        startWd var debugCtx =
            forkOS $ watchdogExec var debugCtx
        startVtyThread queue keyboard video debugCtx =
            if doRunVty then do
                cfg <- standardIOConfig
                vty <- mkVty $ cfg { mouseMode = Just True, vmin = Just 1}
                var <- newTVarIO 0
                forkOS $ peripheralThread vty queue var keyboard video debugCtx
                startWd var debugCtx
                return $ Just vty
                else do
                    putStrLn "vty is disabled"
                    return Nothing
        decoder intM = makeDecoderList (combinedList intM)
        combinedList intM = x86InstrList
            ++ (internalInstrList intM)
        initPicPit = do
            -- Init PIC
            writeOp (Port8 0x20) 0x17
            writeOp (Port8 0x21) 0x08 -- Master interrupts 0x20-0x27
            writeOp (Port8 0x21) 0x00
            writeOp (Port8 0x21) 0x03
            writeOp (Port8 0x21) 0x00
            -- Init PIT
            writeOp (Port8 0x43) 0x34 -- Timer0, Mode2, 2 Bytes, HEX
            writeOp (Port8 0x40) 10
            writeOp (Port8 0x40) 10
            return ()

main :: IO ()
main = do
    opts <- execParser optsParser
    runBinary opts
    return ()
    where
        optsParser = info
            (helper <*> mainOpts)
            (fullDesc <> progDesc "Prism 8086 emulator" <> header "Prism")
        mainOpts = AppOpts <$>
                    strArgument (metavar "BIN" <> help "Binary executable path") <*>
                    switch (long "gdb" <> help "Enable GDB server and stop on first instruction") <*>
                    switch (long "pause" <> help "Pause after start") <*>
                    switch (long "no-vty" <> help "Disable vty") <*>
                    switch (long "floppy" <> help "Floppy mode") <*>
                    strOption (long "gdb-log" <> help "GDB log level: debug, info, error" <> value "debug") <*>
                    switch (long "external-log" <> help "Write logs to an external file")

-------------------------------------------------------------------------------
