{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (foldM, when)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Semigroup ((<>))
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict (fromList)

import Control.Monad.Logger (LogLevel(..))
import Control.Monad.Trans.State
import Control.Concurrent.STM
import Control.Concurrent

import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.IO (FilePath, Handle, openFile, hSeek, hFileSize, IOMode(..), SeekMode(..))

import Options.Applicative
import Options.Applicative.Types

import Foreign.Storable (peekByteOff)
import Foreign.Marshal.Array (pokeArray)
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

-------------------------------------------------------------------------------

maxMemorySize = 1024 * 1024
bootloaderStart = 0x7C00

-------------------------------------------------------------------------------

convertKeyToKeycode :: Key -> Uint8
convertKeyToKeycode key = case key of
    KEsc -> 110
    KEnter -> 42
    KLeft -> 79
    KRight -> 89
    KUp -> 83
    KDown -> 84
    KFun n | n <= 12 -> 111 + (fromIntegral n)
    KBackTab -> 16
    KPrtScr -> 124
    KPause -> 126
    KIns -> 75
    KHome -> 80
    KPageUp -> 85
    KDel -> 76
    KEnd -> 81
    KPageDown -> 86
    KBS -> 15 -- backspace
    KChar c -> case c of
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        _ -> 0
    _ -> 0


convertKeyToAscii :: Key -> Uint8
convertKeyToAscii (KChar c) = fromIntegral $ fromEnum c
convertKeyToAscii _ = 0


peripheralThread :: Vty -> PrismCmdQueue -> TVar SharedKeyboardState -> TVar SharedVideoState -> IO ()
peripheralThread vty (PrismCmdQueue queue) keyboard video = do
    runP $ picForImage $ resize videoColumns videoRows emptyImage
    where
        videoColumns = 80
        videoRows = 25
        videoCharLength = videoColumns * videoRows
        drawVideoScreen :: Ptr Uint8 -> Int -> IO Image
        drawVideoScreen mem scroll =
            ((crop videoColumns videoRows) . vertCat) <$> mapM drawRow [0..videoRows]
            where
                drawRow rowN = do
                    horizCat <$> mapM (drawChar rowN) [0..videoColumns]
                drawChar rowN columnN = do
                    val <- (peekByteOff mem (((rowN * videoColumns) + columnN) * 2) :: IO Uint8)
                    return $ string defAttr [((toEnum . fromEnum) $ val :: Char)]
        execVideo pic VideoFullDraw = do
            s <- atomically $ readTVar video
            img <- drawVideoScreen (videoMemory s) (videoScrollPos s)
            let cursor = videoCursor s
                pic' = picForImage img
                pic'' = pic' { picCursor = (Cursor (videoCursorColumn cursor) (videoCursorRow cursor)) }
            update vty pic''
            return pic''
        execVideo pic (VideoDrawChar char attr) = do
            s <- atomically $ readTVar video
            let cursor = videoCursor s
                c = (toEnum . fromEnum) $ char :: Char
                pic' = addToTop pic $ translate (videoCursorColumn cursor) (videoCursorRow cursor) $ string defAttr [c]
                --line = translateX (videoCursorColumn cursor) $ translateY (videoCursorRow cursor) $ char defAttr c
                --pic' = picForImage $ horizJoin i $ crop videoColumns videoRows line
            update vty pic'
            return pic'
        execVideo pic VideoUpdateCursor = return pic
        readKey = do
            event <- nextEventNonblocking vty
            case event of
                Just (EvKey (KChar 'c') [MCtrl]) -> do
                    atomically $ writeTQueue queue PCmdStop
                    return ()
                Just (EvKey key mods) -> do
                    atomically $ do
                        ks <- readTVar keyboard
                        let keyCode = convertKeyToKeycode key
                            keyAscii = convertKeyToAscii key
                            pcKey = PcKey keyAscii keyCode
                            ks' = SharedKeyboardState (sharedFlags ks) ((sharedKeys ks) ++ [pcKey])
                        writeTVar keyboard ks'
                        writeTQueue queue $ PCmdInterruptUp (PrismIRQ 1)
                    return ()
                _ -> return ()
        runP pic = do
            videoCommands <- atomically $ do
                s <- readTVar video
                let cmd = videoCommands s
                writeTVar video $ s { videoCommands = [] }
                return cmd
            --pic' <- foldM execVideo pic videoCommands
            pic' <- if (length videoCommands) > 0 then
                execVideo pic VideoFullDraw
                else
                    return pic
            {-pic' <- (if (length $ picLayers pic) > 15 then
                execVideo pic VideoFullDraw
                else
                    foldM execVideo pic videoCommands)-}
            readKey
            threadDelay 10000
            runP pic'

-------------------------------------------------------------------------------

buildFloppy :: Handle -> Uint32 -> IO (Maybe PcDisk)
buildFloppy handle loc =
    (diskFloppySizeToChs . fromIntegral <$> hFileSize handle) >>= mapM (\ chs ->
            return $ PcDisk 0 loc chs (readFloppy handle) (writeFloppy handle))

readFloppy :: Handle -> Int -> Int -> IO BS.ByteString
readFloppy handle offset length = do
    liftIO $ putStrLn $ "Read offset = " ++ (show offset) ++ " len = " ++ (show length)
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
        normLength = if length > maxLength then maxLength else length

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
                    let normLength = if (BS.length dt) > maxLength then maxLength else (BS.length dt)
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
        logLevelGDB :: !String
    }

buildPC :: (MonadIO m) => AppOpts -> m (PC, IOCtx, [InterruptHandlerLocation], (TVar SharedKeyboardState, TVar SharedVideoState))
buildPC opts = do
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
        memEntries = []
        mkIOCtx pc queue =
            let (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) =
                    createPeripheralsL pc maxMemorySize pageSize portEntries memEntries
            in
                IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue emptyScheduler devices) memRegion portRegion

runBinary :: AppOpts -> IO ()
runBinary opts = do
    let binPath_ = binPath opts
        enableGDB_ = enableGDB opts
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
        forkIO $ gdbThread logLevel $ GDBState True 1000 (commCmdQueue comm) (commRspQueue comm)
        return ()
        )
    memReg <- allocMemReg
    memMain <- allocMemMain maxMemorySize
    let (MemMain ptrMem) = memMain
    (pc, ioCtx, intList, states) <- buildPC opts
    vty <- startVtyThread (commCmdQueue comm) (fst states) (snd states)
    when (not $ floppyMode opts) $ do
        readCodeToPtr binPath_ ptrMem 0
        return ()
    let ctx = makeCtx memReg memMain ioCtx
    intM <- configureInterrupts memMain 0xFF000 intList
    ctxNew <- runPrismM ctx $ do
        clearRegs
        writeOp ip bootloaderStart
        writeOp cs 0
        writeOp ss 0
        writeOp sp 0x7C00
        setPcMemory pc
        when (floppyMode opts) $ rebootPc pc
        decodeHaltCpu (decoder intM) comm
    case vty of
        Just v -> do
            threadDelay 1000000
            shutdown v
        _ -> return ()
    liftIO . putStrLn . show $ ctxNew
    printRegs $ ctxReg ctxNew
    where
        doRunVty = not $ disableVty opts
        startVtyThread queue keyboard video =
            if doRunVty then do
                cfg <- standardIOConfig
                vty <- mkVty $ cfg { mouseMode = Just True, vmin = Just 1}
                forkIO $ peripheralThread vty queue keyboard video
                return $ Just vty
                else do
                    putStrLn "vty is disabled"
                    return Nothing
        decoder intM = makeDecoderList (combinedList intM)
        combinedList intM = x86InstrList
            ++ (internalInstrList intM)

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
                    strOption (long "gdb-log" <> help "GDB log level: debug, info, error" <> value "debug")

-------------------------------------------------------------------------------
