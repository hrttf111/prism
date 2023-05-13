module Main where

import Control.Monad (foldM)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Semigroup ((<>))
import qualified Data.Map.Strict (fromList)

import Control.Monad.Trans.State
import Control.Concurrent.STM
import Control.Concurrent

import Data.Word (Word8)
import qualified Data.ByteString as BS
import System.IO (FilePath)

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

peripheralThread :: Vty -> PrismCmdQueue -> TVar SharedKeyboardState -> TVar SharedVideoState -> IO ()
peripheralThread vty queue keyboard video = do
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
            threadDelay 10000
            runP pic'

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
        enableGDB :: !Bool
    }

buildPC :: (MonadIO m) => m (IOCtx, [InterruptHandlerLocation], (TVar SharedKeyboardState, TVar SharedVideoState))
buildPC = do
    queue <- liftIO $ createIOQueue
    pc <- createPC
    let states = getPcBiosSharedState pc
    return $ (mkIOCtx pc queue, intList, states)
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

runBinary :: FilePath -> Bool -> IO ()
runBinary binPath_  enableGDB_ = do
    cfg <- standardIOConfig
    vty <- mkVty $ cfg { mouseMode = Just True, vmin = Just 1}
    --
    comm <- newPrismComm enableGDB_
    if enableGDB_ then
        (forkIO . gdbThread $ GDBState True 1000 (commCmdQueue comm) (commRspQueue comm)) >> return ()
        else
            return ()
    memReg <- allocMemReg
    memMain <- allocMemMain maxMemorySize
    let (MemMain ptrMem) = memMain
    (ioCtx, intList, states) <- buildPC
    forkIO $ peripheralThread vty (commCmdQueue comm) (fst states) (snd states)
    (_, codeLen) <- readCodeToPtr binPath_ ptrMem 0
    let ctx = makeCtx memReg memMain ioCtx
    intM <- configureInterrupts memMain 0xFF000 intList
    ctxNew <- runPrismM ctx $ do
        clearRegs
        writeOp ip bootloaderStart
        writeOp cs 0
        decodeHaltCpu (decoder intM) comm
    threadDelay 1000000
    shutdown vty
    liftIO . putStrLn . show $ ctxNew
    printRegs $ ctxReg ctxNew
    where
        decoder intM = makeDecoderList (combinedList intM)
        combinedList intM = x86InstrList
            ++ (internalInstrList intM)

main :: IO ()
main = do
    opts <- execParser optsParser
    runBinary (binPath opts) (enableGDB opts)
    return ()
    where
        optsParser = info
            (helper <*> mainOpts)
            (fullDesc <> progDesc "Prism 8086 emulator" <> header "Prism")
        mainOpts = AppOpts <$> strArgument (metavar "BIN" <> help "Binary executable path") <*>
                    switch (long "gdb" <> help "Enable GDB server and stop on first instruction")

-------------------------------------------------------------------------------
