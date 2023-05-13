module Main where

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

import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Prism.Cpu
import Prism.Decoder
import Prism.Peripherals
import Prism.Command
import Prism.Run
import Prism.Instructions
import Prism.GDB
import Prism.PC

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

maxMemorySize = 1024 * 1024
bootloaderStart = 0x7C00

peripheralThread :: PrismCmdQueue -> TVar SharedKeyboardState -> TVar SharedVideoState -> IO ()
peripheralThread queue keyboard video = runP
    where
        execVideo VideoFullDraw = return ()
        execVideo (VideoDrawChar char attr) = do
            putStrLn "Char"
            putStrLn $ [((toEnum . fromEnum) $ char :: Char)]
        execVideo VideoUpdateCursor = return ()
        runP = do
            videoCommands <- atomically $ do
                s <- readTVar video
                let cmd = videoCommands s
                writeTVar video $ s { videoCommands = [] }
                return cmd
            mapM_ execVideo videoCommands
            threadDelay 100000
            runP

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
    comm <- newPrismComm enableGDB_
    if enableGDB_ then
        (forkIO . gdbThread $ GDBState True 1000 (commCmdQueue comm) (commRspQueue comm)) >> return ()
        else
            return ()
    memReg <- allocMemReg
    memMain <- allocMemMain maxMemorySize
    let (MemMain ptrMem) = memMain
    (ioCtx, intList, states) <- buildPC
    (forkIO $ peripheralThread (commCmdQueue comm) (fst states) (snd states))
    (_, codeLen) <- readCodeToPtr binPath_ ptrMem 0
    --(ioCtx, peripheral) <- makeDummyIO maxMemorySize PeripheralDevices
    let ctx = makeCtx memReg memMain ioCtx
    intM <- configureInterrupts memMain 0xFF000 intList
    ctxNew <- runPrismM ctx $ do
        clearRegs
        writeOp ip bootloaderStart
        writeOp cs 0
        decodeHaltCpu (decoder intM) comm
    threadDelay 1000000
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
