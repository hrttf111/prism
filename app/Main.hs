module Main where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Semigroup ((<>))
import qualified Data.Map.Strict (fromList)

import Control.Monad.Trans.State
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

data PeripheralDevices = PeripheralDevices {
    }

writeTtyChar :: PrismM ()
writeTtyChar = do
    valAl <- readOp al
    liftIO $ putStrLn $ [((toEnum . fromEnum) $ valAl :: Char)]

videoInterrupt :: InterruptHandler
videoInterrupt _ = do
    liftIO $ putStrLn "Interrupt"
    valAh <- readOp ah
    case valAh of
        14 ->
            writeTtyChar
        v -> do
            liftIO $ putStrLn $ "Unknown video function " ++ (show v)

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
    (_, codeLen) <- readCodeToPtr binPath_ ptrMem 0
    (ioCtx, peripheral) <- makeDummyIO maxMemorySize PeripheralDevices
    let ctx = makeCtx memReg memMain ioCtx
    intM <- configureInterrups memMain 0xFF000 [(PrismInt 0x10, videoInterrupt)]
    ctxNew <- runPrismM ctx $ do
        clearRegs
        --(liftIO $ BS.readFile binPath_) >>= copyMainMem 0
        --writeOp ss 1000
        --writeOp sp 640
        --writeOp ds (div 8000 16)
        writeOp ip bootloaderStart
        writeOp cs 0
        decodeHaltCpu (decoder intM) comm
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
