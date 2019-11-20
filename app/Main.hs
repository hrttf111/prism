module Main where

import Prism
import PrismDecoder
import PrismCpu
import PrismIO
import PrismShow
import PrismInterrupt

import Instruction.Transfer
import Instruction.Arithmetic
import Instruction.Processor
import Instruction.Logical
import Instruction.Control

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Semigroup ((<>))
import qualified Data.Map.Strict (fromList)

import Options.Applicative
import Options.Applicative.Types

import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc
import Foreign.Ptr


data AppOpts = AppOpts {
        binPath :: !FilePath
    }

regSize = 64
maxMemorySize = 1024 * 1024
bootloaderStart = 0x7C00

writeTtyChar :: MonadIO m => Ctx -> m Ctx
writeTtyChar ctx = do
    valAl <- readReg8 memReg al
    liftIO $ putStrLn $ [((toEnum . fromEnum) $ valAl :: Char)]
    return ctx
    where
        memReg = ctxReg ctx

videoInterrupt :: InterruptHandler
videoInterrupt ctx _ = do
    valAh <- readReg8 memReg ah
    case valAh of
        14 -> writeTtyChar ctx
        v -> do
            liftIO $ putStrLn $ "Unknown video function " ++ (show v)
            return ctx
    return ctx
    where
        memReg = ctxReg ctx

interruptMap = Data.Map.Strict.fromList [(1, videoInterrupt)]
configureInterrups mem addr intList = 
    (mapHandlersToInts <$> writeInternalInterruptHandlers mem addr intInternalList)
        >>= setInterruptsToMemory mem
    where
        intInternalList = map fst intList
        mapHandlersToInts = foldl func []
        func lst (internalInt, addr) = 
            lst ++ (map (\(_, PrismInt i) -> (i, addr)) $ filter ((==internalInt) . fst) intList)

runBinary :: [PrismInstruction] -> FilePath -> IO ()
runBinary instrList binPath_ = do
    ptrReg <- callocBytes regSize
    ptrMem <- callocBytes maxMemorySize
    (_, codeLen) <- readCodeToPtr binPath_ ptrMem 0
    let ctx = makePrismCtx (MemReg ptrReg) (MemMain ptrMem)
    configureInterrups (ctxMem ctx) 0xFF000 [(1, PrismInt 0x10)]
    writeRegIP (ctxReg ctx) bootloaderStart
    ctxNew <- runPrism $ decodeHalt decoder ctx
    liftIO . putStrLn . show $ ctxNew
    printRegs $ ctxReg ctxNew
    where
        decoder = makeDecoderList combinedList
        combinedList = instrList 
            ++ (segmentInstrList instrList) 
            ++ (internalInstrList interruptMap)

instrList = transferInstrList 
    ++ arithmeticInstrList
    ++ processorInstrList
    ++ logicalInstrList
    ++ controlInstrList

main :: IO ()
main = do
    opts <- execParser optsParser
    runBinary instrList $ binPath opts
    return ()
    where
        optsParser = info
            (helper <*> mainOpts)
            (fullDesc <> progDesc "Prism 8086 emulator" <> header "Prism")
        mainOpts = AppOpts <$> strArgument (metavar "BIN" <> help "Binary executable path") 
