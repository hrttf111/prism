module Main where

import Prism
import PrismDecoder
import PrismCpu
import PrismIO
import PrismShow

import Instruction.Transfer
import Instruction.Arithmetic
import Instruction.Processor
import Instruction.Logical
import Instruction.Control

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Semigroup ((<>))

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

runBinary :: [PrismInstruction] -> FilePath -> IO ()
runBinary instrList binPath_ = do
    ptrReg <- callocBytes regSize
    ptrMem <- callocBytes maxMemorySize
    (_, codeLen) <- readCodeToPtr binPath_ ptrMem 0
    let ctx = Ctx (MemReg ptrReg) (MemMain ptrMem) clearFlags clearEFlags Nothing
    writeRegIP (ctxReg ctx) bootloaderStart
    ctxNew <- runPrism $ decodeMemIp decoder 0x10010 ctx
    liftIO . putStrLn . show $ ctxNew
    printRegs $ ctxReg ctxNew
    where
        decoder = makeDecoderList instrList

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
