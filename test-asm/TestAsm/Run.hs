{-# LANGUAGE FlexibleContexts #-}

module TestAsm.Run where

import Control.Monad.Trans (MonadIO, liftIO)

import Control.Concurrent

import qualified Data.ByteString as B

import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Array (allocaArray, callocArray, pokeArray)
import Foreign.Marshal.Utils (fillBytes)

import Prism.Cpu
import Prism.Decoder

import TestAsm.Common
import Assembler

-------------------------------------------------------------------------------

createTestEnv1 :: MonadIO m =>
                  --IOCtx ->
                  Maybe ThreadId ->
                  [PrismInstruction] ->
                  --[InterruptHandlerLocation] ->
                  m TestEnv
createTestEnv1 threadId instrList = liftIO $ do
    ptrA <- allocMemRegRaw
    memReg <- allocMemReg
    memMain <- allocMemMain memSize
    --intM <- configureInterrups (MemMain ptrMem) intHandlersOffset intList
    --let decoder = makeDecoderList (instrList ++ (internalInstrList intM))
    let decoder = makeDecoderList instrList
    asmTest <- makeAsmTest
    return $ TestEnv 
                threadId
                makeAsmStr
                makeAsmStr16
                (execNative asmTest ptrA)
                (execP memReg memMain decoder)
    where
        memSize = 65000
        codeStart = 12000 :: Uint16
        intHandlersOffset = 60000
        execNative asmTest ptrA mainCode =
            MemReg <$> execCode asmTest mainCode ptrA
        execP memReg memMain decoder mainCode runner = do
            let ctx = makeCtx memReg memMain --ioCtx
                instrEnd = fromIntegral codeStart + B.length mainCode
            runPrismM ctx $ func1 decoder mainCode instrEnd runner
        func1 decoder mainCode instrEnd runner = do
            clearRegs
            copyMainMem (fromIntegral codeStart) mainCode
            writeOp ss 1000
            writeOp sp 640
            writeOp ds 8000
            writeOp cs (div codeStart 16)
            runner decoder instrEnd

-------------------------------------------------------------------------------
