{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ExecPrism where

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString as B

import Data.List (intercalate)

import Prism.Cpu
import Prism.Decoder
import Prism.Run
import Prism.Command
import Prism.Peripherals
import Prism.Instructions (internalInstrList)

import TestAsm.Common (ProgramExecutor(..), OperandSupport(..), MemRange(..), MemRangeRes(..), AllRegs(..))

-------------------------------------------------------------------------------

data ExecutorPrismRes = ExecutorPrismRes {
    eprMemReg :: MemReg,
    eprMemMain :: MemMain
}

type PrismRunner = PrismDecoder -> Int -> PrismM ()

data ExecutorPrism = ExecutorPrism {
    epPrismCtx :: Ctx,
    epDecoder :: PrismDecoder,
    epCodeStart :: Uint16,
    epRunner :: PrismRunner
}

instance (MonadIO m) => ProgramExecutor ExecutorPrism ExecutorPrismRes m where
    execProgram ep mainCode = liftIO $ do
        let run = epRunner ep
            decoder = epDecoder ep
            codeStart = epCodeStart ep
            dataSegment = 8000
        runPrismM (epPrismCtx ep) $ do
            clearRegs
            copyMainMem (fromIntegral codeStart) mainCode
            writeOp ss 1000
            writeOp sp 640
            writeOp ds (div dataSegment 16)
            writeOp cs (div codeStart 16)
            run decoder $ (fromIntegral codeStart + B.length mainCode)
        return $ ExecutorPrismRes (ctxReg $ epPrismCtx ep) (ctxMem $ epPrismCtx ep)

-------------------------------------------------------------------------------

createPrismExecutor :: MonadIO m =>
                       IOCtx ->
                       [PrismInstruction] ->
                       [InterruptHandlerLocation] ->
                       DebugCtx ->
                       PrismRunner ->
                       m ExecutorPrism
createPrismExecutor ioCtx instrList intList debugCtx runner = liftIO $ do
    memReg <- allocMemReg
    memMain <- allocMemMain memSize
    intM <- configureInterrupts memMain intHandlersOffset intList
    let decoder = makeDecoderList (instrList ++ (internalInstrList intM))
        ctx = makeCtx memReg memMain ioCtx debugCtx
    return $ ExecutorPrism ctx decoder codeStart runner
    where
        memSize = 65000
        codeStart = 12000
        intHandlersOffset = 60000

createPrismExecutorNoIO :: (MonadIO m) => [PrismInstruction] -> PrismRunner -> m ExecutorPrism
createPrismExecutorNoIO instrList runner = do
    (ioCtx, _) <- liftIO $ makeDummyIO (1024*1024) devicesStub
    createPrismExecutor ioCtx instrList [] debugCtx runner
    where
        debugCtx = DebugCtx (\_ _ _ -> return ()) (\_ _ -> False)
        devicesStub = 0 :: Int

-------------------------------------------------------------------------------

instance (MonadIO m) => OperandSupport ExecutorPrismRes Reg8 Uint8 m where
    readSourceOp epr reg =
        readOpRaw (eprMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorPrismRes Reg16 Uint16 m where
    readSourceOp epr reg =
        readOpRaw (eprMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorPrismRes RegSeg Uint16 m where
    readSourceOp epr reg =
        readOpRaw (eprMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorPrismRes MemPhy8 Uint8 m where
    readSourceOp epr mem =
        readOpRaw (eprMemMain epr) mem

instance (MonadIO m) => OperandSupport ExecutorPrismRes MemPhy16 Uint16 m where
    readSourceOp epr mem =
        readOpRaw (eprMemMain epr) mem

instance (MonadIO m) => OperandSupport ExecutorPrismRes MemRange MemRangeRes m where
    readSourceOp epr (MemRange start end) =
        MemRangeRes <$> mapM (\mem -> readOpRaw (eprMemMain epr) $ MemPhy8 $ fromIntegral mem) [start..end]

instance (MonadIO m) => OperandSupport ExecutorPrismRes AllRegs String m where
    readSourceOp epr _ = do
        sr <- (intercalate "\n") <$> (printRegs $ eprMemReg epr)
        sf <- (intercalate "\n") <$> (printFlags $ eprMemReg epr)
        return $ sr ++ "\n" ++ sf

instance (MonadIO m) => OperandSupport ExecutorPrismRes Flag Bool m where
    readSourceOp epr reg =
        readOpRaw (eprMemReg epr) reg

instance (MonadIO m) => OperandSupport ExecutorPrismRes EFlag Bool m where
    readSourceOp epr reg =
        readOpRaw (eprMemReg epr) reg

-------------------------------------------------------------------------------
