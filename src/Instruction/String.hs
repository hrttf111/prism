module Instruction.String where

import Control.Monad.Trans (liftIO, MonadIO)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

movs8 :: Ctx -> PrismM
movs8 ctx = do
    valDi <- readReg16 memReg di
    valSi <- readReg16 memReg si
    valMemSi <- readMem8 memReg memMain regSeg (MemDirect valSi)
    writeMem8 memReg memMain (Just es) (MemDirect valDi) valMemSi
    writeReg16 memReg di (advance valDi)
    writeReg16 memReg si (advance valSi)
    return ctx
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-1 else val+1
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx

movs16 :: Ctx -> PrismM
movs16 ctx = do
    valDi <- readReg16 memReg di
    valSi <- readReg16 memReg si
    valMemSi <- readMem16 memReg memMain regSeg (MemDirect valSi)
    writeMem16 memReg memMain (Just es) (MemDirect valDi) valMemSi
    writeReg16 memReg di (advance valDi)
    writeReg16 memReg si (advance valSi)
    return ctx
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-2 else val+2
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx

-------------------------------------------------------------------------------

stringInstrList = [
        makeInstructionS 0xA4 Nothing (decodeImplicit movs8),
        makeInstructionS 0xA5 Nothing (decodeImplicit movs16)
    ]
