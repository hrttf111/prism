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
    liftIO $ putStrLn $ " val = " ++ (show valMemSi)
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

cmps8 :: Ctx -> PrismM
cmps8 ctx = do
    valDi <- readReg16 memReg di
    valSi <- readReg16 memReg si
    valMemSi <- readMem8 memReg memMain regSeg (MemDirect valSi)
    valMemDi <- readMem8 memReg memMain (Just es) (MemDirect valDi)
    writeReg16 memReg di (advance valDi)
    writeReg16 memReg si (advance valSi)
    let result = valMemSi - valMemDi
    return $ ctx { ctxFlags = (flags result valMemDi valMemSi) }
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-1 else val+1
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx
        flags after source dest = 
            Flags (calcCFBorrow8 dest after) (calcPF8 after) (calcAFBorrow8 dest after) (calcZF8 after) (calcSF8 after) (calcOFSub8 dest source after)

cmps16 :: Ctx -> PrismM
cmps16 ctx = do
    valDi <- readReg16 memReg di
    valSi <- readReg16 memReg si
    valMemSi <- readMem16 memReg memMain regSeg (MemDirect valSi)
    valMemDi <- readMem16 memReg memMain (Just es) (MemDirect valDi)
    writeReg16 memReg di (advance valDi)
    writeReg16 memReg si (advance valSi)
    let result = valMemSi - valMemDi
    return $ ctx { ctxFlags = (flags result valMemDi valMemSi) }
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-2 else val+2
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx
        flags after source dest = 
            Flags (calcCFBorrow16 dest after) (calcPF16 after) (calcAFBorrow16 dest after) (calcZF16 after) (calcSF16 after) (calcOFSub16 dest source after)

-------------------------------------------------------------------------------

scas8 :: Ctx -> PrismM
scas8 ctx = do
    valAl <- readReg8 memReg al
    valDi <- readReg16 memReg di
    valMemDi <- readMem8 memReg memMain (Just es) (MemDirect valDi)
    writeReg16 memReg di (advance valDi)
    let result = valAl - valMemDi
    return $ ctx { ctxFlags = (flags result valMemDi valAl) }
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-1 else val+1
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx
        flags after source dest = 
            Flags (calcCFBorrow8 dest after) (calcPF8 after) (calcAFBorrow8 dest after) (calcZF8 after) (calcSF8 after) (calcOFSub8 dest source after)

scas16 :: Ctx -> PrismM
scas16 ctx = do
    valAx <- readReg16 memReg ax
    valDi <- readReg16 memReg di
    valMemDi <- readMem16 memReg memMain (Just es) (MemDirect valDi)
    writeReg16 memReg di (advance valDi)
    let result = valAx - valMemDi
    return $ ctx { ctxFlags = (flags result valMemDi valAx) }
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-2 else val+2
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx
        flags after source dest = 
            Flags (calcCFBorrow16 dest after) (calcPF16 after) (calcAFBorrow16 dest after) (calcZF16 after) (calcSF16 after) (calcOFSub16 dest source after)

-------------------------------------------------------------------------------

lods8 :: Ctx -> PrismM
lods8 ctx = do
    valSi <- readReg16 memReg si
    valMemSi <- readMem8 memReg memMain regSeg (MemDirect valSi)
    writeReg16 memReg si (advance valSi)
    writeReg8 memReg al valMemSi
    return ctx
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-1 else val+1
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx

lods16 :: Ctx -> PrismM
lods16 ctx = do
    valSi <- readReg16 memReg si
    valMemSi <- readMem16 memReg memMain regSeg (MemDirect valSi)
    writeReg16 memReg si (advance valSi)
    writeReg16 memReg ax valMemSi
    return ctx
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-2 else val+2
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = ctxReplaceSeg ctx

-------------------------------------------------------------------------------

stos8 :: Ctx -> PrismM
stos8 ctx = do
    valDi <- readReg16 memReg di
    valAl <- readReg8 memReg al
    writeMem8 memReg memMain (Just es) (MemDirect valDi) valAl
    writeReg16 memReg di (advance valDi)
    return ctx
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-1 else val+1
        memReg = ctxReg ctx
        memMain = ctxMem ctx

stos16 :: Ctx -> PrismM
stos16 ctx = do
    valDi <- readReg16 memReg di
    valAx <- readReg16 memReg ax
    writeMem16 memReg memMain (Just es) (MemDirect valDi) valAx
    writeReg16 memReg di (advance valDi)
    return ctx
    where
        df = (eflagDF . ctxEFlags $ ctx)
        advance val = if df then val-2 else val+2
        memReg = ctxReg ctx
        memMain = ctxMem ctx

-------------------------------------------------------------------------------

rep :: (Ctx -> PrismM) -> Bool -> Ctx -> PrismM
rep execInstr zfOne ctx = do
    cxVal <- readReg16 memReg cx
    ip <- readRegIP memReg
    if cxVal == 0 then doExit ctx
    else do
        writeReg16 memReg cx (cxVal - 1)
        newCtx <- execInstr ctx
        writeRegIP memReg ip
        instr <- peekFirstByte (ctxMem ctx) =<< getInstrAddress memReg cs ip
        case instr of
            0xA6 -> processZf newCtx
            0xA7 -> processZf newCtx
            0xAE -> processZf newCtx
            0xAF -> processZf newCtx
            _ -> doNext newCtx
    where
        memReg = ctxReg ctx
        doExit = updateIP 1
        doNext = rep execInstr zfOne
        processZf newCtx =
            if (flagZF . ctxFlags $ newCtx) then
                if zfOne then doNext newCtx
                    else doExit newCtx
            else
                if zfOne then doExit newCtx
                    else doNext newCtx

getRepInstrList :: (Ctx -> PrismM) -> [PrismInstruction]
getRepInstrList execInstr = [
        makeInstructionS 0xF2 Nothing (decodeImplicit $ rep execInstr False),
        makeInstructionS 0xF3 Nothing (decodeImplicit $ rep execInstr True)
    ]

repInstrList :: [PrismInstruction] -> [PrismInstruction]
repInstrList = getRepInstrList . decodeExecOne . makeDecoderList

-------------------------------------------------------------------------------

stringInstrList = [
        makeInstructionS 0xA4 Nothing (decodeImplicit movs8),
        makeInstructionS 0xA5 Nothing (decodeImplicit movs16),
        makeInstructionS 0xA6 Nothing (decodeImplicit cmps8),
        makeInstructionS 0xA7 Nothing (decodeImplicit cmps16),
        makeInstructionS 0xAA Nothing (decodeImplicit stos8),
        makeInstructionS 0xAB Nothing (decodeImplicit stos16),
        makeInstructionS 0xAC Nothing (decodeImplicit lods8),
        makeInstructionS 0xAD Nothing (decodeImplicit lods16),
        makeInstructionS 0xAE Nothing (decodeImplicit scas8),
        makeInstructionS 0xAF Nothing (decodeImplicit scas16)
    ]
