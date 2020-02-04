{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Instruction.String where

import Control.Monad.Trans (liftIO, MonadIO)

import Prism
import PrismCpu
import PrismDecoder
import PrismRun

-------------------------------------------------------------------------------

class (OperandVal b, OperandMem a b) => StringOp k a b | k -> a where
    advance1 :: k -> Ctx -> Disp -> Disp
    memType :: k -> Disp -> a

data StringOp8 = StringOp8
data StringOp16 = StringOp16

instance StringOp StringOp8 Mem8 Uint8 where
    advance1 _ ctx val = if (eflagDF . ctxEFlags $ ctx) then val-1 else val+1
    memType _ k = Mem8 $ MemDirect k

instance StringOp StringOp16 Mem16 Uint16 where
    advance1 _ ctx val = if (eflagDF . ctxEFlags $ ctx) then val-2 else val+2
    memType _ k = Mem16 $ MemDirect k

-------------------------------------------------------------------------------

movs :: (StringOp k a b) => k -> FuncImplicit
movs strOp ctx = do
    valDi <- readOp ctx di
    valSi <- readOp ctx si
    valMemSi <- readOp ctx $ memType strOp valSi
    writeMemOp ctx es (memType strOp valDi) valMemSi
    writeOp ctx di (advance1 strOp ctx valDi)
    writeOp ctx si (advance1 strOp ctx valSi)
    return ctx

-------------------------------------------------------------------------------

cmps :: (StringOp k a b) => k -> FuncImplicit
cmps strOp ctx = do
    valDi <- readOp ctx di
    valSi <- readOp ctx si
    valMemSi <- readOp ctx $ memType strOp valSi
    valMemDi <- readMemOp ctx es $ memType strOp valDi
    writeOp ctx di (advance1 strOp ctx valDi)
    writeOp ctx si (advance1 strOp ctx valSi)
    let result = valMemSi - valMemDi
    return $ ctx { ctxFlags = (flags result valMemDi valMemSi) }
    where
        flags after source dest = 
            Flags (calcCFBorrow dest after) (calcPF after) (calcAFBorrow dest after) (calcZF after) (calcSF after) (calcOFSub dest source after)

-------------------------------------------------------------------------------

scas :: (OperandVal b, StringOp k a b, OperandReg a1 b) => k -> a1 -> FuncImplicit
scas strOp reg ctx = do
    valRegA <- readOp ctx reg
    valDi <- readOp ctx di
    valMemDi <- readMemOp ctx es $ memType strOp valDi
    writeOp ctx di (advance1 strOp ctx valDi)
    let result = valRegA - valMemDi
    return $ ctx { ctxFlags = (flags result valMemDi valRegA) }
    where
        flags after source dest = 
            Flags (calcCFBorrow dest after) (calcPF after) (calcAFBorrow dest after) (calcZF after) (calcSF after) (calcOFSub dest source after)

-------------------------------------------------------------------------------

lods :: (OperandVal b, StringOp k a b, OperandReg a1 b) => k -> a1 -> FuncImplicit
lods strOp reg ctx = do
    valSi <- readOp ctx si
    valMemSi <- readOp ctx $ memType strOp valSi
    writeOp ctx si (advance1 strOp ctx valSi)
    writeOp ctx reg valMemSi
    return ctx

-------------------------------------------------------------------------------

stos :: (OperandVal b, StringOp k a b, OperandReg a1 b) => k -> a1 -> FuncImplicit
stos strOp reg ctx = do
    valDi <- readOp ctx di
    valRegA <- readOp ctx reg
    writeMemOp ctx es (memType strOp valDi) valRegA
    writeOp ctx di (advance1 strOp ctx valDi)
    return ctx

-------------------------------------------------------------------------------

rep :: (Ctx -> PrismM) -> Bool -> FuncImplicit
rep execInstr zfOne ctx = do
    cxVal <- readOp ctx cx
    ip <- readRegIP memReg
    if cxVal == 0 then doExit ctx
    else do
        writeOp ctx cx (cxVal - 1)
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
        makeInstructionS 0xA4 Nothing (decodeImplicit $ movs StringOp8),
        makeInstructionS 0xA5 Nothing (decodeImplicit $ movs StringOp16),
        makeInstructionS 0xA6 Nothing (decodeImplicit $ cmps StringOp8),
        makeInstructionS 0xA7 Nothing (decodeImplicit $ cmps StringOp16),
        makeInstructionS 0xAA Nothing (decodeImplicit $ stos StringOp8 al),
        makeInstructionS 0xAB Nothing (decodeImplicit $ stos StringOp16 ax),
        makeInstructionS 0xAC Nothing (decodeImplicit $ lods StringOp8 al),
        makeInstructionS 0xAD Nothing (decodeImplicit $ lods StringOp16 ax),
        makeInstructionS 0xAE Nothing (decodeImplicit $ scas StringOp8 al),
        makeInstructionS 0xAF Nothing (decodeImplicit $ scas StringOp16 ax)
    ]
