{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Prism.Instructions.String where

import Prism.Cpu
import Prism.InstructionM

-------------------------------------------------------------------------------

class (MemSegWrapper b) => StringMemOp k b b1 | k -> b b1 where
    memType :: k -> Disp -> b
    memTypeExp :: k -> RegSeg -> Disp -> b1

class StringOp k where
    advance1 :: k -> Bool -> Disp -> Disp

data StringOp8 = StringOp8
data StringOp16 = StringOp16

instance StringOp StringOp8 where
    advance1 _ df_ val = if df_ then val-1 else val+1

instance StringMemOp StringOp8 MemSeg8 MemSegExp8 where
    memType _ v = wrapMemSeg $ MemDirect v
    memTypeExp _ regSeg v = MemSegExp8 (regSeg, MemDirect v)

instance StringOp StringOp16 where
    advance1 _ df_ val = if df_ then val-2 else val+2

instance StringMemOp StringOp16 MemSeg16 MemSegExp16 where
    memType _ v = wrapMemSeg $ MemDirect v
    memTypeExp _ regSeg v = MemSegExp16 (regSeg, MemDirect v)

-------------------------------------------------------------------------------

movs :: ( CpuMonad m
        , Operand b m v
        , Operand b1 m v
        , StringOp k
        , StringMemOp k b b1) => k -> FuncImplicit m
movs strOp = do
    valDi <- readOp di
    valSi <- readOp si
    valMemSi <- readOp $ memType strOp valSi
    writeOp (memTypeExp strOp es valDi) valMemSi
    df_ <- getFlag DF
    writeOp di (advance1 strOp df_ valDi)
    writeOp si (advance1 strOp df_ valSi)

-------------------------------------------------------------------------------

cmps :: ( CpuMonad m
        , Operand b m v
        , Operand b1 m v
        , StringOp k
        , StringMemOp k b b1) => k -> FuncImplicit m
cmps strOp = do
    valDi <- readOp di
    valSi <- readOp si
    valMemSi <- readOp $ memType strOp valSi
    valMemDi <- readOp $ memType strOp valDi
    df_ <- getFlag DF
    writeOp di (advance1 strOp df_ valDi)
    writeOp si (advance1 strOp df_ valSi)
    let result = valMemSi - valMemDi
    setFlags $ flags result valMemDi valMemSi
    where
        flags after source dest = 
            Flags (calcCFBorrow dest after)
                  (calcPF after)
                  (calcAFBorrow dest after)
                  (calcZF after)
                  (calcSF after)
                  (calcOFSub dest source after)

-------------------------------------------------------------------------------

scas :: ( CpuMonad m
        , Operand b m v
        , Operand b1 m v
        , StringOp k
        , Operand a m v
        , StringMemOp k b b1) => k -> a -> FuncImplicit m
scas strOp reg = do
    valRegA <- readOp reg
    valDi <- readOp di
    valMemDi <- readOp (memTypeExp strOp es valDi)
    df_ <- getFlag DF
    writeOp di (advance1 strOp df_ valDi)
    let result = valRegA - valMemDi
    setFlags $ flags result valMemDi valRegA
    where
        flags after source dest = 
            Flags (calcCFBorrow dest after)
                  (calcPF after)
                  (calcAFBorrow dest after)
                  (calcZF after)
                  (calcSF after)
                  (calcOFSub dest source after)

-------------------------------------------------------------------------------

lods :: ( CpuMonad m
        , Operand b m v
        , Operand b1 m v
        , StringOp k
        , Operand a m v
        , StringMemOp k b b1) => k -> a -> FuncImplicit m
lods strOp reg = do
    valSi <- readOp si
    valMemSi <- readOp $ memType strOp valSi
    df_ <- getFlag DF
    writeOp si (advance1 strOp df_ valSi)
    writeOp reg valMemSi

-------------------------------------------------------------------------------

stos :: ( CpuMonad m
        , Operand b m v
        , Operand b1 m v
        , StringOp k
        , Operand a m v
        , StringMemOp k b b1) => k -> a -> FuncImplicit m
stos strOp reg = do
    valDi <- readOp di
    valRegA <- readOp reg
    writeOp (memTypeExp strOp es valDi) valRegA
    df_ <- getFlag DF
    writeOp di (advance1 strOp df_ valDi)

-------------------------------------------------------------------------------
{-
rep :: (CpuMonad m) => m () -> Bool -> FuncImplicit m
rep execInstr zfOne = do
    cxVal <- readOp cx
    ip_ <- readOp ip
    if cxVal == 0 then doExit
    else do
        writeOp cx (cxVal - 1)
        newCtx <- execInstr ctx
        writeOp ip ip_
        instr <- peekFirstByte (ctxMem ctx) =<< getInstrAddress memReg cs ip
        case instr of
            0xA6 -> processZf newCtx
            0xA7 -> processZf newCtx
            0xAE -> processZf newCtx
            0xAF -> processZf newCtx
            _ -> doNext newCtx
    where
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
-}
-------------------------------------------------------------------------------
