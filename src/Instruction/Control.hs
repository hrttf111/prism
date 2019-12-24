module Instruction.Control where

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Prism
import PrismDecoder
import PrismCpu

import Instruction.Transfer

-------------------------------------------------------------------------------

jmpShort :: FuncImm1 Imm8
jmpShort ctx val = jmpNear ctx $ signExterndWord val

jmpNear :: FuncImm1 Imm16
jmpNear ctx val = do
    ipVal <- readRegIP memReg
    writeRegIP memReg (ipVal + val)
    return ctx
    where
        memReg = ctxReg ctx

jmpIntra :: FuncImm1 Imm16
jmpIntra ctx ipVal = do
    writeRegIP memReg ipVal
    return ctx
    where
        memReg = ctxReg ctx

jmpInter :: FuncImm2 Imm16
jmpInter ctx ipVal csVal = do
    writeOp ctx cs csVal
    writeRegIP memReg ipVal
    return ctx
    where
        memReg = ctxReg ctx

instrJMem32 :: FuncImm2 Imm16 -> FuncO1M Mem16
instrJMem32 func ctx mem = do
    (val1, val2) <- readMem32 (ctxReg ctx) (ctxMem ctx) seg (unwrapMem mem)
    func ctx val1 val2
    where
        seg = findRegSegData ctx

-------------------------------------------------------------------------------

callNear :: FuncImm1 Imm16
callNear ctx val = do
    ipVal <- readRegIP memReg
    push16 ctx ipVal
    writeRegIP memReg (ipVal + val)
    return ctx
    where
        memReg = ctxReg ctx

callIntra :: FuncImm1 Imm16
callIntra ctx val = do
    ipVal <- readRegIP memReg
    push16 ctx ipVal
    writeRegIP memReg val
    return ctx
    where
        memReg = ctxReg ctx

callInter :: FuncImm2 Imm16
callInter ctx ipVal csVal = do
    ipValOld <- readRegIP memReg
    csValOld <- readOp ctx cs
    push16 ctx csValOld
    push16 ctx ipValOld
    writeOp ctx cs csVal
    writeRegIP memReg ipVal
    return ctx
    where
        memReg = ctxReg ctx

retIntra :: FuncImm1 Imm16
retIntra ctx val = do
    ipVal <- pop16 ctx
    writeRegIP memReg ipVal
    if val /= 0 then do
        spOld <- readOp ctx sp
        let spNew = spOld + val
        writeOp ctx sp spNew
        return ctx
        else return ctx
    where
        memReg = ctxReg ctx

retInter :: FuncImm1 Imm16
retInter ctx val = do
    csVal <- pop16 ctx
    ipVal <- pop16 ctx
    writeRegIP memReg ipVal
    writeOp ctx cs csVal
    if val /= 0 then do
        spOld <- readOp ctx sp
        let spNew = spOld + val
        writeOp ctx sp spNew
        return ctx
        else return ctx
    where
        memReg = ctxReg ctx

-------------------------------------------------------------------------------

cfIsSet = flagCF . ctxFlags
cfIsClear = not . cfIsSet

ofIsSet = flagOF . ctxFlags
ofIsClear = not . ofIsSet

zfIsSet = flagZF . ctxFlags
zfIsClear = not . zfIsSet

sfIsSet = flagSF . ctxFlags
sfIsClear = not . sfIsSet

pfIsSet = flagPF . ctxFlags
pfIsClear = not . pfIsSet

sfXorOf ctx = (/=) (flagSF flags) (flagOF flags)
    where
        flags = ctxFlags ctx

ja ctx val = if (cfIsClear ctx && zfIsClear ctx) then jmpShort ctx val else return ctx
jnbe = ja

jae = jnc
jnb = jnc

jb = jc
jnae = jc

jbe ctx val = if (cfIsSet ctx || zfIsSet ctx) then jmpShort ctx val else return ctx
jna = jbe

jc ctx val = if cfIsSet ctx then jmpShort ctx val else return ctx

je ctx val = if zfIsSet ctx then jmpShort ctx val else return ctx
jz = je

jg ctx val = if not (sfXorOf ctx || zfIsSet ctx) then jmpShort ctx val else return ctx
jnle = jg

jge ctx val = if not $ sfXorOf ctx then jmpShort ctx val else return ctx
jnl = jge

jl ctx val = if sfXorOf ctx then jmpShort ctx val else return ctx
jnge = jl

jle ctx val = if sfXorOf ctx || zfIsSet ctx then jmpShort ctx val else return ctx
jng = jle

jnc ctx val = if cfIsClear ctx then jmpShort ctx val else return ctx

jne ctx val = if zfIsClear ctx then jmpShort ctx val else return ctx
jnz = jne

jno ctx val = if ofIsClear ctx then jmpShort ctx val else return ctx

jnp ctx val = if pfIsClear ctx then jmpShort ctx val else return ctx
jpo = jnp

jns ctx val = if sfIsClear ctx then jmpShort ctx val else return ctx

jo ctx val = if ofIsSet ctx then jmpShort ctx val else return ctx

jp ctx val = if pfIsSet ctx then jmpShort ctx val else return ctx
jpe = jp

js ctx val = if sfIsSet ctx then jmpShort ctx val else return ctx

-------------------------------------------------------------------------------

loop :: FuncImm1 Imm8
loop ctx val = do
    regVal <- readOp ctx cx
    let newRegVal = regVal - 1
    writeOp ctx cx newRegVal
    if newRegVal /= 0 then jmpShort ctx val else return ctx

loopZ :: FuncImm1 Imm8
loopZ ctx val = do
    regVal <- readOp ctx cx
    let newRegVal = regVal - 1
    writeOp ctx cx newRegVal
    if newRegVal /= 0 && zfIsSet ctx then jmpShort ctx val else return ctx

loopE = loopZ

loopNZ :: FuncImm1 Imm8
loopNZ ctx val = do
    regVal <- readOp ctx cx
    let newRegVal = regVal - 1
    writeOp ctx cx newRegVal
    if newRegVal /= 0 && zfIsClear ctx then jmpShort ctx val else return ctx

loopNE = loopNZ

jcxz :: FuncImm1 Imm8
jcxz ctx val = do
    regVal <- readOp ctx cx
    if regVal == 0 then jmpShort ctx val else return ctx

-------------------------------------------------------------------------------

controlInstrList = [
        --JXY
        makeInstructionS 0x70 Nothing (decodeImm8 jo),
        makeInstructionS 0x71 Nothing (decodeImm8 jno),
        makeInstructionS 0x72 Nothing (decodeImm8 jc),
        makeInstructionS 0x73 Nothing (decodeImm8 jnc),
        makeInstructionS 0x74 Nothing (decodeImm8 jz),
        makeInstructionS 0x75 Nothing (decodeImm8 jnz),
        makeInstructionS 0x76 Nothing (decodeImm8 jna),
        makeInstructionS 0x77 Nothing (decodeImm8 ja),
        makeInstructionS 0x78 Nothing (decodeImm8 js),
        makeInstructionS 0x79 Nothing (decodeImm8 jns),
        makeInstructionS 0x7A Nothing (decodeImm8 jp),
        makeInstructionS 0x7B Nothing (decodeImm8 jnp),
        makeInstructionS 0x7C Nothing (decodeImm8 jl),
        makeInstructionS 0x7D Nothing (decodeImm8 jnl),
        makeInstructionS 0x7E Nothing (decodeImm8 jle),
        makeInstructionS 0x7F Nothing (decodeImm8 jnle),
        --LOOP
        makeInstructionS 0xE0 Nothing (decodeImm8 loopNZ),
        makeInstructionS 0xE1 Nothing (decodeImm8 loopZ),
        makeInstructionS 0xE2 Nothing (decodeImm8 loop),
        --JMP
        makeInstructionS 0xE9 Nothing (decodeImm16 jmpNear),
        makeInstructionS 0xEA Nothing (decodeImm32 jmpInter),
        makeInstructionS 0xEB Nothing (decodeImm8 jmpShort),
        makeInstructionS 0xFF (Just 4) (decodeN16 (instrON1 jmpIntra) (instrON1 jmpIntra)),
        makeInstructionS 0xFF (Just 5) (decodeN16 emptySingle (instrJMem32 jmpInter)),
        --CALL
        makeInstructionS 0xE8 Nothing (decodeImm16 callNear),
        makeInstructionS 0x9A Nothing (decodeImm32 callInter),
        makeInstructionS 0xFF (Just 2) (decodeN16 (instrON1 callIntra) (instrON1 callIntra)),
        makeInstructionS 0xFF (Just 3) (decodeN16 emptySingle (instrJMem32 callInter)),
        --RET
        makeInstructionS 0xC2 Nothing (decodeImm16 retIntra),
        makeInstructionS 0xC3 Nothing (decodeImplicit $ flip retIntra 0),
        makeInstructionS 0xCA Nothing (decodeImm16 retInter),
        makeInstructionS 0xCB Nothing (decodeImplicit $ flip retInter 0)
    ]
