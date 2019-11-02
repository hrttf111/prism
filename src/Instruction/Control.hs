module Instruction.Control where

import Control.Monad.Trans (lift, liftIO, MonadIO)
--import Data.Bits ((.&.), (.|.), xor)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

jmpShort :: Ctx -> Imm8 -> PrismM
jmpShort ctx val = jmpNear ctx $ signExterndWord val

jmpNear :: Ctx -> Imm16 -> PrismM
jmpNear ctx val = do
    ipVal <- readRegIP memReg
    writeRegIP memReg (ipVal + val)
    return ctx
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

loop :: Ctx -> Imm8 -> PrismM
loop ctx val = do
    regVal <- readReg16 memReg cx
    let newRegVal = regVal - 1
    writeReg16 memReg cx newRegVal
    if newRegVal /= 0 then jmpShort ctx val else return ctx
    where
        memReg = ctxReg ctx

loopZ :: Ctx -> Imm8 -> PrismM
loopZ ctx val = do
    regVal <- readReg16 memReg cx
    let newRegVal = regVal - 1
    writeReg16 memReg cx newRegVal
    if newRegVal /= 0 && zfIsSet ctx then jmpShort ctx val else return ctx
    where
        memReg = ctxReg ctx
loopE = loopZ

loopNZ :: Ctx -> Imm8 -> PrismM
loopNZ ctx val = do
    regVal <- readReg16 memReg cx
    let newRegVal = regVal - 1
    writeReg16 memReg cx newRegVal
    if newRegVal /= 0 && zfIsClear ctx then jmpShort ctx val else return ctx
    where
        memReg = ctxReg ctx
loopNE = loopNZ

jcxz :: Ctx -> Imm8 -> PrismM
jcxz ctx val = do
    regVal <- readReg16 memReg cx
    if regVal == 0 then jmpShort ctx val else return ctx
    where
        memReg = ctxReg ctx

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
        --inter
        makeInstructionS 0xEB Nothing (decodeImm8 jmpShort)
    ]
