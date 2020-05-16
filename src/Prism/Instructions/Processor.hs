module Prism.Instructions.Processor where

import Prism.Cpu
import Prism.Instruction

-------------------------------------------------------------------------------

clc :: FuncImplicit
clc = setFlag CF False

stc :: FuncImplicit
stc = setFlag CF True

cmc :: FuncImplicit
cmc = modifyFlag not CF

cld :: FuncImplicit
cld = setFlag DF False

std :: FuncImplicit
std = setFlag DF True

cli :: FuncImplicit
cli = setFlag IF False

sti :: FuncImplicit
sti = setFlag IF True

-------------------------------------------------------------------------------

hlt :: FuncImplicit
hlt = halt

wait :: FuncImplicit
wait = return ()

lock :: FuncImplicit
lock = return ()

nop :: FuncImplicit
nop = return ()

-------------------------------------------------------------------------------

--int :: FuncImm1 Imm8
--int ctx val = return $ ctx { ctxInterrupts = addInterruptInt (ctxInterrupts ctx) val }

--into :: FuncImplicit
--into ctx = int ctx 4

--iret :: FuncImplicit
--iret = loadInterruptCtx

-------------------------------------------------------------------------------

segmentOverride :: FuncImplicit -> RegSeg -> FuncImplicit
segmentOverride execInstr regSeg = do
    overrideSegment $ Just regSeg
    execInstr
    overrideSegment Nothing

-------------------------------------------------------------------------------
