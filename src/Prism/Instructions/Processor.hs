module Prism.Instructions.Processor where

import Control.Monad (when)

import Numeric

import Prism.Cpu
import Prism.InstructionM
import qualified Prism.Log as Log

-------------------------------------------------------------------------------

clc :: (CpuMonad m) => FuncImplicit m
clc = setFlag CF False

stc :: (CpuMonad m) => FuncImplicit m
stc = setFlag CF True

cmc :: (CpuMonad m) => FuncImplicit m
cmc = modifyFlag not CF

cld :: (CpuMonad m) => FuncImplicit m
cld = setFlag DF False

std :: (CpuMonad m) => FuncImplicit m
std = setFlag DF True

cli :: (CpuMonad m) => FuncImplicit m
cli = setFlag IF False

sti :: (CpuMonad m) => FuncImplicit m
sti = setFlag IF True

-------------------------------------------------------------------------------

hlt :: (CpuMonad m) => FuncImplicit m
hlt = cpuHalt

wait :: (CpuMonad m) => FuncImplicit m
wait = return ()

lock :: (CpuMonad m) => FuncImplicit m
lock = return ()

nop :: (CpuMonad m) => FuncImplicit m
nop = return ()

-------------------------------------------------------------------------------

int :: (CpuMonad m) => FuncImm1 Imm8 m
int i = do
    Log.traceInterrupt i
    when (i == 0x24) $ do
        cpuLog Error Log.commonF "  !!!! Int = 0x24"
    raiseInterrupt . PrismInt $ i

into :: (CpuMonad m) => FuncImplicit m
into = int 4

iret :: (CpuMonad m) => FuncImplicit m
iret = do
    Log.traceRetInterrupt
    retInterrupt

-------------------------------------------------------------------------------

segmentOverride :: (CpuMonad m) => FuncImplicit m -> RegSeg -> FuncImplicit m
segmentOverride execInstr regSeg = do
    cpuOverrideSegment $ Just regSeg
    execInstr
    cpuOverrideSegment Nothing

-------------------------------------------------------------------------------
