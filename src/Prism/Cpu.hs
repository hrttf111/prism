module Prism.Cpu (
        ------------------------------------------------------
          Imm8, Imm16
        , Uint8, Uint16, Uint32
        , EA, Disp, MemOffset, MemSegType
        ------------------------------------------------------
        , OperandVal, Operand (..)
        , OperandReg, OperandMem
        , MemRegManipulator (..)
        , MemAddress (..)
        , MemDecoder (..), RegDecoder (..)
        ------------------------------------------------------
        , CpuFlag (..), CpuFlags (..)
        , Reg8 (..), Reg16 (..), RegSeg (..)
        , Flag (..), Flags (..)
        , EFlag (..), EFlags (..)
        , PrismInt (..), PrismIRQ (..)
        , CpuDebug (..)
        , CpuMonad (..)
        ------------------------------------------------------
        , al, cl, dl, bl, ah, ch, dh, bh
        , ax, cx, dx, bx, sp, bp, si, di
        , cs, ds, es, ss
        , ip, flagsInternal
        , printRegs
        ------------------------------------------------------
        , MemReg (..), MemMain (..), Ctx (..)
        , MemSeg8 (..), MemSeg16 (..), MemPhy8 (..), MemPhy16 (..)
        , MemSeg (..)
        , PrismM
        ------------------------------------------------------
        , allocMemRegRaw, allocMemReg, allocMemMain
        , clearRegs, copyMainMem 
        ------------------------------------------------------
        , makeCtx, makePrismM
        , runPrismM
        ------------------------------------------------------
        , calcCFCarry, calcCFBorrow
        , calcAFCarry, calcAFBorrow
        , calcPF, calcZF, calcSF
        , calcOFAdd, calcOFSub
        , flagsToVal, eflagsToVal
        , valToFlags, valToEFlags
        , regToFlags
        , readFlags
        ------------------------------------------------------
        , negV
        , signExtendWordN
        , signExtendWord
        , signExtendDoubleword, signExtendDoubleword32
        , toSignedCompl2, toUnsignedComp2
        , signedOp, signedOp1, signedOpS
        ------------------------------------------------------
        , pushP, popP, pushV, popV
        , modifyFlag
        ------------------------------------------------------
    ) where

import Prism.Cpu.Val
import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Flags
import Prism.Cpu.Trans
import Prism.Cpu.Memory
import Prism.Cpu.Registers
import Prism.Cpu.Primitives

type PrismM = CpuTrans

runPrismM :: Ctx -> PrismM () -> IO Ctx
runPrismM ctx c = runCpu ctx c

makePrismM :: Ctx -> PrismM ()
makePrismM = makeTransM

-------------------------------------------------------------------------------
