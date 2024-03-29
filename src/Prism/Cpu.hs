module Prism.Cpu (
        ------------------------------------------------------
          Imm8, Imm16
        , Uint8, Uint16, Uint32, Uint64
        , EA, Disp, MemOffset, MemSegType
        , CpuCycles (..), CpuCyclesDelta (..)
        , maxCpuCyclesDelta, calcCpuCyclesDelta
        , addCpuCyclesDelta
        ------------------------------------------------------
        , OperandVal, Operand (..)
        , OperandReg, OperandMem
        , MemArithmetics (..)
        , MemSegWrapper (..)
        , MemOpManipulator (..)
        , MemAddress (..)
        , MemDecoder (..), RegDecoder (..)
        , Port8 (..), Port16 (..)
        ------------------------------------------------------
        , CpuFlag (..), CpuFlags (..)
        , Reg8 (..), Reg16 (..), RegSeg (..)
        , Flag (..), Flags (..)
        , EFlag (..), EFlags (..)
        , PrismInt (..), PrismIRQ (..)
        , CpuDebug (..), CpuDebugM (..)
        , InterruptDispatcher (..)
        , InterruptRun (..)
        , CpuMonad (..)
        , DirectCommand (..)
        , RunPeripheralsDirect (..)
        ------------------------------------------------------
        , IOHandlerIndex, IOPageIndex, IOPageOffset
        , MemLocation (..), IOPage (..)
        , PortIORegion (..), MemIORegion (..)
        , PortInternal8 (..), PortInternal16 (..)
        , MMIOInternal8 (..), MMIOInternal16 (..)
        , PeripheralsMonad (..)
        , RunPeripheralsM (..)
        , emptyPage, emptyHandler
        , findMemIndex, findPortIndex
        , IOCtx (..), DebugCtx(..)
        , PrismInterrupts (..)
        ------------------------------------------------------
        , al, cl, dl, bl, ah, ch, dh, bh
        , ax, cx, dx, bx, sp, bp, si, di
        , cs, ds, es, ss
        , ip, flagsInternal
        , printRegs
        ------------------------------------------------------
        , MemReg (..), MemMain (..), Ctx (..)
        , MemSeg8 (..), MemSeg16 (..)
        , MemSegExp8 (..), MemSegExp16 (..)
        , MemPhy8 (..), MemPhy16 (..)
        , MemPhyDir8 (..), MemPhyDir16(..), MemPhyDir32(..)
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
        , clearFlags 
        , valToFlags, valToEFlags
        , regToFlags
        , readFlags
        , getFlagVal, getEFlagVal
        , showFlags, showEFlags, printFlags
        ------------------------------------------------------
        , negV
        , signExtendWordN
        , signExtendWord
        , signExtendDoubleword, signExtendDoubleword32
        , toSignedCompl2, toUnsignedComp2
        , signedOp, signedOp1, signedOpS
        , bcdToHex8, hexToBcd8, bcdToHex16, hexToBcd16
        ------------------------------------------------------
        , pushP, popP, pushV, popV
        , modifyFlag
        ------------------------------------------------------
        , Trace (..), Debug (..), Info (..), Warning (..), Error (..)
        , LogFeature (..)
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
import Prism.Cpu.Peripherals

type PrismM = CpuTrans

runPrismM :: Ctx -> PrismM () -> IO Ctx
runPrismM ctx c = runCpu ctx c

makePrismM :: Ctx -> PrismM ()
makePrismM = makeTransM

-------------------------------------------------------------------------------
