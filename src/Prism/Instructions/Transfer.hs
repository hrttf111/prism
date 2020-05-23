{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prism.Instructions.Transfer where

import Data.Bits (shiftR)

import Prism.Cpu
import Prism.InstructionM

-------------------------------------------------------------------------------

mov :: (OperandVal v) => FuncV2 v
mov source _ = source

{-# SPECIALISE mov :: FuncV2 Uint8 #-}
{-# SPECIALISE mov :: FuncV2 Uint16 #-}

-------------------------------------------------------------------------------

xchg :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncO2M a1 a2 m
xchg op1 op2 = do
    val1 <- readOp op1
    val2 <- readOp op2
    writeOp op1 val2
    writeOp op2 val1

{-# SPECIALISE INLINE xchg :: FuncO2M Reg8 Reg8 PrismM #-}
{-# SPECIALISE INLINE xchg :: FuncO2M Reg16 Reg16 PrismM #-}
{-# SPECIALISE INLINE xchg :: FuncO2M Reg8 MemSeg8 PrismM #-}
{-# SPECIALISE INLINE xchg :: FuncO2M MemSeg8 Reg8 PrismM #-}
{-# SPECIALISE INLINE xchg :: FuncO2M Reg16 MemSeg16 PrismM #-}
{-# SPECIALISE INLINE xchg :: FuncO2M MemSeg16 Reg16 PrismM #-}

-------------------------------------------------------------------------------

pushOp :: (CpuMonad m, Operand a m Uint16) => FuncO1M a m
pushOp = pushP

{-# SPECIALISE INLINE pushOp :: FuncO1M Reg16 PrismM #-}
{-# SPECIALISE INLINE pushOp :: FuncO1M MemSeg16 PrismM #-}

popOp :: (CpuMonad m, Operand a m Uint16) => FuncO1M a m
popOp = popP

{-# SPECIALISE INLINE popOp :: FuncO1M Reg16 PrismM #-}
{-# SPECIALISE INLINE popOp :: FuncO1M MemSeg16 PrismM #-}

-------------------------------------------------------------------------------

lea16 :: (CpuMonad m) => FuncO2M MemSeg16 Reg16 m
lea16 mem reg =
    getEA mem >>= writeOp reg

{-# SPECIALISE INLINE lea16 :: FuncO2M MemSeg16 Reg16 PrismM #-}

-------------------------------------------------------------------------------

lxs16 :: (CpuMonad m) => RegSeg -> FuncO2M MemSeg16 Reg16 m
lxs16 regSeg mem reg = do
    offset <- getPA mem
    let segVal = fromIntegral $ shiftR offset 16
        regVal = fromIntegral offset
    writeOp reg regVal
    writeOp regSeg segVal

{-# SPECIALISE INLINE lxs16 :: RegSeg -> FuncO2M MemSeg16 Reg16 PrismM #-}

lds16 :: (CpuMonad m) => FuncO2M MemSeg16 Reg16 m
lds16 = lxs16 ds

{-# SPECIALISE INLINE lds16 :: FuncO2M MemSeg16 Reg16 PrismM #-}

les16 :: (CpuMonad m) => FuncO2M MemSeg16 Reg16 m
les16 = lxs16 es

{-# SPECIALISE INLINE les16 :: FuncO2M MemSeg16 Reg16 PrismM #-}

-------------------------------------------------------------------------------

pushf :: (CpuMonad m) => FuncImplicit m
pushf = do
    (flags :: Flags) <- getFlags
    (eflags :: EFlags) <- getFlags
    pushV $ flagsToVal flags $ eflagsToVal eflags 0

{-# SPECIALISE INLINE pushf :: FuncImplicit PrismM #-}

popf :: (CpuMonad m) => FuncImplicit m
popf = do
    val <- popV
    let flags = valToFlags val
        eflags = valToEFlags val
    setFlags flags
    setFlags eflags

{-# SPECIALISE INLINE popf :: FuncImplicit PrismM #-}

lahf :: (CpuMonad m) => FuncImplicit m
lahf = do
    (flags :: Flags) <- getFlags
    let val = (fromIntegral $ flagsToVal flags 0) :: Uint8
    writeOp ah val

{-# SPECIALISE INLINE lahf :: FuncImplicit PrismM #-}

sahf :: (CpuMonad m) => FuncImplicit m
sahf = do
    of_ <- getFlag OF
    val <- readOp ah
    let flags_ = valToFlags $ (fromIntegral val :: Uint16)
        flags = flags_ { flagOF = of_ }
    setFlags flags

{-# SPECIALISE INLINE sahf :: FuncImplicit PrismM #-}

-------------------------------------------------------------------------------

xlat :: (CpuMonad m) => FuncImplicit m
xlat = do
    val <- readOp al
    let disp = fromIntegral val :: Disp
    val1 <- readOp $ MemSeg8 (MemBx disp)
    writeOp al val1

{-# SPECIALISE INLINE xlat :: FuncImplicit PrismM #-}

-------------------------------------------------------------------------------

portInAlDx :: (CpuMonad m) => FuncImplicit m
portInAlDx = do
    portNum <- readOp dx
    val <- readOp $ Port8 portNum
    writeOp al val

portInAlImm :: (CpuMonad m) => FuncImm1 Uint8 m
portInAlImm portNum =
    readOp (Port8 $ fromIntegral portNum) >>= writeOp al

portInAxDx :: (CpuMonad m) => FuncImplicit m
portInAxDx = do
    portNum <- readOp dx
    val <- readOp $ Port16 portNum
    writeOp ax val

portInAxImm :: (CpuMonad m) => FuncImm1 Uint8 m
portInAxImm portNum =
    (readOp $ Port16 $ fromIntegral portNum) >>= writeOp ax

portOutAlDx :: (CpuMonad m) => FuncImplicit m
portOutAlDx = do
    portNum <- readOp dx
    val <- readOp al
    writeOp (Port8 portNum) val

portOutAlImm :: (CpuMonad m) => FuncImm1 Uint8 m
portOutAlImm portNum =
    readOp al >>= writeOp (Port8 $ fromIntegral portNum)

portOutAxDx :: (CpuMonad m) => FuncImplicit m
portOutAxDx = do
    portNum <- readOp dx
    val <- readOp ax
    writeOp (Port16 portNum) val

portOutAxImm :: (CpuMonad m) => FuncImm1 Uint8 m
portOutAxImm portNum =
    readOp ax >>= writeOp (Port16 $ fromIntegral portNum)

-------------------------------------------------------------------------------
