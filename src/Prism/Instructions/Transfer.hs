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

{-
portIn8 :: MonadIO m => Ctx -> Uint16 -> m Uint8
portIn8 ctx portNum = do
    let handler = findPortIndex (ioCtxPortRegion $ ctxIO ctx) portNum
    ioPortRead (ctxIO ctx) handler portNum

portIn16 :: MonadIO m => Ctx -> Uint16 -> m Uint16
portIn16 ctx portNum = do
    let handler = findPortIndex (ioCtxPortRegion $ ctxIO ctx) portNum
    ioPortRead (ctxIO ctx) handler portNum

portOut8 :: MonadIO m => Ctx -> Uint16 -> Uint8 -> m ()
portOut8 ctx portNum val = do
    let handler = findPortIndex (ioCtxPortRegion $ ctxIO ctx) portNum
    ioPortWrite (ctxIO ctx) handler portNum val

portOut16 :: MonadIO m => Ctx -> Uint16 -> Uint16 -> m ()
portOut16 ctx portNum val = do
    let handler = findPortIndex (ioCtxPortRegion $ ctxIO ctx) portNum
    ioPortWrite (ctxIO ctx) handler portNum val

portInAlDx :: FuncImplicit
portInAlDx ctx = do
    portNum <- readOp ctx dx
    val <- portIn8 ctx portNum
    writeOp ctx al val
    return ctx

portInAlImm :: FuncImm1 Uint8
portInAlImm ctx portNum = do
    (portIn8 ctx $ fromIntegral portNum) >>= writeOp ctx al
    return ctx

portInAxDx :: FuncImplicit
portInAxDx ctx = do
    portNum <- readOp ctx dx
    val <- portIn16 ctx portNum
    writeOp ctx ax val
    return ctx

portInAxImm :: FuncImm1 Uint8
portInAxImm ctx portNum = do
    val <- portIn16 ctx $ fromIntegral portNum
    writeOp ctx ax val
    return ctx

portOutAlDx :: FuncImplicit
portOutAlDx ctx = do
    portNum <- readOp ctx dx
    val <- readOp ctx al
    portOut8 ctx portNum val
    return ctx

portOutAlImm :: FuncImm1 Uint8
portOutAlImm ctx portNum = do
    val <- readOp ctx al
    portOut8 ctx (fromIntegral portNum) val
    return ctx

portOutAxDx :: FuncImplicit
portOutAxDx ctx = do
    portNum <- readOp ctx dx
    val <- readOp ctx ax
    portOut16 ctx portNum val
    return ctx

portOutAxImm :: FuncImm1 Uint8
portOutAxImm ctx portNum = do
    val <- readOp ctx ax
    portOut16 ctx (fromIntegral portNum) val
    return ctx
-}
-------------------------------------------------------------------------------
