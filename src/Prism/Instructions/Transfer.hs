{-# LANGUAGE FlexibleContexts #-}

module Prism.Instructions.Transfer where

import Prism.Cpu
import Prism.Instruction

-------------------------------------------------------------------------------

mov :: OperandVal v => FuncV2 v
mov source _ = source

{-# SPECIALISE mov :: FuncV2 Uint8 #-}
{-# SPECIALISE mov :: FuncV2 Uint16 #-}

-------------------------------------------------------------------------------

xchg :: (OperandFunc2 a1 a2 v) => FuncO2M a1 a2
xchg op1 op2 = do
    val1 <- readOp op1
    val2 <- readOp op2
    writeOp op1 val2
    writeOp op2 val1

{-# SPECIALISE xchg :: FuncO2M Reg8 Reg8 #-}
{-# SPECIALISE xchg :: FuncO2M Reg16 Reg16 #-}
{-# SPECIALISE xchg :: FuncO2M Reg8 MemSeg8 #-}
{-# SPECIALISE xchg :: FuncO2M MemSeg8 Reg8 #-}
{-# SPECIALISE xchg :: FuncO2M Reg16 MemSeg16 #-}
{-# SPECIALISE xchg :: FuncO2M MemSeg16 Reg16 #-}

-------------------------------------------------------------------------------

pushOp :: (Operand a PrismM Uint16) => FuncO1M a
pushOp = pushP

popOp :: (Operand a PrismM Uint16) => FuncO1M a
popOp = popP

-------------------------------------------------------------------------------

lea16 :: FuncO2M MemSeg16 Reg16
lea16 mem reg =
    getEA mem >>= writeOp reg

-------------------------------------------------------------------------------

{-
lxs16 :: RegSeg -> FuncO2M Mem16 Reg16
lxs16 regSeg1 ctx mem reg = do
    ptr <- getMemOffset memReg regSeg (unwrapMem mem)
    let segVal = fromIntegral $ shiftR ptr 16
        regVal = fromIntegral ptr
    writeOp ctx reg regVal
    writeOp ctx regSeg1 segVal
    return ctx
    where
        memReg = ctxReg ctx
        regSeg = findRegSegData ctx

lds16 = lxs16 ds
les16 = lxs16 es

-------------------------------------------------------------------------------

pushf :: FuncImplicit
pushf ctx = push16 ctx val
    where
        val = flagsToVal (ctxFlags ctx) $ eflagsToVal (ctxEFlags ctx) 0

popf :: FuncImplicit
popf ctx = do
    val <- pop16 ctx
    let flags = valToFlags val
        eflags = valToEFlags val
    return $ ctx { ctxFlags = flags, ctxEFlags = eflags } 

lahf :: FuncImplicit
lahf ctx = do
    writeOp ctx ah val
    return ctx
    where
        val = (fromIntegral $ flagsToVal (ctxFlags ctx) 0) :: Uint8

sahf :: FuncImplicit
sahf ctx = do
    val <- readOp ctx ah
    let flags_ = valToFlags $ (fromIntegral val :: Uint16)
        of_ = flagOF $ ctxFlags ctx
        flags = flags_ { flagOF = of_ }
    return $ ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

xlat :: FuncImplicit
xlat ctx = do
    val <- readOp ctx al
    let disp = fromIntegral val :: Disp
    val1 <- readOp ctx $ Mem8 (MemBx disp)
    writeOp ctx al val1
    return ctx

-------------------------------------------------------------------------------

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
