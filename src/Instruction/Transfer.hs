module Instruction.Transfer where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

mov :: OperandVal b => FuncV2 b
mov ctx source _ = (ctx, source)

{-# SPECIALISE mov :: FuncV2 Uint8 #-}
{-# SPECIALISE mov :: FuncV2 Uint16 #-}

-------------------------------------------------------------------------------

xchg :: OperandFunc2 a1 a2 b => FuncO2M a1 a2
xchg ctx op1 op2 = do
    val1 <- readOp ctx op1
    val2 <- readOp ctx op2
    writeOp ctx op1 val2
    writeOp ctx op2 val1
    return ctx

{-# SPECIALISE xchg :: FuncO2M Reg8 Reg8 #-}
{-# SPECIALISE xchg :: FuncO2M Reg16 Reg16 #-}
{-# SPECIALISE xchg :: FuncO2M Reg8 Mem8 #-}
{-# SPECIALISE xchg :: FuncO2M Mem8 Reg8 #-}
{-# SPECIALISE xchg :: FuncO2M Reg16 Mem16 #-}
{-# SPECIALISE xchg :: FuncO2M Mem16 Reg16 #-}

-------------------------------------------------------------------------------

push16 :: Ctx -> Uint16 -> PrismM
push16 ctx val = do
    valSp <- readReg16 memReg sp
    let valNewSp = valSp - 2
    writeReg16 memReg sp valNewSp
    writeMemSp16 memReg memMain val
    return ctx
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx

pop16 :: MonadIO m => Ctx -> m Uint16
pop16 ctx = do
    val <- readMemSp16 memReg memMain
    valSp <- readReg16 memReg sp
    let valNewSp = valSp + 2
    writeReg16 memReg sp valNewSp
    return val 
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx

--pushOp :: Operand a Uint16 => Ctx -> a -> PrismM
--pushOp ctx op =
    --readOp ctx op >>= push16

pushReg16 :: Ctx -> Reg16 -> PrismM
pushReg16 ctx reg = do
    val <- readOp ctx reg
    push16 ctx val

popReg16 :: Ctx -> Reg16 -> PrismM
popReg16 ctx reg = do
    val <- pop16 ctx
    writeOp ctx reg val
    return ctx

pushMem :: Ctx -> Mem -> PrismM
pushMem ctx mem = do
    val <- readOp ctx $ Mem16 mem
    push16 ctx val

popMem :: Ctx -> Mem -> PrismM
popMem ctx mem = do
    val <- pop16 ctx
    writeOp ctx (Mem16 mem) val
    return ctx

pushSeg :: RegSeg -> Ctx -> PrismM
pushSeg regSeg ctx = do
    val <- readOp ctx regSeg
    push16 ctx val

popSeg :: RegSeg -> Ctx -> PrismM
popSeg regSeg ctx = do
    val <- pop16 ctx
    writeOp ctx regSeg val
    return ctx

-------------------------------------------------------------------------------

lea16 :: FuncMemReg16
lea16 ctx mem reg = do
    offset <- getEA (ctxReg ctx) mem
    writeOp ctx reg offset
    return ctx

-------------------------------------------------------------------------------

lxs16 :: RegSeg -> FuncMemReg16
lxs16 regSeg1 ctx mem reg = do
    ptr <- getMemOffset memReg regSeg mem
    let segVal = fromIntegral $ shiftR ptr 16
        regVal = fromIntegral ptr
    writeReg16 memReg reg regVal
    writeSeg memReg regSeg1 segVal
    return ctx
    where
        memReg = ctxReg ctx
        regSeg = findRegSegData ctx

lds16 :: FuncMemReg16
lds16 = lxs16 ds

les16 :: FuncMemReg16
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
portIn8 ctx portNum =
    --todo
    return 0

portIn16 :: MonadIO m => Ctx -> Uint16 -> m Uint16
portIn16 ctx portNum =
    --todo
    return 0

portOut8 :: MonadIO m => Ctx -> Uint16 -> Uint8 -> m ()
portOut8 ctx portNum val =
    --todo
    return ()

portOut16 :: MonadIO m => Ctx -> Uint16 -> Uint16 -> m ()
portOut16 ctx portNum val =
    --todo
    return ()

portInAlDx :: FuncImplicit
portInAlDx ctx = do
    portNum <- readOp ctx dx
    val <- portIn8 ctx portNum
    writeOp ctx al val
    return ctx

portInAlImm :: FuncImm8
portInAlImm ctx portNum = do
    (portIn8 ctx $ fromIntegral portNum) >>= writeOp ctx al
    return ctx

portInAxDx :: FuncImplicit
portInAxDx ctx = do
    portNum <- readOp ctx dx
    val <- portIn16 ctx portNum
    writeOp ctx ax val
    return ctx

portInAxImm :: FuncImm8
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

portOutAlImm :: FuncImm8
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

portOutAxImm :: FuncImm8
portOutAxImm ctx portNum = do
    val <- readOp ctx ax
    portOut16 ctx (fromIntegral portNum) val
    return ctx

-------------------------------------------------------------------------------

transferInstrList = [
        --MOV
        makeInstructionS 0x88 Nothing (decodeRM8 (instrRegToRm mov) (instrRegToRm mov)),
        makeInstructionS 0x89 Nothing (decodeRM16 (instrRegToRm mov) (instrRegToRm mov)),
        makeInstructionS 0x8A Nothing (decodeRM8 (instrRmToReg mov) (instrRmToReg mov)),
        makeInstructionS 0x8B Nothing (decodeRM16 (instrRmToReg mov) (instrRmToReg mov)),
        makeInstructionS 0x8C Nothing (decodeRMS16 (instrRegToRm mov) (instrRegToRm mov)),
        makeInstructionS 0x8E Nothing (decodeRMS16 (instrRmToReg mov) (instrRmToReg mov)),
        makeInstructionS 0xA0 Nothing (decodeAccMem8 al $ instrMemToReg8 mov),
        makeInstructionS 0xA1 Nothing (decodeAccMem16 ax $ instrMemToReg16 mov),
        makeInstructionS 0xA2 Nothing (decodeAccMem8 al $ instrRegToMem8 mov),
        makeInstructionS 0xA3 Nothing (decodeAccMem16 ax $ instrRegToMem16 mov),
        makeInstructionS 0xB0 Nothing (decodeAcc al $ instrOI1 mov),
        makeInstructionS 0xB1 Nothing (decodeAcc cl $ instrOI1 mov),
        makeInstructionS 0xB2 Nothing (decodeAcc dl $ instrOI1 mov),
        makeInstructionS 0xB3 Nothing (decodeAcc bl $ instrOI1 mov),
        makeInstructionS 0xB4 Nothing (decodeAcc ah $ instrOI1 mov),
        makeInstructionS 0xB5 Nothing (decodeAcc ch $ instrOI1 mov),
        makeInstructionS 0xB6 Nothing (decodeAcc dh $ instrOI1 mov),
        makeInstructionS 0xB7 Nothing (decodeAcc bh $ instrOI1 mov),
        makeInstructionS 0xB8 Nothing (decodeAcc ax $ instrOI1 mov),
        makeInstructionS 0xB9 Nothing (decodeAcc cx $ instrOI1 mov),
        makeInstructionS 0xBA Nothing (decodeAcc dx $ instrOI1 mov),
        makeInstructionS 0xBB Nothing (decodeAcc bx $ instrOI1 mov),
        makeInstructionS 0xBC Nothing (decodeAcc sp $ instrOI1 mov),
        makeInstructionS 0xBD Nothing (decodeAcc bp $ instrOI1 mov),
        makeInstructionS 0xBE Nothing (decodeAcc si $ instrOI1 mov),
        makeInstructionS 0xBF Nothing (decodeAcc di $ instrOI1 mov),
        makeInstructionS 0xC6 (Just 0) (decodeNI8 (instrOI1 mov) (instrOI1 mov)),
        makeInstructionS 0xC7 (Just 0) (decodeNI16 (instrOI1 mov) (instrOI1 mov)),
        --XCHG
        makeInstructionS 0x86 Nothing (decodeRM8 xchg xchg),
        makeInstructionS 0x87 Nothing (decodeRM16 xchg xchg),
        --makeInstructionS 0x90 Nothing (decodeAccReg16 ax ax xchg), NOP
        makeInstructionS 0x91 Nothing (decodeAccReg16 ax cx xchg),
        makeInstructionS 0x92 Nothing (decodeAccReg16 ax dx xchg),
        makeInstructionS 0x93 Nothing (decodeAccReg16 ax bx xchg),
        makeInstructionS 0x94 Nothing (decodeAccReg16 ax sp xchg),
        makeInstructionS 0x95 Nothing (decodeAccReg16 ax bp xchg),
        makeInstructionS 0x96 Nothing (decodeAccReg16 ax si xchg),
        makeInstructionS 0x97 Nothing (decodeAccReg16 ax di xchg),
        --PUSH/POP
        makeInstructionS 0x06 Nothing (decodeImplicit $ pushSeg es),
        makeInstructionS 0x07 Nothing (decodeImplicit $ popSeg es),
        makeInstructionS 0x0E Nothing (decodeImplicit $ pushSeg cs),
        makeInstructionS 0x16 Nothing (decodeImplicit $ pushSeg ss),
        makeInstructionS 0x17 Nothing (decodeImplicit $ popSeg ss),
        makeInstructionS 0x1E Nothing (decodeImplicit $ pushSeg ds),
        makeInstructionS 0x1F Nothing (decodeImplicit $ popSeg ds),
        makeInstructionS 0x50 Nothing (decodeReg16 ax pushReg16),
        makeInstructionS 0x51 Nothing (decodeReg16 cx pushReg16),
        makeInstructionS 0x52 Nothing (decodeReg16 dx pushReg16),
        makeInstructionS 0x53 Nothing (decodeReg16 bx pushReg16),
        makeInstructionS 0x54 Nothing (decodeReg16 bp pushReg16),
        makeInstructionS 0x55 Nothing (decodeReg16 sp pushReg16),
        makeInstructionS 0x56 Nothing (decodeReg16 si pushReg16),
        makeInstructionS 0x57 Nothing (decodeReg16 di pushReg16),
        makeInstructionS 0x58 Nothing (decodeReg16 ax popReg16),
        makeInstructionS 0x59 Nothing (decodeReg16 cx popReg16),
        makeInstructionS 0x5A Nothing (decodeReg16 dx popReg16),
        makeInstructionS 0x5B Nothing (decodeReg16 bx popReg16),
        makeInstructionS 0x5C Nothing (decodeReg16 bp popReg16),
        makeInstructionS 0x5D Nothing (decodeReg16 sp popReg16),
        makeInstructionS 0x5E Nothing (decodeReg16 si popReg16),
        makeInstructionS 0x5F Nothing (decodeReg16 di popReg16),
        makeInstructionS 0x8F (Just 0) (decodeN16 popReg16 popMem),
        makeInstructionS 0xFF (Just 6) (decodeN16 pushReg16 pushMem),
        --LEA
        makeInstructionS 0x8D Nothing (decodeRm16 emptyRegReg lea16),
        --LES
        makeInstructionS 0xC4 Nothing (decodeRm16 emptyRegReg les16),
        --LDS
        makeInstructionS 0xC5 Nothing (decodeRm16 emptyRegReg lds16),
        --PUSHF/POPF
        makeInstructionS 0x9C Nothing (decodeImplicit pushf),
        makeInstructionS 0x9D Nothing (decodeImplicit popf),
        --LAHF/SAHF
        makeInstructionS 0x9E Nothing (decodeImplicit sahf),
        makeInstructionS 0x9F Nothing (decodeImplicit lahf),
        --XLAT
        makeInstructionS 0xD7 Nothing (decodeImplicit xlat),
        --IN/OUT
        makeInstructionS 0xE4 Nothing (decodeImm8 portInAlImm),
        makeInstructionS 0xE5 Nothing (decodeImm8 portInAxImm),
        makeInstructionS 0xE6 Nothing (decodeImm8 portOutAlImm),
        makeInstructionS 0xE7 Nothing (decodeImm8 portOutAxImm),
        makeInstructionS 0xEC Nothing (decodeImplicit portInAlDx),
        makeInstructionS 0xED Nothing (decodeImplicit portInAxDx),
        makeInstructionS 0xEE Nothing (decodeImplicit portOutAlDx),
        makeInstructionS 0xEF Nothing (decodeImplicit portOutAxDx)
    ]
