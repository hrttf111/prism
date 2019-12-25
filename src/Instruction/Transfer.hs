{-# LANGUAGE FlexibleContexts #-}

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

pushOp :: Operand a Uint16 => FuncO1M a
pushOp ctx op =
    readOp ctx op >>= push16 ctx

popOp :: Operand a Uint16 => FuncO1M a
popOp ctx op =
    pop16 ctx >>= writeOp ctx op >> return ctx

-------------------------------------------------------------------------------

lea16 :: FuncO2M Mem16 Reg16
lea16 ctx mem reg = do
    offset <- getEA (ctxReg ctx) (unwrapMem mem)
    writeOp ctx reg offset
    return ctx

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

transferInstrList = [
        --MOV
        makeInstructionS 0x88 Nothing (decodeRM8 (instrRegToRm mov) (instrRegToRm mov)),
        makeInstructionS 0x89 Nothing (decodeRM16 (instrRegToRm mov) (instrRegToRm mov)),
        makeInstructionS 0x8A Nothing (decodeRM8 (instrRmToReg mov) (instrRmToReg mov)),
        makeInstructionS 0x8B Nothing (decodeRM16 (instrRmToReg mov) (instrRmToReg mov)),
        makeInstructionS 0x8C Nothing (decodeRMS16 (instrRegToRm mov) (instrRegToRm mov)),
        makeInstructionS 0x8E Nothing (decodeRMS16 (instrRmToReg mov) (instrRmToReg mov)),
        makeInstructionS 0xA0 Nothing (decodeStRM8 al $ instrOp1ToOp2 mov),
        makeInstructionS 0xA1 Nothing (decodeStRM16 ax $ instrOp1ToOp2 mov),
        makeInstructionS 0xA2 Nothing (decodeStRM8 al $ instrOp2ToOp1 mov),
        makeInstructionS 0xA3 Nothing (decodeStRM16 ax $ instrOp2ToOp1 mov),
        makeInstructionS 0xB0 Nothing (decodeStRI al $ instrOI1 mov),
        makeInstructionS 0xB1 Nothing (decodeStRI cl $ instrOI1 mov),
        makeInstructionS 0xB2 Nothing (decodeStRI dl $ instrOI1 mov),
        makeInstructionS 0xB3 Nothing (decodeStRI bl $ instrOI1 mov),
        makeInstructionS 0xB4 Nothing (decodeStRI ah $ instrOI1 mov),
        makeInstructionS 0xB5 Nothing (decodeStRI ch $ instrOI1 mov),
        makeInstructionS 0xB6 Nothing (decodeStRI dh $ instrOI1 mov),
        makeInstructionS 0xB7 Nothing (decodeStRI bh $ instrOI1 mov),
        makeInstructionS 0xB8 Nothing (decodeStRI ax $ instrOI1 mov),
        makeInstructionS 0xB9 Nothing (decodeStRI cx $ instrOI1 mov),
        makeInstructionS 0xBA Nothing (decodeStRI dx $ instrOI1 mov),
        makeInstructionS 0xBB Nothing (decodeStRI bx $ instrOI1 mov),
        makeInstructionS 0xBC Nothing (decodeStRI sp $ instrOI1 mov),
        makeInstructionS 0xBD Nothing (decodeStRI bp $ instrOI1 mov),
        makeInstructionS 0xBE Nothing (decodeStRI si $ instrOI1 mov),
        makeInstructionS 0xBF Nothing (decodeStRI di $ instrOI1 mov),
        makeInstructionS 0xC6 (Just 0) (decodeNI8 (instrOI1 mov) (instrOI1 mov)),
        makeInstructionS 0xC7 (Just 0) (decodeNI16 (instrOI1 mov) (instrOI1 mov)),
        --XCHG
        makeInstructionS 0x86 Nothing (decodeRM8 xchg xchg),
        makeInstructionS 0x87 Nothing (decodeRM16 xchg xchg),
        --makeInstructionS 0x90 Nothing (decodeStRR ax ax xchg), NOP
        makeInstructionS 0x91 Nothing (decodeStRR ax cx xchg),
        makeInstructionS 0x92 Nothing (decodeStRR ax dx xchg),
        makeInstructionS 0x93 Nothing (decodeStRR ax bx xchg),
        makeInstructionS 0x94 Nothing (decodeStRR ax sp xchg),
        makeInstructionS 0x95 Nothing (decodeStRR ax bp xchg),
        makeInstructionS 0x96 Nothing (decodeStRR ax si xchg),
        makeInstructionS 0x97 Nothing (decodeStRR ax di xchg),
        --PUSH/POP
        makeInstructionS 0x06 Nothing (decodeStR es pushOp),
        makeInstructionS 0x07 Nothing (decodeStR es popOp),
        makeInstructionS 0x0E Nothing (decodeStR cs pushOp),
        makeInstructionS 0x16 Nothing (decodeStR ss pushOp),
        makeInstructionS 0x17 Nothing (decodeStR ss popOp),
        makeInstructionS 0x1E Nothing (decodeStR ds pushOp),
        makeInstructionS 0x1F Nothing (decodeStR ds popOp),
        makeInstructionS 0x50 Nothing (decodeStR ax pushOp),
        makeInstructionS 0x51 Nothing (decodeStR cx pushOp),
        makeInstructionS 0x52 Nothing (decodeStR dx pushOp),
        makeInstructionS 0x53 Nothing (decodeStR bx pushOp),
        makeInstructionS 0x54 Nothing (decodeStR bp pushOp),
        makeInstructionS 0x55 Nothing (decodeStR sp pushOp),
        makeInstructionS 0x56 Nothing (decodeStR si pushOp),
        makeInstructionS 0x57 Nothing (decodeStR di pushOp),
        makeInstructionS 0x58 Nothing (decodeStR ax popOp),
        makeInstructionS 0x59 Nothing (decodeStR cx popOp),
        makeInstructionS 0x5A Nothing (decodeStR dx popOp),
        makeInstructionS 0x5B Nothing (decodeStR bx popOp),
        makeInstructionS 0x5C Nothing (decodeStR bp popOp),
        makeInstructionS 0x5D Nothing (decodeStR sp popOp),
        makeInstructionS 0x5E Nothing (decodeStR si popOp),
        makeInstructionS 0x5F Nothing (decodeStR di popOp),
        makeInstructionS 0x8F (Just 0) (decodeN16 popOp popOp),
        makeInstructionS 0xFF (Just 6) (decodeN16 pushOp pushOp),
        --LEA
        makeInstructionS 0x8D Nothing (decodeRM16 emptyRegReg lea16),
        --LES
        makeInstructionS 0xC4 Nothing (decodeRM16 emptyRegReg les16),
        --LDS
        makeInstructionS 0xC5 Nothing (decodeRM16 emptyRegReg lds16),
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
