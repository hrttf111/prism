module Instruction.Processor where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR)

import Prism
import PrismCpu
import PrismDecoder
import PrismInterrupt
import PrismRun

-------------------------------------------------------------------------------

clc :: FuncImplicit
clc ctx = return $ ctx { ctxFlags = flags }
    where
        flags = (ctxFlags ctx) { flagCF = False }

stc :: FuncImplicit
stc ctx = return $ ctx { ctxFlags = flags }
    where
        flags = (ctxFlags ctx) { flagCF = True }

cmc :: FuncImplicit
cmc ctx = return $ ctx { ctxFlags = flags }
    where
        cf = not . flagCF . ctxFlags $ ctx
        flags = (ctxFlags ctx) { flagCF = cf }

cld :: FuncImplicit
cld ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagDF = False }

std :: FuncImplicit
std ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagDF = True }

cli :: FuncImplicit
cli ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagIF = False }

sti :: FuncImplicit
sti ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagIF = True }

-------------------------------------------------------------------------------

hlt :: FuncImplicit
hlt ctx = return $ ctx {ctxStop = True}

wait :: FuncImplicit
wait = return

lock :: FuncImplicit
lock = return

nop :: FuncImplicit
nop = return
-------------------------------------------------------------------------------

int :: FuncImm1 Imm8
int ctx val = return $ ctx { ctxInterrupts = addInterruptInt (ctxInterrupts ctx) val }

into :: FuncImplicit
into ctx = int ctx 4

iret :: FuncImplicit
iret = loadInterruptCtx

-------------------------------------------------------------------------------

segmentOverride :: FuncImplicit -> RegSeg -> FuncImplicit
segmentOverride execInstr regSeg ctx =
    execInstr newCtx >>= \c -> return $ c { ctxReplaceSeg = Nothing }
    where
        newCtx = ctx { ctxReplaceSeg = Just regSeg }

getSegmentInstrList :: (Ctx -> PrismM) -> [PrismInstruction]
getSegmentInstrList execInstr = [
        makeInstructionS 0x26 Nothing (decodeImplicit $ segmentOverride execInstr es),
        makeInstructionS 0x2E Nothing (decodeImplicit $ segmentOverride execInstr cs),
        makeInstructionS 0x36 Nothing (decodeImplicit $ segmentOverride execInstr ss),
        makeInstructionS 0x3E Nothing (decodeImplicit $ segmentOverride execInstr ds)
    ]

segmentInstrList :: [PrismInstruction] -> [PrismInstruction]
segmentInstrList = getSegmentInstrList . decodeExecOne . makeDecoderList

-------------------------------------------------------------------------------

processorInstrList = [
        makeInstructionS 0x90 Nothing (decodeImplicit $ nop),
        makeInstructionS 0x9B Nothing (decodeImplicit $ wait),
        makeInstructionS 0xF0 Nothing (decodeImplicit $ lock),
        makeInstructionS 0xF4 Nothing (decodeImplicit $ hlt),
        makeInstructionS 0xF5 Nothing (decodeImplicit $ cmc),
        makeInstructionS 0xF8 Nothing (decodeImplicit $ clc),
        makeInstructionS 0xF9 Nothing (decodeImplicit $ stc),
        makeInstructionS 0xFA Nothing (decodeImplicit $ cli),
        makeInstructionS 0xFB Nothing (decodeImplicit $ sti),
        makeInstructionS 0xFC Nothing (decodeImplicit $ cld),
        makeInstructionS 0xFD Nothing (decodeImplicit $ std),
        makeInstructionS 0xCC Nothing (decodeImplicit $ flip int 3),
        makeInstructionS 0xCD Nothing (decodeImm8 int),
        makeInstructionS 0xCE Nothing (decodeImplicit into),
        makeInstructionS 0xCF Nothing (decodeImplicit iret)
    ]
