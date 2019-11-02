module Instruction.Processor where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

clc :: Ctx -> PrismM
clc ctx = return $ ctx { ctxFlags = flags }
    where
        flags = (ctxFlags ctx) { flagCF = False }

stc :: Ctx -> PrismM
stc ctx = return $ ctx { ctxFlags = flags }
    where
        flags = (ctxFlags ctx) { flagCF = True }

cmc :: Ctx -> PrismM
cmc ctx = return $ ctx { ctxFlags = flags }
    where
        cf = not . flagCF . ctxFlags $ ctx
        flags = (ctxFlags ctx) { flagCF = cf }

cld :: Ctx -> PrismM
cld ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagDF = False }

std :: Ctx -> PrismM
std ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagDF = True }

cli :: Ctx -> PrismM
cli ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagIF = False }

sti :: Ctx -> PrismM
sti ctx = return $ ctx { ctxEFlags = flags }
    where
        flags = (ctxEFlags ctx) { eflagIF = True }

-------------------------------------------------------------------------------

hlt :: Ctx -> PrismM
hlt = return

wait :: Ctx -> PrismM
wait = return

lock :: Ctx -> PrismM
lock = return

-------------------------------------------------------------------------------

processorInstrList = [
        makeInstructionS 0x9B Nothing (decodeImplicit $ wait),
        makeInstructionS 0xF0 Nothing (decodeImplicit $ lock),
        makeInstructionS 0xF4 Nothing (decodeImplicit $ hlt),
        makeInstructionS 0xF5 Nothing (decodeImplicit $ cmc),
        makeInstructionS 0xF8 Nothing (decodeImplicit $ clc),
        makeInstructionS 0xF9 Nothing (decodeImplicit $ stc),
        makeInstructionS 0xFA Nothing (decodeImplicit $ cli),
        makeInstructionS 0xFB Nothing (decodeImplicit $ sti),
        makeInstructionS 0xFC Nothing (decodeImplicit $ cld),
        makeInstructionS 0xFD Nothing (decodeImplicit $ std)
    ]
