module PrismInterrupt where

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Data.List

import Prism
import PrismCpu

addInterruptSingleStep :: PrismInterrupts -> PrismInterrupts
addInterruptSingleStep interrupts = interrupts { intSingleStep = newList, intInterruptUp = True }
    where
       newList = PrismInt 1 : intSingleStep interrupts

addInterruptInt :: PrismInterrupts -> Uint8 -> PrismInterrupts
addInterruptInt interrupts val = interrupts { intListHigh = newList, intInterruptUp = True }
    where
       newList = PrismInt val : intListHigh interrupts

execTF :: Ctx -> Ctx
execTF ctx = if tf then ctx { ctxInterrupts = addInterruptSingleStep $ ctxInterrupts ctx} else ctx
    where
        tf = eflagTF . ctxEFlags $ ctx

interruptActive :: Ctx -> Bool
interruptActive = intInterruptUp . ctxInterrupts

getNextInterrupt :: PrismInterrupts -> Maybe (PrismInterrupts, PrismInt)
getNextInterrupt interrupts =
    case uncons $ intListHigh interrupts of 
        Just (currentInt, interrupts_) ->
            Just (interrupts { intListHigh = interrupts_}, currentInt)
        Nothing ->
            case uncons $ intSingleStep interrupts of
                Just (currentInt, interrupts_) ->
                    Just (interrupts { intSingleStep = interrupts_}, currentInt)
                Nothing ->
                    Nothing

processInterrupts :: MonadIO m => Ctx -> m Ctx
processInterrupts ctx =
    case (getNextInterrupt $ ctxInterrupts ctx) of
        Just (newInterrupts, (PrismInt val)) -> do
            (ipVal, csVal) <- readMemDirect32 (ctxReg ctx) (ctxMem ctx) (4 * (fromIntegral val))
            saveInterruptCtx ctx
            let newCtx = clearIntFlags $ ctx { ctxInterrupts = newInterrupts }
            jmpInterrupt newCtx ipVal csVal
        Nothing -> return $ ctx { ctxInterrupts = (ctxInterrupts $ ctx) { intInterruptUp = False } }

clearIntFlags :: Ctx -> Ctx
clearIntFlags ctx = ctx { ctxEFlags = newEFlags }
    where
        newEFlags = EFlags False False (eflagDF $ ctxEFlags ctx)

saveInterruptCtx :: MonadIO m => Ctx -> m Ctx
saveInterruptCtx ctx = do
    pushInt ctx val
    readSeg memReg cs >>= pushInt ctx
    readRegIP memReg >>= pushInt ctx
    return ctx
    where
        val = flagsToVal (ctxFlags ctx) $ eflagsToVal (ctxEFlags ctx) 0
        memReg = ctxReg ctx

loadInterruptCtx :: MonadIO m => Ctx -> m Ctx
loadInterruptCtx ctx = do
    popInt ctx >>= writeRegIP memReg
    popInt ctx >>= writeSeg memReg cs
    val <- popInt ctx
    let flags = valToFlags val
        eflags = valToEFlags val
    return $ ctx { ctxFlags = flags, ctxEFlags = eflags }
    where
        memReg = ctxReg ctx

jmpInterrupt :: MonadIO m => Ctx -> Imm16 -> Imm16 -> m Ctx
jmpInterrupt ctx ipVal csVal = do
    writeSeg memReg cs csVal
    writeRegIP memReg ipVal
    return ctx
    where
        memReg = ctxReg ctx

pushInt :: MonadIO m => Ctx -> Uint16 -> m Ctx
pushInt ctx val = do
    valSp <- readReg16 memReg sp
    let valNewSp = valSp - 2
    writeReg16 memReg sp valNewSp
    writeMemSp16 memReg memMain val
    return ctx
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx

popInt :: MonadIO m => Ctx -> m Uint16
popInt ctx = do
    val <- readMemSp16 memReg memMain
    valSp <- readReg16 memReg sp
    let valNewSp = valSp + 2
    writeReg16 memReg sp valNewSp
    return val 
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
