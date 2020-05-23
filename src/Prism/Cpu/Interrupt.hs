{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Prism.Cpu.Interrupt where

import Control.Monad.State.Strict

import Data.List (uncons)
import Data.Maybe (isJust)

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Flags
import Prism.Cpu.Registers
import Prism.Cpu.Primitives
import Prism.Cpu.Ports

-------------------------------------------------------------------------------

instance InterruptRun CpuTrans where
    interruptActive = intInterruptUp . ctxInterrupts <$> get
    retInterrupt = loadInterruptCtx
    processInterrupts = processInterrupts_
    raiseInterrupt int = do
        c <- get
        let interrupts = ctxInterrupts c
            newList = int : (intListHigh interrupts)
            interrupts_ = interrupts { intListHigh = newList
                                     , intInterruptUp = True }
        put $ c { ctxInterrupts = interrupts_ }

-------------------------------------------------------------------------------

processInterrupts_ :: CpuTrans ()
processInterrupts_ = do
    res <- getNextInterrupt
    case res of
        Just (newInterrupts, (PrismInt val)) -> do
            let offset = 4 * (fromIntegral val)
                mem = MemPhy16 offset
                mem2 = MemPhy16 $ offset + 2
            ipVal <- readOp mem
            csVal <- readOp mem2
            saveInterruptCtx
            setFlag IF False
            writeOp cs csVal
            writeOp ip ipVal
            modify (\s -> s { ctxInterrupts = newInterrupts } )
        Nothing ->
            modify (\s -> s { ctxInterrupts = (ctxInterrupts s) {intInterruptUp = False } } )

getNextInterruptHigh :: PrismInterrupts -> Maybe (PrismInterrupts, PrismInt)
getNextInterruptHigh interrupts =
    case uncons $ intListHigh interrupts of 
        Just (currentInt, interrupts_) ->
            Just (interrupts { intListHigh = interrupts_}, currentInt)
        Nothing ->
            case uncons $ intSingleStep interrupts of
                Just (currentInt, interrupts_) ->
                    Just (interrupts { intSingleStep = interrupts_}, currentInt)
                Nothing ->
                    Nothing

getNextInterrupt :: CpuTrans (Maybe (PrismInterrupts, PrismInt))
getNextInterrupt = do
    if_ <- getFlag IF
    interrupts <- ctxInterrupts <$> get
    let res = getNextInterruptHigh interrupts
    if isJust res then
        return res
        else if if_ && (intIntrOn interrupts) then do
            int <- ackIrq
            return $ Just (interrupts {intIntrOn = False}, int)
            else
                return Nothing

-------------------------------------------------------------------------------

saveInterruptCtx :: CpuTrans ()
saveInterruptCtx = do
    flags <- getFlags
    eflags <- getFlags
    pushV $ flagsToVal flags $ eflagsToVal eflags 0
    pushP cs
    pushP ip

loadInterruptCtx :: CpuTrans ()
loadInterruptCtx = do
    popP ip
    popP cs
    val <- popV
    let flags = valToFlags val
        eflags = valToEFlags val
    setFlags flags
    setFlags eflags

-------------------------------------------------------------------------------
