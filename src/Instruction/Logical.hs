module Instruction.Logical where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR, complement)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

not8 :: Ctx -> Uint8 -> (Ctx, Uint8)
not8 ctx val = (ctx, complement val)

not16 :: Ctx -> Uint16 -> (Ctx, Uint16)
not16 ctx val = (ctx, complement val)

-------------------------------------------------------------------------------

logicalInstrList = [
        --NOT
        makeInstructionS 0xF6 (Just 2) (decodeN8 (instrReg8 not8) (instrMem8 not8)),
        makeInstructionS 0xF7 (Just 2) (decodeN16 (instrReg16 not16) (instrMem16 not16))
    ]
