module Prism.Cpu.Val where

import Data.Bits (bit, clearBit, testBit, finiteBitSize)

import Prism.Cpu.Types

-------------------------------------------------------------------------------

negV :: (OperandVal b) => b
negV = (div maxBound 2) + 1

signExtendWordN :: (OperandVal b1, OperandVal b2) => b1 -> b2
signExtendWordN val | val > 0x80 = (+0xFF00) $ fromIntegral val
signExtendWordN val = fromIntegral val

signExtendWord :: Uint8 -> Uint16
signExtendWord val | val > 0x80 = (+0xFF00) $ fromIntegral val
signExtendWord val = fromIntegral val

signExtendDoubleword :: Uint16 -> (Uint16, Uint16)
signExtendDoubleword val | val > 0x8000 = (0xFFFF, val)
signExtendDoubleword val = (0x0000, val)

signExtendDoubleword32 :: Uint16 -> Uint32
signExtendDoubleword32 val | val > 0x8000 = (+0xFFFF0000) $ fromIntegral val
signExtendDoubleword32 val = fromIntegral val

toSignedCompl2 :: (OperandVal a, OperandVal b) => a -> b
toSignedCompl2 val = valUnsigned - valSigned
    where
        upperBit = (finiteBitSize val) - 1
        valUnsigned = fromIntegral $ clearBit val upperBit
        valSigned = if testBit val upperBit then bit upperBit else 0

toUnsignedComp2 :: (OperandVal a, OperandVal b) => a -> b
toUnsignedComp2 = toSignedCompl2

signedOp :: (OperandVal a, OperandVal b) => (b -> b -> b) -> a -> a -> a
signedOp func val1 val2 = toUnsignedComp2 $ func sVal1 sVal2
    where
        sVal1 = toSignedCompl2 val1
        sVal2 = toSignedCompl2 val2

signedOp1 :: (OperandVal a, OperandVal b) => (b -> b -> b) -> a -> a -> b
signedOp1 func val1 val2 = func sVal1 sVal2
    where
        sVal1 = toSignedCompl2 val1
        sVal2 = toSignedCompl2 val2

signedOpS :: (OperandVal a, OperandVal b) => (b -> b) -> a -> a
signedOpS func val1 = toUnsignedComp2 $ func $ toSignedCompl2 val1

-------------------------------------------------------------------------------

maxCpuCyclesDelta = maxBound :: CpuCyclesDelta

calcCpuCyclesDelta :: CpuCycles -> CpuCycles -> CpuCyclesDelta
calcCpuCyclesDelta (CpuCycles t1) (CpuCycles t2) =
    CpuCyclesDelta $ (fromIntegral t2) - (fromIntegral t1)

-------------------------------------------------------------------------------
