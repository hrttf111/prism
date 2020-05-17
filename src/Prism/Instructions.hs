module Prism.Instructions (
        x86InstrList
    ) where

import Prism.Cpu
import Prism.Decoder
import Prism.Instruction
import Prism.Run

import Prism.Instructions.Transfer
import Prism.Instructions.Arithmetic
import Prism.Instructions.Processor

-------------------------------------------------------------------------------

transferInstrList = [
        --MOV
        makeInstructionS 0x88 Nothing (decodeRM8 (instrRegToRmw mov) (instrRegToRmw mov)),
        makeInstructionS 0x89 Nothing (decodeRM16 (instrRegToRmw mov) (instrRegToRmw mov)),
        makeInstructionS 0x8A Nothing (decodeRM8 (instrRmToRegw mov) (instrRmToRegw mov)),
        makeInstructionS 0x8B Nothing (decodeRM16 (instrRmToRegw mov) (instrRmToRegw mov)),
        makeInstructionS 0x8C Nothing (decodeRMS16 (instrRegToRmw mov) (instrRegToRmw mov)),
        makeInstructionS 0x8E Nothing (decodeRMS16 (instrRmToRegw mov) (instrRmToRegw mov)),
        makeInstructionS 0xA0 Nothing (decodeStRM8 al $ instrOp1ToOp2w mov),
        makeInstructionS 0xA1 Nothing (decodeStRM16 ax $ instrOp1ToOp2w mov),
        makeInstructionS 0xA2 Nothing (decodeStRM8 al $ instrOp2ToOp1w mov),
        makeInstructionS 0xA3 Nothing (decodeStRM16 ax $ instrOp2ToOp1w mov),
        makeInstructionS 0xB0 Nothing (decodeStRI al $ instrOI1w mov),
        makeInstructionS 0xB1 Nothing (decodeStRI cl $ instrOI1w mov),
        makeInstructionS 0xB2 Nothing (decodeStRI dl $ instrOI1w mov),
        makeInstructionS 0xB3 Nothing (decodeStRI bl $ instrOI1w mov),
        makeInstructionS 0xB4 Nothing (decodeStRI ah $ instrOI1w mov),
        makeInstructionS 0xB5 Nothing (decodeStRI ch $ instrOI1w mov),
        makeInstructionS 0xB6 Nothing (decodeStRI dh $ instrOI1w mov),
        makeInstructionS 0xB7 Nothing (decodeStRI bh $ instrOI1w mov),
        makeInstructionS 0xB8 Nothing (decodeStRI ax $ instrOI1w mov),
        makeInstructionS 0xB9 Nothing (decodeStRI cx $ instrOI1w mov),
        makeInstructionS 0xBA Nothing (decodeStRI dx $ instrOI1w mov),
        makeInstructionS 0xBB Nothing (decodeStRI bx $ instrOI1w mov),
        makeInstructionS 0xBC Nothing (decodeStRI sp $ instrOI1w mov),
        makeInstructionS 0xBD Nothing (decodeStRI bp $ instrOI1w mov),
        makeInstructionS 0xBE Nothing (decodeStRI si $ instrOI1w mov),
        makeInstructionS 0xBF Nothing (decodeStRI di $ instrOI1w mov),
        makeInstructionS 0xC6 (Just 0) (decodeNI8 (instrOI1w mov) (instrOI1w mov)),
        makeInstructionS 0xC7 (Just 0) (decodeNI16 (instrOI1w mov) (instrOI1w mov)),
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
        makeInstructionS 0xD7 Nothing (decodeImplicit xlat)
        --IN/OUT
        {-makeInstructionS 0xE4 Nothing (decodeImm8 portInAlImm),
        makeInstructionS 0xE5 Nothing (decodeImm8 portInAxImm),
        makeInstructionS 0xE6 Nothing (decodeImm8 portOutAlImm),
        makeInstructionS 0xE7 Nothing (decodeImm8 portOutAxImm),
        makeInstructionS 0xEC Nothing (decodeImplicit portInAlDx),
        makeInstructionS 0xED Nothing (decodeImplicit portInAxDx),
        makeInstructionS 0xEE Nothing (decodeImplicit portOutAlDx),
        makeInstructionS 0xEF Nothing (decodeImplicit portOutAxDx)-}
    ]

-------------------------------------------------------------------------------

arithmeticInstrList = [
        --ADD
        makeInstructionS 0x00 Nothing (decodeRM8 (instrRegToRmF add) (instrRegToRmF add)),
        makeInstructionS 0x01 Nothing (decodeRM16 (instrRegToRmF add) (instrRegToRmF add)),
        makeInstructionS 0x02 Nothing (decodeRM8 (instrRmToRegF add) (instrRmToRegF add)),
        makeInstructionS 0x03 Nothing (decodeRM16 (instrRmToRegF add) (instrRmToRegF add)),
        makeInstructionS 0x04 Nothing (decodeStRI al $ instrOFI1 add),
        makeInstructionS 0x05 Nothing (decodeStRI ax $ instrOFI1 add),
        makeInstructionS 0x80 (Just 0) (decodeNI8 (instrOFI1 add) (instrOFI1 add)),
        makeInstructionS 0x81 (Just 0) (decodeNI16 (instrOFI1 add) (instrOFI1 add)),
        makeInstructionS 0x82 (Just 0) (decodeNI8 (instrOFI1 add) (instrOFI1 add)),
        makeInstructionS 0x83 (Just 0) (decodeNC16 (instrOFI1 add) (instrOFI1 add)),
        --ADC
        makeInstructionS 0x10 Nothing (decodeRM8 (instrRegToRmF adc) (instrRegToRmF adc)),
        makeInstructionS 0x11 Nothing (decodeRM16 (instrRegToRmF adc) (instrRegToRmF adc)),
        makeInstructionS 0x12 Nothing (decodeRM8 (instrRmToRegF adc) (instrRmToRegF adc)),
        makeInstructionS 0x13 Nothing (decodeRM16 (instrRmToRegF adc) (instrRmToRegF adc)),
        makeInstructionS 0x14 Nothing (decodeStRI al $ instrOFI1 adc),
        makeInstructionS 0x15 Nothing (decodeStRI ax $ instrOFI1 adc),
        makeInstructionS 0x80 (Just 0x2) (decodeNI8 (instrOFI1 adc) (instrOFI1 adc)),
        makeInstructionS 0x81 (Just 0x2) (decodeNI16 (instrOFI1 adc) (instrOFI1 adc)),
        makeInstructionS 0x82 (Just 0x2) (decodeNI8 (instrOFI1 adc) (instrOFI1 adc)),
        makeInstructionS 0x83 (Just 0x2) (decodeNC16 (instrOFI1 adc) (instrOFI1 adc)),
        --INC
        makeInstructionS 0x40 Nothing (decodeStR ax (instrOF1 inc)),
        makeInstructionS 0x41 Nothing (decodeStR cx (instrOF1 inc)),
        makeInstructionS 0x42 Nothing (decodeStR dx (instrOF1 inc)),
        makeInstructionS 0x43 Nothing (decodeStR bx (instrOF1 inc)),
        makeInstructionS 0x44 Nothing (decodeStR sp (instrOF1 inc)),
        makeInstructionS 0x45 Nothing (decodeStR bp (instrOF1 inc)),
        makeInstructionS 0x46 Nothing (decodeStR si (instrOF1 inc)),
        makeInstructionS 0x47 Nothing (decodeStR di (instrOF1 inc)),
        makeInstructionS 0xFE (Just 0) (decodeN8 (instrOF1 inc) (instrOF1 inc)),
        makeInstructionS 0xFF (Just 0) (decodeN16 (instrOF1 inc) (instrOF1 inc)),
        --SUB
        makeInstructionS 0x28 Nothing (decodeRM8 (instrRegToRmF sub) (instrRegToRmF sub)),
        makeInstructionS 0x29 Nothing (decodeRM16 (instrRegToRmF sub) (instrRegToRmF sub)),
        makeInstructionS 0x2A Nothing (decodeRM8 (instrRmToRegF sub) (instrRmToRegF sub)),
        makeInstructionS 0x2B Nothing (decodeRM16 (instrRmToRegF sub) (instrRmToRegF sub)),
        makeInstructionS 0x2C Nothing (decodeStRI al $ instrOFI1 sub),
        makeInstructionS 0x2D Nothing (decodeStRI ax $ instrOFI1 sub),
        makeInstructionS 0x80 (Just 0x5) (decodeNI8 (instrOFI1 sub) (instrOFI1 sub)),
        makeInstructionS 0x81 (Just 0x5) (decodeNI16 (instrOFI1 sub) (instrOFI1 sub)),
        makeInstructionS 0x82 (Just 0x5) (decodeNI8 (instrOFI1 sub) (instrOFI1 sub)),
        makeInstructionS 0x83 (Just 0x5) (decodeNC16 (instrOFI1 sub) (instrOFI1 sub)),
        --SBB
        makeInstructionS 0x18 Nothing (decodeRM8 (instrRegToRmF sbb) (instrRegToRmF sbb)),
        makeInstructionS 0x19 Nothing (decodeRM16 (instrRegToRmF sbb) (instrRegToRmF sbb)),
        makeInstructionS 0x1A Nothing (decodeRM8 (instrRmToRegF sbb) (instrRmToRegF sbb)),
        makeInstructionS 0x1B Nothing (decodeRM16 (instrRmToRegF sbb) (instrRmToRegF sbb)),
        makeInstructionS 0x1C Nothing (decodeStRI al $ instrOFI1 sbb),
        makeInstructionS 0x1D Nothing (decodeStRI ax $ instrOFI1 sbb),
        makeInstructionS 0x80 (Just 0x3) (decodeNI8 (instrOFI1 sbb) (instrOFI1 sbb)),
        makeInstructionS 0x81 (Just 0x3) (decodeNI16 (instrOFI1 sbb) (instrOFI1 sbb)),
        makeInstructionS 0x82 (Just 0x3) (decodeNI8 (instrOFI1 sbb) (instrOFI1 sbb)),
        makeInstructionS 0x83 (Just 0x3) (decodeNC16 (instrOFI1 sbb) (instrOFI1 sbb)),
        --DEC
        makeInstructionS 0x48 Nothing (decodeStR ax (instrOF1 dec)),
        makeInstructionS 0x49 Nothing (decodeStR cx (instrOF1 dec)),
        makeInstructionS 0x4A Nothing (decodeStR dx (instrOF1 dec)),
        makeInstructionS 0x4B Nothing (decodeStR bx (instrOF1 dec)),
        makeInstructionS 0x4C Nothing (decodeStR sp (instrOF1 dec)),
        makeInstructionS 0x4D Nothing (decodeStR bp (instrOF1 dec)),
        makeInstructionS 0x4E Nothing (decodeStR si (instrOF1 dec)),
        makeInstructionS 0x4F Nothing (decodeStR di (instrOF1 dec)),
        makeInstructionS 0xFE (Just 1) (decodeN8 (instrOF1 dec) (instrOF1 dec)),
        makeInstructionS 0xFF (Just 1) (decodeN16 (instrOF1 dec) (instrOF1 dec)),
        --CMP
        makeInstructionS 0x38 Nothing (decodeRM8 (instrRegToRmF cmp) (instrRegToRmF cmp)),
        makeInstructionS 0x39 Nothing (decodeRM16 (instrRegToRmF cmp) (instrRegToRmF cmp)),
        makeInstructionS 0x3A Nothing (decodeRM8 (instrRmToRegF cmp) (instrRmToRegF cmp)),
        makeInstructionS 0x3B Nothing (decodeRM16 (instrRmToRegF cmp) (instrRmToRegF cmp)),
        makeInstructionS 0x3C Nothing (decodeStRI al $ instrOFI1 cmp),
        makeInstructionS 0x3D Nothing (decodeStRI ax $ instrOFI1 cmp),
        makeInstructionS 0x80 (Just 0x7) (decodeNI8 (instrOFI1 cmp) (instrOFI1 cmp)),
        makeInstructionS 0x81 (Just 0x7) (decodeNI16 (instrOFI1 cmp) (instrOFI1 cmp)),
        makeInstructionS 0x82 (Just 0x7) (decodeNI8 (instrOFI1 cmp) (instrOFI1 cmp)),
        makeInstructionS 0x83 (Just 0x7) (decodeNC16 (instrOFI1 cmp) (instrOFI1 cmp)),
        --NEG
        makeInstructionS 0xF6 (Just 3) (decodeN8 (instrOF1 neg) (instrOF1 neg)),
        makeInstructionS 0xF7 (Just 3) (decodeN16 (instrOF1 neg) (instrOF1 neg)),
        --AAA/AAD/AAM/AAS
        makeInstructionS 0x37 Nothing (decodeStR ax $ instrOF1 aaa),
        makeInstructionS 0xD5 (Just 1) (decodeStR ax $ instrOF1 aad),
        makeInstructionS 0xD4 (Just 1) (decodeStR ax $ instrOF1 aam),
        makeInstructionS 0x3F Nothing (decodeStR ax $ instrOF1 aas),
        --DAA/DAS
        makeInstructionS 0x27 Nothing (decodeStR al $ instrOF1 daa),
        makeInstructionS 0x2F Nothing (decodeStR al $ instrOF1 das),
        --MUL
        makeInstructionS 0xF6 (Just 4) (decodeN8 (instrON1 $ muldivInstr8 mul8) (instrON1 $ muldivInstr8 mul8)),
        makeInstructionS 0xF7 (Just 4) (decodeN16 (instrON1 $ muldivInstr16 mul16) (instrON1 $ muldivInstr16 mul16)),
        makeInstructionS 0xF6 (Just 5) (decodeN8 (instrON1 $ muldivInstr8 imul8) (instrON1 $ muldivInstr8 imul8)),
        makeInstructionS 0xF7 (Just 5) (decodeN16 (instrON1 $ muldivInstr16 imul16) (instrON1 $ muldivInstr16 imul16)),
        --DIV
        makeInstructionS 0xF6 (Just 6) (decodeN8 (instrON1 $ divInstr8 div8) (instrON1 $ divInstr8 div8)),
        makeInstructionS 0xF7 (Just 6) (decodeN16 (instrON1 $ divInstr16 div16) (instrON1 $ divInstr16 div16)),
        makeInstructionS 0xF6 (Just 7) (decodeN8 (instrON1 $ divInstr8 idiv8) (instrON1 $ divInstr8 idiv8)),
        makeInstructionS 0xF7 (Just 7) (decodeN16 (instrON1 $ divInstr16 idiv16) (instrON1 $ divInstr16 idiv16)),
        --CBW/CWD
        makeInstructionS 0x98 Nothing (decodeImplicit cbw),
        makeInstructionS 0x99 Nothing (decodeImplicit cwd)
    ]

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
        makeInstructionS 0xFD Nothing (decodeImplicit $ std)
        --makeInstructionS 0xCC Nothing (decodeImplicit $ flip int 3),
        --makeInstructionS 0xCD Nothing (decodeImm8 int),
        --makeInstructionS 0xCE Nothing (decodeImplicit into),
        --makeInstructionS 0xCF Nothing (decodeImplicit iret)
    ]

-------------------------------------------------------------------------------

getSegmentInstrList :: (PrismM ()) -> [PrismInstruction]
getSegmentInstrList execInstr = [
        makeInstructionS 0x26 Nothing (decodeImplicit $ segmentOverride execInstr es),
        makeInstructionS 0x2E Nothing (decodeImplicit $ segmentOverride execInstr cs),
        makeInstructionS 0x36 Nothing (decodeImplicit $ segmentOverride execInstr ss),
        makeInstructionS 0x3E Nothing (decodeImplicit $ segmentOverride execInstr ds)
    ]

segmentInstrList :: [PrismInstruction] -> [PrismInstruction]
segmentInstrList = getSegmentInstrList . decodeExecOne . makeDecoderList

-------------------------------------------------------------------------------

x86InstrList :: [PrismInstruction]
x86InstrList = instrList ++ (segmentInstrList instrList)
    where
        instrList = transferInstrList
                    ++ arithmeticInstrList
                    ++ processorInstrList

-------------------------------------------------------------------------------
