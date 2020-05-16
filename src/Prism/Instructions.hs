module Prism.Instructions (
        transferInstrList
    ) where

import Prism.Cpu
import Prism.Decoder
import Prism.Instruction

import Prism.Instructions.Transfer

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
