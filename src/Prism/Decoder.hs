module Prism.Decoder (
        ------------------------------------------------------
          decodeImplicit 
        , decodeImm8, decodeImm16, decodeImm32
        , decodeStR, decodeStRI, decodeStRR
        , decodeStRM8, decodeStRM16
        , decodeNI8, decodeNI16
        , decodeRM8, decodeRM16, decodeRMS16
        , decodeN8, decodeN16
        ------------------------------------------------------
        , PrismDecoder (..)
        , PrismInstruction (..)
        , makeDecoder
        , makeInstructionS
        , makeDecoderList 
        ------------------------------------------------------
        , peekInstrBytesM
        ------------------------------------------------------
    ) where

import Prism.Decoder.Common
import Prism.Decoder.Instruction
import Prism.Decoder.Decoding

-------------------------------------------------------------------------------
