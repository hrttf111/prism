module Prism.Decoder (
        ------------------------------------------------------
          decodeImm8, decodeImm16, decodeImm32
        , decodeStRM8, decodeStRM16
        , decodeNI8, decodeNI16
        , decodeRM8, decodeRM16, decodeRMS16
        , decodeN8, decodeN16
        ------------------------------------------------------
        , PrismDecoder
        , makeDecoder
        , makeInstructionS
        , makeDecoderList 
        ------------------------------------------------------
    ) where

import Prism.Decoder.Common
import Prism.Decoder.Instruction
import Prism.Decoder.Decoding

-------------------------------------------------------------------------------
