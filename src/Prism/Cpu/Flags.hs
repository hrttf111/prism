{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Cpu.Flags where

import Control.Monad.State.Strict (get, modify)

import Prism.Cpu.Types
import Prism.Cpu.Monad

-------------------------------------------------------------------------------

instance CpuFlag Flag CpuTrans where
    getFlag CF =
        (flagCF . ctxFlags) <$> get
    getFlag PF =
        (flagPF . ctxFlags) <$> get
    getFlag AF =
        (flagAF . ctxFlags) <$> get
    getFlag ZF =
        (flagZF . ctxFlags) <$> get
    getFlag SF =
        (flagSF . ctxFlags) <$> get
    getFlag OF =
        (flagOF . ctxFlags) <$> get
    setFlag CF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagCF = b } } )
    setFlag PF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagPF = b } } )
    setFlag AF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagAF = b } } )
    setFlag ZF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagZF = b } } )
    setFlag SF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagSF = b } } )
    setFlag OF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagOF = b } } )

instance CpuFlag EFlag CpuTrans where
    getFlag TF =
        (eflagTF . ctxEFlags) <$> get
    getFlag IF =
        (eflagIF . ctxEFlags) <$> get
    getFlag DF =
        (eflagDF . ctxEFlags) <$> get
    setFlag TF b =
        modify (\s -> s { ctxEFlags = (ctxEFlags s) { eflagTF = b } } )
    setFlag IF b =
        modify (\s -> s { ctxEFlags = (ctxEFlags s) { eflagIF = b } } )
    setFlag DF b =
        modify (\s -> s { ctxEFlags = (ctxEFlags s) { eflagDF = b } } )

-------------------------------------------------------------------------------

instance CpuFlags Flags CpuTrans where
    getFlags = ctxFlags <$> get
    setFlags f = modify (\s -> s { ctxFlags = f } )

instance CpuFlags EFlags CpuTrans where
    getFlags = ctxEFlags <$> get
    setFlags f = modify (\s -> s { ctxEFlags = f } )

-------------------------------------------------------------------------------
