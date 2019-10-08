{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PrismShow where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad (foldM)

import Data.Either (fromRight)

import Prism
import PrismDecoder


showAny3 :: (Show a, Show b) => String -> Ctx -> a -> b -> PrismCtx IO Ctx
showAny3 name ctx a b = liftIO $ do
    putStrLn $ name ++ " " ++ (show a) ++ ", " ++ (show b)
    return ctx

showAny3Rev name ctx a b = showAny3 name ctx b a

makeShowDecodeFunc :: String -> (a -> b -> c) -> (String -> a) -> (String -> b) -> c
makeShowDecodeFunc name f a b = f (a name) (b name)

type RmShowFunc a b = (Show a, Show b) => Ctx -> a -> b -> PrismCtx IO Ctx

makeShowFunctionN :: (Show a, Show b, Show c, Show d)
    => String
    -> Uint8
    -> Maybe Uint8
    -> (RmShowFunc a b -> RmShowFunc c d-> PrismInstrFunc)
    -> PrismInstruction
makeShowFunctionN name opcode rm func = 
    makeInstructionS opcode rm $ makeShowDecodeFunc name func showAny3 showAny3

makeShowFunctionNRev :: (Show a, Show b, Show c, Show d)
    => String
    -> Uint8
    -> Maybe Uint8
    -> (RmShowFunc a b -> RmShowFunc c d-> PrismInstrFunc)
    -> PrismInstruction
makeShowFunctionNRev name opcode rm func = 
    makeInstructionS opcode rm $ makeShowDecodeFunc name func showAny3Rev showAny3Rev

-------------------------------------------------------------------------------

instructionShow = [
        makeShowFunctionN "ADD" 0x00 Nothing decodeRm8,
        makeShowFunctionN "ADD" 0x01 Nothing decodeRm16,
        makeShowFunctionNRev "ADD" 0x02 Nothing decodeRm8,
        makeShowFunctionNRev "ADD" 0x03 Nothing decodeRm16
    ]

makeShowDecoder :: PrismDecoder
makeShowDecoder = fromRight emptyDecoder $ makeDecoder <$> mergedInstr
    where
        listResult = foldM (flip addInstrList) emptyList instructionShow
        emptyList = [] :: InstructionList
        mergedInstr :: Either String [PrismInstruction]
        mergedInstr = (\ l -> map (uncurry mergeInstruction) l) <$> listResult
        emptyDecoder = makeDecoder []
