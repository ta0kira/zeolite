{-# LANGUAGE Safe #-}

module CompilerCxx.Code (
  emptyCode,
  indentCompiled,
  onlyCode,
  onlyCodes,
  setTraceContext,
) where

import qualified Data.Set as Set

import CompilerState
import TypeCategory


emptyCode :: CompiledData [String]
emptyCode = onlyCodes []

onlyCode :: String -> CompiledData [String]
onlyCode = onlyCodes . (:[])

onlyCodes :: [String] -> CompiledData [String]
onlyCodes = CompiledData Set.empty

indentCompiled :: CompiledData [String] -> CompiledData [String]
indentCompiled (CompiledData r o) = CompiledData r $ map ("  " ++) o

setTraceContext :: Show c => [c] -> String
setTraceContext c = "SET_CONTEXT_POINT(" ++ show (formatFullContext c) ++ ")"
