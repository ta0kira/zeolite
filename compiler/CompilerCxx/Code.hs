{-# LANGUAGE Safe #-}

module CompilerCxx.Code (
  indentCompiled,
  onlyCode,
  onlyCodes,
) where

import qualified Data.Set as Set

import CompilerState


onlyCode :: String -> CompiledData [String]
onlyCode = onlyCodes . (:[])

onlyCodes :: [String] -> CompiledData [String]
onlyCodes = CompiledData Set.empty

indentCompiled :: CompiledData [String] -> CompiledData [String]
indentCompiled (CompiledData r o) = CompiledData r $ map ("  " ++) o
