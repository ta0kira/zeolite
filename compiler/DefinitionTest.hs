{-# LANGUAGE Safe #-}

module DefinitionTest where

import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParseDefinition
import DefinedCategory
import TestBase
import TypesBase


tests :: [IO (CompileInfo ())]
tests = [
    checkParseSuccess "testfiles/definitions.0rx",
    checkParseSuccess "testfiles/internal_params.0rx",
    checkParseSuccess "testfiles/internal_filters.0rx"
  ]

checkParseSuccess f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [DefinedCategory SourcePos]
  return $ check parsed
  where
  check c
    | isCompileError c = compileError $ "Parse " ++ f ++ ":\n" ++ show (getCompileError c)
    | otherwise = return ()

checkParseFail f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [DefinedCategory SourcePos]
  return $ check parsed
  where
  check c
    | isCompileError c = return ()
    | otherwise = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++
                                 show (getCompileSuccess c) ++ "\n"
