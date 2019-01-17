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
    checkParseSuccess "testfiles/definitions.txt"
  ]

checkParseSuccess f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [DefinedCategory SourcePos]
  return $ check parsed
  where
    check (Left es) = compileError $ "Parse " ++ f ++ ":\n" ++ show es
    check _ = return ()

checkParseFail f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [DefinedCategory SourcePos]
  return $ check parsed
  where
    check (Right t) =
      compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t ++ "\n"
    check _ = return ()
