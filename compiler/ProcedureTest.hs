{-# LANGUAGE Safe #-}

module ProcedureTest where

import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParseProcedure
import Procedure
import TestBase
import TypesBase


main = runAllTests [
    checkParseSuccess "testfiles/procedures.txt"
  ]

checkParseSuccess f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check (Left es) = compileError $ "Parse " ++ f ++ ":\n" ++ show es
    check _ = return ()

checkParseFail f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check (Right t) =
      compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t ++ "\n"
    check _ = return ()
