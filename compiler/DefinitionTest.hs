{- -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

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
    checkParseSuccess "testfiles/internal_inheritance.0rx",
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
