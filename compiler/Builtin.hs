{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

module Builtin (
  boolRequiredValue,
  builtinCategories,
  charRequiredValue,
  defaultCategories,
  emptyValue,
  floatRequiredValue,
  formattedRequiredValue,
  intRequiredValue,
  stringRequiredValue,
) where

import Text.Parsec
import qualified Data.Map as Map

import ParseCategory
import ParserBase
import TypeCategory
import TypeInstance
import TypesBase


defaultCategories :: CategoryMap c
defaultCategories = Map.empty

builtinCategories :: (MergeableM m, CompileErrorM m, Monad m) => m (CategoryMap SourcePos)
builtinCategories = unwrap parsed >>= mapNames where
  parsed = parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) sourceName builtinSource
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t
  mapNames = includeNewTypes Map.empty
  sourceName = "(builtin)"

boolRequiredValue :: ValueType
boolRequiredValue = requiredSingleton BuiltinBool
stringRequiredValue :: ValueType
stringRequiredValue = requiredSingleton BuiltinString
charRequiredValue :: ValueType
charRequiredValue = requiredSingleton BuiltinChar
intRequiredValue :: ValueType
intRequiredValue = requiredSingleton BuiltinInt
floatRequiredValue :: ValueType
floatRequiredValue = requiredSingleton BuiltinFloat
formattedRequiredValue :: ValueType
formattedRequiredValue = requiredSingleton BuiltinFormatted
emptyValue :: ValueType
emptyValue = ValueType OptionalValue $ TypeMerge MergeUnion []

builtinSource :: String
builtinSource = concat $ map (++ "\n") [
    "concrete Bool {",
    "  refines AsBool",
    "  refines AsInt",
    "  refines AsFloat",
    "  refines Formatted",
    "",
    "  defines Equals<Bool>",
    "}",
    "",
    "concrete Char {",
    "  refines AsBool",
    "  refines AsChar",
    "  refines AsInt",
    "  refines AsFloat",
    "  refines Formatted",
    "",
    "  defines Equals<Char>",
    "  defines LessThan<Char>",
    "}",
    "",
    "concrete Int {",
    "  refines AsBool",
    "  refines AsChar",
    "  refines AsInt",
    "  refines AsFloat",
    "  refines Formatted",
    "",
    "  defines Equals<Int>",
    "  defines LessThan<Int>",
    "}",
    "",
    "concrete Float {",
    "  refines AsBool",
    "  refines AsInt",
    "  refines AsFloat",
    "  refines Formatted",
    "",
    "  defines Equals<Float>",
    "  defines LessThan<Float>",
    "}",
    "",
    "concrete String {",
    "  refines AsBool",
    "  refines Formatted",
    "  refines ReadPosition<Char>",
    "",
    "  defines Equals<String>",
    "  defines LessThan<String>",
    "",
    "  @value subSequence (Int,Int) -> (String)",
    "}",
    "",
    "@value interface AsBool {",
    "  asBool () -> (Bool)",
    "}",
    "",
    "@value interface AsInt {",
    "  asInt () -> (Int)",
    "}",
    "",
    "@value interface AsChar {",
    "  asChar () -> (Char)",
    "}",
    "",
    "@value interface AsFloat {",
    "  asFloat () -> (Float)",
    "}",
    "",
    "@type interface LessThan<#x|> {",
    "  lessThan (#x,#x) -> (Bool)",
    "}",
    "",
    "@type interface Equals<#x|> {",
    "  equals (#x,#x) -> (Bool)",
    "}",
    "",
    "@value interface Formatted {",
    "  formatted () -> (String)",
    "}",
    "",
    "@value interface ReadPosition<|#x> {",
    "  readPosition (Int) -> (#x)",
    "  readSize () -> (Int)",
    "  subSequence (Int,Int) -> (ReadPosition<#x>)",
    "}"]
