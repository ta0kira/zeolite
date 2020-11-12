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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Parser.TypeInstance (
) where

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))

import Base.Mergeable (mergeAll,mergeAny)
import Parser.Common
import Types.GeneralType
import Types.Positional
import Types.TypeInstance


instance ParseFromSource GeneralInstance where
  sourceParser = try allT <|> try anyT <|> intersectOrUnion <|> single where
    allT = labeled "all" $ do
      kwAll
      return minBound
    anyT = labeled "any" $ do
      kwAny
      return maxBound
    intersectOrUnion = labeled "union or intersection" $ do
      sepAfter $ string_ "["
      t1 <- labeled "type" $ sepAfter sourceParser
      t <- intersect t1 <|> union t1
      sepAfter $ string_ "]"
      return t
    intersect t1 = do
      ts <- many1 (sepAfter (string_ "&") >> labeled "type" sourceParser)
      return $ mergeAll (t1:ts)
    union t1 = do
      ts <- many1 (sepAfter (string_ "|") >> labeled "type" sourceParser)
      return $ mergeAny (t1:ts)
    single = do
      t <- sourceParser
      return $ singleType t

instance ParseFromSource ValueType where
  sourceParser = do
    r <- getWeak <|> getOptional <|> getRequired
    t <- sourceParser
    return $ ValueType r t
    where
      getWeak = labeled "weak" $ do
        try kwWeak
        return WeakValue
      getOptional = labeled "optional" $ do
        try kwOptional
        return OptionalValue
      getRequired = return RequiredValue

instance ParseFromSource CategoryName where
  sourceParser = labeled "type name" $ do
    noKeywords
    b <- upper
    e <- sepAfter $ many alphaNum
    return $ box (b:e)
    where
      box n
        | n == "Bool"         = BuiltinBool
        | n == "Char"         = BuiltinChar
        | n == "Int"          = BuiltinInt
        | n == "Float"        = BuiltinFloat
        | n == "String"       = BuiltinString
        | n == "Formatted"    = BuiltinFormatted
        | otherwise = CategoryName n

instance ParseFromSource ParamName where
  sourceParser = labeled "param name" $ do
    noKeywords
    char_ '#'
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ ParamName ('#':b:e)

instance ParseFromSource TypeInstance where
  sourceParser = labeled "type" $ do
    n <- sourceParser
    as <- labeled "type args" $ try args <|> return []
    return $ TypeInstance n (Positional as)
    where
      args = between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy sourceParser (sepAfter $ string ","))

instance ParseFromSource DefinesInstance where
  sourceParser = labeled "type" $ do
    n <- sourceParser
    as <- labeled "type args" $ try args <|> return []
    return $ DefinesInstance n (Positional as)
    where
      args = between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy sourceParser (sepAfter $ string ","))

instance ParseFromSource TypeInstanceOrParam where
  sourceParser = try param <|> inst where
    param = labeled "param" $ do
      n <- sourceParser
      return $ JustParamName False n
    inst = labeled "type" $ do
      t <- sourceParser
      return $ JustTypeInstance t

instance ParseFromSource TypeFilter where
  sourceParser = requires <|> allows <|> defines where
    requires = labeled "requires filter" $ do
      try kwRequires
      t <- sourceParser
      return $ TypeFilter FilterRequires $ singleType t
    allows = labeled "allows filter" $ do
      try kwAllows
      t <- sourceParser
      return $ TypeFilter FilterAllows $ singleType t
    defines = labeled "defines filter" $ do
      try kwDefines
      t <- sourceParser
      return $ DefinesFilter t
