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

module Parser.TypeInstance (
) where

import Control.Applicative ((<|>))

import Base.GeneralType
import Base.Mergeable (mergeAll,mergeAny)
import Base.Positional
import Parser.Common
import Parser.TextParser hiding ((<|>),single)
import Types.TypeInstance


instance ParseFromSource GeneralInstance where
  sourceParser = single <|> allT <|> anyT <|> intersectOrUnion where
    allT = labeled "all" $ do
      kwAll
      return minBound
    anyT = labeled "any" $ do
      kwAny
      return maxBound
    intersectOrUnion = labeled "union or intersection" $ do
      sepAfter $ string_ "["
      t1 <- labeled "type" $ sourceParser
      t <- intersect t1 <|> union t1
      sepAfter $ string_ "]"
      return t
    intersect t1 = do
      ts <- some (sepAfter (string_ "&") >> labeled "type" sourceParser)
      return $ mergeAll (t1:ts)
    union t1 = do
      ts <- some (sepAfter (string_ "|") >> labeled "type" sourceParser)
      return $ mergeAny (t1:ts)
    single = fmap singleType sourceParser

instance ParseFromSource ValueType where
  sourceParser = do
    r <- getWeak <|> getOptional <|> getRequired
    t <- sourceParser
    return $ ValueType r t
    where
      getWeak = labeled "weak" $ do
        kwWeak
        return WeakValue
      getOptional = labeled "optional" $ do
        kwOptional
        return OptionalValue
      getRequired = return RequiredValue

instance ParseFromSource CategoryName where
  sourceParser = labeled "type name" $ do
    noKeywords
    b <- upperChar
    e <- sepAfter $ many alphaNumChar
    return $ box (b:e)
    where
      box n
        | n == "Bool"      = BuiltinBool
        | n == "Char"      = BuiltinChar
        | n == "Int"       = BuiltinInt
        | n == "Float"     = BuiltinFloat
        | n == "String"    = BuiltinString
        | n == "Formatted" = BuiltinFormatted
        | otherwise = CategoryName n

instance ParseFromSource ParamName where
  sourceParser = labeled "param name" $ do
    noKeywords
    char_ '#'
    b <- lowerChar
    e <- sepAfter $ many alphaNumChar
    return $ ParamName ('#':b:e)

instance ParseFromSource TypeInstance where
  sourceParser = labeled "type instance" $ do
    n <- sourceParser
    as <- labeled "type args" $ args <|> return []
    return $ TypeInstance n (Positional as)
    where
      args = between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy sourceParser (sepAfter $ string ","))

instance ParseFromSource DefinesInstance where
  sourceParser = labeled "type instance" $ do
    n <- sourceParser
    as <- labeled "type args" $ args <|> return []
    return $ DefinesInstance n (Positional as)
    where
      args = between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy sourceParser (sepAfter $ string ","))

instance ParseFromSource TypeInstanceOrParam where
  sourceParser = inst <|> param where
    param = labeled "param" $ fmap (JustParamName False) sourceParser
    inst = labeled "type instance" $ fmap JustTypeInstance sourceParser

instance ParseFromSource TypeFilter where
  sourceParser = requires <|> allows <|> defines where
    requires = labeled "requires filter" $ do
      kwRequires
      t <- sourceParser
      return $ TypeFilter FilterRequires $ singleType t
    allows = labeled "allows filter" $ do
      kwAllows
      t <- sourceParser
      return $ TypeFilter FilterAllows $ singleType t
    defines = labeled "defines filter" $ do
      kwDefines
      t <- sourceParser
      return $ DefinesFilter t
