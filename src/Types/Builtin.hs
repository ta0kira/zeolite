{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

module Types.Builtin (
  ExpressionValue(..),
  PrimitiveType(..),
  boolRequiredValue,
  charRequiredValue,
  defaultCategories,
  defaultCategoryDeps,
  emptyType,
  floatRequiredValue,
  formattedRequiredValue,
  intRequiredValue,
  isPrimitiveType,
  orderOptionalValue,
  stringRequiredValue,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.GeneralType
import Base.Positional
import Types.TypeCategory
import Types.TypeInstance


defaultCategories :: CategoryMap c
defaultCategories = Map.empty

defaultCategoryDeps :: Set.Set CategoryName
defaultCategoryDeps = Set.fromList []

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
orderOptionalValue :: GeneralInstance -> ValueType
orderOptionalValue t = ValueType OptionalValue $ singleType $ JustTypeInstance $ TypeInstance BuiltinOrder (Positional [t])

emptyType :: ValueType
emptyType = ValueType OptionalValue minBound

data PrimitiveType =
  PrimBool |
  PrimString |
  PrimChar |
  PrimInt |
  PrimFloat
  deriving (Eq,Show)

isPrimitiveType :: ValueType -> Bool
isPrimitiveType t
  | t == boolRequiredValue  = True
  | t == intRequiredValue   = True
  | t == floatRequiredValue = True
  | t == charRequiredValue  = True
  | otherwise               = False

data ExpressionValue =
  OpaqueMulti String |
  WrappedSingle String |
  UnwrappedSingle String |
  BoxedPrimitive PrimitiveType String |
  UnboxedPrimitive PrimitiveType String |
  LazySingle ExpressionValue
  deriving (Show)
