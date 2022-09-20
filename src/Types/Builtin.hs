{- -----------------------------------------------------------------------------
Copyright 2019-2022 Kevin P. Barry

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
  emptyType,
  floatRequiredValue,
  formattedRequiredValue,
  intRequiredValue,
  isOpaqueMulti,
  orderOptionalValue,
  pointerRequiredValue,
  stringRequiredValue,
) where

import qualified Data.Map as Map

import Base.GeneralType
import Base.Positional
import Types.TypeCategory
import Types.TypeInstance


defaultCategories :: CategoryMap c
defaultCategories = Map.empty

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
pointerRequiredValue :: ValueType
pointerRequiredValue = requiredSingleton BuiltinPointer
formattedRequiredValue :: ValueType
formattedRequiredValue = requiredSingleton BuiltinFormatted
orderOptionalValue :: GeneralInstance -> ValueType
orderOptionalValue t = ValueType OptionalValue $ singleType $ JustTypeInstance $ TypeInstance BuiltinOrder (Positional [t])

emptyType :: ValueType
emptyType = ValueType OptionalValue minBound

data PrimitiveType =
  PrimBool |
  PrimChar |
  PrimInt |
  PrimFloat |
  PrimString |
  PrimPointer
  deriving (Eq,Show)

data ExpressionValue =
  -- Multi argument/return tuple.
  OpaqueMulti String |
  -- Single value that needs to be wrapped. (Can convert to UnwrappedSingle.)
  WrappedSingle String |
  -- Single value that will not be wrapped. (Can convert to WrappedSingle.)
  UnwrappedSingle String |
  -- Primitive value that needs to be boxed. (Can convert to UnboxedPrimitive.)
  BoxedPrimitive PrimitiveType String |
  -- Primitive value that will not be boxed. (Can convert to BoxedPrimitive.)
  UnboxedPrimitive PrimitiveType String |
  -- Value with lazy initialization. Requires indirection to get/set.
  LazySingle ExpressionValue
  deriving (Show)

isOpaqueMulti :: ExpressionValue -> Bool
isOpaqueMulti (OpaqueMulti _) = True
isOpaqueMulti _               = False
