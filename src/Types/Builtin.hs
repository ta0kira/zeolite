{- -----------------------------------------------------------------------------
Copyright 2019-2023 Kevin P. Barry

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

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module Types.Builtin (
  ExpressionValue(..),
  PrimitiveType(..),
  boolRequiredValue,
  charRequiredValue,
  emptyType,
  floatRequiredValue,
  formattedRequiredValue,
  intRequiredValue,
  isIdentifierRequiredValue,
  isPointerRequiredValue,
  isOpaqueMulti,
  orderOptionalValue,
  requiredStaticTypes,
  stringRequiredValue,
) where

import qualified Data.Set as Set

import Base.GeneralType
import Base.MergeTree (reduceMergeTree)
import Base.Positional
import Types.TypeInstance


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

isPointerRequiredValue :: ValueType -> Bool
isPointerRequiredValue (ValueType RequiredValue t) = check $ extractSingle t where
  check (Just (JustTypeInstance (TypeInstance BuiltinPointer (Positional [_])))) = True
  check _ = False
  extractSingle = reduceMergeTree (const Nothing) (const Nothing) Just
isPointerRequiredValue _ = False

isIdentifierRequiredValue :: ValueType -> Bool
isIdentifierRequiredValue (ValueType RequiredValue t) = check $ extractSingle t where
  check (Just (JustTypeInstance (TypeInstance BuiltinIdentifier (Positional [_])))) = True
  check _ = False
  extractSingle = reduceMergeTree (const Nothing) (const Nothing) Just
isIdentifierRequiredValue _ = False

emptyType :: ValueType
emptyType = ValueType OptionalValue minBound

data PrimitiveType =
  PrimBool |
  PrimChar |
  PrimInt |
  PrimFloat |
  PrimString |
  PrimPointer |
  PrimIdentifier
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

requiredStaticTypes :: Set.Set CategoryName
#ifdef darwin_HOST_OS
-- Weak linking doesn't work on MacOS, so we need these in order to link against
-- boxed.cpp without linker errors.
requiredStaticTypes = Set.fromList [
    BuiltinBool,
    BuiltinChar,
    BuiltinInt,
    BuiltinFloat,
    BuiltinPointer,
    BuiltinIdentifier
  ]
#else
requiredStaticTypes = Set.empty
#endif
