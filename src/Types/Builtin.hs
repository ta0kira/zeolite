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

module Types.Builtin (
  boolRequiredValue,
  charRequiredValue,
  defaultCategories,
  emptyValue,
  floatRequiredValue,
  formattedRequiredValue,
  intRequiredValue,
  stringRequiredValue,
) where

import qualified Data.Map as Map

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
formattedRequiredValue :: ValueType
formattedRequiredValue = requiredSingleton BuiltinFormatted
emptyValue :: ValueType
emptyValue = ValueType OptionalValue minBound
