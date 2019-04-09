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

module CompilerCxx.Code (
  ExprValue(..),
  PrimitiveType(..),
  categoryBase,
  emptyCode,
  functionLabelType,
  indentCompiled,
  isPrimType,
  onlyCode,
  onlyCodes,
  paramType,
  predTraceContext,
  readStoredVariable,
  setTraceContext,
  typeBase,
  useAsArgs,
  useAsReturns,
  useAsUnboxed,
  useAsUnwrapped,
  useAsWhatever,
  valueAsUnwrapped,
  valueAsWrapped,
  valueBase,
  variableProxyType,
  variableStoredType,
  writeStoredVariable,
) where

import qualified Data.Set as Set

import Builtin
import CompilerState
import DefinedCategory
import TypeCategory
import TypeInstance
import TypesBase


emptyCode :: CompiledData [String]
emptyCode = onlyCodes []

onlyCode :: String -> CompiledData [String]
onlyCode = onlyCodes . (:[])

onlyCodes :: [String] -> CompiledData [String]
onlyCodes = CompiledData Set.empty

indentCompiled :: CompiledData [String] -> CompiledData [String]
indentCompiled (CompiledData r o) = CompiledData r $ map ("  " ++) o

setTraceContext :: Show c => [c] -> String
setTraceContext c = "SET_CONTEXT_POINT(" ++ show (formatFullContext c) ++ ")"

predTraceContext :: Show c => [c] -> String
predTraceContext c = "PRED_CONTEXT_POINT(" ++ show (formatFullContext c) ++ ")"

data PrimitiveType =
  PrimBool |
  PrimString |
  PrimInt |
  PrimFloat
  deriving (Eq,Show)

isPrimType :: ValueType -> Bool
isPrimType t
  | t == boolRequiredValue  = True
  | t == intRequiredValue   = True
  | t == floatRequiredValue = True
  | otherwise               = False

data ExprValue =
  OpaqueMulti String |
  WrappedSingle String |
  UnwrappedSingle String |
  BoxedPrimitive PrimitiveType String |
  UnboxedPrimitive PrimitiveType String
  deriving (Show)

useAsWhatever :: ExprValue -> String
useAsWhatever (OpaqueMulti e)        = e
useAsWhatever (WrappedSingle e)      = e
useAsWhatever (UnwrappedSingle e)    = e
useAsWhatever (BoxedPrimitive _ e)   = e
useAsWhatever (UnboxedPrimitive _ e) = e

useAsReturns :: ExprValue -> String
useAsReturns (OpaqueMulti e)                 = "(" ++ e ++ ")"
useAsReturns (WrappedSingle e)               = "ReturnTuple(" ++ e ++ ")"
useAsReturns (UnwrappedSingle e)             = "ReturnTuple(" ++ e ++ ")"
useAsReturns (BoxedPrimitive PrimBool e)     = "ReturnTuple(Box_Bool(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimString e)   = "ReturnTuple(Box_String(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimInt e)      = "ReturnTuple(Box_Int(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimFloat e)    = "ReturnTuple(Box_Float(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimBool e)   = "ReturnTuple(Box_Bool(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimString e) = "ReturnTuple(Box_String(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimInt e)    = "ReturnTuple(Box_Int(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimFloat e)  = "ReturnTuple(Box_Float(" ++ e ++ "))"

useAsArgs :: ExprValue -> String
useAsArgs (OpaqueMulti e)                 = "(" ++ e ++ ")"
useAsArgs (WrappedSingle e)               = "ArgTuple(" ++ e ++ ")"
useAsArgs (UnwrappedSingle e)             = "ArgTuple(" ++ e ++ ")"
useAsArgs (BoxedPrimitive PrimBool e)     = "ArgTuple(Box_Bool(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimString e)   = "ArgTuple(Box_String(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimInt e)      = "ArgTuple(Box_Int(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimFloat e)    = "ArgTuple(Box_Float(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimBool e)   = "ArgTuple(Box_Bool(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimString e) = "ArgTuple(Box_String(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimInt e)    = "ArgTuple(Box_Int(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimFloat e)  = "ArgTuple(Box_Float(" ++ e ++ "))"

useAsUnwrapped :: ExprValue -> String
useAsUnwrapped (OpaqueMulti e)                 = "(" ++ e ++ ").Only()"
useAsUnwrapped (WrappedSingle e)               = "(" ++ e ++ ")"
useAsUnwrapped (UnwrappedSingle e)             = "(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimBool e)     = "Box_Bool(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimString e)   = "Box_String(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimInt e)      = "Box_Int(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimFloat e)    = "Box_Float(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimBool e)   = "Box_Bool(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimString e) = "Box_String(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimInt e)    = "Box_Int(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimFloat e)  = "Box_Float(" ++ e ++ ")"

useAsUnboxed :: PrimitiveType -> ExprValue -> String
useAsUnboxed t (OpaqueMulti e)
  | t == PrimBool   = "(" ++ e ++ ").Only()->AsBool()"
  | t == PrimString = "(" ++ e ++ ").Only()->AsString()"
  | t == PrimInt    = "(" ++ e ++ ").Only()->AsInt()"
  | t == PrimFloat  = "(" ++ e ++ ").Only()->AsFloat()"
useAsUnboxed t (WrappedSingle e)
  | t == PrimBool   = "(" ++ e ++ ")->AsBool()"
  | t == PrimString = "(" ++ e ++ ")->AsString()"
  | t == PrimInt    = "(" ++ e ++ ")->AsInt()"
  | t == PrimFloat  = "(" ++ e ++ ")->AsFloat()"
useAsUnboxed t (UnwrappedSingle e)
  | t == PrimBool   = "(" ++ e ++ ")->AsBool()"
  | t == PrimString = "(" ++ e ++ ")->AsString()"
  | t == PrimInt    = "(" ++ e ++ ")->AsInt()"
  | t == PrimFloat  = "(" ++ e ++ ")->AsFloat()"
useAsUnboxed _ (BoxedPrimitive _ e)   = "(" ++ e ++ ")"
useAsUnboxed _ (UnboxedPrimitive _ e) = "(" ++ e ++ ")"

valueAsWrapped :: ExprValue -> ExprValue
valueAsWrapped (UnwrappedSingle e)             = WrappedSingle e
valueAsWrapped (BoxedPrimitive _ e)            = WrappedSingle e
valueAsWrapped (UnboxedPrimitive PrimBool e)   = WrappedSingle $ "Box_Bool(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimString e) = WrappedSingle $ "Box_String(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimInt e)    = WrappedSingle $ "Box_Int(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimFloat e)  = WrappedSingle $ "Box_Float(" ++ e ++ ")"
valueAsWrapped v                               = v

valueAsUnwrapped :: ExprValue -> ExprValue
valueAsUnwrapped (OpaqueMulti e)                 = UnwrappedSingle $ "(" ++ e ++ ").Only()"
valueAsUnwrapped (WrappedSingle e)               = UnwrappedSingle e
valueAsUnwrapped (UnboxedPrimitive PrimBool e)   = UnwrappedSingle $ "Box_Bool(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimString e) = UnwrappedSingle $ "Box_String(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimInt e)    = UnwrappedSingle $ "Box_Int(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimFloat e)  = UnwrappedSingle $ "Box_Float(" ++ e ++ ")"
valueAsUnwrapped v                               = v

variableStoredType :: ValueType -> String
variableStoredType t
  | t == boolRequiredValue   = "bool"
  | t == intRequiredValue    = "PrimInt"
  | t == floatRequiredValue  = "PrimFloat"
  | isWeakValue t            = "W<TypeValue>"
  | otherwise                = "S<TypeValue>"

variableProxyType :: ValueType -> String
variableProxyType t
  | t == boolRequiredValue   = "bool"
  | t == intRequiredValue    = "PrimInt"
  | t == floatRequiredValue  = "PrimFloat"
  | isWeakValue t            = "W<TypeValue>&"
  | otherwise                = "S<TypeValue>&"

readStoredVariable :: ValueType -> String -> ExprValue
readStoredVariable t s
  | t == boolRequiredValue   = UnboxedPrimitive PrimBool s
  | t == intRequiredValue    = UnboxedPrimitive PrimInt s
  | t == floatRequiredValue  = UnboxedPrimitive PrimFloat s
  | otherwise                = UnwrappedSingle s

writeStoredVariable :: ValueType -> ExprValue -> String
writeStoredVariable t e
  | t == boolRequiredValue   = useAsUnboxed PrimBool e
  | t == intRequiredValue    = useAsUnboxed PrimInt e
  | t == floatRequiredValue  = useAsUnboxed PrimFloat e
  | otherwise                = useAsUnwrapped e

functionLabelType :: ScopedFunction c -> String
functionLabelType f =
  "Function<" ++ scope ++ "," ++ show pn ++ "," ++ show an ++ "," ++ show rn ++ ">" where
    pn = length $ psParams $ sfParams f
    an = length $ psParams $ sfArgs f
    rn = length $ psParams $ sfReturns f
    scope
      | sfScope f == CategoryScope = "SymbolScope::CATEGORY"
      | sfScope f == TypeScope     = "SymbolScope::TYPE"
      | sfScope f == ValueScope    = "SymbolScope::VALUE"

categoryBase :: String
categoryBase = "TypeCategory"

typeBase :: String
typeBase = "TypeInstance"

valueBase :: String
valueBase = "TypeValue"

paramType :: String
paramType = typeBase ++ "&"
