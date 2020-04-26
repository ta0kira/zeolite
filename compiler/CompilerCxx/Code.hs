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

module CompilerCxx.Code (
  ExprValue(..),
  PrimitiveType(..),
  categoryBase,
  clearCompiled,
  emptyCode,
  escapeChar,
  escapeChars,
  functionLabelType,
  indentCompiled,
  isPrimType,
  newFunctionLabel,
  onlyCode,
  onlyCodes,
  paramType,
  predTraceContext,
  readStoredVariable,
  setTraceContext,
  startCleanupTracing,
  startFunctionTracing,
  typeBase,
  useAsArgs,
  useAsReturns,
  useAsUnboxed,
  useAsUnwrapped,
  useAsWhatever,
  valueAsUnwrapped,
  valueAsWrapped,
  valueBase,
  variableLazyType,
  variableProxyType,
  variableStoredType,
  writeStoredVariable,
) where

import Data.Char
import Data.List (intercalate)
import qualified Data.Set as Set

import Base.TypesBase
import Compilation.CompilerState
import CompilerCxx.Naming
import Types.Builtin
import Types.DefinedCategory
import Types.TypeCategory
import Types.TypeInstance


emptyCode :: CompiledData [String]
emptyCode = onlyCodes []

onlyCode :: String -> CompiledData [String]
onlyCode = onlyCodes . (:[])

onlyCodes :: [String] -> CompiledData [String]
onlyCodes = CompiledData Set.empty

indentCompiled :: CompiledData [String] -> CompiledData [String]
indentCompiled (CompiledData r o) = CompiledData r $ map ("  " ++) o

clearCompiled :: CompiledData [String] -> CompiledData [String]
clearCompiled (CompiledData r _) = CompiledData r []

startFunctionTracing :: String -> String
startFunctionTracing f = "TRACE_FUNCTION(" ++ show f ++ ")"

startCleanupTracing :: String
startCleanupTracing = "TRACE_CLEANUP"

setTraceContext :: Show c => [c] -> [String]
setTraceContext c
  | null c = []
  | otherwise = ["SET_CONTEXT_POINT(" ++ escapeChars (formatFullContext c) ++ ")"]

predTraceContext :: Show c => [c] -> String
predTraceContext c
  | null c = ""
  | otherwise = "PRED_CONTEXT_POINT(" ++ escapeChars (formatFullContext c) ++ ")"

data PrimitiveType =
  PrimBool |
  PrimString |
  PrimChar |
  PrimInt |
  PrimFloat
  deriving (Eq,Show)

isPrimType :: ValueType -> Bool
isPrimType t
  | t == boolRequiredValue  = True
  | t == intRequiredValue   = True
  | t == floatRequiredValue = True
  | t == charRequiredValue  = True
  | otherwise               = False

data ExprValue =
  OpaqueMulti String |
  WrappedSingle String |
  UnwrappedSingle String |
  BoxedPrimitive PrimitiveType String |
  UnboxedPrimitive PrimitiveType String |
  LazySingle ExprValue
  deriving (Show)

getFromLazy (OpaqueMulti e)        = OpaqueMulti $ e ++ ".Get()"
getFromLazy (WrappedSingle e)      = WrappedSingle $ e ++ ".Get()"
getFromLazy (UnwrappedSingle e)    = UnwrappedSingle $ e ++ ".Get()"
getFromLazy (BoxedPrimitive t e)   = BoxedPrimitive t $ e ++ ".Get()"
getFromLazy (UnboxedPrimitive t e) = UnboxedPrimitive t  $ e ++ ".Get()"
getFromLazy (LazySingle e)         = LazySingle $ getFromLazy e

useAsWhatever :: ExprValue -> String
useAsWhatever (OpaqueMulti e)        = e
useAsWhatever (WrappedSingle e)      = e
useAsWhatever (UnwrappedSingle e)    = e
useAsWhatever (BoxedPrimitive _ e)   = e
useAsWhatever (UnboxedPrimitive _ e) = e
useAsWhatever (LazySingle e)         = useAsWhatever $ getFromLazy e

useAsReturns :: ExprValue -> String
useAsReturns (OpaqueMulti e)                 = "(" ++ e ++ ")"
useAsReturns (WrappedSingle e)               = "ReturnTuple(" ++ e ++ ")"
useAsReturns (UnwrappedSingle e)             = "ReturnTuple(" ++ e ++ ")"
useAsReturns (BoxedPrimitive PrimBool e)     = "ReturnTuple(Box_Bool(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimString e)   = "ReturnTuple(Box_String(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimChar e)     = "ReturnTuple(Box_Char(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimInt e)      = "ReturnTuple(Box_Int(" ++ e ++ "))"
useAsReturns (BoxedPrimitive PrimFloat e)    = "ReturnTuple(Box_Float(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimBool e)   = "ReturnTuple(Box_Bool(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimString e) = "ReturnTuple(Box_String(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimChar e)   = "ReturnTuple(Box_Char(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimInt e)    = "ReturnTuple(Box_Int(" ++ e ++ "))"
useAsReturns (UnboxedPrimitive PrimFloat e)  = "ReturnTuple(Box_Float(" ++ e ++ "))"
useAsReturns (LazySingle e)                  = useAsReturns $ getFromLazy e

useAsArgs :: ExprValue -> String
useAsArgs (OpaqueMulti e)                 = "(" ++ e ++ ")"
useAsArgs (WrappedSingle e)               = "ArgTuple(" ++ e ++ ")"
useAsArgs (UnwrappedSingle e)             = "ArgTuple(" ++ e ++ ")"
useAsArgs (BoxedPrimitive PrimBool e)     = "ArgTuple(Box_Bool(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimString e)   = "ArgTuple(Box_String(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimChar e)     = "ArgTuple(Box_Char(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimInt e)      = "ArgTuple(Box_Int(" ++ e ++ "))"
useAsArgs (BoxedPrimitive PrimFloat e)    = "ArgTuple(Box_Float(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimBool e)   = "ArgTuple(Box_Bool(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimString e) = "ArgTuple(Box_String(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimChar e)   = "ArgTuple(Box_Char(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimInt e)    = "ArgTuple(Box_Int(" ++ e ++ "))"
useAsArgs (UnboxedPrimitive PrimFloat e)  = "ArgTuple(Box_Float(" ++ e ++ "))"
useAsArgs (LazySingle e)                  = useAsArgs $ getFromLazy e

useAsUnwrapped :: ExprValue -> String
useAsUnwrapped (OpaqueMulti e)                 = "(" ++ e ++ ").Only()"
useAsUnwrapped (WrappedSingle e)               = "(" ++ e ++ ")"
useAsUnwrapped (UnwrappedSingle e)             = "(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimBool e)     = "Box_Bool(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimString e)   = "Box_String(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimChar e)     = "Box_Char(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimInt e)      = "Box_Int(" ++ e ++ ")"
useAsUnwrapped (BoxedPrimitive PrimFloat e)    = "Box_Float(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimBool e)   = "Box_Bool(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimString e) = "Box_String(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimChar e) = "Box_Char(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimInt e)    = "Box_Int(" ++ e ++ ")"
useAsUnwrapped (UnboxedPrimitive PrimFloat e)  = "Box_Float(" ++ e ++ ")"
useAsUnwrapped (LazySingle e)                  = useAsUnwrapped $ getFromLazy e

useAsUnboxed :: PrimitiveType -> ExprValue -> String
useAsUnboxed t (OpaqueMulti e)
  | t == PrimBool   = "(" ++ e ++ ").Only()->AsBool()"
  | t == PrimString = "(" ++ e ++ ").Only()->AsString()"
  | t == PrimChar   = "(" ++ e ++ ").Only()->AsChar()"
  | t == PrimInt    = "(" ++ e ++ ").Only()->AsInt()"
  | t == PrimFloat  = "(" ++ e ++ ").Only()->AsFloat()"
useAsUnboxed t (WrappedSingle e)
  | t == PrimBool   = "(" ++ e ++ ")->AsBool()"
  | t == PrimString = "(" ++ e ++ ")->AsString()"
  | t == PrimChar   = "(" ++ e ++ ")->AsChar()"
  | t == PrimInt    = "(" ++ e ++ ")->AsInt()"
  | t == PrimFloat  = "(" ++ e ++ ")->AsFloat()"
useAsUnboxed t (UnwrappedSingle e)
  | t == PrimBool   = "(" ++ e ++ ")->AsBool()"
  | t == PrimString = "(" ++ e ++ ")->AsString()"
  | t == PrimChar   = "(" ++ e ++ ")->AsChar()"
  | t == PrimInt    = "(" ++ e ++ ")->AsInt()"
  | t == PrimFloat  = "(" ++ e ++ ")->AsFloat()"
useAsUnboxed _ (BoxedPrimitive _ e)   = "(" ++ e ++ ")"
useAsUnboxed _ (UnboxedPrimitive _ e) = "(" ++ e ++ ")"
useAsUnboxed t (LazySingle e) = useAsUnboxed t $ getFromLazy e

valueAsWrapped :: ExprValue -> ExprValue
valueAsWrapped (UnwrappedSingle e)             = WrappedSingle e
valueAsWrapped (BoxedPrimitive _ e)            = WrappedSingle e
valueAsWrapped (UnboxedPrimitive PrimBool e)   = WrappedSingle $ "Box_Bool(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimString e) = WrappedSingle $ "Box_String(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimChar e)   = WrappedSingle $ "Box_Char(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimInt e)    = WrappedSingle $ "Box_Int(" ++ e ++ ")"
valueAsWrapped (UnboxedPrimitive PrimFloat e)  = WrappedSingle $ "Box_Float(" ++ e ++ ")"
valueAsWrapped (LazySingle e)                  = valueAsWrapped $ getFromLazy e
valueAsWrapped v                               = v

valueAsUnwrapped :: ExprValue -> ExprValue
valueAsUnwrapped (OpaqueMulti e)                 = UnwrappedSingle $ "(" ++ e ++ ").Only()"
valueAsUnwrapped (WrappedSingle e)               = UnwrappedSingle e
valueAsUnwrapped (UnboxedPrimitive PrimBool e)   = UnwrappedSingle $ "Box_Bool(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimString e) = UnwrappedSingle $ "Box_String(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimChar e)   = UnwrappedSingle $ "Box_Char(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimInt e)    = UnwrappedSingle $ "Box_Int(" ++ e ++ ")"
valueAsUnwrapped (UnboxedPrimitive PrimFloat e)  = UnwrappedSingle $ "Box_Float(" ++ e ++ ")"
valueAsUnwrapped (LazySingle e)                  = valueAsUnwrapped $ getFromLazy e
valueAsUnwrapped v                               = v

variableStoredType :: ValueType -> String
variableStoredType t
  | t == boolRequiredValue   = "bool"
  | t == intRequiredValue    = "PrimInt"
  | t == floatRequiredValue  = "PrimFloat"
  | t == charRequiredValue   = "PrimChar"
  | isWeakValue t            = "W<TypeValue>"
  | otherwise                = "S<TypeValue>"

variableLazyType :: ValueType -> String
variableLazyType t = "LazyInit<" ++ variableStoredType t ++ ">"

variableProxyType :: ValueType -> String
variableProxyType t
  | t == boolRequiredValue   = "bool"
  | t == intRequiredValue    = "PrimInt"
  | t == floatRequiredValue  = "PrimFloat"
  | t == charRequiredValue   = "PrimChar"
  | isWeakValue t            = "W<TypeValue>&"
  | otherwise                = "S<TypeValue>&"

readStoredVariable :: Bool -> ValueType -> String -> ExprValue
readStoredVariable True t s = LazySingle $ readStoredVariable False t s
readStoredVariable False t s
  | t == boolRequiredValue   = UnboxedPrimitive PrimBool s
  | t == intRequiredValue    = UnboxedPrimitive PrimInt s
  | t == floatRequiredValue  = UnboxedPrimitive PrimFloat s
  | t == charRequiredValue   = UnboxedPrimitive PrimChar s
  | otherwise                = UnwrappedSingle s

writeStoredVariable :: ValueType -> ExprValue -> String
writeStoredVariable t e
  | t == boolRequiredValue   = useAsUnboxed PrimBool e
  | t == intRequiredValue    = useAsUnboxed PrimInt e
  | t == floatRequiredValue  = useAsUnboxed PrimFloat e
  | t == charRequiredValue   = useAsUnboxed PrimChar e
  | otherwise                = useAsUnwrapped e

functionLabelType :: ScopedFunction c -> String
functionLabelType = getType . sfScope where
  getType CategoryScope = "const CategoryFunction&"
  getType TypeScope     = "const TypeFunction&"
  getType ValueScope    = "const ValueFunction&"

newFunctionLabel :: Int -> ScopedFunction c -> String
newFunctionLabel i f = "(*new " ++ (getType $ sfScope f) ++ "{ " ++ intercalate ", " args ++ " })" where
  args = [
      paramCount,
      argCount,
      returnCount,
      category,
      function,
      collection,
      functionNum
    ]
  paramCount  = show $ length $ psParams $ sfParams f
  argCount    = show $ length $ psParams $ sfArgs f
  returnCount = show $ length $ psParams $ sfReturns f
  category    = show $ show $ sfType f
  function    = show $ show $ sfName f
  collection  = collectionName $ sfType f
  functionNum = show i
  getType CategoryScope = "CategoryFunction"
  getType TypeScope     = "TypeFunction"
  getType ValueScope    = "ValueFunction"


categoryBase :: String
categoryBase = "TypeCategory"

typeBase :: String
typeBase = "TypeInstance"

valueBase :: String
valueBase = "TypeValue"

paramType :: String
paramType = typeBase ++ "&"

unescapedChars :: Set.Set Char
unescapedChars = Set.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ','.']

escapeChar :: Char -> String
escapeChar c
  | c `Set.member` unescapedChars = [c]
  | otherwise = ['\\','x',asHex c1,asHex c2] where
    c1 = (ord c) `div` 16
    c2 = (ord c) `mod` 16
    asHex n
      | n < 10    = chr $ n + (ord '0')
      | otherwise = chr $ n + (ord 'A') - 10

escapeChars :: String -> String
escapeChars cs
  | null cs = "\"\""
  | otherwise = escapeAll False "" cs where
    -- Creates alternating substrings of (un)escaped characters.
    escapeAll False ss (c:cs)
      | c `Set.member` unescapedChars = escapeAll False (ss ++ [c]) cs
      | otherwise = maybeQuote ss ++ escapeAll True "" (c:cs)
    escapeAll True ss (c:cs)
      | c `Set.member` unescapedChars = maybeQuote ss ++ escapeAll False "" (c:cs)
      | otherwise = escapeAll True (ss ++ escapeChar c) cs
    escapeAll _ ss "" = maybeQuote ss
    maybeQuote ss
      | null ss = ""
      | otherwise = "\"" ++ ss ++ "\""
