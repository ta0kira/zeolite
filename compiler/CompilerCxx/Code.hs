{-# LANGUAGE Safe #-}

module CompilerCxx.Code (
  ExprValue(..),
  PrimitiveType(..),
  emptyCode,
  indentCompiled,
  onlyCode,
  onlyCodes,
  predTraceContext,
  setTraceContext,
  useAsUnboxed,
  useAsUnwrapped,
  useAsWhatever,
  useAsWrapped,
  valueAsUnwrapped,
  valueAsWrapped,
) where

import qualified Data.Set as Set

import CompilerState
import TypeCategory


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

useAsWrapped :: ExprValue -> String
useAsWrapped (OpaqueMulti e)                 = "(" ++ e ++ ")"
useAsWrapped (WrappedSingle e)               = "ReturnTuple(" ++ e ++ ")"
useAsWrapped (UnwrappedSingle e)             = "ReturnTuple(" ++ e ++ ")"
useAsWrapped (BoxedPrimitive PrimBool e)     = "ReturnTuple(Box_Bool(" ++ e ++ "))"
useAsWrapped (BoxedPrimitive PrimString e)   = "ReturnTuple(Box_String(" ++ e ++ "))"
useAsWrapped (BoxedPrimitive PrimInt e)      = "ReturnTuple(Box_Int(" ++ e ++ "))"
useAsWrapped (BoxedPrimitive PrimFloat e)    = "ReturnTuple(Box_Float(" ++ e ++ "))"
useAsWrapped (UnboxedPrimitive PrimBool e)   = "ReturnTuple(Box_Bool(" ++ e ++ "))"
useAsWrapped (UnboxedPrimitive PrimString e) = "ReturnTuple(Box_String(" ++ e ++ "))"
useAsWrapped (UnboxedPrimitive PrimInt e)    = "ReturnTuple(Box_Int(" ++ e ++ "))"
useAsWrapped (UnboxedPrimitive PrimFloat e)  = "ReturnTuple(Box_Float(" ++ e ++ "))"

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
