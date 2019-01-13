{-# LANGUAGE Safe #-}

module Procedure (
) where

import Function
import TypeCategory
import TypeInstance
import TypesBase

import Data.List (intercalate)


data ExecutableProcedure c =
  ExecutableProcedure {
    epContext :: [c],
    epName :: FunctionName,
    epScope :: SymbolScope,
    epType :: FunctionType,
    epArgs :: ArgValues c,
    epReturns :: ReturnValues c,
    epProcedure :: Procedure c
  }

data ArgValues c =
  ArgValues {
    avContext :: [c],
    avNames :: ParamSet (InputValue c)
  }
  deriving (Eq,Ord)

instance Show c => Show (ArgValues c) where
  show (ArgValues c v) =
    "(" ++ intercalate ",\n" (map show $ psParams v) ++ ")" ++
    "/*" ++ formatFullContext c ++ "*/"

data ReturnValues c =
  NamedReturns {
    nrContext :: [c],
    nrNames :: ParamSet (OutputValue c)
  } |
  UnnamedReturns {
    urContext :: [c]
  }
  deriving (Eq,Ord)

instance Show c => Show (ReturnValues c) where
  show (NamedReturns c v) =
    "(" ++ intercalate ",\n" (map show $ psParams v) ++ ")" ++
    "/*" ++ formatFullContext c ++ "*/"
  show (UnnamedReturns c) = "/*unnamed returns: " ++ formatFullContext c ++ "*/"

data VariableName c =
  VariableName {
    vnContext :: [c],
    vnName :: String
  }
  deriving (Eq,Ord)

instance Show c => Show (VariableName c) where
  show (VariableName c n) = n ++ "/*" ++ formatFullContext c ++ "*/"

data InputValue c =
  InputValue {
    ivVariable :: VariableName c
  } |
  IgnoreValue {
    ivContext :: [c]
  }
  deriving (Eq,Ord)

instance Show c => Show (InputValue c) where
  show (InputValue v) = show v
  show (IgnoreValue c) = "_" ++ "/*" ++ formatFullContext c ++ "*/"

data OutputValue c =
  OutputValue {
    ovVariable :: VariableName c
  }
  deriving (Eq,Ord)

instance Show c => Show (OutputValue c) where
  show (OutputValue v) = show v


data Procedure c =
  Procedure [Statement c]
  deriving (Eq,Show)

data Statement c =
  EmptyReturn [c] |
  ExplicitReturn [c] |
  Assignment [c] (Destination c) (Expression c) |
  NoValueExpression [c] (VoidExpression c)
  deriving (Eq,Show)

data Destination c =
  Destination [c] (ParamSet (Assignable c))
  deriving (Eq,Show)

data Assignable c =
  CreateVariable [c] ValueType (VariableName c) |
  ExistingVariable [c] (InputValue c)
  deriving (Eq,Show)

data VoidExpression c =
  Conditional (IfThenElse c) |
  Loop (WhileLoop c) |
  WithScope (ScopedBlock c) (VoidExpression c)
  deriving (Eq,Show)

data Expression c =
  Expression [c] (ParamSet ValueType) (Action c)
  deriving (Eq,Show)

data ParamOrVariable =
  ParamOrVariable {
    povName :: String
  }
  deriving (Eq,Ord,Show)

data CategoryOrSingleton =
  CategoryOrSingleton {
    cosName :: String
  }
  deriving (Eq,Ord,Show)

data Action c =
  VariableValue (OutputValue c) |
  ValueCall [c] ParamOrVariable (ParamSet (Expression c)) |
  TypeCall [c] TypeInstance (ParamSet (Expression c)) |
  CategoryOrSingletonCall [c] CategoryOrSingleton (ParamSet (Expression c)) |
  BuiltinCall (Builtin c)
  deriving (Eq,Show)

data IfThenElse c =
  IfStatement [c] (Expression c) (Procedure c) (IfThenElse c) |
  ElseStatement [c] (Procedure c) |
  TerminateConditional
  deriving (Eq,Show)

data WhileLoop c =
  WhileLoop [c] (Expression c) (Procedure c)
  deriving (Eq,Show)

data ScopedBlock c =
  ScopedBlock [c] (Procedure c)
  deriving (Eq,Show)

data Builtin c =
  CallReduce [c] TypeInstance TypeInstance (Expression c) |
  CallRequire [c] (Expression c) |
  CallStrong [c] (Expression c)
  deriving (Eq,Show)
