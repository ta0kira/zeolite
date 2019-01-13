{-# LANGUAGE Safe #-}

module Procedure (
  ArgValues(..),
  Assignable(..),
  ExecutableProcedure(..),
  Expression(..),
  ExpressionStart(..),
  FunctionCall(..),
  IfElifElse(..),
  InputValue(..),
  OutputValue(..),
  Procedure(..),
  ReturnValues(..),
  ScopedBlock(..),
  Statement(..),
  ValueOperation(..),
  VariableName(..),
  VoidExpression(..),
  WhileLoop(..),
) where

import Function
import TypeCategory
import TypeInstance
import TypesBase

import Data.List (intercalate)


-- NOTE: Requires FunctionType and SymbolScope to compile, but those might not
-- be available when this needs to be parsed.
data ExecutableProcedure c =
  ExecutableProcedure {
    epContext :: [c],
    epName :: FunctionName,
    epArgs :: ArgValues c,
    epReturns :: ReturnValues c,
    epProcedure :: Procedure c
  }
  deriving (Show) -- TODO: Remove Show? Or add proper formatting.

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
  Procedure [c] [Statement c]
  deriving (Eq,Show)

data Statement c =
  EmptyReturn [c] |
  ExplicitReturn [c] (ParamSet (Expression c)) |
  Assignment [c] (ParamSet (Assignable c)) (Expression c) |
  NoValueExpression (VoidExpression c)
  deriving (Eq,Show)

data Assignable c =
  CreateVariable [c] ValueType (VariableName c) |
  ExistingVariable [c] (InputValue c)
  deriving (Eq,Show)

data VoidExpression c =
  Conditional (IfElifElse c) |
  Loop (WhileLoop c) |
  WithScope (ScopedBlock c) (VoidExpression c)
  deriving (Eq,Show)

data Expression c =
  Expression [c] (ParamSet ValueType) (ExpressionStart c) [ValueOperation c]
  deriving (Eq,Show)

data FunctionCall c =
  FunctionCall [c] FunctionName (ParamSet GeneralInstance) (ParamSet (Expression c))
  deriving (Eq,Show)

data ExpressionStart c =
  VariableValue (OutputValue c) |
  -- NOTE: If the type has no args, it could be a category function.
  TypeCall [c] TypeInstanceOrParam (FunctionCall c) |
  UnqualifiedCall [c] (FunctionCall c)
  deriving (Eq,Show)

data ValueOperation c =
  Conversion [c] TypeInstance |
  ValueCall [c] (ParamSet ValueType) (FunctionCall c)
  deriving (Eq,Show)

data IfElifElse c =
  IfStatement [c] (Expression c) (Procedure c) (IfElifElse c) |
  ElseStatement [c] (Procedure c) |
  TerminateConditional
  deriving (Eq,Show)

data WhileLoop c =
  WhileLoop [c] (Expression c) (Procedure c)
  deriving (Eq,Show)

-- TODO: This will likely require some magic if the statement is an assignement.
data ScopedBlock c =
  ScopedBlock [c] (Procedure c) (Statement c)
  deriving (Eq,Show)
