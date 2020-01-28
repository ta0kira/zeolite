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

module Procedure (
  ArgValues(..),
  Assignable(..),
  ExecutableProcedure(..),
  Expression(..),
  ExpressionStart(..),
  FunctionCall(..),
  FunctionQualifier(..),
  FunctionSpec(..),
  IfElifElse(..),
  InputValue(..),
  Operator(..),
  OutputValue(..),
  Procedure(..),
  ReturnValues(..),
  ScopedBlock(..),
  Statement(..),
  ValueLiteral(..),
  ValueOperation(..),
  VariableName(..),
  VoidExpression(..),
  WhileLoop(..),
  getExpressionContext,
  getStatementContext,
  isDiscardedInput,
  isUnnamedReturns,
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
    epEnd :: [c],
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

instance Show c => Show (ArgValues c) where
  show (ArgValues c v) =
    "(" ++ intercalate ",\n" (map show $ psParams v) ++ ")" ++
    " /*" ++ formatFullContext c ++ "*/"

data ReturnValues c =
  NamedReturns {
    nrContext :: [c],
    nrNames :: ParamSet (OutputValue c)
  } |
  UnnamedReturns {
    urContext :: [c]
  }

isUnnamedReturns :: ReturnValues c -> Bool
isUnnamedReturns (UnnamedReturns _) = True
isUnnamedReturns _                  = False

instance Show c => Show (ReturnValues c) where
  show (NamedReturns c v) =
    "(" ++ intercalate ",\n" (map show $ psParams v) ++ ")" ++
    " /*" ++ formatFullContext c ++ "*/"
  show (UnnamedReturns c) = "/*unnamed returns: " ++ formatFullContext c ++ "*/"

data VariableName =
  VariableName {
    vnName :: String
  }
  deriving (Eq,Ord)

instance Show VariableName where
  show (VariableName n) = n

data InputValue c =
  InputValue {
    ivContext :: [c],
    ivName :: VariableName
  } |
  DiscardInput {
    iiContext :: [c]
  }

isDiscardedInput :: InputValue c -> Bool
isDiscardedInput (DiscardInput _) = True
isDiscardedInput _                = False

instance Show c => Show (InputValue c) where
  show (InputValue c v) = show v ++ " /*" ++ formatFullContext c ++ "*/"
  show (DiscardInput c) = "_" ++ " /*" ++ formatFullContext c ++ "*/"

data OutputValue c =
  OutputValue {
    ovContext :: [c],
    ovName :: VariableName
  }

instance Show c => Show (OutputValue c) where
  show (OutputValue c v) = show v ++ " /*" ++ formatFullContext c ++ "*/"


data Procedure c =
  Procedure [c] [Statement c]
  deriving (Show)

data Statement c =
  EmptyReturn [c] |
  ExplicitReturn [c] (ParamSet (Expression c)) |
  LoopBreak [c] |
  LoopContinue [c] |
  FailCall [c] (Expression c) |
  IgnoreValues [c] (Expression c) |
  Assignment [c] (ParamSet (Assignable c)) (Expression c) |
  NoValueExpression [c] (VoidExpression c)
  deriving (Show)

getStatementContext :: Statement c -> [c]
getStatementContext (EmptyReturn c)         = c
getStatementContext (ExplicitReturn c _)    = c
getStatementContext (LoopBreak c)           = c
getStatementContext (LoopContinue c)        = c
getStatementContext (FailCall c _)          = c
getStatementContext (IgnoreValues c _)      = c
getStatementContext (Assignment c _ _)      = c
getStatementContext (NoValueExpression c _) = c

data Assignable c =
  CreateVariable [c] ValueType VariableName |
  ExistingVariable (InputValue c)
  deriving (Show)

data VoidExpression c =
  Conditional (IfElifElse c) |
  Loop (WhileLoop c) |
  WithScope (ScopedBlock c)
  deriving (Show)

data IfElifElse c =
  IfStatement [c] (Expression c) (Procedure c) (IfElifElse c) |
  ElseStatement [c] (Procedure c) |
  TerminateConditional
  deriving (Show)

data WhileLoop c =
  WhileLoop [c] (Expression c) (Procedure c) (Maybe (Procedure c))
  deriving (Show)

data ScopedBlock c =
  ScopedBlock [c] (Procedure c) (Maybe (Procedure c)) (Statement c)
  deriving (Show)

data Expression c =
  Expression [c] (ExpressionStart c) [ValueOperation c] |
  Literal (ValueLiteral c) |
  UnaryExpression [c] (Operator c) (Expression c) |
  InfixExpression [c] (Expression c) (Operator c) (Expression c) |
  InitializeValue [c] TypeInstance (ParamSet GeneralInstance) (ParamSet (Expression c))
  deriving (Show)

data FunctionQualifier c =
  CategoryFunction [c] CategoryName |
  TypeFunction [c] TypeInstanceOrParam |
  ValueFunction [c] (Expression c) |
  UnqualifiedFunction
  deriving (Show)

data FunctionSpec c =
  FunctionSpec [c] (FunctionQualifier c) FunctionName (ParamSet GeneralInstance)
  deriving (Show)

data Operator c =
  NamedOperator String |
  FunctionOperator [c] (FunctionSpec c)
  deriving (Show)

getExpressionContext :: Expression c -> [c]
getExpressionContext (Expression c _ _)        = c
getExpressionContext (Literal l)               = getValueLiteralContext l
getExpressionContext (UnaryExpression c _ _)   = c
getExpressionContext (InfixExpression c _ _ _) = c
getExpressionContext (InitializeValue c _ _ _) = c

data FunctionCall c =
  FunctionCall [c] FunctionName (ParamSet GeneralInstance) (ParamSet (Expression c))
  deriving (Show)

data ExpressionStart c =
  NamedVariable (OutputValue c) |
  CategoryCall [c] CategoryName (FunctionCall c) |
  TypeCall [c] TypeInstanceOrParam (FunctionCall c) |
  UnqualifiedCall [c] (FunctionCall c) |
  BuiltinCall [c] (FunctionCall c) |
  ParensExpression [c] (Expression c) |
  InlineAssignment [c] VariableName (Expression c)
  deriving (Show)

data ValueLiteral c =
  StringLiteral [c] String |
  CharLiteral [c] String |
  IntegerLiteral [c] String |
  DecimalLiteral [c] String String |
  BoolLiteral [c] Bool |
  EmptyLiteral [c]
  deriving (Show)

getValueLiteralContext :: ValueLiteral c -> [c]
getValueLiteralContext (StringLiteral c _)    = c
getValueLiteralContext (CharLiteral c _)      = c
getValueLiteralContext (IntegerLiteral c _)   = c
getValueLiteralContext (DecimalLiteral c _ _) = c
getValueLiteralContext (BoolLiteral c _)      = c
getValueLiteralContext (EmptyLiteral c)       = c

data ValueOperation c =
  ConvertedCall [c] TypeInstance (FunctionCall c) |
  ValueCall [c] (FunctionCall c)
  deriving (Show)
