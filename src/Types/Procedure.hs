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

module Types.Procedure (
  ArgValues(..),
  Assignable(..),
  ExecutableProcedure(..),
  Expression(..),
  ExpressionStart(..),
  ExpressionType,
  FunctionCall(..),
  FunctionQualifier(..),
  FunctionSpec(..),
  IfElifElse(..),
  InputValue(..),
  InstanceOrInferred(..),
  IteratedLoop(..),
  MacroName(..),
  Operator(..),
  OutputValue(..),
  PragmaProcedure(..),
  Procedure(..),
  ReturnValues(..),
  ScopedBlock(..),
  Statement(..),
  TestProcedure(..),
  TraceType(..),
  ValueLiteral(..),
  ValueOperation(..),
  VariableName(..),
  VoidExpression(..),
  assignableName,
  getExpressionContext,
  getOperatorContext,
  getOperatorName,
  getStatementContext,
  isAssignableDiscard,
  isDiscardedInput,
  isFunctionOperator,
  isNoTrace,
  isTraceCreation,
  isRawCodeLine,
  isUnnamedReturns,
) where

import Data.List (intercalate)

import Base.Positional
import Types.Builtin
import Types.TypeCategory
import Types.TypeInstance


-- NOTE: Requires FunctionType and SymbolScope to compile, but those might not
-- be available when this needs to be parsed.
data ExecutableProcedure c =
  ExecutableProcedure {
    epContext :: [c],
    epPragmas :: [PragmaProcedure c],
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
    avNames :: Positional (InputValue c)
  }

instance Show c => Show (ArgValues c) where
  show (ArgValues c v) =
    "(" ++ intercalate ",\n" (map show $ pValues v) ++ ")" ++
    " /*" ++ formatFullContext c ++ "*/"

data ReturnValues c =
  NamedReturns {
    nrContext :: [c],
    nrNames :: Positional (OutputValue c)
  } |
  UnnamedReturns {
    urContext :: [c]
  }

isUnnamedReturns :: ReturnValues c -> Bool
isUnnamedReturns (UnnamedReturns _) = True
isUnnamedReturns _                  = False

instance Show c => Show (ReturnValues c) where
  show (NamedReturns c v) =
    "(" ++ intercalate ",\n" (map show $ pValues v) ++ ")" ++
    " /*" ++ formatFullContext c ++ "*/"
  show (UnnamedReturns c) = "/*unnamed returns: " ++ formatFullContext c ++ "*/"

data VariableName =
  VariableName {
    vnName :: String
  } |
  VariableSelf
  deriving (Eq,Ord)

instance Show VariableName where
  show (VariableName n) = n
  show VariableSelf     = "self"

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

discardInputName :: VariableName
discardInputName = VariableName "_"

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

data TestProcedure c =
  TestProcedure {
    tpContext :: [c],
    tpName :: FunctionName,
    tpProcedure :: Procedure c
  }
  deriving (Show)

data Procedure c =
  Procedure [c] [Statement c]
  deriving (Show)

data Statement c =
  EmptyReturn [c] |
  ExplicitReturn [c] (Positional (Expression c)) |
  LoopBreak [c] |
  LoopContinue [c] |
  FailCall [c] (Expression c) |
  RawFailCall String |
  IgnoreValues [c] (Expression c) |
  Assignment [c] (Positional (Assignable c)) (Expression c) |
  DeferredVariables [c] [Assignable c] |
  NoValueExpression [c] (VoidExpression c) |
  MarkReadOnly [c] [VariableName] |
  MarkHidden [c] [VariableName] |
  ValidateRefs [c] [VariableName] |
  ShowVariable [c] ValueType VariableName |
  RawCodeLine String
  deriving (Show)

isRawCodeLine :: Statement c -> Bool
isRawCodeLine (RawCodeLine _) = True
isRawCodeLine _               = False

getStatementContext :: Statement c -> [c]
getStatementContext (EmptyReturn c)         = c
getStatementContext (ExplicitReturn c _)    = c
getStatementContext (LoopBreak c)           = c
getStatementContext (LoopContinue c)        = c
getStatementContext (FailCall c _)          = c
getStatementContext (RawFailCall _)         = []
getStatementContext (IgnoreValues c _)      = c
getStatementContext (Assignment c _ _)      = c
getStatementContext (DeferredVariables c _) = c
getStatementContext (NoValueExpression c _) = c
getStatementContext (MarkReadOnly c _)      = c
getStatementContext (MarkHidden c _)        = c
getStatementContext (ValidateRefs c _)      = c
getStatementContext (ShowVariable _ _ _)    = []
getStatementContext (RawCodeLine _)         = []

data Assignable c =
  CreateVariable [c] ValueType VariableName |
  ExistingVariable (InputValue c)
  deriving (Show)

assignableName :: Assignable c -> VariableName
assignableName (CreateVariable _ _ n)              = n
assignableName (ExistingVariable (InputValue _ n)) = n
assignableName _                                   = discardInputName

isAssignableDiscard :: Assignable c -> Bool
isAssignableDiscard (CreateVariable _ _ _) = False
isAssignableDiscard (ExistingVariable v)   = isDiscardedInput v

data VoidExpression c =
  Conditional (IfElifElse c) |
  Loop (IteratedLoop c) |
  WithScope (ScopedBlock c) |
  Unconditional (Procedure c) |
  LineComment String
  deriving (Show)

data IfElifElse c =
  IfStatement [c] (Expression c) (Procedure c) (IfElifElse c) |
  ElseStatement [c] (Procedure c) |
  TerminateConditional
  deriving (Show)

data IteratedLoop c =
  WhileLoop [c] (Expression c) (Procedure c) (Maybe (Procedure c)) |
  TraverseLoop [c] (Expression c) [c] (Assignable c) (Procedure c) (Maybe (Procedure c))
  deriving (Show)

data ScopedBlock c =
  ScopedBlock [c] (Procedure c) (Maybe (Procedure c)) [c] (Statement c)
  deriving (Show)

data Expression c =
  Expression [c] (ExpressionStart c) [ValueOperation c] |
  Literal (ValueLiteral c) |
  UnaryExpression [c] (Operator c) (Expression c) |
  InfixExpression [c] (Expression c) (Operator c) (Expression c) |
  RawExpression ExpressionType ExpressionValue
  deriving (Show)

type ExpressionType = Positional ValueType

data FunctionQualifier c =
  CategoryFunction [c] CategoryName |
  TypeFunction [c] TypeInstanceOrParam |
  -- TODO: Does this need to allow conversion calls?
  ValueFunction [c] (Expression c) |
  UnqualifiedFunction
  deriving (Show)

data InstanceOrInferred c =
  AssignedInstance [c] GeneralInstance |
  InferredInstance [c]

instance Show c => Show (InstanceOrInferred c) where
  show (AssignedInstance _ t) = show t
  show (InferredInstance _)   = "?"

data FunctionSpec c =
  FunctionSpec [c] (FunctionQualifier c) FunctionName (Positional (InstanceOrInferred c))
  deriving (Show)

data Operator c =
  NamedOperator [c] String |
  FunctionOperator [c] (FunctionSpec c)
  deriving (Show)

getOperatorContext :: Operator c -> [c]
getOperatorContext (NamedOperator c _)    = c
getOperatorContext (FunctionOperator c _) = c

isFunctionOperator :: Operator c -> Bool
isFunctionOperator (FunctionOperator _ _) = True
isFunctionOperator _                      = False

getOperatorName :: Operator c -> FunctionName
getOperatorName (NamedOperator _ n)                         = FunctionName n
getOperatorName (FunctionOperator _ (FunctionSpec _ _ n _)) = n

getExpressionContext :: Expression c -> [c]
getExpressionContext (Expression c _ _)        = c
getExpressionContext (Literal l)               = getValueLiteralContext l
getExpressionContext (UnaryExpression c _ _)   = c
getExpressionContext (InfixExpression c _ _ _) = c
getExpressionContext (RawExpression _ _)       = []

data FunctionCall c =
  FunctionCall [c] FunctionName (Positional (InstanceOrInferred c)) (Positional (Expression c))
  deriving (Show)

data ExpressionStart c =
  NamedVariable (OutputValue c) |
  NamedMacro [c] MacroName |
  CategoryCall [c] CategoryName (FunctionCall c) |
  TypeCall [c] TypeInstanceOrParam (FunctionCall c) |
  UnqualifiedCall [c] (FunctionCall c) |
  BuiltinCall [c] (FunctionCall c) |
  ParensExpression [c] (Expression c) |
  InlineAssignment [c] VariableName (Expression c) |
  InitializeValue [c] (Maybe TypeInstance) (Positional (Expression c)) |
  UnambiguousLiteral (ValueLiteral c)
  deriving (Show)

data ValueLiteral c =
  StringLiteral [c] String |
  CharLiteral [c] Char |
  IntegerLiteral [c] Bool Integer |
  DecimalLiteral [c] Integer Integer |
  BoolLiteral [c] Bool |
  EmptyLiteral [c]
  deriving (Show)

getValueLiteralContext :: ValueLiteral c -> [c]
getValueLiteralContext (StringLiteral c _)    = c
getValueLiteralContext (CharLiteral c _)      = c
getValueLiteralContext (IntegerLiteral c _ _) = c
getValueLiteralContext (DecimalLiteral c _ _) = c
getValueLiteralContext (BoolLiteral c _)      = c
getValueLiteralContext (EmptyLiteral c)       = c

data ValueOperation c =
  ConvertedCall [c] TypeInstance (FunctionCall c) |
  ValueCall [c] (FunctionCall c)
  deriving (Show)

newtype MacroName =
  MacroName {
    mnName :: String
  }
  deriving (Eq,Ord)

instance Show MacroName where
  show = mnName

data TraceType = NoTrace | TraceCreation deriving (Show)

data PragmaProcedure c =
  PragmaTracing {
    ptContext :: [c],
    ptType :: TraceType
  }
  deriving (Show)

isNoTrace :: PragmaProcedure c -> Bool
isNoTrace (PragmaTracing _ NoTrace) = True
isNoTrace _                         = False

isTraceCreation :: PragmaProcedure c -> Bool
isTraceCreation (PragmaTracing _ TraceCreation) = True
isTraceCreation _                               = False
