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
  ValueLiteral(..),
  ValueOperation(..),
  VariableName(..),
  VoidExpression(..),
  WhileLoop(..),
  getExpressionContext,
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
  IgnoreValues [c] (Expression c) |
  Assignment [c] (ParamSet (Assignable c)) (Expression c) |
  NoValueExpression (VoidExpression c)
  deriving (Show)

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
  ScopedBlock [c] (Procedure c) (Statement c)
  deriving (Show)

data Expression c =
  Expression [c] (ExpressionStart c) [ValueOperation c] |
  Literal (ValueLiteral c) |
  UnaryExpression [c] String (Expression c) |
  InfixExpression [c] (Expression c) String (Expression c) |
  InitializeValue [c] TypeInstance (ParamSet GeneralInstance) (ParamSet (Expression c))
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

-- TODO: Add character, binary, octal.
data ValueLiteral c =
  StringLiteral [c] String |
  IntegerLiteral [c] String |
  HexLiteral [c] String |
  DecimalLiteral [c] String String |
  BoolLiteral [c] Bool |
  EmptyLiteral [c]
  deriving (Show)

getValueLiteralContext :: ValueLiteral c -> [c]
getValueLiteralContext (StringLiteral c _)    = c
getValueLiteralContext (IntegerLiteral c _)   = c
getValueLiteralContext (HexLiteral c _)       = c
getValueLiteralContext (DecimalLiteral c _ _) = c
getValueLiteralContext (BoolLiteral c _)      = c
getValueLiteralContext (EmptyLiteral c)       = c

data ValueOperation c =
  ConvertedCall [c] TypeInstance (FunctionCall c) |
  ValueCall [c] (FunctionCall c)
  deriving (Show)
