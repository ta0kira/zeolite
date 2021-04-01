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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compilation.CompilerState (
  CleanupBlock(..),
  CompilerContext(..),
  CompiledData(..),
  CompilerState,
  LoopSetup(..),
  JumpType(..),
  MemberValue(..),
  ReturnVariable(..),
  UsedVariable(..),
  autoSelfType,
  concatM,
  csAddRequired,
  csAddUsed,
  csAddVariable,
  csAllFilters,
  csCheckValueInit,
  csCheckVariableInit,
  csClearOutput,
  csCurrentScope,
  csExprLookup,
  csGetCategoryFunction,
  csGetCleanup,
  csGetLoop,
  csGetNoTrace,
  csGetOutput,
  csGetParamScope,
  csGetTypeFunction,
  csGetVariable,
  csInheritReturns,
  csInheritUsed,
  csIsNamedReturns,
  csIsUnreachable,
  csPrimNamedReturns,
  csPushCleanup,
  csRegisterReturn,
  csReleaseExprMacro,
  csReserveExprMacro,
  csResolver,
  csSameType,
  csSelfType,
  csSetHidden,
  csSetJumpType,
  csSetNoTrace,
  csSetReadOnly,
  csStartCleanup,
  csStartLoop,
  csUpdateAssigned,
  csWrite,
  emptyCleanupBlock,
  getCleanContext,
  runDataCompiler,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..),execStateT,get,put)
import Data.Functor
import Prelude hiding (foldr)
import qualified Data.Set as Set

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

import Base.CompilerError
import Base.GeneralType
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


type CompilerState a m = StateT a m

class (Functor m, Monad m) => CompilerContext c m s a | a -> c s where
  ccCurrentScope :: a -> m SymbolScope
  ccResolver :: a -> m AnyTypeResolver
  ccSameType :: a -> TypeInstance -> m Bool
  ccSelfType :: a -> m TypeInstance
  ccAllFilters :: a -> m ParamFilters
  ccGetParamScope :: a -> ParamName -> m SymbolScope
  ccAddRequired :: a -> Set.Set CategoryName -> m a
  ccGetRequired :: a -> m (Set.Set CategoryName)
  ccGetCategoryFunction :: a -> [c] -> Maybe CategoryName -> FunctionName -> m (ScopedFunction c)
  ccGetTypeFunction :: a -> [c] -> Maybe GeneralInstance -> FunctionName -> m (ScopedFunction c)
  ccCheckValueInit :: a -> [c] -> TypeInstance -> ExpressionType -> m ()
  ccGetVariable :: a -> UsedVariable c -> m (VariableValue c)
  ccAddVariable :: a -> UsedVariable c -> VariableValue c -> m a
  ccSetReadOnly :: a -> UsedVariable c -> m a
  ccSetHidden :: a -> UsedVariable c -> m a
  ccCheckVariableInit :: a -> [UsedVariable c] -> m ()
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccClearOutput :: a -> m a
  ccUpdateAssigned :: a -> VariableName -> m a
  ccAddUsed :: a -> UsedVariable c -> m a
  ccInheritUsed :: a -> a -> m a
  ccInheritReturns :: a -> [a] -> m a
  ccRegisterReturn :: a -> [c] -> Maybe ExpressionType -> m a
  ccPrimNamedReturns :: a -> m [ReturnVariable]
  ccIsUnreachable :: a -> m Bool
  ccIsNamedReturns :: a -> m Bool
  ccSetJumpType :: a -> [c] -> JumpType -> m a
  ccStartLoop :: a -> LoopSetup s -> m a
  ccGetLoop :: a -> m (LoopSetup s)
  ccStartCleanup :: a -> [c] -> m a
  ccPushCleanup :: a -> a -> m a
  ccGetCleanup :: a -> JumpType -> m (CleanupBlock c s)
  ccExprLookup :: a -> [c] -> MacroName -> m (Expression c)
  ccReserveExprMacro :: a -> [c] -> MacroName -> m a
  ccReleaseExprMacro :: a -> [c] -> MacroName -> m a
  ccSetNoTrace :: a -> Bool -> m a
  ccGetNoTrace :: a -> m Bool

data MemberValue c =
  MemberValue {
    mvContext :: [c],
    mvName :: VariableName,
    mvType :: ValueType
  }
  deriving (Show)

data ReturnVariable =
  ReturnVariable {
    rvIndex :: Int,
    rvName :: VariableName,
    rvType :: ValueType
  }
  deriving (Show)

data LoopSetup s =
  LoopSetup {
    lsUpdate :: s
  } |
  NotInLoop

data UsedVariable c =
  UsedVariable {
    uvContext :: [c],
    uvName :: VariableName
  }
  deriving (Eq,Ord,Show)

data CleanupBlock c s =
  CleanupBlock {
    csCleanup :: s,
    csUsesVars :: [UsedVariable c],
    csJumpType :: JumpType,
    csRequires :: Set.Set CategoryName
  }
  deriving (Show)

emptyCleanupBlock :: Monoid s => CleanupBlock c s
emptyCleanupBlock = CleanupBlock mempty [] NextStatement Set.empty

data JumpType =
  NextStatement |
  JumpContinue |
  JumpBreak |
  JumpReturn |
  JumpFailCall |
  JumpMax  -- Max value for use as initial state in folds.
  deriving (Eq,Ord,Show)

csCurrentScope :: CompilerContext c m s a => CompilerState a m SymbolScope
csCurrentScope = fmap ccCurrentScope get >>= lift

csResolver :: CompilerContext c m s a => CompilerState a m AnyTypeResolver
csResolver = fmap ccResolver get >>= lift

csSameType :: CompilerContext c m s a => TypeInstance -> CompilerState a m Bool
csSameType t = fmap (\x -> ccSameType x t) get >>= lift

csSelfType :: CompilerContext c m s a => CompilerState a m TypeInstance
csSelfType = fmap ccSelfType get >>= lift

csAllFilters :: CompilerContext c m s a => CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csGetParamScope :: CompilerContext c m s a => ParamName -> CompilerState a m SymbolScope
csGetParamScope n = fmap (\x -> ccGetParamScope x n) get >>= lift

csAddRequired :: CompilerContext c m s a => Set.Set CategoryName -> CompilerState a m ()
csAddRequired ns = fmap (\x -> ccAddRequired x ns) get >>= lift >>= put

csGetRequired :: CompilerContext c m s a => CompilerState a m (Set.Set CategoryName)
csGetRequired = fmap ccGetRequired get >>= lift

csGetCategoryFunction :: CompilerContext c m s a =>
  [c] -> Maybe CategoryName -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetCategoryFunction c t n = fmap (\x -> ccGetCategoryFunction x c t n) get >>= lift

csGetTypeFunction :: CompilerContext c m s a =>
  [c] -> Maybe GeneralInstance -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetTypeFunction c t n = fmap (\x -> ccGetTypeFunction x c t n) get >>= lift

csCheckValueInit :: CompilerContext c m s a =>
  [c] -> TypeInstance -> ExpressionType -> CompilerState a m ()
csCheckValueInit c t as = fmap (\x -> ccCheckValueInit x c t as) get >>= lift

csGetVariable :: CompilerContext c m s a =>
  UsedVariable c -> CompilerState a m (VariableValue c)
csGetVariable v = fmap (\x -> ccGetVariable x v) get >>= lift

csAddVariable :: CompilerContext c m s a =>
  UsedVariable c -> VariableValue c -> CompilerState a m ()
csAddVariable v t = fmap (\x -> ccAddVariable x v t) get >>= lift >>= put

csSetReadOnly :: CompilerContext c m s a =>
  UsedVariable c -> CompilerState a m ()
csSetReadOnly v = fmap (\x -> ccSetReadOnly x v) get >>= lift >>= put

csSetHidden :: CompilerContext c m s a =>
  UsedVariable c -> CompilerState a m ()
csSetHidden v = fmap (\x -> ccSetHidden x v) get >>= lift >>= put

csCheckVariableInit :: CompilerContext c m s a =>
  [UsedVariable c] -> CompilerState a m ()
csCheckVariableInit vs = fmap (\x -> ccCheckVariableInit x vs) get >>= lift

csWrite :: CompilerContext c m s a => s -> CompilerState a m ()
csWrite o = fmap (\x -> ccWrite x o) get >>= lift >>= put

csClearOutput :: CompilerContext c m s a => CompilerState a m ()
csClearOutput = fmap (\x -> ccClearOutput x) get >>= lift >>= put

csGetOutput :: CompilerContext c m s a => CompilerState a m s
csGetOutput = fmap ccGetOutput get >>= lift

csUpdateAssigned :: CompilerContext c m s a => VariableName -> CompilerState a m ()
csUpdateAssigned n = fmap (\x -> ccUpdateAssigned x n) get >>= lift >>= put

csAddUsed :: CompilerContext c m s a => UsedVariable c -> CompilerState a m ()
csAddUsed n = fmap (\x -> ccAddUsed x n) get >>= lift >>= put

csInheritUsed :: CompilerContext c m s a => a -> CompilerState a m ()
csInheritUsed c = fmap (\x -> ccInheritUsed x c) get >>= lift >>= put

csInheritReturns :: CompilerContext c m s a => [a] -> CompilerState a m ()
csInheritReturns xs = fmap (\x -> ccInheritReturns x xs) get >>= lift >>= put

csRegisterReturn :: CompilerContext c m s a =>
  [c] -> Maybe ExpressionType -> CompilerState a m ()
csRegisterReturn c rs = fmap (\x -> ccRegisterReturn x c rs) get >>= lift >>= put

csPrimNamedReturns :: CompilerContext c m s a => CompilerState a m [ReturnVariable]
csPrimNamedReturns = fmap ccPrimNamedReturns get >>= lift

csIsUnreachable :: CompilerContext c m s a => CompilerState a m Bool
csIsUnreachable = fmap ccIsUnreachable get >>= lift

csIsNamedReturns :: CompilerContext c m s a => CompilerState a m Bool
csIsNamedReturns = fmap ccIsNamedReturns get >>= lift

csSetJumpType :: CompilerContext c m s a => [c] -> JumpType -> CompilerState a m ()
csSetJumpType c j = fmap (\x -> ccSetJumpType x c j) get >>= lift >>= put

csStartLoop :: CompilerContext c m s a => LoopSetup s -> CompilerState a m ()
csStartLoop l = fmap (\x -> ccStartLoop x l) get >>= lift >>= put

csStartCleanup :: CompilerContext c m s a => [c] -> CompilerState a m ()
csStartCleanup c = fmap (\x -> ccStartCleanup x c) get >>= lift >>= put

csGetLoop :: CompilerContext c m s a => CompilerState a m (LoopSetup s)
csGetLoop = fmap ccGetLoop get >>= lift

csPushCleanup :: CompilerContext c m s a => a -> CompilerState a m ()
csPushCleanup l = fmap (\x -> ccPushCleanup x l) get >>= lift >>= put

csGetCleanup :: CompilerContext c m s a => JumpType -> CompilerState a m (CleanupBlock c s)
csGetCleanup j = fmap (\x -> ccGetCleanup x j) get >>= lift

csExprLookup :: CompilerContext c m s a => [c] -> MacroName -> CompilerState a m (Expression c)
csExprLookup c n = fmap (\x -> ccExprLookup x c n) get >>= lift

csReserveExprMacro :: CompilerContext c m s a => [c] -> MacroName -> CompilerState a m ()
csReserveExprMacro c n = fmap (\x -> ccReserveExprMacro x c n) get >>= lift >>= put

csReleaseExprMacro :: CompilerContext c m s a => [c] -> MacroName -> CompilerState a m ()
csReleaseExprMacro c n = fmap (\x -> ccReleaseExprMacro x c n) get >>= lift >>= put

csSetNoTrace :: CompilerContext c m s a => Bool -> CompilerState a m ()
csSetNoTrace t = fmap (\x -> ccSetNoTrace x t) get >>= lift >>= put

csGetNoTrace :: CompilerContext c m s a => CompilerState a m Bool
csGetNoTrace = fmap ccGetNoTrace get >>= lift

data CompiledData s =
  CompiledData {
    cdRequired :: Set.Set CategoryName,
    cdOutput :: s
  }

instance Semigroup s => Semigroup (CompiledData s) where
  (CompiledData r1 s1) <> (CompiledData r2 s2) =
    CompiledData (r1 `Set.union` r2) (s1 <> s2)

instance (Semigroup s, Monoid s) => Monoid (CompiledData s) where
  mempty = CompiledData Set.empty mempty
  mappend = (<>)

runDataCompiler :: CompilerContext c m s a =>
  CompilerState a m b -> a -> m (CompiledData s)
runDataCompiler x ctx = do
  ctx' <- execStateT x ctx
  required <- ccGetRequired ctx'
  output <- ccGetOutput ctx'
  return $ CompiledData {
      cdRequired = required,
      cdOutput = output
    }

concatM :: (Semigroup s, Monoid s, CollectErrorsM m) => [m (CompiledData s)] -> m (CompiledData s)
concatM = fmap mconcat . collectAllM

getCleanContext :: CompilerContext c m s a => CompilerState a m a
getCleanContext = get >>= lift . ccClearOutput

autoSelfType :: CompilerContext c m s a => CompilerState a m GeneralInstance
autoSelfType = do
  scope <- csCurrentScope
  case scope of
       CategoryScope -> return selfType
       _ -> fmap (singleType . JustTypeInstance) csSelfType
