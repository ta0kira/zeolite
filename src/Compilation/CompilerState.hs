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

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compilation.CompilerState (
  CleanupSetup(..),
  CompilerContext(..),
  CompiledData(..),
  CompilerState,
  ExpressionType,
  LoopSetup(..),
  MemberValue(..),
  ReturnVariable(..),
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
  csIsNamedReturns,
  csIsUnreachable,
  csPrimNamedReturns,
  csPushCleanup,
  csRegisterReturn,
  csRequiresTypes,
  csResolver,
  csSameType,
  csSetNoReturn,
  csSetNoTrace,
  csStartLoop,
  csUpdateAssigned,
  csWrite,
  getCleanContext,
  resetBackgroundStateT,
  reviseErrorStateT,
  runDataCompiler,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..),execStateT,get,mapStateT,put)
import Data.Foldable
import Data.Functor
import Data.Monoid
import Prelude hiding (foldr)
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Types.DefinedCategory
import Types.Positional
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


type CompilerState a m = StateT a m

class (Functor m, Monad m) => CompilerContext c m s a | a -> c s where
  ccCurrentScope :: a -> m SymbolScope
  ccResolver :: a -> m AnyTypeResolver
  ccSameType :: a -> TypeInstance -> m Bool
  ccAllFilters :: a -> m ParamFilters
  ccGetParamScope :: a -> ParamName -> m SymbolScope
  ccRequiresTypes :: a -> Set.Set CategoryName -> m a
  ccGetRequired :: a -> m (Set.Set CategoryName)
  ccGetCategoryFunction :: a -> [c] -> Maybe CategoryName -> FunctionName -> m (ScopedFunction c)
  ccGetTypeFunction :: a -> [c] -> Maybe GeneralInstance -> FunctionName -> m (ScopedFunction c)
  ccCheckValueInit :: a -> [c] -> TypeInstance -> ExpressionType -> Positional GeneralInstance -> m ()
  ccGetVariable :: a -> [c] -> VariableName -> m (VariableValue c)
  ccAddVariable :: a -> [c] -> VariableName -> VariableValue c -> m a
  ccCheckVariableInit :: a -> [c] -> VariableName -> m ()
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccClearOutput :: a -> m a
  ccUpdateAssigned :: a -> VariableName -> m a
  ccInheritReturns :: a -> [a] -> m a
  ccRegisterReturn :: a -> [c] -> Maybe ExpressionType -> m a
  ccPrimNamedReturns :: a -> m [ReturnVariable]
  ccIsUnreachable :: a -> m Bool
  ccIsNamedReturns :: a -> m Bool
  ccSetNoReturn :: a -> m a
  ccStartLoop :: a -> LoopSetup s -> m a
  ccGetLoop :: a -> m (LoopSetup s)
  ccPushCleanup :: a -> CleanupSetup a s -> m a
  ccGetCleanup :: a -> m (CleanupSetup a s)
  ccExprLookup :: a -> [c] -> String -> m (Expression c)
  ccSetNoTrace :: a -> Bool -> m a
  ccGetNoTrace :: a -> m Bool

type ExpressionType = Positional ValueType

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

data CleanupSetup a s =
  CleanupSetup {
    csReturnContext :: [a],
    csCleanup :: s
  }

instance Show c => Show (VariableValue c) where
  show (VariableValue c _ t _) = show t ++ formatFullContextBrace c

reviseErrorStateT :: (CompileErrorM m) => CompilerState a m b -> String -> CompilerState a m b
reviseErrorStateT x s = mapStateT (`reviseErrorM` s) x

resetBackgroundStateT :: (CompileErrorM m) => CompilerState a m b -> CompilerState a m b
resetBackgroundStateT x = mapStateT resetBackgroundM x

csCurrentScope :: CompilerContext c m s a => CompilerState a m SymbolScope
csCurrentScope = fmap ccCurrentScope get >>= lift

csResolver :: CompilerContext c m s a => CompilerState a m AnyTypeResolver
csResolver = fmap ccResolver get >>= lift

csSameType :: CompilerContext c m s a => TypeInstance -> CompilerState a m Bool
csSameType t = fmap (\x -> ccSameType x t) get >>= lift

csAllFilters :: CompilerContext c m s a => CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csGetParamScope :: CompilerContext c m s a => ParamName -> CompilerState a m SymbolScope
csGetParamScope n = fmap (\x -> ccGetParamScope x n) get >>= lift

csRequiresTypes :: CompilerContext c m s a => Set.Set CategoryName -> CompilerState a m ()
csRequiresTypes ns = fmap (\x -> ccRequiresTypes x ns) get >>= lift >>= put

csGetRequired :: CompilerContext c m s a => CompilerState a m (Set.Set CategoryName)
csGetRequired = fmap ccGetRequired get >>= lift

csGetCategoryFunction :: CompilerContext c m s a =>
  [c] -> Maybe CategoryName -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetCategoryFunction c t n = fmap (\x -> ccGetCategoryFunction x c t n) get >>= lift

csGetTypeFunction :: CompilerContext c m s a =>
  [c] -> Maybe GeneralInstance -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetTypeFunction c t n = fmap (\x -> ccGetTypeFunction x c t n) get >>= lift

csCheckValueInit :: CompilerContext c m s a =>
  [c] -> TypeInstance -> ExpressionType -> Positional GeneralInstance -> CompilerState a m ()
csCheckValueInit c t as ps = fmap (\x -> ccCheckValueInit x c t as ps) get >>= lift

csGetVariable :: CompilerContext c m s a =>
  [c] -> VariableName -> CompilerState a m (VariableValue c)
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: CompilerContext c m s a =>
  [c] -> VariableName -> VariableValue c -> CompilerState a m ()
csAddVariable c n t = fmap (\x -> ccAddVariable x c n t) get >>= lift >>= put

csCheckVariableInit :: CompilerContext c m s a =>
  [c] -> VariableName -> CompilerState a m ()
csCheckVariableInit c n = fmap (\x -> ccCheckVariableInit x c n) get >>= lift

csWrite :: CompilerContext c m s a => s -> CompilerState a m ()
csWrite o = fmap (\x -> ccWrite x o) get >>= lift >>= put

csClearOutput :: CompilerContext c m s a => CompilerState a m ()
csClearOutput = fmap (\x -> ccClearOutput x) get >>= lift >>= put

csGetOutput :: CompilerContext c m s a => CompilerState a m s
csGetOutput = fmap ccGetOutput get >>= lift

csUpdateAssigned :: CompilerContext c m s a => VariableName -> CompilerState a m ()
csUpdateAssigned n = fmap (\x -> ccUpdateAssigned x n) get >>= lift >>= put

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

csSetNoReturn :: CompilerContext c m s a => CompilerState a m ()
csSetNoReturn = fmap ccSetNoReturn get >>= lift >>= put

csStartLoop :: CompilerContext c m s a => LoopSetup s -> CompilerState a m ()
csStartLoop l = fmap (\x -> ccStartLoop x l) get >>= lift >>= put

csGetLoop :: CompilerContext c m s a => CompilerState a m (LoopSetup s)
csGetLoop = fmap ccGetLoop get >>= lift

csPushCleanup :: CompilerContext c m s a => CleanupSetup a s -> CompilerState a m ()
csPushCleanup l = fmap (\x -> ccPushCleanup x l) get >>= lift >>= put

csGetCleanup :: CompilerContext c m s a => CompilerState a m (CleanupSetup a s)
csGetCleanup = fmap ccGetCleanup get >>= lift

csExprLookup :: CompilerContext c m s a => [c] -> String -> CompilerState a m (Expression c)
csExprLookup c n = fmap (\x -> ccExprLookup x c n) get >>= lift

csSetNoTrace :: CompilerContext c m s a => Bool -> CompilerState a m ()
csSetNoTrace t = fmap (\x -> ccSetNoTrace x t) get >>= lift >>= put

csGetNoTrace :: CompilerContext c m s a => CompilerState a m Bool
csGetNoTrace = fmap ccGetNoTrace get >>= lift

data CompiledData s =
  CompiledData {
    cdRequired :: Set.Set CategoryName,
    cdOutput :: s
  }

instance Monoid s => Mergeable (CompiledData s) where
  mergeAny ds = CompiledData req out where
    flat = foldr (:) [] ds
    req = Set.unions $ map cdRequired flat
    out = foldr (<>) mempty $ map cdOutput flat
  mergeAll ds = CompiledData req out where
    flat = foldr (:) [] ds
    req = Set.unions $ map cdRequired flat
    out = foldr (<>) mempty $ map cdOutput flat

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

getCleanContext :: CompilerContext c m s a => CompilerState a m a
getCleanContext = get >>= lift . ccClearOutput
