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

module CompilerState (
  CleanupSetup(..),
  CompilerContext(..),
  CompiledData(..),
  CompilerState(..),
  ExpressionType(..),
  LoopSetup(..),
  MemberValue(..),
  ReturnVariable(..),
  csAddVariable,
  csAllFilters,
  csCheckValueInit,
  csCheckVariableInit,
  csClearOutput,
  csCurrentScope,
  csGetCategoryFunction,
  csGetCleanup,
  csGetLoop,
  csGetOutput,
  csGetParamScope,
  csGetTypeFunction,
  csGetVariable,
  csInheritReturns,
  csIsUnreachable,
  csPrimNamedReturns,
  csPushCleanup,
  csRegisterReturn,
  csRequiresTypes,
  csResolver,
  csSameType,
  csSetNoReturn,
  csStartLoop,
  csUpdateAssigned,
  csWrite,
  getCleanContext,
  reviseErrorStateT,
  runDataCompiler,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..),execStateT,get,mapStateT,put)
import Data.Monoid
import qualified Data.Set as Set

import DefinedCategory
import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


type CompilerState a m = StateT a m

class Monad m => CompilerContext c m s a | a -> c s where
  ccCurrentScope :: a -> m SymbolScope
  ccResolver :: a -> m (TypeResolver m)
  ccSameType :: a -> TypeInstance -> m Bool
  ccAllFilters :: a -> m ParamFilters
  ccGetParamScope :: a -> ParamName -> m SymbolScope
  ccRequiresTypes :: a -> Set.Set CategoryName -> m a
  ccGetRequired :: a -> m (Set.Set CategoryName)
  ccGetCategoryFunction :: a -> [c] -> Maybe CategoryName -> FunctionName -> m (ScopedFunction c)
  ccGetTypeFunction :: a -> [c] -> Maybe GeneralInstance -> FunctionName -> m (ScopedFunction c)
  ccCheckValueInit :: a -> [c] -> TypeInstance -> ParamSet ValueType -> ParamSet GeneralInstance -> m ()
  ccGetVariable :: a -> [c] -> VariableName -> m (VariableValue c)
  ccAddVariable :: a -> [c] -> VariableName -> VariableValue c -> m a
  ccCheckVariableInit :: a -> [c] -> VariableName -> m ()
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccClearOutput :: a -> m a
  ccUpdateAssigned :: a -> VariableName -> m a
  ccInheritReturns :: a -> [a] -> m a
  ccRegisterReturn :: a -> [c] -> ExpressionType -> m a
  ccPrimNamedReturns :: a -> m [ReturnVariable]
  ccIsUnreachable :: a -> m Bool
  ccSetNoReturn :: a -> m a
  ccStartLoop :: a -> LoopSetup s -> m a
  ccGetLoop :: a -> m (LoopSetup s)
  ccPushCleanup :: a -> CleanupSetup a s -> m a
  ccGetCleanup :: a -> m (CleanupSetup a s)

type ExpressionType = ParamSet ValueType

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
  show (VariableValue c _ t _) = show t ++ " [" ++ formatFullContext c ++ "]"

reviseErrorStateT :: (CompileErrorM m) => CompilerState a m b -> String -> CompilerState a m b
reviseErrorStateT x s = mapStateT (`reviseError` s) x

csCurrentScope :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m SymbolScope
csCurrentScope = fmap ccCurrentScope get >>= lift

csResolver :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (TypeResolver m)
csResolver = fmap ccResolver get >>= lift

csSameType :: (Monad m, CompilerContext c m s a) =>
  TypeInstance -> CompilerState a m Bool
csSameType t = fmap (\x -> ccSameType x t) get >>= lift

csAllFilters :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csGetParamScope :: (Monad m, CompilerContext c m s a) =>
  ParamName -> CompilerState a m SymbolScope
csGetParamScope n = fmap (\x -> ccGetParamScope x n) get >>= lift

csRequiresTypes :: (Monad m, CompilerContext c m s a) =>
  Set.Set CategoryName -> CompilerState a m ()
csRequiresTypes ns = fmap (\x -> ccRequiresTypes x ns) get >>= lift >>= put

csGetRequired :: (Monad m, CompilerContext c m s a) => CompilerState a m (Set.Set CategoryName)
csGetRequired = fmap ccGetRequired get >>= lift

csGetCategoryFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> Maybe CategoryName -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetCategoryFunction c t n = fmap (\x -> ccGetCategoryFunction x c t n) get >>= lift

csGetTypeFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> Maybe GeneralInstance -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetTypeFunction c t n = fmap (\x -> ccGetTypeFunction x c t n) get >>= lift

csCheckValueInit :: (Monad m, CompilerContext c m s a) =>
  [c] -> TypeInstance -> ParamSet ValueType -> ParamSet GeneralInstance -> CompilerState a m ()
csCheckValueInit c t as ps = fmap (\x -> ccCheckValueInit x c t as ps) get >>= lift

csGetVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> CompilerState a m (VariableValue c)
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> VariableValue c -> CompilerState a m ()
csAddVariable c n t = fmap (\x -> ccAddVariable x c n t) get >>= lift >>= put

csCheckVariableInit :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> CompilerState a m ()
csCheckVariableInit c n = fmap (\x -> ccCheckVariableInit x c n) get >>= lift

csWrite :: (Monad m, CompilerContext c m s a) => s -> CompilerState a m ()
csWrite o = fmap (\x -> ccWrite x o) get >>= lift >>= put

csClearOutput :: (Monad m, CompilerContext c m s a) => CompilerState a m ()
csClearOutput = fmap (\x -> ccClearOutput x) get >>= lift >>= put

csGetOutput :: (Monad m, CompilerContext c m s a) => CompilerState a m s
csGetOutput = fmap ccGetOutput get >>= lift

csUpdateAssigned :: (Monad m, CompilerContext c m s a) =>
  VariableName -> CompilerState a m ()
csUpdateAssigned n = fmap (\x -> ccUpdateAssigned x n) get >>= lift >>= put

csInheritReturns :: (Monad m, CompilerContext c m s a) =>
  [a] -> CompilerState a m ()
csInheritReturns xs = fmap (\x -> ccInheritReturns x xs) get >>= lift >>= put

csRegisterReturn :: (Monad m, CompilerContext c m s a) =>
  [c] -> ParamSet ValueType -> CompilerState a m ()
csRegisterReturn c rs = fmap (\x -> ccRegisterReturn x c rs) get >>= lift >>= put

csPrimNamedReturns :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m [ReturnVariable]
csPrimNamedReturns = fmap ccPrimNamedReturns get >>= lift

csIsUnreachable :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m Bool
csIsUnreachable = fmap ccIsUnreachable get >>= lift

csSetNoReturn :: (Monad m, CompilerContext c m s a) => CompilerState a m ()
csSetNoReturn = fmap ccSetNoReturn get >>= lift >>= put

csStartLoop :: (Monad m, CompilerContext c m s a) =>
  LoopSetup s -> CompilerState a m ()
csStartLoop l = fmap (\x -> ccStartLoop x l) get >>= lift >>= put

csGetLoop :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (LoopSetup s)
csGetLoop = fmap ccGetLoop get >>= lift

csPushCleanup :: (Monad m, CompilerContext c m s a) =>
  CleanupSetup a s -> CompilerState a m ()
csPushCleanup l = fmap (\x -> ccPushCleanup x l) get >>= lift >>= put

csGetCleanup :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (CleanupSetup a s)
csGetCleanup = fmap ccGetCleanup get >>= lift

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

runDataCompiler :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m b -> a -> m (CompiledData s)
runDataCompiler x ctx = do
  ctx' <- execStateT x ctx
  required <- ccGetRequired ctx'
  output <- ccGetOutput ctx'
  return $ CompiledData {
      cdRequired = required,
      cdOutput = output
    }

getCleanContext :: (Monad m, CompilerContext c m s a) => CompilerState a m a
getCleanContext = get >>= lift . ccClearOutput
