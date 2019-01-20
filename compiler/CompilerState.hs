{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompilerState (
  Compiler(..),
  CompilerContext(..),
  CompilerState(..),
  VariableValue(..),
  csAddVariable,
  csAllFilters,
  csCurrentScope,
  csCheckReturn,
  csGetFunction,
  csGetOutput,
  csGetParamScope,
  csGetVariable,
  csResolver,
  csUpdateAssigned,
  csWrite,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..),get,put)
import qualified Data.Set as Set

import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


type CompilerState a m = StateT a m

class Compiler a m b where
  compile :: b -> CompilerState a m ()

class Monad m => CompilerContext c m s a | a -> c s where
  ccCurrentScope :: a -> m SymbolScope
  ccResolver :: a -> m (TypeResolver m ())
  ccAllFilters :: a -> m ParamFilters
  ccGetParamScope :: a -> ParamName -> m SymbolScope
  ccRequiresType :: a -> TypeName -> m a
  ccGetRequired :: a -> m (Set.Set TypeName)
  ccGetFunction :: a -> [c] -> FunctionName -> Maybe GeneralInstance -> m (ScopedFunction c)
  ccGetVariable :: a -> [c] -> VariableName -> m (VariableValue c)
  ccAddVariable :: a -> [c] -> VariableName -> VariableValue c -> m a
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccUpdateAssigned :: a -> VariableName -> m a
  ccCheckReturn :: a -> [c] -> ParamSet ValueType -> m ()

data VariableValue c =
  VariableValue {
    vvContext :: [c],
    vvScope :: SymbolScope,
    vvType :: ValueType
  }
  deriving (Eq)

instance Show c => Show (VariableValue c) where
  show (VariableValue c _ t) = show t ++ " [" ++ formatFullContext c ++ "]"

csCurrentScope :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m SymbolScope
csCurrentScope = fmap ccCurrentScope get >>= lift

csResolver :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (TypeResolver m ())
csResolver = fmap ccResolver get >>= lift

csAllFilters :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csGetParamScope :: (Monad m, CompilerContext c m s a) =>
  ParamName -> CompilerState a m SymbolScope
csGetParamScope n = fmap (\x -> ccGetParamScope x n) get >>= lift

csRequiresType :: (Monad m, CompilerContext c m s a) =>
  TypeName -> CompilerState a m ()
csRequiresType n = fmap (\x -> ccRequiresType x n) get >>= lift >>= put

csGetRequired :: (Monad m, CompilerContext c m s a) => CompilerState a m (Set.Set TypeName)
csGetRequired = fmap ccGetRequired get >>= lift

csGetFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> FunctionName -> Maybe GeneralInstance -> CompilerState a m (ScopedFunction c)
csGetFunction c n t = fmap (\x -> ccGetFunction x c n t) get >>= lift

csGetVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> CompilerState a m (VariableValue c)
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> VariableValue c -> CompilerState a m ()
csAddVariable c n t = fmap (\x -> ccAddVariable x c n t) get >>= lift >>= put

csWrite :: (Monad m, CompilerContext c m s a) => s -> CompilerState a m ()
csWrite o = fmap (\x -> ccWrite x o) get >>= lift >>= put

csGetOutput :: (Monad m, CompilerContext c m s a) => CompilerState a m s
csGetOutput = fmap ccGetOutput get >>= lift

csCheckReturn :: (Monad m, CompilerContext c m s a) =>
  [c] -> ParamSet ValueType -> CompilerState a m ()
csCheckReturn c rs = fmap (\x -> ccCheckReturn x c rs) get >>= lift

csUpdateAssigned :: (Monad m, CompilerContext c m s a) =>
  VariableName -> CompilerState a m ()
csUpdateAssigned n = fmap (\x -> ccUpdateAssigned x n) get >>= lift >>= put
