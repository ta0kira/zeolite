{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompilerState (
  Compiler(..),
  CompilerContext(..),
  CompilerState(..),
  csAddVariable,
  csAllFilters,
  csCheckReturn,
  csGetFunction,
  csGetOutput,
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
  ccResolver :: a -> m (TypeResolver m ())
  ccAllFilters :: a -> m ParamFilters
  ccRequiresType :: a -> TypeName -> m a
  ccGetRequired :: a -> m (Set.Set TypeName)
  ccGetFunction :: a -> [c] -> FunctionName -> Maybe GeneralInstance -> m (ScopedFunction c)
  ccGetVariable :: a -> [c] -> VariableName -> m (PassedValue c)
  ccAddVariable :: a -> [c] -> VariableName -> PassedValue c -> m a
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccUpdateAssigned :: a -> VariableName -> m a
  ccCheckReturn :: a -> [c] -> ParamSet ValueType -> m ()

csResolver :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (TypeResolver m ())
csResolver = fmap ccResolver get >>= lift

csAllFilters :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csRequiresType :: (Monad m, CompilerContext c m s a) =>
  TypeName -> CompilerState a m ()
csRequiresType n = fmap (\x -> ccRequiresType x n) get >>= lift >>= put

csGetRequired :: (Monad m, CompilerContext c m s a) => CompilerState a m (Set.Set TypeName)
csGetRequired = fmap ccGetRequired get >>= lift

csGetFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> FunctionName -> Maybe GeneralInstance -> CompilerState a m (ScopedFunction c)
csGetFunction c n t = fmap (\x -> ccGetFunction x c n t) get >>= lift

csGetVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> CompilerState a m (PassedValue c)
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> PassedValue c -> CompilerState a m ()
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
