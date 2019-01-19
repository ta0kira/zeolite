{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompilerState (
  Compiler(..),
  CompilerContext(..),
  OutputType(..),
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

class Compiler a b where
  compile :: b -> CompilerState a m ()

class Monad m => CompilerContext c m s a | a -> c s where
  ccResolver :: a -> m (TypeResolver m ())
  ccAllFilters :: a -> m ParamFilters
  ccRequiresType :: a -> TypeName -> m a
  ccGetFunction :: a -> [c] -> FunctionName -> Maybe GeneralInstance -> m (ScopedFunction c)
  ccGetVariable :: a -> [c] -> VariableName -> m (OutputType c)
  ccAddVariable :: a -> [c] -> VariableName -> OutputType c -> m a
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccUpdateAssigned :: a -> VariableName -> m a
  ccCheckReturn :: a -> [c] -> ParamSet ValueType -> m ()

data OutputType c =
  OutputType {
    otContext :: [c],
    otType :: ValueType
  }
  deriving (Eq,Ord)

instance Show c => Show (OutputType c) where
  show (OutputType c v) = show v ++ " /*" ++ formatFullContext c ++ "*/"

csResolver :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (TypeResolver m ())
csResolver = fmap ccResolver get >>= lift

csAllFilters :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csRequiresType :: (Monad m, CompilerContext c m s a) =>
  TypeName -> CompilerState a m ()
csRequiresType n = fmap (\x -> ccRequiresType x n) get >>= lift >>= put

csGetFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> FunctionName -> Maybe GeneralInstance -> CompilerState a m (ScopedFunction c)
csGetFunction c n t = fmap (\x -> ccGetFunction x c n t) get >>= lift

csGetVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> CompilerState a m (OutputType c)
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: (Monad m, CompilerContext c m s a) =>
  [c] -> VariableName -> OutputType c -> CompilerState a m ()
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
