{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerState (
  Compiler(..),
  CompilerContext,
  csAddVariable,
  csAllFilters,
  csFunction,
  csResolver,
  csGetVariable,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..),get,put)

import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


class Compiler c m a b where
  compile :: a -> CompilerState c m b

data CompilerContext c m p =
  CompilerContext {
    ccResolver :: TypeResolver m p,
    ccAllFilters :: ParamFilters,
    ccFunction :: [c] -> GeneralInstance -> FunctionName -> m (ScopedFunction c),
    ccGetVariable :: [c] -> VariableName c -> m ValueType,
    ccAddVariable :: [c] -> VariableName c -> ValueType -> m (CompilerContext c m p)
  }

type CompilerState c m = StateT (CompilerContext c m ()) m

csResolver :: Monad m => CompilerState c m (TypeResolver m ())
csResolver = fmap ccResolver get

csAllFilters :: Monad m => CompilerState c m ParamFilters
csAllFilters = fmap ccAllFilters get

csFunction :: Monad m => [c] -> GeneralInstance -> FunctionName ->
                         CompilerState c m (ScopedFunction c)
csFunction c t n = fmap (\x -> ccFunction x c t n) get >>= lift

csGetVariable :: Monad m => [c] -> VariableName c -> CompilerState c m ValueType
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: Monad m => [c] -> VariableName c -> ValueType -> CompilerState c m ()
csAddVariable c n t = fmap (\x -> ccAddVariable x c n t) get >>= lift >>= put
