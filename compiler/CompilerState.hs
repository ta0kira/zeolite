{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerState (
  Compiler(..),
  CompilerContext,
  csAddVariable,
  csAllFilters,
  csFunction,
  csGetOutput,
  csGetVariable,
  csRequiresAssign,
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


class Monad m => Compiler c m s a where
  compile :: a -> CompilerState c m s ()

data CompilerContext c m p s =
  CompilerContext {
    ccResolver :: TypeResolver m p,
    ccAllFilters :: ParamFilters,
    ccFunction :: [c] -> GeneralInstance -> FunctionName -> m (ScopedFunction c),
    ccGetVariable :: [c] -> VariableName -> m ValueType,
    ccAddVariable :: [c] -> VariableName -> ValueType -> m (CompilerContext c m p s),
    ccWrite :: s -> m (CompilerContext c m p s),
    ccGetOutput :: s,
    ccRequiresAssign :: Set.Set VariableName,
    ccUpdateAssigned :: VariableName -> m (CompilerContext c m p s)
  }

type CompilerState c m s = StateT (CompilerContext c m () s) m

csResolver :: Monad m => CompilerState c m s (TypeResolver m ())
csResolver = fmap ccResolver get

csAllFilters :: Monad m => CompilerState c m s ParamFilters
csAllFilters = fmap ccAllFilters get

csFunction :: Monad m => [c] -> GeneralInstance -> FunctionName ->
                         CompilerState c m s (ScopedFunction c)
csFunction c t n = fmap (\x -> ccFunction x c t n) get >>= lift

csGetVariable :: Monad m => [c] -> VariableName -> CompilerState c m s ValueType
csGetVariable c n = fmap (\x -> ccGetVariable x c n) get >>= lift

csAddVariable :: Monad m => [c] -> VariableName -> ValueType -> CompilerState c m s ()
csAddVariable c n t = fmap (\x -> ccAddVariable x c n t) get >>= lift >>= put

csWrite :: Monad m => s -> CompilerState c m s ()
csWrite o = fmap (\x -> ccWrite x o) get >>= lift >>= put

csGetOutput :: Monad m => CompilerState c m s s
csGetOutput = fmap ccGetOutput get

csRequiresAssign :: Monad m => CompilerState c m s (Set.Set VariableName)
csRequiresAssign = fmap ccRequiresAssign get

csUpdateAssigned :: Monad m => VariableName -> CompilerState c m s ()
csUpdateAssigned n = fmap (\x -> ccUpdateAssigned x n) get >>= lift >>= put
