{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompilerState (
  CompilerContext(..),
  CompiledData(..),
  CompilerState(..),
  ExpressionType(..),
  MemberValue(..),
  VariableValue(..),
  csAddVariable,
  csAllFilters,
  csCurrentScope,
  csGetCategoryFunction,
  csGetOutput,
  csGetParamScope,
  csGetTypeFunction,
  csGetValueInit,
  csGetVariable,
  csInheritReturns,
  csRegisterReturn,
  csRequiresTypes,
  csResolver,
  csUpdateAssigned,
  csWrite,
  reviseErrorStateT,
  runDataCompiler,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..),execStateT,get,mapStateT,put)
import Data.Monoid
import qualified Data.Set as Set

import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


type CompilerState a m = StateT a m

class Monad m => CompilerContext c m s a | a -> c s where
  ccCurrentScope :: a -> m SymbolScope
  ccResolver :: a -> m (TypeResolver m)
  ccAllFilters :: a -> m ParamFilters
  ccGetParamScope :: a -> ParamName -> m SymbolScope
  ccRequiresTypes :: a -> Set.Set TypeName -> m a
  ccGetRequired :: a -> m (Set.Set TypeName)
  ccGetCategoryFunction :: a -> [c] -> Maybe TypeName -> FunctionName -> m (ScopedFunction c)
  ccGetTypeFunction :: a -> [c] -> Maybe GeneralInstance -> FunctionName -> m (ScopedFunction c)
  ccGetValueInit :: a -> [c] -> TypeInstance -> m (ParamSet (MemberValue c))
  ccGetVariable :: a -> [c] -> VariableName -> m (VariableValue c)
  ccAddVariable :: a -> [c] -> VariableName -> VariableValue c -> m a
  ccWrite :: a -> s -> m a
  ccGetOutput :: a -> m s
  ccUpdateAssigned :: a -> VariableName -> m a
  ccInheritReturns :: a -> [a] -> m a
  ccRegisterReturn :: a -> [c] -> ExpressionType -> m a

type ExpressionType = ParamSet ValueType

data VariableValue c =
  VariableValue {
    vvContext :: [c],
    vvScope :: SymbolScope,
    vvType :: ValueType
  }

data MemberValue c =
  MemberValue {
    mvContext :: [c],
    mvName :: VariableName,
    mvType :: ValueType
  }
  deriving (Show)

instance Show c => Show (VariableValue c) where
  show (VariableValue c _ t) = show t ++ " [" ++ formatFullContext c ++ "]"

reviseErrorStateT :: (CompileErrorM m) => CompilerState a m b -> String -> CompilerState a m b
reviseErrorStateT x s = mapStateT (`reviseError` s) x

csCurrentScope :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m SymbolScope
csCurrentScope = fmap ccCurrentScope get >>= lift

csResolver :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m (TypeResolver m)
csResolver = fmap ccResolver get >>= lift

csAllFilters :: (Monad m, CompilerContext c m s a) =>
  CompilerState a m ParamFilters
csAllFilters = fmap ccAllFilters get >>= lift

csGetParamScope :: (Monad m, CompilerContext c m s a) =>
  ParamName -> CompilerState a m SymbolScope
csGetParamScope n = fmap (\x -> ccGetParamScope x n) get >>= lift

csRequiresTypes :: (Monad m, CompilerContext c m s a) =>
  Set.Set TypeName -> CompilerState a m ()
csRequiresTypes n = fmap (\x -> ccRequiresTypes x n) get >>= lift >>= put

csGetRequired :: (Monad m, CompilerContext c m s a) => CompilerState a m (Set.Set TypeName)
csGetRequired = fmap ccGetRequired get >>= lift

csGetCategoryFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> Maybe TypeName -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetCategoryFunction c t n = fmap (\x -> ccGetCategoryFunction x c t n) get >>= lift

csGetTypeFunction :: (Monad m, CompilerContext c m s a) =>
  [c] -> Maybe GeneralInstance -> FunctionName -> CompilerState a m (ScopedFunction c)
csGetTypeFunction c t n = fmap (\x -> ccGetTypeFunction x c t n) get >>= lift

csGetValueInit :: (Monad m, CompilerContext c m s a) =>
  [c] -> TypeInstance -> CompilerState a m (ParamSet (MemberValue c))
csGetValueInit c as = fmap (\x -> ccGetValueInit x c as) get >>= lift

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

csRegisterReturn :: (Monad m, CompilerContext c m s a) =>
  [c] -> ParamSet ValueType -> CompilerState a m ()
csRegisterReturn c rs = fmap (\x -> ccRegisterReturn x c rs) get >>= lift >>= put

csUpdateAssigned :: (Monad m, CompilerContext c m s a) =>
  VariableName -> CompilerState a m ()
csUpdateAssigned n = fmap (\x -> ccUpdateAssigned x n) get >>= lift >>= put

csInheritReturns :: (Monad m, CompilerContext c m s a) =>
  [a] -> CompilerState a m ()
csInheritReturns xs = fmap (\x -> ccInheritReturns x xs) get >>= lift >>= put

data CompiledData s =
  CompiledData {
    cdRequired :: Set.Set TypeName,
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
