{- -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module TypesBase (
  CompileError(..),
  CompileErrorM(..),
  GeneralType(..),
  Mergeable(..),
  MergeableM(..),
  MergeType(..),
  ParamSet(..),
  StorageType(..),
  SymbolScope(..),
  Variance(..),
  alwaysPairParams,
  processParamPairs,
  processParamPairsT,
  checkGeneralType,
  composeVariance,
  paramAllowsVariance,
  partitionByScope,
) where

import Control.Monad.Trans (MonadTrans(..))


class Mergeable a where
  mergeAny :: Foldable f => f a -> a
  mergeAll :: Foldable f => f a -> a
  mergeNested :: a -> a -> a
  mergeNested x y = mergeAll [x,y]
  mergeDefault :: a
  mergeDefault = mergeAll Nothing

class MergeableM m where
  mergeAnyM :: (Mergeable a, Foldable f) => f (m a) -> m a
  mergeAllM :: (Mergeable a, Foldable f) => f (m a) -> m a
  mergeNestedM :: Mergeable a => m a -> m a -> m a
  mergeNestedM x y = mergeAllM [x,y]
  mergeDefaultM :: Mergeable a => m a
  mergeDefaultM = mergeAllM Nothing

instance Mergeable () where
  mergeAny = const ()
  mergeAll = const ()

instance Mergeable Bool where
  mergeAny = any id
  mergeAll = all id

instance Mergeable [a] where
  mergeAny = foldr (++) []
  mergeAll = foldr (++) []

class CompileError a where
  compileError :: String -> a
  isCompileError :: a -> Bool
  reviseError :: a -> String -> a
  reviseError e _ = e
  compileWarning :: a -> String -> a
  compileWarning w _ = w

class CompileErrorM m where
  compileErrorM :: String -> m a
  isCompileErrorM :: m a -> Bool
  collectAllOrErrorM :: Foldable f => f (m a) -> m [a]
  collectOneOrErrorM :: Foldable f => f (m a) -> m a
  reviseErrorM :: m a -> String -> m a
  reviseErrorM e _ = e
  compileWarningM :: m a -> String -> m a
  compileWarningM w _ = w

instance CompileErrorM m => CompileError (m a) where
  compileError = compileErrorM
  isCompileError = isCompileErrorM
  reviseError = reviseErrorM

data MergeType =
  MergeUnion |
  MergeIntersect
  deriving (Eq,Ord)

data GeneralType a =
  SingleType {
    stType :: a
  } |
  TypeMerge {
    tmMerge :: MergeType,
    tmTypes :: [GeneralType a]
  }
  deriving (Eq,Ord)

checkGeneralType :: (MergeableM m, Mergeable c) => (a -> b -> m c) -> GeneralType a -> GeneralType b -> m c
checkGeneralType f ti1 ti2 = singleCheck ti1 ti2 where
  singleCheck (SingleType t1) (SingleType t2) = t1 `f` t2
  -- NOTE: The merge-alls must be expanded strictly before the merge-anys.
  singleCheck ti1 (TypeMerge MergeIntersect t2) = mergeAllM $ map (ti1 `singleCheck`) t2
  singleCheck (TypeMerge MergeUnion     t1) ti2 = mergeAllM $ map (`singleCheck` ti2) t1
  singleCheck (TypeMerge MergeIntersect t1) ti2 = mergeAnyM $ map (`singleCheck` ti2) t1
  singleCheck ti1 (TypeMerge MergeUnion     t2) = mergeAnyM $ map (ti1 `singleCheck`) t2

newtype ParamSet a =
  ParamSet {
    psParams :: [a]
  }
  deriving (Eq,Ord,Show)

instance Functor ParamSet where
  fmap f = ParamSet . fmap f . psParams

alwaysPairParams :: Monad m => a -> b -> m (a,b)
alwaysPairParams x y = return (x,y)

processParamPairs :: (Show a, Show b, CompileErrorM m, Monad m) =>
  (a -> b -> m c) -> ParamSet a -> ParamSet b -> m [c]
processParamPairs f (ParamSet ps1) (ParamSet ps2)
  | length ps1 == length ps2 =
    collectAllOrErrorM $ map (uncurry f) (zip ps1 ps2)
  | otherwise =
    compileError $ "Parameter count mismatch: " ++ show ps1 ++ " vs. " ++ show ps2

processParamPairsT :: (MonadTrans t, Monad (t m), Show a, Show b, CompileErrorM m, Monad m) =>
  (a -> b -> t m c) -> ParamSet a -> ParamSet b -> t m [c]
processParamPairsT f (ParamSet ps1) (ParamSet ps2)
  | length ps1 == length ps2 =
    sequence $ map (uncurry f) (zip ps1 ps2)
  | otherwise =
    lift $ compileError $ "Parameter count mismatch: " ++ show ps1 ++ " vs. " ++ show ps2

data Variance =
  Contravariant |
  Invariant |
  Covariant
  deriving (Eq,Ord)

instance Show Variance where
  show Contravariant = "contravariant"
  show Invariant     = "invariant"
  show Covariant     = "covariant"

composeVariance :: Variance -> Variance -> Variance
composeVariance Covariant      Covariant      = Covariant
composeVariance Contravariant  Contravariant  = Covariant
composeVariance Contravariant  Covariant      = Contravariant
composeVariance Covariant      Contravariant  = Contravariant
composeVariance _              _              = Invariant

paramAllowsVariance :: Variance -> Variance -> Bool
Covariant     `paramAllowsVariance` Covariant     = True
Contravariant `paramAllowsVariance` Contravariant = True
Invariant     `paramAllowsVariance` Covariant     = True
Invariant     `paramAllowsVariance` Invariant     = True
Invariant     `paramAllowsVariance` Contravariant = True
_             `paramAllowsVariance` _             = False

data StorageType =
  WeakValue |
  OptionalValue |
  RequiredValue
  deriving (Eq,Ord)

data SymbolScope =
  LocalScope |
  CategoryScope |
  TypeScope |
  ValueScope
  deriving (Eq,Ord,Show)

partitionByScope :: (a -> SymbolScope) -> [a] -> ([a],[a],[a])
partitionByScope f = foldr bin empty where
  empty = ([],[],[])
  bin x (cs,ts,vs)
    | f x == CategoryScope = (x:cs,ts,vs)
    | f x == TypeScope     = (cs,x:ts,vs)
    | f x == ValueScope    = (cs,ts,x:vs)
    | otherwise = (cs,ts,vs)
