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
  Variance(..),
  alwaysPairParams,
  checkParamsMatch,
  checkGeneralType,
  composeVariance,
  paramAllowsVariance,
) where


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

instance (MergeableM m, Mergeable a) => Mergeable (m a) where
  mergeAny = mergeAnyM
  mergeAll = mergeAllM
  mergeNested = mergeNestedM
  mergeDefault = mergeDefaultM

instance Mergeable () where
  mergeAny = const ()
  mergeAll = const ()

instance Mergeable Bool where
  mergeAny = any id
  mergeAll = all id

class CompileError a where
  compileError :: String -> a
  isCompileError :: a -> Bool

class CompileErrorM m where
  compileErrorM :: String -> m a
  isCompileErrorM :: m a -> Bool
  collectAllOrErrorM :: Foldable f => f (m a) -> m [a]
  collectOneOrErrorM :: Foldable f => f (m a) -> m a

instance CompileErrorM m => CompileError (m a) where
  compileError = compileErrorM
  isCompileError = isCompileErrorM

data MergeType =
  MergeUnion |
  MergeIntersect
  deriving (Eq,Ord,Show)

data GeneralType a =
  SingleType {
    stType :: a
  } |
  TypeMerge {
    tmMerge :: MergeType,
    tmTypes :: [GeneralType a]
  }
  deriving (Eq,Ord)

checkGeneralType :: Mergeable c => (a -> b -> c) -> GeneralType a -> GeneralType b -> c
checkGeneralType f ti1 ti2 = singleCheck ti1 ti2 where
  singleCheck (SingleType t1) (SingleType t2) = t1 `f` t2
  singleCheck (TypeMerge MergeUnion     t1) ti2 = mergeAll $ map (`singleCheck` ti2) t1
  singleCheck (TypeMerge MergeIntersect t1) ti2 = mergeAny $ map (`singleCheck` ti2) t1
  singleCheck ti1 (TypeMerge MergeUnion     t2) = mergeAny $ map (ti1 `singleCheck`) t2
  singleCheck ti1 (TypeMerge MergeIntersect t2) = mergeAll $ map (ti1 `singleCheck`) t2

newtype ParamSet a =
  ParamSet {
    psParams :: [a]
  }
  deriving (Eq,Ord,Show)

alwaysPairParams :: Monad m => a -> b -> m (a,b)
alwaysPairParams x y = return (x,y)

checkParamsMatch :: (Show a, Show b, CompileErrorM m, Monad m) =>
  (a -> b -> m c) -> ParamSet a -> ParamSet b -> m [c]
checkParamsMatch f (ParamSet ps1) (ParamSet ps2)
  | length ps1 == length ps2 =
    collectAllOrErrorM $ map (uncurry f) (zip ps1 ps2)
  | otherwise =
    compileError $ "Parameter count mismatch: " ++ show ps1 ++ " vs. " ++ show ps2

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
  deriving (Eq,Ord,Show)
