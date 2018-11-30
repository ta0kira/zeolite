{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module TypesBase (
  CompileError(..),
  CompileErrorM(..),
  GeneralType(..),
  Mergeable(..),
  MergeType(..),
  ParamSet(..),
  Variance(..),
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
    tiType :: a
  } |
  TypeMerge {
    tmMerge :: MergeType,
    tmTypes :: [GeneralType a]
  }
  deriving (Eq,Show)

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
  deriving (Eq,Show)

checkParamsMatch :: (Mergeable c, CompileError c) => (a -> b -> c) -> ParamSet a -> ParamSet b -> c
checkParamsMatch f (ParamSet ps1) (ParamSet ps2) = checkedMerge ps1 ps2 where
  checkedMerge []      []      = mergeAll $ map (\(p1,p2) -> (p1 `f` p2)) (zip ps1 ps2)
  checkedMerge (_:ps1) (_:ps2) = checkedMerge ps1 ps2
  checkedMerge _       _       = compileError "Parameter count mismatch"

data Variance =
  Covariant |
  Contravariant |
  Invariant
  deriving (Eq,Ord,Show)

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
