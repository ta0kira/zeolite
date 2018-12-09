{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module TypeInstance (
  AssignedParams,
  FilterDirection(..),
  GeneralInstance,
  InstanceParams,
  InstanceVariances,
  ParamFilters,
  ParamName(..),
  TypeFilter(..),
  TypeInstance(..),
  TypeInstanceOrParam(..),
  TypeName(..),
  TypeResolver(..),
  ValueType(..),
  checkGeneralMatch,
  checkValueTypeMatch,
) where

import Data.List (intercalate)
import qualified Data.Map as Map

import TypesBase


type GeneralInstance = GeneralType TypeInstanceOrParam

instance Show GeneralInstance where
  show (SingleType t) = show t
  show (TypeMerge MergeUnion []) = "all"
  show (TypeMerge MergeUnion ts) = "(" ++ intercalate "|" (map show ts) ++ ")"
  show (TypeMerge MergeIntersect []) = "any"
  show (TypeMerge MergeIntersect ts) = "(" ++ intercalate "&" (map show ts) ++ ")"

data ValueType =
  ValueType {
    vtRequired :: Tangibility,
    vtType :: GeneralInstance
  }
  deriving (Eq)

instance Show ValueType where
  show (ValueType WeakValue t)     = "weak " ++ show t
  show (ValueType OptionalValue t) = "optional " ++ show t
  show (ValueType RequiredValue t) = show t

newtype TypeName =
  TypeName {
    tnName :: String
  }
  deriving (Eq,Ord)

instance Show TypeName where
  show (TypeName n) = n

newtype ParamName =
  ParamName {
    pnName :: String
  }
  deriving (Eq,Ord)

instance Show ParamName where
  show (ParamName n) = n

data TypeInstance =
  TypeInstance {
    tiName :: TypeName,
    tiParams :: InstanceParams
  }
  deriving (Eq)

instance Show TypeInstance where
  show (TypeInstance n (ParamSet [])) = show n
  show (TypeInstance n (ParamSet ts)) =
    show n ++ "<" ++ intercalate "," (map show ts) ++ ">"

data TypeInstanceOrParam =
  JustTypeInstance {
    jtiType :: TypeInstance
  } |
  JustParamName {
    jpnName :: ParamName
  }
  deriving (Eq)

instance Show TypeInstanceOrParam where
  show (JustTypeInstance t) = show t
  show (JustParamName n)    = show n

data FilterDirection =
  FilterRequires |
  FilterAllows
  deriving (Eq,Show)

data TypeFilter =
  TypeFilter {
    tfDirection :: FilterDirection,
    tfType :: TypeInstanceOrParam
  }
  deriving (Eq)

instance Show TypeFilter where
  show (TypeFilter FilterRequires t) = "requires " ++ show t
  show (TypeFilter FilterAllows t)   = "allows "   ++ show t

viewTypeFilter :: ParamName -> TypeFilter -> String
viewTypeFilter n f = show n ++ " " ++ show f

type InstanceParams = ParamSet GeneralInstance

type InstanceVariances = ParamSet Variance

type AssignedParams = Map.Map ParamName GeneralInstance

type ParamFilters = Map.Map ParamName [TypeFilter]

data TypeResolver m p =
  TypeResolver {
    -- Convert an instance of one category to an instance of the other.
    trFind :: TypeInstance -> TypeName -> m (p,InstanceParams),
    -- Get the parameter variances for the category.
    trVariance :: TypeName -> m InstanceVariances,
    -- Validates an instance's param args against required filters.
    tfValidate :: ParamFilters -> TypeInstance -> m (),
    -- Labels params for an instance using the category's param names.
    trParams :: TypeName -> InstanceParams -> m AssignedParams
  }

filterLookup :: (CompileErrorM m, Monad m) =>
  ParamFilters -> ParamName -> m [TypeFilter]
filterLookup ps n = resolve $ n `Map.lookup` ps where
  resolve (Just x) = return x
  resolve _        = compileError $ "Param " ++ show n ++ " not found"

checkValueTypeMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> ValueType -> ValueType -> m p
checkValueTypeMatch r f ts1@(ValueType r1 t1) ts2@(ValueType r2 t2)
  | r1 < r2 =
    compileError $ "Cannot convert " ++ show ts1 ++ " to " ++ show ts2
  | otherwise = checkGeneralMatch r f Covariant t1 t2

-- NOTE: This doesn't verify the filters required to create an instance of a
-- type category. That should be done during instantiation of the instances and
-- during validation of the category system. (This does verify filters imposed
-- by individual free params, though.)
checkGeneralMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance ->
  GeneralInstance -> GeneralInstance -> m p
-- Necessary so that any -> any.
checkGeneralMatch _ _ _ (TypeMerge MergeIntersect []) (TypeMerge MergeIntersect []) = mergeDefault
checkGeneralMatch r f v ts1 ts2 = checkGeneralType (checkSingleMatch r f v) ts1 ts2

checkSingleMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance ->
  TypeInstanceOrParam -> TypeInstanceOrParam -> m p
checkSingleMatch r f v (JustTypeInstance t1) (JustTypeInstance t2) =
  checkInstanceToInstance r f v t1 t2
checkSingleMatch r f v (JustParamName p1) (JustTypeInstance t2) =
  checkParamToInstance r f v p1 t2
checkSingleMatch r f v (JustTypeInstance t1) (JustParamName p2) =
  checkInstanceToParam r f v t1 p2
checkSingleMatch r f v (JustParamName p1) (JustParamName p2) =
  checkParamToParam r f v p1 p2

checkInstanceToInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> TypeInstance -> TypeInstance -> m p
checkInstanceToInstance r f Invariant t1@(TypeInstance n1 _) t2@(TypeInstance n2 _)
  | t1 == t2 = mergeDefault
  | otherwise =
    compileError $ "Invariance requires equality: " ++ show t1 ++ " vs. " ++ show t2
checkInstanceToInstance r f Contravariant t1 t2 =
  checkInstanceToInstance r f Covariant t2 t1
checkInstanceToInstance r f Covariant t1@(TypeInstance n1 ps1) t2@(TypeInstance n2 ps2)
  | n1 == n2 = do
    checkParamsMatch (\_ _ -> return ()) ps1 ps2
    zipped <- return $ ParamSet $ zip (psParams ps1) (psParams ps2)
    variance <- trVariance r n1
    -- NOTE: Covariant is identity, so v2 has technically been composed with it.
    checkParamsMatch (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance zipped
  | otherwise = do
    (p2,ps1') <- (trFind r) t1 n2
    (return p2) `mergeNested` (checkInstanceToInstance r f Covariant (TypeInstance n2 ps1') t2)

checkParamToInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> ParamName -> TypeInstance -> m p
checkParamToInstance r _ Invariant n1 t2@(TypeInstance n2 _) =
    compileError $ "Invariance requires equality: " ++ show n1 ++ " vs. " ++ show t2
checkParamToInstance r f Contravariant p1 t2 =
  checkInstanceToParam r f Covariant t2 p1
checkParamToInstance r f Covariant n1 t2@(TypeInstance n2 ps2) = checked where
  checked = do
    cs1 <- f `filterLookup` n1
    mergeAny $ map checkConstraintToInstance cs1
  checkConstraintToInstance (TypeFilter FilterRequires t) =
    -- x -> F implies x -> T only if F -> T
    checkSingleMatch r f Covariant t (JustTypeInstance t2)
  checkConstraintToInstance f =
    -- F -> x cannot imply x -> T
    compileError $ "Constraint " ++ viewTypeFilter n1 f ++
                   " does not imply " ++ show n1 ++ " -> " ++ show t2

checkInstanceToParam :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> TypeInstance -> ParamName -> m p
checkInstanceToParam _ _ Invariant t1@(TypeInstance n1 _) n2 =
    compileError $ "Invariance requires equality: " ++ show t1 ++ " vs. " ++ show n2
checkInstanceToParam r f Contravariant t1 p2 =
  checkParamToInstance r f Covariant p2 t1
checkInstanceToParam r f Covariant t1@(TypeInstance n1 ps1) n2 = checked where
  checked = do
    cs2 <- f `filterLookup` n2
    mergeAny $ map checkInstanceToConstraint cs2
  checkInstanceToConstraint (TypeFilter FilterAllows t) =
    -- F -> x implies T -> x only if T -> F
    checkSingleMatch r f Covariant (JustTypeInstance t1) t
  checkInstanceToConstraint f =
    -- x -> F cannot imply T -> x
    compileError $ "Constraint " ++ viewTypeFilter n2 f ++
                   " does not imply " ++ show t1 ++ " -> " ++ show n2

checkParamToParam :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> ParamName -> ParamName -> m p
checkParamToParam r f Invariant n1 n2
    | n1 == n2 = mergeDefault
    | otherwise =
      -- Even with identical fiters, if the names are different then it's
      -- possible that the substituted types will be different.
    compileError $ "Invariance requires equality: " ++ show n1 ++ " vs. " ++ show n2
checkParamToParam r f Contravariant p1 p2 =
  checkParamToParam r f Covariant p2 p1
checkParamToParam r f Covariant n1 n2 = checked where
  self1 = TypeFilter FilterRequires (JustParamName n1)
  self2 = TypeFilter FilterAllows   (JustParamName n2)
  checked
    | n1 == n2 = mergeDefault
    | otherwise = do
      cs1 <- f `filterLookup` n1
      cs2 <- f `filterLookup` n2
      pairs <- return $ [(c1,c2) | c1 <- cs1, c2 <- cs2] ++
                        [(self1,c2) | c2 <- cs2] ++
                        [(c1,self2) | c1 <- cs1]
      mergeAny $ map (\(c1,c2) -> checkConstraintToConstraint c1 c2) pairs
  checkConstraintToConstraint (TypeFilter FilterRequires t1) (TypeFilter FilterAllows t2)
    | t1 == (JustParamName n1) && t2 == (JustParamName n2) =
      compileError $ "Infinite recursion in " ++ show n1 ++ " -> " ++ show n2
    -- x -> F1, F2 -> y implies x -> y only if F1 -> F2
    | otherwise = checkSingleMatch r f Covariant t1 t2
  checkConstraintToConstraint f1 f2 =
    -- x -> F1, y -> F2 cannot imply x -> y
    -- F1 -> x, F1 -> y cannot imply x -> y
    -- F1 -> x, y -> F2 cannot imply x -> y
    compileError $ "Constraints " ++ viewTypeFilter n1 f1 ++ " and " ++
                   viewTypeFilter n2 f2 ++ " do not imply " ++
                   show n1 ++ " -> " ++ show n2
