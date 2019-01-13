{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module TypeInstance (
  DefinesInstance(..),
  FilterDirection(..),
  GeneralInstance,
  InstanceFilters,
  InstanceParams,
  InstanceVariances,
  ParamFilters,
  ParamVariances,
  ParamName(..),
  TypeFilter(..),
  TypeInstance(..),
  TypeInstanceOrParam(..),
  TypeName(..),
  TypeResolver(..),
  ValueType(..),
  checkGeneralMatch,
  checkValueTypeMatch,
  uncheckedSubFilter,
  uncheckedSubFilters,
  uncheckedSubInstance,
  uncheckedSubValueType,
  getValueForParam,
  validateAssignment,
  validateDefinesInstance,
  validateDefinesVariance,
  validateGeneralInstance,
  validateInstanceVariance,
  validateTypeFilter,
  validateTypeInstance,
) where

import Control.Monad (when)
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
    vtRequired :: StorageType,
    vtType :: GeneralInstance
  }
  deriving (Eq,Ord)

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
  deriving (Eq,Ord)

instance Show TypeInstance where
  show (TypeInstance n (ParamSet [])) = show n
  show (TypeInstance n (ParamSet ts)) =
    show n ++ "<" ++ intercalate "," (map show ts) ++ ">"

data DefinesInstance =
  DefinesInstance {
    diName :: TypeName,
    diParams :: InstanceParams
  }
  deriving (Eq,Ord)

instance Show DefinesInstance where
  show (DefinesInstance n (ParamSet [])) = show n
  show (DefinesInstance n (ParamSet ts)) =
    show n ++ "<" ++ intercalate "," (map show ts) ++ ">"

data TypeInstanceOrParam =
  JustTypeInstance {
    jtiType :: TypeInstance
  } |
  JustParamName {
    jpnName :: ParamName
  }
  deriving (Eq,Ord)

instance Show TypeInstanceOrParam where
  show (JustTypeInstance t) = show t
  show (JustParamName n)    = show n

data FilterDirection =
  FilterRequires |
  FilterAllows
  deriving (Eq,Ord,Show)

data TypeFilter =
  TypeFilter {
    tfDirection :: FilterDirection,
    tfType :: TypeInstanceOrParam
  } |
  DefinesFilter {
    dfType :: DefinesInstance
  }
  deriving (Eq,Ord)

instance Show TypeFilter where
  show (TypeFilter FilterRequires t) = "requires " ++ show t
  show (TypeFilter FilterAllows t)   = "allows "   ++ show t
  show (DefinesFilter t)             = "defines "  ++ show t

isTypeFilter :: TypeFilter -> Bool
isTypeFilter (TypeFilter _ _) = True
isTypeFilter _                = False

isDefinesFilter :: TypeFilter -> Bool
isDefinesFilter (DefinesFilter _) = True
isDefinesFilter _                 = False

viewTypeFilter :: ParamName -> TypeFilter -> String
viewTypeFilter n f = show n ++ " " ++ show f

type InstanceParams = ParamSet GeneralInstance
type InstanceVariances = ParamSet Variance
type InstanceFilters = ParamSet [TypeFilter]

type ParamFilters = Map.Map ParamName [TypeFilter]
type ParamVariances = Map.Map ParamName Variance

-- TODO: Get rid of p here?
data TypeResolver m p =
  TypeResolver {
    -- Performs parameter substitution for refines.
    trRefines :: TypeInstance -> TypeName -> m (p,InstanceParams),
    -- Performs parameter substitution for defines.
    trDefines :: TypeInstance -> TypeName -> m (p,InstanceParams),
    -- Get the parameter variances for the category.
    trVariance :: TypeName -> m InstanceVariances,
    -- Gets filters for the assigned parameters.
    trTypeFilters :: TypeInstance -> m (p,InstanceFilters),
    -- Gets filters for the assigned parameters.
    trDefinesFilters :: DefinesInstance -> m (p,InstanceFilters),
    -- Returns True if the type is concrete.
    trConcrete :: TypeName -> m Bool
  }

filterLookup :: (CompileErrorM m, Monad m) =>
  ParamFilters -> ParamName -> m [TypeFilter]
filterLookup ps n = resolve $ n `Map.lookup` ps where
  resolve (Just x) = return x
  resolve _        = compileError $ "Param " ++ show n ++ " not found"

getValueForParam :: (CompileErrorM m, Monad m) =>
  Map.Map ParamName GeneralInstance -> ParamName -> m GeneralInstance
getValueForParam pa n =
  case n `Map.lookup` pa of
        (Just x) -> return x
        _ -> compileError $ "Param " ++ show n ++ " does not exist"

checkValueTypeMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> ValueType -> ValueType -> m p
checkValueTypeMatch r f ts1@(ValueType r1 t1) ts2@(ValueType r2 t2)
  | r1 < r2 =
    compileError $ "Cannot convert " ++ show ts1 ++ " to " ++ show ts2
  | otherwise = checkGeneralMatch r f Covariant t1 t2

checkGeneralMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance ->
  GeneralInstance -> GeneralInstance -> m p
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
checkInstanceToInstance r f Invariant t1 t2
    | t1 == t2 = mergeDefault
    | otherwise =
      -- Implicit equality, inferred by t1 <-> t2.
      mergeAll [checkInstanceToInstance r f Covariant     t1 t2,
                checkInstanceToInstance r f Contravariant t1 t2]
checkInstanceToInstance r f Contravariant t1 t2 =
  checkInstanceToInstance r f Covariant t2 t1
checkInstanceToInstance r f Covariant t1@(TypeInstance n1 ps1) t2@(TypeInstance n2 ps2)
  | n1 == n2 = do
    paired <- processParamPairs alwaysPairParams ps1 ps2
    let zipped = ParamSet paired
    variance <- trVariance r n1
    -- NOTE: Covariant is identity, so v2 has technically been composed with it.
    processParamPairs (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance zipped >> mergeDefault
  | otherwise = do
    (p2,ps1') <- trRefines r t1 n2 `reviseError` (show n1 ++ " does not refine " ++ show n2)
    (return p2) `mergeNested` (checkInstanceToInstance r f Covariant (TypeInstance n2 ps1') t2)

checkParamToInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> ParamName -> TypeInstance -> m p
checkParamToInstance r f Invariant n1 t2 =
  -- Implicit equality, inferred by n1 <-> t2.
  mergeAll [checkParamToInstance r f Covariant     n1 t2,
            checkParamToInstance r f Contravariant n1 t2]
checkParamToInstance r f Contravariant p1 t2 =
  checkInstanceToParam r f Covariant t2 p1
checkParamToInstance r f Covariant n1 t2@(TypeInstance n2 ps2) = do
  cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
  mergeAny (map checkConstraintToInstance cs1) `reviseError`
    ("No filters imply " ++ show n1 ++ " -> " ++ show t2)
  where
    checkConstraintToInstance (TypeFilter FilterRequires t) =
      -- x -> F implies x -> T only if F -> T
      checkSingleMatch r f Covariant t (JustTypeInstance t2)
    checkConstraintToInstance f =
      -- F -> x cannot imply x -> T
      -- DefinesInstance cannot be converted to TypeInstance
      compileError $ "Constraint " ++ viewTypeFilter n1 f ++
                    " does not imply " ++ show n1 ++ " -> " ++ show t2

checkInstanceToParam :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> TypeInstance -> ParamName -> m p
checkInstanceToParam r f Invariant t1 n2 =
  -- Implicit equality, inferred by t1 <-> n2.
  mergeAll [checkInstanceToParam r f Covariant     t1 n2,
            checkInstanceToParam r f Contravariant t1 n2]
checkInstanceToParam r f Contravariant t1 p2 =
  checkParamToInstance r f Covariant p2 t1
checkInstanceToParam r f Covariant t1@(TypeInstance n1 ps1) n2 = do
  cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
  mergeAny (map checkInstanceToConstraint cs2) `reviseError`
    ("No filters imply " ++ show t1 ++ " -> " ++ show n2)
  where
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
      -- Implicit equality, inferred by n1 <-> n2.
      mergeAll [checkParamToParam r f Covariant     n1 n2,
                checkParamToParam r f Contravariant n1 n2]
checkParamToParam r f Contravariant n1 n2 =
  checkParamToParam r f Covariant n2 n1
checkParamToParam r f Covariant n1 n2
  | n1 == n2 = mergeDefault
  | otherwise = do
    cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
    cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
    let typeFilters = [(c1,c2) | c1 <- cs1, c2 <- cs2] ++
                      [(self1,c2) | c2 <- cs2] ++
                      [(c1,self2) | c1 <- cs1]
    mergeAny (map (\(c1,c2) -> checkConstraintToConstraint c1 c2) typeFilters) `reviseError`
      ("No filters imply " ++ show n1 ++ " -> " ++ show n2)
    where
      self1 = TypeFilter FilterRequires (JustParamName n1)
      self2 = TypeFilter FilterAllows   (JustParamName n2)
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

validateGeneralInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> GeneralInstance -> m ()
validateGeneralInstance r f ta@(TypeMerge MergeIntersect ts) = do
  mergeAll (map checkConcrete ts)
  mergeAll (map (validateGeneralInstance r f) ts)
  where
    checkConcrete (SingleType (JustTypeInstance t)) = do
      c <- trConcrete r (tiName t)
      when c $ compileError $ "Concrete type " ++ show (tiName t) ++
                              " cannot be used in intersection " ++ show ta
    checkConcrete _ = return ()
validateGeneralInstance r f ta@(TypeMerge _ ts) =
  mergeAll (map (validateGeneralInstance r f) ts)
validateGeneralInstance r f (SingleType (JustTypeInstance t)) =
  validateTypeInstance r f t
validateGeneralInstance _ f (SingleType (JustParamName n)) =
  when (not $ n `Map.member` f) $
    compileError $ "Param " ++ show n ++ " does not exist"

validateTypeInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> TypeInstance -> m ()
validateTypeInstance r f t@(TypeInstance n ps) = do
  (_,fa) <- trTypeFilters r t
  processParamPairs (validateAssignment r f) ps fa
  mergeAll (map (validateGeneralInstance r f) (psParams ps)) `reviseError`
    ("Recursive error in " ++ show t)

validateDefinesInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> DefinesInstance -> m ()
validateDefinesInstance r f t@(DefinesInstance n ps) = do
  (_,fa) <- trDefinesFilters r t
  processParamPairs (validateAssignment r f) ps fa
  mergeAll (map (validateGeneralInstance r f) (psParams ps)) `reviseError`
    ("Recursive error in " ++ show t)

validateTypeFilter :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> TypeFilter -> m ()
validateTypeFilter r f (TypeFilter FilterRequires ta@(JustTypeInstance t)) = do
  c <- trConcrete r (tiName t)
  when c $ compileError $ "Concrete type " ++ show (tiName t) ++
                          " cannot be used in a requires filter"
  validateGeneralInstance r f (SingleType ta)
validateTypeFilter r f (TypeFilter _ t) =
  validateGeneralInstance r f (SingleType t)
validateTypeFilter r f (DefinesFilter t) =
  validateDefinesInstance r f t

validateAssignment :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> GeneralInstance -> [TypeFilter] -> m p
validateAssignment r f t fs = mergeAll (map (checkFilter t) fs) where
  checkFilter t1 (TypeFilter FilterRequires t2) = do
    checkGeneralMatch r f Covariant t1 (SingleType t2)
  checkFilter t1 (TypeFilter FilterAllows t2) = do
    checkGeneralMatch r f Contravariant t1 (SingleType t2)
  checkFilter t1@(TypeMerge _ _) (DefinesFilter t) =
    compileError $ "Merged type " ++ show t1 ++ " cannot satisfy defines constraint " ++ show t
  checkFilter t1@(SingleType t) (DefinesFilter f) = checkDefinesFilter f t
  requireExactlyOne t [_] = mergeDefault
  requireExactlyOne t []  =
    compileError $ "No types in intersection define " ++ show t
  requireExactlyOne t ts  =
    (compileError $ "Multiple types in intersection define " ++ show t) `mergeNested`
      (mergeAll $ map (compileError . show) ts)
  checkDefinesFilter f2@(DefinesInstance n2 _) (JustTypeInstance t1) = do
    (_,ps1') <- trDefines r t1 n2 `reviseError` (show (tiName t1) ++ " does not define " ++ show n2)
    checkDefines f2 (DefinesInstance n2 ps1')
  checkDefinesFilter f2 (JustParamName n1) = do
      fs1 <- fmap (map dfType . filter isDefinesFilter) $ f `filterLookup` n1
      mergeAny (map (checkDefines f2) fs1) `reviseError`
        ("No filters imply " ++ show n1 ++ " defines " ++ show f2)
  checkDefines f2@(DefinesInstance n2 ps2) f1@(DefinesInstance n1 ps1)
    | n1 == n2 = do
      paired <- processParamPairs alwaysPairParams ps1 ps2
      variance <- trVariance r n2
      processParamPairs (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance (ParamSet paired)
      mergeDefault
    | otherwise = compileError $ "Constraint " ++ show f1 ++ " does not imply " ++ show f2

validateInstanceVariance :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamVariances -> Variance -> GeneralInstance -> m ()
validateInstanceVariance r vm v (SingleType (JustTypeInstance (TypeInstance n ps))) = do
  vs <- trVariance r n
  paired <- processParamPairs alwaysPairParams vs ps
  mergeAll (map (\(v2,p) -> validateInstanceVariance r vm (v `composeVariance` v2) p) paired)
validateInstanceVariance r vm v (TypeMerge MergeUnion ts) =
  mergeAll (map (validateInstanceVariance r vm v) ts)
validateInstanceVariance r vm v (TypeMerge MergeIntersect ts) =
  mergeAll (map (validateInstanceVariance r vm v) ts)
validateInstanceVariance r vm v (SingleType (JustParamName n)) =
  case n `Map.lookup` vm of
      Nothing -> compileError $ "Param " ++ show n ++ " is undefined"
      (Just v0) -> when (not $ v0 `paramAllowsVariance` v) $
                        compileError $ "Param " ++ show n ++ " cannot be " ++ show v

validateDefinesVariance :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamVariances -> Variance -> DefinesInstance -> m ()
validateDefinesVariance r vm v (DefinesInstance n ps) = do
  vs <- trVariance r n
  paired <- processParamPairs alwaysPairParams vs ps
  mergeAll (map (\(v2,p) -> validateInstanceVariance r vm (v `composeVariance` v2) p) paired)

uncheckedSubValueType :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> ValueType -> m ValueType
uncheckedSubValueType replace (ValueType s t) = do
  t' <- uncheckedSubInstance replace t
  return $ ValueType s t'

uncheckedSubInstance :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> GeneralInstance -> m GeneralInstance
uncheckedSubInstance replace = subAll where
  subAll (TypeMerge MergeUnion ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (TypeMerge MergeUnion gs)
  subAll (TypeMerge MergeIntersect ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (TypeMerge MergeIntersect gs)
  subAll (SingleType t) = subInstance t
  subInstance (JustTypeInstance (TypeInstance n (ParamSet ts))) = do
    gs <- collectAllOrErrorM $ map subAll ts
    let t2 = SingleType $ JustTypeInstance $ TypeInstance n (ParamSet gs)
    return (t2)
  subInstance (JustParamName n) = replace n

uncheckedSubFilter :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> TypeFilter -> m TypeFilter
uncheckedSubFilter replace (TypeFilter d t) = do
  t' <- uncheckedSubInstance replace (SingleType t)
  return (TypeFilter d (stType t'))
uncheckedSubFilter replace (DefinesFilter (DefinesInstance n ts)) = do
  ts' <- collectAllOrErrorM $ map (uncheckedSubInstance replace) (psParams ts)
  return (DefinesFilter (DefinesInstance n (ParamSet ts')))

uncheckedSubFilters :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> ParamFilters -> m ParamFilters
uncheckedSubFilters replace fa = do
  fa' <- collectAllOrErrorM $ map subParam $ Map.toList fa
  return $ Map.fromList fa'
  where
    subParam (n,fs) = do
      fs' <- collectAllOrErrorM $ map (uncheckedSubFilter replace) fs
      return (n,fs')
