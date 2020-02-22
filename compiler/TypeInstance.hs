{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module TypeInstance (
  AnyTypeResolver(..),
  CategoryName(..),
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
  TypeResolver(..),
  ValueType(..),
  checkDefinesMatch,
  checkGeneralMatch,
  checkValueTypeMatch,
  uncheckedSubFilter,
  uncheckedSubFilters,
  uncheckedSubInstance,
  uncheckedSubValueType,
  getValueForParam,
  isBuiltinCategory,
  isDefinesFilter,
  isRequiresFilter,
  isWeakValue,
  requiredParam,
  requiredSingleton,
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
  show (TypeMerge MergeUnion ts) = "[" ++ intercalate "|" (map show ts) ++ "]"
  show (TypeMerge MergeIntersect []) = "any"
  show (TypeMerge MergeIntersect ts) = "[" ++ intercalate "&" (map show ts) ++ "]"

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

isWeakValue :: ValueType -> Bool
isWeakValue = (== WeakValue) . vtRequired

requiredSingleton :: CategoryName -> ValueType
requiredSingleton n = ValueType RequiredValue $ SingleType $ JustTypeInstance $ TypeInstance n (ParamSet [])

requiredParam :: ParamName -> ValueType
requiredParam n = ValueType RequiredValue $ SingleType $ JustParamName n

data CategoryName =
  CategoryName {
    tnName :: String
  } |
  BuiltinBool |
  BuiltinChar |
  BuiltinInt |
  BuiltinFloat |
  BuiltinString |
  BuiltinFormatted |
  CategoryNone

instance Show CategoryName where
  show (CategoryName n)    = n
  show BuiltinBool         = "Bool"
  show BuiltinChar         = "Char"
  show BuiltinInt          = "Int"
  show BuiltinFloat        = "Float"
  show BuiltinString       = "String"
  show BuiltinFormatted    = "Formatted"
  show CategoryNone        = "(none)"

instance Eq CategoryName where
  c1 == c2 = show c1 == show c2

instance Ord CategoryName where
  c1 <= c2 = show c1 <= show c2

isBuiltinCategory :: CategoryName -> Bool
isBuiltinCategory _ = False

newtype ParamName =
  ParamName {
    pnName :: String
  }
  deriving (Eq,Ord)

instance Show ParamName where
  show (ParamName n) = n

data TypeInstance =
  TypeInstance {
    tiName :: CategoryName,
    tiParams :: InstanceParams
  }
  deriving (Eq,Ord)

instance Show TypeInstance where
  show (TypeInstance n (ParamSet [])) = show n
  show (TypeInstance n (ParamSet ts)) =
    show n ++ "<" ++ intercalate "," (map show ts) ++ ">"

data DefinesInstance =
  DefinesInstance {
    diName :: CategoryName,
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
  deriving (Eq,Ord)

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

isRequiresFilter :: TypeFilter -> Bool
isRequiresFilter (TypeFilter isRequiresFilter _) = True
isRequiresFilter _                               = False

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

class TypeResolver r where
  -- Performs parameter substitution for refines.
  trRefines :: (MergeableM m, CompileErrorM m, Monad m) =>
    r -> TypeInstance -> CategoryName -> m InstanceParams
  -- Performs parameter substitution for defines.
  trDefines :: (MergeableM m, CompileErrorM m, Monad m) =>
    r -> TypeInstance -> CategoryName -> m InstanceParams
  -- Get the parameter variances for the category.
  trVariance :: (MergeableM m, CompileErrorM m, Monad m) =>
    r -> CategoryName -> m InstanceVariances
  -- Gets filters for the assigned parameters.
  trTypeFilters :: (MergeableM m, CompileErrorM m, Monad m) =>
    r -> TypeInstance -> m InstanceFilters
  -- Gets filters for the assigned parameters.
  trDefinesFilters :: (MergeableM m, CompileErrorM m, Monad m) =>
    r -> DefinesInstance -> m InstanceFilters
  -- Returns True if the type is concrete.
  trConcrete :: (MergeableM m, CompileErrorM m, Monad m) =>
    r -> CategoryName -> m Bool

data AnyTypeResolver = forall r. TypeResolver r => AnyTypeResolver r

instance TypeResolver AnyTypeResolver where
  trRefines (AnyTypeResolver r) = trRefines r
  trDefines (AnyTypeResolver r) = trDefines r
  trVariance (AnyTypeResolver r) = trVariance r
  trTypeFilters (AnyTypeResolver r) = trTypeFilters r
  trDefinesFilters (AnyTypeResolver r) = trDefinesFilters r
  trConcrete (AnyTypeResolver r) = trConcrete r

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

checkValueTypeMatch :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> ValueType -> ValueType -> m ()
checkValueTypeMatch r f ts1@(ValueType r1 t1) ts2@(ValueType r2 t2)
  | r1 < r2 =
    compileError $ "Cannot convert " ++ show ts1 ++ " to " ++ show ts2
  | otherwise = checkGeneralMatch r f Covariant t1 t2

checkGeneralMatch :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> Variance ->
  GeneralInstance -> GeneralInstance -> m ()
checkGeneralMatch r f Invariant ts1 ts2 = do
  -- This ensures that any and all behave as expected in Invariant positions.
  checkGeneralType (checkSingleMatch r f Covariant) ts1 ts2
  checkGeneralType (checkSingleMatch r f Covariant) ts2 ts1
checkGeneralMatch r f Contravariant ts1 ts2 =
  checkGeneralType (checkSingleMatch r f Covariant) ts2 ts1
checkGeneralMatch r f Covariant ts1 ts2 =
  checkGeneralType (checkSingleMatch r f Covariant) ts1 ts2

checkSingleMatch :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> Variance ->
  TypeInstanceOrParam -> TypeInstanceOrParam -> m ()
checkSingleMatch r f v (JustTypeInstance t1) (JustTypeInstance t2) =
  checkInstanceToInstance r f v t1 t2
checkSingleMatch r f v (JustParamName p1) (JustTypeInstance t2) =
  checkParamToInstance r f v p1 t2
checkSingleMatch r f v (JustTypeInstance t1) (JustParamName p2) =
  checkInstanceToParam r f v t1 p2
checkSingleMatch r f v (JustParamName p1) (JustParamName p2) =
  checkParamToParam r f v p1 p2

checkInstanceToInstance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> TypeInstance -> TypeInstance -> m ()
checkInstanceToInstance r f Invariant t1 t2
    | t1 == t2 = mergeDefaultM
    | otherwise =
      -- Implicit equality, inferred by t1 <-> t2.
      mergeAllM [checkInstanceToInstance r f Covariant     t1 t2,
                 checkInstanceToInstance r f Contravariant t1 t2]
checkInstanceToInstance r f Contravariant t1 t2 =
  checkInstanceToInstance r f Covariant t2 t1
checkInstanceToInstance r f Covariant t1@(TypeInstance n1 ps1) t2@(TypeInstance n2 ps2)
  | n1 == n2 = do
    paired <- processParamPairs alwaysPairParams ps1 ps2
    let zipped = ParamSet paired
    variance <- trVariance r n1
    -- NOTE: Covariant is identity, so v2 has technically been composed with it.
    processParamPairs (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance zipped >> mergeDefaultM
  | otherwise = do
    ps1' <- trRefines r t1 n2
    checkInstanceToInstance r f Covariant (TypeInstance n2 ps1') t2

checkParamToInstance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> ParamName -> TypeInstance -> m ()
checkParamToInstance r f Invariant n1 t2 =
  -- Implicit equality, inferred by n1 <-> t2.
  mergeAllM [checkParamToInstance r f Covariant     n1 t2,
             checkParamToInstance r f Contravariant n1 t2]
checkParamToInstance r f Contravariant p1 t2 =
  checkInstanceToParam r f Covariant t2 p1
checkParamToInstance r f Covariant n1 t2@(TypeInstance n2 ps2) = do
  cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
  mergeAnyM (map checkConstraintToInstance cs1) `reviseError`
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

checkInstanceToParam :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> TypeInstance -> ParamName -> m ()
checkInstanceToParam r f Invariant t1 n2 =
  -- Implicit equality, inferred by t1 <-> n2.
  mergeAllM [checkInstanceToParam r f Covariant     t1 n2,
             checkInstanceToParam r f Contravariant t1 n2]
checkInstanceToParam r f Contravariant t1 p2 =
  checkParamToInstance r f Covariant p2 t1
checkInstanceToParam r f Covariant t1@(TypeInstance n1 ps1) n2 = do
  cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
  mergeAnyM (map checkInstanceToConstraint cs2) `reviseError`
    ("No filters imply " ++ show t1 ++ " -> " ++ show n2)
  where
    checkInstanceToConstraint (TypeFilter FilterAllows t) =
      -- F -> x implies T -> x only if T -> F
      checkSingleMatch r f Covariant (JustTypeInstance t1) t
    checkInstanceToConstraint f =
      -- x -> F cannot imply T -> x
      compileError $ "Constraint " ++ viewTypeFilter n2 f ++
                    " does not imply " ++ show t1 ++ " -> " ++ show n2

checkParamToParam :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> ParamName -> ParamName -> m ()
checkParamToParam r f Invariant n1 n2
    | n1 == n2 = mergeDefaultM
    | otherwise =
      -- Implicit equality, inferred by n1 <-> n2.
      mergeAllM [checkParamToParam r f Covariant     n1 n2,
                 checkParamToParam r f Contravariant n1 n2]
checkParamToParam r f Contravariant n1 n2 =
  checkParamToParam r f Covariant n2 n1
checkParamToParam r f Covariant n1 n2
  | n1 == n2 = mergeDefaultM
  | otherwise = do
    cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
    cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
    let typeFilters = [(c1,c2) | c1 <- cs1, c2 <- cs2] ++
                      [(self1,c2) | c2 <- cs2] ++
                      [(c1,self2) | c1 <- cs1]
    mergeAnyM (map (\(c1,c2) -> checkConstraintToConstraint c1 c2) typeFilters) `reviseError`
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

validateGeneralInstance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> GeneralInstance -> m ()
validateGeneralInstance r f ta@(TypeMerge _ ts)
  | length ts == 1 = compileError $ "Unions and intersections must have at least 2 types to avoid ambiguity"
validateGeneralInstance r f ta@(TypeMerge MergeIntersect ts) =
  mergeAllM (map (validateGeneralInstance r f) ts)
validateGeneralInstance r f ta@(TypeMerge _ ts) =
  mergeAllM (map (validateGeneralInstance r f) ts)
validateGeneralInstance r f (SingleType (JustTypeInstance t)) =
  validateTypeInstance r f t
validateGeneralInstance _ f (SingleType (JustParamName n)) =
  when (not $ n `Map.member` f) $
    compileError $ "Param " ++ show n ++ " does not exist"

validateTypeInstance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> TypeInstance -> m ()
validateTypeInstance r f t@(TypeInstance n ps) = do
  fa <- trTypeFilters r t
  processParamPairs (validateAssignment r f) ps fa
  mergeAllM (map (validateGeneralInstance r f) (psParams ps)) `reviseError`
    ("Recursive error in " ++ show t)

validateDefinesInstance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> DefinesInstance -> m ()
validateDefinesInstance r f t@(DefinesInstance n ps) = do
  fa <- trDefinesFilters r t
  processParamPairs (validateAssignment r f) ps fa
  mergeAllM (map (validateGeneralInstance r f) (psParams ps)) `reviseError`
    ("Recursive error in " ++ show t)

validateTypeFilter :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> TypeFilter -> m ()
validateTypeFilter r f (TypeFilter _ t) =
  validateGeneralInstance r f (SingleType t)
validateTypeFilter r f (DefinesFilter t) =
  validateDefinesInstance r f t

validateAssignment :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> GeneralInstance -> [TypeFilter] -> m ()
validateAssignment r f t fs = mergeAllM (map (checkFilter t) fs) where
  checkFilter t1 (TypeFilter FilterRequires t2) = do
    checkGeneralMatch r f Covariant t1 (SingleType t2)
  checkFilter t1 (TypeFilter FilterAllows t2) = do
    checkGeneralMatch r f Contravariant t1 (SingleType t2)
  checkFilter t1@(TypeMerge _ _) (DefinesFilter t) =
    compileError $ "Merged type " ++ show t1 ++ " cannot satisfy defines constraint " ++ show t
  checkFilter t1@(SingleType t) (DefinesFilter f) = checkDefinesFilter f t
  requireExactlyOne t [_] = mergeDefaultM
  requireExactlyOne t []  =
    compileError $ "No types in intersection define " ++ show t
  requireExactlyOne t ts  =
    (compileError $ "Multiple types in intersection define " ++ show t) `mergeNestedM`
      (mergeAllM $ map (compileError . show) ts)
  checkDefinesFilter f2@(DefinesInstance n2 _) (JustTypeInstance t1) = do
    ps1' <- trDefines r t1 n2
    checkDefinesMatch r f f2 (DefinesInstance n2 ps1')
  checkDefinesFilter f2 (JustParamName n1) = do
      fs1 <- fmap (map dfType . filter isDefinesFilter) $ f `filterLookup` n1
      mergeAnyM (map (checkDefinesMatch r f f2) fs1) `reviseError`
        ("No filters imply " ++ show n1 ++ " defines " ++ show f2)

checkDefinesMatch :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamFilters -> DefinesInstance -> DefinesInstance -> m ()
checkDefinesMatch r f f2@(DefinesInstance n2 ps2) f1@(DefinesInstance n1 ps1)
  | n1 == n2 = do
    paired <- processParamPairs alwaysPairParams ps1 ps2
    variance <- trVariance r n2
    processParamPairs (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance (ParamSet paired)
    mergeDefaultM
  | otherwise = compileError $ "Constraint " ++ show f1 ++ " does not imply " ++ show f2

validateInstanceVariance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamVariances -> Variance -> GeneralInstance -> m ()
validateInstanceVariance r vm v (SingleType (JustTypeInstance (TypeInstance n ps))) = do
  vs <- trVariance r n
  paired <- processParamPairs alwaysPairParams vs ps
  mergeAllM (map (\(v2,p) -> validateInstanceVariance r vm (v `composeVariance` v2) p) paired)
validateInstanceVariance r vm v (TypeMerge MergeUnion ts) =
  mergeAllM (map (validateInstanceVariance r vm v) ts)
validateInstanceVariance r vm v (TypeMerge MergeIntersect ts) =
  mergeAllM (map (validateInstanceVariance r vm v) ts)
validateInstanceVariance r vm v (SingleType (JustParamName n)) =
  case n `Map.lookup` vm of
      Nothing -> compileError $ "Param " ++ show n ++ " is undefined"
      (Just v0) -> when (not $ v0 `paramAllowsVariance` v) $
                        compileError $ "Param " ++ show n ++ " cannot be " ++ show v

validateDefinesVariance :: (MergeableM m, CompileErrorM m, Monad m, TypeResolver r) =>
  r -> ParamVariances -> Variance -> DefinesInstance -> m ()
validateDefinesVariance r vm v (DefinesInstance n ps) = do
  vs <- trVariance r n
  paired <- processParamPairs alwaysPairParams vs ps
  mergeAllM (map (\(v2,p) -> validateInstanceVariance r vm (v `composeVariance` v2) p) paired)

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
