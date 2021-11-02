{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

module Types.TypeInstance (
  AnyTypeResolver(..),
  CategoryName(..),
  DefinesInstance(..),
  FilterDirection(..),
  GeneralInstance,
  InferredTypeGuess(..),
  InstanceFilters,
  InstanceParams,
  InstanceVariances,
  ParamFilters,
  ParamValues,
  ParamVariances,
  ParamName(..),
  StorageType(..),
  TypeFilter(..),
  TypeInstance(..),
  TypeInstanceOrParam(..),
  TypeResolver(..),
  ValueType(..),
  checkDefinesMatch,
  checkGeneralMatch,
  checkValueAssignment,
  checkValueTypeImmutable,
  checkValueTypeMatch,
  filterLookup,
  fixTypeParams,
  flipFilter,
  getValueForParam,
  hasInferredParams,
  isDefinesFilter,
  isRequiresFilter,
  isWeakValue,
  mapTypeGuesses,
  noInferredTypes,
  replaceSelfFilter,
  replaceSelfInstance,
  replaceSelfSingle,
  replaceSelfValueType,
  requiredParam,
  requiredSingleton,
  selfType,
  uncheckedSubFilter,
  uncheckedSubFilters,
  uncheckedSubInstance,
  uncheckedSubSingle,
  uncheckedSubValueType,
  unfixTypeParams,
  validateAssignment,
  validateDefinesInstance,
  validateDefinesVariance,
  validateGeneralInstance,
  validateGeneralInstanceForCall,
  validateInstanceVariance,
  validateTypeInstance,
  validateTypeInstanceForCall,
  validateTypeFilter,
) where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.MergeTree
import Base.Mergeable
import Base.Positional
import Types.Variance


type GeneralInstance = GeneralType TypeInstanceOrParam

instance Show GeneralInstance where
  show = reduceMergeTree showAny showAll show where
    showAny [] = "all"
    showAny ts = "[" ++ intercalate "|" ts ++ "]"
    showAll [] = "any"
    showAll ts = "[" ++ intercalate "&" ts ++ "]"

data StorageType =
  WeakValue |
  OptionalValue |
  RequiredValue
  deriving (Eq,Ord)

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
requiredSingleton n = ValueType RequiredValue $ singleType $ JustTypeInstance $ TypeInstance n (Positional [])

requiredParam :: ParamName -> ValueType
requiredParam n = ValueType RequiredValue $ singleType $ JustParamName False n

data CategoryName =
  CategoryName {
    tnName :: String
  } |
  BuiltinBool |
  BuiltinChar |
  BuiltinCharBuffer |
  BuiltinInt |
  BuiltinFloat |
  BuiltinString |
  BuiltinFormatted |
  BuiltinOrder |
  CategoryNone

instance Show CategoryName where
  show (CategoryName n)    = n
  show BuiltinBool         = "Bool"
  show BuiltinChar         = "Char"
  show BuiltinCharBuffer   = "CharBuffer"
  show BuiltinInt          = "Int"
  show BuiltinFloat        = "Float"
  show BuiltinString       = "String"
  show BuiltinFormatted    = "Formatted"
  show BuiltinOrder        = "Order"
  show CategoryNone        = "(none)"

instance Eq CategoryName where
  c1 == c2 = show c1 == show c2

instance Ord CategoryName where
  c1 <= c2 = show c1 <= show c2

data ParamName =
  ParamName {
    pnName :: String
  } |
  ParamSelf
  deriving (Eq,Ord)

instance Show ParamName where
  show (ParamName n) = n
  show ParamSelf     = "#self"

selfType :: GeneralInstance
selfType = singleType $ JustParamName True ParamSelf

data TypeInstance =
  TypeInstance {
    tiName :: CategoryName,
    tiParams :: InstanceParams
  }
  deriving (Eq,Ord)

instance Show TypeInstance where
  show (TypeInstance n (Positional [])) = show n
  show (TypeInstance n (Positional ts)) =
    show n ++ "<" ++ intercalate "," (map show ts) ++ ">"

data DefinesInstance =
  DefinesInstance {
    diName :: CategoryName,
    diParams :: InstanceParams
  }
  deriving (Eq,Ord)

instance Show DefinesInstance where
  show (DefinesInstance n (Positional [])) = show n
  show (DefinesInstance n (Positional ts)) =
    show n ++ "<" ++ intercalate "," (map show ts) ++ ">"

data InferredTypeGuess =
  InferredTypeGuess {
    itgParam :: ParamName,
    itgGuess :: GeneralInstance,
    itgVariance :: Variance
  }
  deriving (Eq,Ord)

instance Show InferredTypeGuess where
  show (InferredTypeGuess n g v) = show n ++ " = " ++ show g ++ " (" ++ show v ++ ")"

data TypeInstanceOrParam =
  JustTypeInstance {
    jtiType :: TypeInstance
  } |
  JustParamName {
    jpnFixed :: Bool,
    jpnName :: ParamName
  } |
  JustInferredType {
    jitParam :: ParamName
  }
  deriving (Eq,Ord)

instance Show TypeInstanceOrParam where
  show (JustTypeInstance t)   = show t
  show (JustParamName True n) = show n ++ "/*fixed*/"
  show (JustParamName _ n)    = show n
  show (JustInferredType n)   = show n ++ "/*inferred*/"

data FilterDirection =
  FilterRequires |
  FilterAllows
  deriving (Eq,Ord)

flipFilter :: FilterDirection -> FilterDirection
flipFilter FilterRequires = FilterAllows
flipFilter FilterAllows   = FilterRequires

data TypeFilter =
  TypeFilter {
    tfDirection :: FilterDirection,
    -- NOTE: This is GeneralInstance instead of TypeInstanceOrParam so that
    -- param substitution can be done safely.
    tfType :: GeneralInstance
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
isRequiresFilter (TypeFilter FilterRequires _) = True
isRequiresFilter _                             = False

isDefinesFilter :: TypeFilter -> Bool
isDefinesFilter (DefinesFilter _) = True
isDefinesFilter _                 = False

viewTypeFilter :: ParamName -> TypeFilter -> String
viewTypeFilter n f = show n ++ " " ++ show f

hasInferredParams :: GeneralInstance -> Bool
hasInferredParams = reduceMergeTree mergeAny mergeAny checkSingle where
  checkSingle (JustInferredType _) = True
  checkSingle _                    = False

type InstanceParams = Positional GeneralInstance
type InstanceVariances = Positional Variance
type InstanceFilters = Positional [TypeFilter]

type ParamFilters   = Map.Map ParamName [TypeFilter]
type ParamVariances = Map.Map ParamName Variance
type ParamValues    = Map.Map ParamName GeneralInstance

class TypeResolver r where
  -- Performs parameter substitution for refines.
  trRefines :: CollectErrorsM m =>
    r -> TypeInstance -> CategoryName -> m InstanceParams
  -- Performs parameter substitution for defines.
  trDefines :: CollectErrorsM m =>
    r -> TypeInstance -> CategoryName -> m InstanceParams
  -- Gets the parameter variances for the category.
  trVariance :: CollectErrorsM m =>
    r -> CategoryName -> m InstanceVariances
  -- Gets filters for the assigned parameters.
  trTypeFilters :: CollectErrorsM m =>
    r -> TypeInstance -> m InstanceFilters
  -- Gets filters for the assigned parameters.
  trDefinesFilters :: CollectErrorsM m =>
    r -> DefinesInstance -> m InstanceFilters
  -- Returns True if the type is concrete.
  trConcrete :: CollectErrorsM m =>
    r -> CategoryName -> m Bool
  -- Returns True if the type is immutable.
  trImmutable ::  CollectErrorsM m =>
    r -> CategoryName -> m Bool

data AnyTypeResolver = forall r. TypeResolver r => AnyTypeResolver r

instance TypeResolver AnyTypeResolver where
  trRefines (AnyTypeResolver r) = trRefines r
  trDefines (AnyTypeResolver r) = trDefines r
  trVariance (AnyTypeResolver r) = trVariance r
  trTypeFilters (AnyTypeResolver r) = trTypeFilters r
  trDefinesFilters (AnyTypeResolver r) = trDefinesFilters r
  trConcrete (AnyTypeResolver r) = trConcrete r
  trImmutable (AnyTypeResolver r) = trImmutable r

filterLookup :: ErrorContextM m =>
  ParamFilters -> ParamName -> m [TypeFilter]
filterLookup ps n = resolve $ n `Map.lookup` ps where
  resolve (Just x) = return x
  resolve _        = compilerErrorM $ "Param " ++ show n ++ " not found"

getValueForParam :: ErrorContextM m =>
  ParamValues -> ParamName -> m GeneralInstance
getValueForParam pa n =
  case n `Map.lookup` pa of
       (Just x) -> return x
       _ -> compilerErrorM $ "Param " ++ show n ++ " not found"

fixTypeParams :: GeneralInstance -> GeneralInstance
fixTypeParams = setParamsFixed True

unfixTypeParams :: GeneralInstance -> GeneralInstance
unfixTypeParams = setParamsFixed False

setParamsFixed :: Bool -> GeneralInstance -> GeneralInstance
setParamsFixed f = mapGeneralType set where
  set (JustTypeInstance (TypeInstance t ts)) =
    JustTypeInstance $ TypeInstance t $ fmap (setParamsFixed f) ts
  set (JustParamName _ n2) = JustParamName f n2
  set t = t

mapTypeGuesses :: MergeTree InferredTypeGuess -> Map.Map ParamName (MergeTree InferredTypeGuess)
mapTypeGuesses = reduceMergeTree mergeAny mergeAll leafToMap where
  leafToMap i = Map.fromList [(itgParam i,mergeLeaf i)]

noInferredTypes :: CollectErrorsM m => m (MergeTree InferredTypeGuess) -> m ()
noInferredTypes g = do
  g' <- g
  let gm = mapTypeGuesses g'
  "Type inference is not allowed here" !!> mapCompilerM_ format (Map.elems gm) where
    format = compilerErrorM . reduceMergeTree showAny showAll show
    showAny gs = "Any of [ " ++ intercalate ", " gs ++ " ]"
    showAll gs = "All of [ " ++ intercalate ", " gs ++ " ]"

checkValueAssignment :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ValueType -> ValueType -> m ()
checkValueAssignment r f t1 t2 = noInferredTypes $ checkValueTypeMatch r f Covariant t1 t2

checkValueTypeImmutable :: (CollectErrorsM m, TypeResolver r) => r -> ParamFilters -> ValueType -> m Bool
checkValueTypeImmutable r _ (ValueType _ t) = reduceMergeTree anyOp allOp leafOp t where
  anyOp = fmap (all id) . sequence
  allOp = fmap (any id) . sequence
  leafOp (JustTypeInstance (TypeInstance n _)) = trImmutable r n
  leafOp _ = return False

checkValueTypeMatch :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> ValueType -> ValueType -> m (MergeTree InferredTypeGuess)
checkValueTypeMatch r f v (ValueType r1 t1) (ValueType r2 t2) = result where
  result = do
    when (not $ storageDir `allowsVariance` v) $ compilerErrorM "Incompatible storage modifiers"
    checkGeneralMatch r f v t1 t2
  storageDir
    | r1 > r2   = Covariant
    | r1 < r2   = Contravariant
    | otherwise = Invariant

checkGeneralMatch :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance ->
  GeneralInstance -> GeneralInstance -> m (MergeTree InferredTypeGuess)
checkGeneralMatch r f v t1 t2 = message !!> result where
  result = do
    ss <- collectFirstM [fmap Just bothSingle,return Nothing]
    collectFirstM [matchInferredRight,getMatcher ss]
  message
    | v == Covariant     = "Cannot convert " ++ show t1 ++ " -> "  ++ show t2
    | v == Contravariant = "Cannot convert " ++ show t1 ++ " <- "  ++ show t2
    | otherwise          = "Cannot convert " ++ show t1 ++ " <-> " ++ show t2
  matchNormal Invariant =
    mergeAllM [matchNormal Contravariant,matchNormal Covariant]
  matchNormal Contravariant =
    pairMergeTree mergeAnyM mergeAllM (checkSingleMatch r f Contravariant) (dualGeneralType t1) (dualGeneralType t2)
  matchNormal Covariant =
    pairMergeTree mergeAnyM mergeAllM (checkSingleMatch r f Covariant) t1 t2
  matchInferredRight = matchOnlyLeaf t2 >>= inferFrom
  inferFrom (JustInferredType p) = return $ mergeLeaf $ InferredTypeGuess p t1 v
  inferFrom _ = emptyErrorM
  bothSingle = do
    t1' <- matchOnlyLeaf t1
    t2' <- matchOnlyLeaf t2
    return (t1',t2')
  getMatcher ss =
    case ss of
         Just (t1',t2') -> checkSingleMatch r f v t1' t2'
         Nothing        -> matchNormal v

checkSingleMatch :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance ->
  TypeInstanceOrParam -> TypeInstanceOrParam -> m (MergeTree InferredTypeGuess)
checkSingleMatch _ _ _ (JustInferredType p1) _ =
  compilerErrorM $ "Inferred parameter " ++ show p1 ++ " is not allowed on the left"
checkSingleMatch _ _ v t1 (JustInferredType p2) =
  return $ mergeLeaf $ InferredTypeGuess p2 (singleType t1) v
checkSingleMatch r f v (JustTypeInstance t1) (JustTypeInstance t2) =
  checkInstanceToInstance r f v t1 t2
checkSingleMatch r f v (JustParamName _ p1) (JustTypeInstance t2) =
  checkParamToInstance r f v p1 t2
checkSingleMatch r f v (JustTypeInstance t1) (JustParamName _ p2) =
  checkInstanceToParam r f v t1 p2
checkSingleMatch r f v (JustParamName _ p1) (JustParamName _ p2) =
  checkParamToParam r f v p1 p2

checkInstanceToInstance :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> TypeInstance -> TypeInstance -> m (MergeTree InferredTypeGuess)
checkInstanceToInstance r f v t1@(TypeInstance n1 ps1) t2@(TypeInstance n2 ps2)
  | n1 == n2 = do
    paired <- fmap Positional $ processPairs alwaysPair ps1 ps2
    variance <- fmap (fmap (composeVariance v)) $ trVariance r n1
    processPairsM (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance paired
  | v == Covariant = do
    ps1' <- trRefines r t1 n2
    checkInstanceToInstance r f Covariant (TypeInstance n2 ps1') t2
  | v == Contravariant = do
    ps2' <- trRefines r t2 n1
    checkInstanceToInstance r f Contravariant t1 (TypeInstance n1 ps2')
  | otherwise = compilerErrorM $ "Category " ++ show n2 ++ " is required but got " ++ show n1

checkParamToInstance :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> ParamName -> TypeInstance -> m (MergeTree InferredTypeGuess)
checkParamToInstance r f Invariant n1 t2 =
  -- Implicit equality, inferred by n1 <-> t2.
  mergeAllM [
      checkParamToInstance r f Covariant     n1 t2,
      checkParamToInstance r f Contravariant n1 t2
    ]
checkParamToInstance r f v@Contravariant n1 t2@(TypeInstance _ _) = do
  cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
  mergeAnyM (map checkConstraintToInstance cs2) <!!
    "No filters imply " ++ show t2 ++ " <- " ++ show n1 ++ " in " ++ show v ++ " contexts"
  where
    checkConstraintToInstance (TypeFilter FilterAllows t) =
      -- F -> x implies T -> x only if T -> F
      checkGeneralMatch r f v t (singleType $ JustTypeInstance t2)
    checkConstraintToInstance f2 =
      -- x -> F cannot imply T -> x
      compilerErrorM $ "Constraint " ++ viewTypeFilter n1 f2 ++
                      " does not imply " ++ show t2 ++ " <- " ++ show n1
checkParamToInstance r f v@Covariant n1 t2@(TypeInstance _ _) = do
  cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
  mergeAnyM (map checkConstraintToInstance cs1) <!!
    "No filters imply " ++ show n1 ++ " -> " ++ show t2 ++ " in " ++ show v ++ " contexts"
  where
    checkConstraintToInstance (TypeFilter FilterRequires t) =
      -- x -> F implies x -> T only if F -> T
      checkGeneralMatch r f v t (singleType $ JustTypeInstance t2)
    checkConstraintToInstance f2 =
      -- F -> x cannot imply x -> T
      -- DefinesInstance cannot be converted to TypeInstance
      compilerErrorM $ "Constraint " ++ viewTypeFilter n1 f2 ++
                      " does not imply " ++ show n1 ++ " -> " ++ show t2

checkInstanceToParam :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> TypeInstance -> ParamName -> m (MergeTree InferredTypeGuess)
checkInstanceToParam r f Invariant t1 n2 =
  -- Implicit equality, inferred by t1 <-> n2.
  mergeAllM [
      checkInstanceToParam r f Covariant     t1 n2,
      checkInstanceToParam r f Contravariant t1 n2
    ]
checkInstanceToParam r f v@Contravariant t1@(TypeInstance _ _) n2 = do
  cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
  mergeAnyM (map checkInstanceToConstraint cs1) <!!
    "No filters imply " ++ show n2 ++ " <- " ++ show t1 ++ " in " ++ show v ++ " contexts"
  where
    checkInstanceToConstraint (TypeFilter FilterRequires t) =
      -- x -> F implies x -> T only if F -> T
      checkGeneralMatch r f v (singleType $ JustTypeInstance t1) t
    checkInstanceToConstraint f2 =
      -- F -> x cannot imply x -> T
      -- DefinesInstance cannot be converted to TypeInstance
      compilerErrorM $ "Constraint " ++ viewTypeFilter n2 f2 ++
                      " does not imply " ++ show n2 ++ " <- " ++ show t1
checkInstanceToParam r f v@Covariant t1@(TypeInstance _ _) n2 = do
  cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
  mergeAnyM (map checkInstanceToConstraint cs2) <!!
    "No filters imply " ++ show t1 ++ " -> " ++ show n2 ++ " in " ++ show v ++ " contexts"
  where
    checkInstanceToConstraint (TypeFilter FilterAllows t) =
      -- F -> x implies T -> x only if T -> F
      checkGeneralMatch r f v (singleType $ JustTypeInstance t1) t
    checkInstanceToConstraint f2 =
      -- x -> F cannot imply T -> x
      compilerErrorM $ "Constraint " ++ viewTypeFilter n2 f2 ++
                      " does not imply " ++ show t1 ++ " -> " ++ show n2

checkParamToParam :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> ParamName -> ParamName -> m (MergeTree InferredTypeGuess)
checkParamToParam r f Invariant n1 n2
  | n1 == n2 = return maxBound
  | otherwise =
    -- Implicit equality, inferred by n1 <-> n2.
    mergeAllM [
        checkParamToParam r f Covariant     n1 n2,
        checkParamToParam r f Contravariant n1 n2
      ]
checkParamToParam r f v n1 n2
  | n1 == n2 = return maxBound
  | otherwise = do
    cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
    cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
    let typeFilters = [(c1,c2) | c1 <- cs1, c2 <- cs2] ++
                      [(self1,c2) | c2 <- cs2] ++
                      [(c1,self2) | c1 <- cs1]
    mergeAnyM (map (\(c1,c2) -> checkConstraintToConstraint v c1 c2) typeFilters) <!!
      "No filters imply " ++ show n1 ++ " -> " ++ show n2
    where
      selfParam1 = singleType $ JustParamName False n1
      selfParam2 = singleType $ JustParamName False n2
      self1
        | v == Covariant = TypeFilter FilterRequires selfParam1
        | otherwise      = TypeFilter FilterAllows   selfParam1
      self2
        | v == Covariant = TypeFilter FilterAllows   selfParam2
        | otherwise      = TypeFilter FilterRequires selfParam2
      checkConstraintToConstraint Covariant (TypeFilter FilterRequires t1) (TypeFilter FilterAllows t2)
        | t1 == selfParam1 && t2 == selfParam2 =
          compilerErrorM $ "Infinite recursion in " ++ show n1 ++ " -> " ++ show n2
        -- x -> F1, F2 -> y implies x -> y only if F1 -> F2
        | otherwise = checkGeneralMatch r f Covariant t1 t2
      checkConstraintToConstraint Covariant f1 f2 =
        -- x -> F1, y -> F2 cannot imply x -> y
        -- F1 -> x, F1 -> y cannot imply x -> y
        -- F1 -> x, y -> F2 cannot imply x -> y
        compilerErrorM $ "Constraints " ++ viewTypeFilter n1 f1 ++ " and " ++
                        viewTypeFilter n2 f2 ++ " do not imply " ++
                        show n1 ++ " -> " ++ show n2
      checkConstraintToConstraint Contravariant (TypeFilter FilterAllows t1) (TypeFilter FilterRequires t2)
        | t1 == selfParam1 && t2 == selfParam2 =
          compilerErrorM $ "Infinite recursion in " ++ show n1 ++ " <- " ++ show n2
        -- x <- F1, F2 <- y implies x <- y only if F1 <- F2
        | otherwise = checkGeneralMatch r f Contravariant t1 t2
      checkConstraintToConstraint Contravariant f1 f2 =
        -- x <- F1, y <- F2 cannot imply x <- y
        -- F1 <- x, F1 <- y cannot imply x <- y
        -- F1 <- x, y <- F2 cannot imply x <- y
        compilerErrorM $ "Constraints " ++ viewTypeFilter n1 f1 ++ " and " ++
                        viewTypeFilter n2 f2 ++ " do not imply " ++
                        show n1 ++ " <- " ++ show n2
      checkConstraintToConstraint _ _ _ = undefined

validateGeneralInstanceForCall :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> GeneralInstance -> m ()
validateGeneralInstanceForCall r f = reduceMergeTree collectAllM_ collectAllM_ validateSingle where
  validateSingle (JustTypeInstance t) = validateTypeInstanceForCall r f t
  validateSingle (JustParamName _ n) = when (not $ n `Map.member` f) $
      compilerErrorM $ "Param " ++ show n ++ " not found"
  validateSingle (JustInferredType n) = compilerErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

validateTypeInstanceForCall :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> TypeInstance -> m ()
validateTypeInstanceForCall r f t@(TypeInstance _ ps) = do
  fa <- trTypeFilters r t
  processPairs_ (validateAssignment r f) ps fa
  mapCompilerM_ (validateGeneralInstanceForCall r f) (pValues ps) <?? "In " ++ show t

validateAssignment :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> GeneralInstance -> [TypeFilter] -> m ()
validateAssignment r f t fs = mapCompilerM_ checkWithMessage fs where
  checkWithMessage f2 = checkFilter t f2 <?? "In verification of filter " ++ show t ++ " " ++ show f2
  checkFilter t1 (TypeFilter FilterRequires t2) =
    noInferredTypes $ checkGeneralMatch r f Covariant t1 t2
  checkFilter t1 (TypeFilter FilterAllows t2) =
    noInferredTypes $ checkGeneralMatch r f Contravariant t1 t2
  checkFilter t1 (DefinesFilter t2) = do
    t1' <- matchOnlyLeaf t1 <!! "Merged type " ++ show t1 ++ " cannot satisfy defines constraint " ++ show t2
    checkDefinesFilter t2 t1'
  checkDefinesFilter f2@(DefinesInstance n2 _) (JustTypeInstance t1) = do
    ps1' <- trDefines r t1 n2
    checkDefinesMatch r f f2 (DefinesInstance n2 ps1')
  checkDefinesFilter f2 (JustParamName _ n1) = do
      fs1 <- fmap (map dfType . filter isDefinesFilter) $ f `filterLookup` n1
      (collectFirstM_ $ map (checkDefinesMatch r f f2) fs1) <!!
        "No filters imply " ++ show n1 ++ " defines " ++ show f2
  checkDefinesFilter _ (JustInferredType n) =
    compilerErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

checkDefinesMatch :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> DefinesInstance -> DefinesInstance -> m ()
checkDefinesMatch r f f2@(DefinesInstance n2 ps2) f1@(DefinesInstance n1 ps1)
  | n1 == n2 = do
    paired <- processPairs alwaysPair ps1 ps2
    variance <- trVariance r n2
    processPairs_ (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance (Positional paired)
  | otherwise = compilerErrorM $ "Constraint " ++ show f1 ++ " does not imply " ++ show f2

validateGeneralInstance :: (CollectErrorsM m, TypeResolver r) =>
  r -> Set.Set ParamName -> GeneralInstance -> m ()
validateGeneralInstance r params = reduceMergeTree collectAllM_ collectAllM_ validateSingle where
  validateSingle (JustTypeInstance t) = validateTypeInstance r params t
  validateSingle (JustParamName _ n) = when (not $ n `Set.member` params) $
      compilerErrorM $ "Param " ++ show n ++ " not found"
  validateSingle (JustInferredType n) = compilerErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

validateTypeInstance :: (CollectErrorsM m, TypeResolver r) =>
  r -> Set.Set ParamName -> TypeInstance -> m ()
validateTypeInstance r params t@(TypeInstance _ ps) = do
  _ <- trTypeFilters r t  -- This just ensures that t exists.
  mapCompilerM_ (validateGeneralInstance r params) (pValues ps) <?? "In " ++ show t

validateDefinesInstance :: (CollectErrorsM m, TypeResolver r) =>
  r -> Set.Set ParamName -> DefinesInstance -> m ()
validateDefinesInstance r params t@(DefinesInstance _ ps) = do
  _ <- trDefinesFilters r t  -- This just ensures that t exists.
  mapCompilerM_ (validateGeneralInstance r params) (pValues ps) <?? "In " ++ show t

validateTypeFilter :: (CollectErrorsM m, TypeResolver r) =>
  r -> Set.Set ParamName -> TypeFilter -> m ()
validateTypeFilter r params (TypeFilter _ t)  = validateGeneralInstance r params t
validateTypeFilter r params (DefinesFilter t) = validateDefinesInstance r params t

validateInstanceVariance :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamVariances -> Variance -> GeneralInstance -> m ()
validateInstanceVariance r vm v = reduceMergeTree collectAllM_ collectAllM_ validateSingle where
  vm' = Map.insert ParamSelf Covariant vm
  validateSingle (JustTypeInstance (TypeInstance n ps)) = do
    vs <- trVariance r n
    paired <- processPairs alwaysPair vs ps
    mapCompilerM_ (\(v2,p) -> validateInstanceVariance r vm' (v `composeVariance` v2) p) paired
  validateSingle (JustParamName _ n) =
    case n `Map.lookup` vm' of
        Nothing -> compilerErrorM $ "Param " ++ show n ++ " is undefined"
        (Just v0) -> when (not $ v0 `allowsVariance` v) $
                          compilerErrorM $ "Param " ++ show n ++ " cannot be " ++ show v
  validateSingle (JustInferredType n) =
    compilerErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

validateDefinesVariance :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamVariances -> Variance -> DefinesInstance -> m ()
validateDefinesVariance r vm v (DefinesInstance n ps) = do
  vs <- trVariance r n
  paired <- processPairs alwaysPair vs ps
  mapCompilerM_ (\(v2,p) -> validateInstanceVariance r vm' (v `composeVariance` v2) p) paired where
    vm' = Map.insert ParamSelf Covariant vm

uncheckedSubValueType :: CollectErrorsM m =>
  (ParamName -> m GeneralInstance) -> ValueType -> m ValueType
uncheckedSubValueType replace (ValueType s t) = do
  t' <- uncheckedSubInstance replace t
  return $ ValueType s t'

uncheckedSubInstance :: CollectErrorsM m => (ParamName -> m GeneralInstance) ->
  GeneralInstance -> m GeneralInstance
uncheckedSubInstance replace = reduceMergeTree subAny subAll subSingle where
  -- NOTE: Don't use mergeAnyM because it will fail if the union is empty.
  subAny = fmap mergeAny . sequence
  subAll = fmap mergeAll . sequence
  subSingle p@(JustParamName _ ParamSelf) = return $ singleType p
  subSingle p@(JustParamName True _)      = return $ singleType p
  subSingle (JustParamName _ n)           = replace n
  subSingle (JustInferredType n)          = replace n
  subSingle (JustTypeInstance t)          = fmap (singleType . JustTypeInstance) $ uncheckedSubSingle replace t

uncheckedSubSingle :: CollectErrorsM m => (ParamName -> m GeneralInstance) ->
  TypeInstance -> m TypeInstance
uncheckedSubSingle replace (TypeInstance n (Positional ts)) = do
  ts' <- mapCompilerM (uncheckedSubInstance replace) ts
  return $ TypeInstance n (Positional ts')

uncheckedSubFilter :: CollectErrorsM m =>
  (ParamName -> m GeneralInstance) -> TypeFilter -> m TypeFilter
uncheckedSubFilter replace (TypeFilter d t) = do
  t' <- uncheckedSubInstance replace t
  return (TypeFilter d t')
uncheckedSubFilter replace (DefinesFilter (DefinesInstance n ts)) = do
  ts' <- mapCompilerM (uncheckedSubInstance replace) (pValues ts)
  return (DefinesFilter (DefinesInstance n (Positional ts')))

uncheckedSubFilters :: CollectErrorsM m =>
  (ParamName -> m GeneralInstance) -> ParamFilters -> m ParamFilters
uncheckedSubFilters replace fa = do
  fa' <- mapCompilerM subParam $ Map.toList fa
  return $ Map.fromList fa'
  where
    subParam (n,fs) = do
      fs' <- mapCompilerM (uncheckedSubFilter replace) fs
      return (n,fs')

replaceSelfValueType :: CollectErrorsM m =>
  GeneralInstance -> ValueType -> m ValueType
replaceSelfValueType self (ValueType s t) = do
  t' <- replaceSelfInstance self t
  return $ ValueType s t'

replaceSelfInstance :: CollectErrorsM m =>
  GeneralInstance -> GeneralInstance -> m GeneralInstance
replaceSelfInstance self = reduceMergeTree subAny subAll subSingle where
  -- NOTE: Don't use mergeAnyM because it will fail if the union is empty.
  subAny = fmap mergeAny . sequence
  subAll = fmap mergeAll . sequence
  subSingle (JustParamName _ ParamSelf) = return self
  subSingle (JustTypeInstance t)        = fmap (singleType . JustTypeInstance) $ replaceSelfSingle self t
  subSingle p                           = return $ singleType p

replaceSelfSingle :: CollectErrorsM m =>
  GeneralInstance -> TypeInstance -> m TypeInstance
replaceSelfSingle self (TypeInstance n (Positional ts)) = do
  ts' <- mapCompilerM (replaceSelfInstance self) ts
  return $ TypeInstance n (Positional ts')

replaceSelfFilter :: CollectErrorsM m =>
  GeneralInstance -> TypeFilter -> m TypeFilter
replaceSelfFilter self (TypeFilter d t) = do
  t' <- replaceSelfInstance self t
  return (TypeFilter d t')
replaceSelfFilter self (DefinesFilter (DefinesInstance n ts)) = do
  ts' <- mapCompilerM (replaceSelfInstance self) (pValues ts)
  return (DefinesFilter (DefinesInstance n (Positional ts')))
