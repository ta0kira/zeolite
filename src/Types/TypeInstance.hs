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
  requiredParam,
  requiredSingleton,
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
  validateInstanceVariance,
  validateTypeFilter,
  validateTypeInstance,
) where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as Map

import Base.CompileError
import Base.MergeTree
import Base.Mergeable
import Types.GeneralType
import Types.Positional
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
  trRefines :: CompileErrorM m =>
    r -> TypeInstance -> CategoryName -> m InstanceParams
  -- Performs parameter substitution for defines.
  trDefines :: CompileErrorM m =>
    r -> TypeInstance -> CategoryName -> m InstanceParams
  -- Get the parameter variances for the category.
  trVariance :: CompileErrorM m =>
    r -> CategoryName -> m InstanceVariances
  -- Gets filters for the assigned parameters.
  trTypeFilters :: CompileErrorM m =>
    r -> TypeInstance -> m InstanceFilters
  -- Gets filters for the assigned parameters.
  trDefinesFilters :: CompileErrorM m =>
    r -> DefinesInstance -> m InstanceFilters
  -- Returns True if the type is concrete.
  trConcrete :: CompileErrorM m =>
    r -> CategoryName -> m Bool

data AnyTypeResolver = forall r. TypeResolver r => AnyTypeResolver r

instance TypeResolver AnyTypeResolver where
  trRefines (AnyTypeResolver r) = trRefines r
  trDefines (AnyTypeResolver r) = trDefines r
  trVariance (AnyTypeResolver r) = trVariance r
  trTypeFilters (AnyTypeResolver r) = trTypeFilters r
  trDefinesFilters (AnyTypeResolver r) = trDefinesFilters r
  trConcrete (AnyTypeResolver r) = trConcrete r

filterLookup :: CompileErrorM m =>
  ParamFilters -> ParamName -> m [TypeFilter]
filterLookup ps n = resolve $ n `Map.lookup` ps where
  resolve (Just x) = return x
  resolve _        = compileErrorM $ "Param " ++ show n ++ " not found"

getValueForParam :: CompileErrorM m =>
  ParamValues -> ParamName -> m GeneralInstance
getValueForParam pa n =
  case n `Map.lookup` pa of
       (Just x) -> return x
       _ -> compileErrorM $ "Param " ++ show n ++ " does not exist"

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

noInferredTypes :: CompileErrorM m => m (MergeTree InferredTypeGuess) -> m ()
noInferredTypes g = do
  g' <- g
  let gm = mapTypeGuesses g'
  "Type inference is not allowed here" !!> (mapErrorsM_ format $ Map.elems gm) where
    format = compileErrorM . reduceMergeTree showAny showAll show
    showAny gs = "Any of [ " ++ intercalate ", " gs ++ " ]"
    showAll gs = "All of [ " ++ intercalate ", " gs ++ " ]"

checkValueAssignment :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> ValueType -> ValueType -> m ()
checkValueAssignment r f t1 t2 = noInferredTypes $ checkValueTypeMatch r f Covariant t1 t2

checkValueTypeMatch :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> Variance -> ValueType -> ValueType -> m (MergeTree InferredTypeGuess)
checkValueTypeMatch r f v ts1@(ValueType r1 t1) ts2@(ValueType r2 t2) = result <!! message where
  message
    | v == Covariant     = "Cannot convert " ++ show ts1 ++ " -> "  ++ show ts2
    | v == Contravariant = "Cannot convert " ++ show ts1 ++ " <- "  ++ show ts2
    | otherwise          = "Cannot convert " ++ show ts1 ++ " <-> " ++ show ts2
  storageDir
    | r1 > r2   = Covariant
    | r1 < r2   = Contravariant
    | otherwise = Invariant
  result = do
    when (not $ storageDir `allowsVariance` v) $ compileErrorM "Incompatible storage modifiers"
    checkGeneralMatch r f v t1 t2

checkGeneralMatch :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> Variance ->
  GeneralInstance -> GeneralInstance -> m (MergeTree InferredTypeGuess)
checkGeneralMatch r f v t1 t2 = do
  ss <- collectFirstM [fmap Just bothSingle,return Nothing]
  collectFirstM [matchInferredRight,getMatcher ss] where
  matchNormal Invariant =
    mergeAllM [matchNormal Contravariant,matchNormal Covariant]
  matchNormal Contravariant =
    pairMergeTree mergeAnyM mergeAllM (checkSingleMatch r f Contravariant) (dualGeneralType t1) (dualGeneralType t2)
  matchNormal Covariant =
    pairMergeTree mergeAnyM mergeAllM (checkSingleMatch r f Covariant) t1 t2
  matchInferredRight = matchOnlyLeaf t2 >>= inferFrom
  inferFrom (JustInferredType p) = return $ mergeLeaf $ InferredTypeGuess p t1 v
  inferFrom _ = compileErrorM ""
  bothSingle = do
    t1' <- matchOnlyLeaf t1
    t2' <- matchOnlyLeaf t2
    return (t1',t2')
  getMatcher ss =
    case ss of
         Just (t1',t2') -> checkSingleMatch r f v t1' t2'
         Nothing        -> matchNormal v

checkSingleMatch :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> Variance ->
  TypeInstanceOrParam -> TypeInstanceOrParam -> m (MergeTree InferredTypeGuess)
checkSingleMatch _ _ _ (JustInferredType p1) _ =
  compileErrorM $ "Inferred parameter " ++ show p1 ++ " is not allowed on the left"
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

checkInstanceToInstance :: (CompileErrorM m, TypeResolver r) =>
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
  | otherwise = compileErrorM $ "Category " ++ show n2 ++ " is required but got " ++ show n1

checkParamToInstance :: (CompileErrorM m, TypeResolver r) =>
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
    ("No filters imply " ++ show t2 ++ " <- " ++ show n1 ++ " in " ++ show v ++ " contexts")
  where
    checkConstraintToInstance (TypeFilter FilterAllows t) =
      -- F -> x implies T -> x only if T -> F
      checkGeneralMatch r f v t (singleType $ JustTypeInstance t2)
    checkConstraintToInstance f2 =
      -- x -> F cannot imply T -> x
      compileErrorM $ "Constraint " ++ viewTypeFilter n1 f2 ++
                      " does not imply " ++ show t2 ++ " <- " ++ show n1
checkParamToInstance r f v@Covariant n1 t2@(TypeInstance _ _) = do
  cs1 <- fmap (filter isTypeFilter) $ f `filterLookup` n1
  mergeAnyM (map checkConstraintToInstance cs1) <!!
    ("No filters imply " ++ show n1 ++ " -> " ++ show t2 ++ " in " ++ show v ++ " contexts")
  where
    checkConstraintToInstance (TypeFilter FilterRequires t) =
      -- x -> F implies x -> T only if F -> T
      checkGeneralMatch r f v t (singleType $ JustTypeInstance t2)
    checkConstraintToInstance f2 =
      -- F -> x cannot imply x -> T
      -- DefinesInstance cannot be converted to TypeInstance
      compileErrorM $ "Constraint " ++ viewTypeFilter n1 f2 ++
                      " does not imply " ++ show n1 ++ " -> " ++ show t2

checkInstanceToParam :: (CompileErrorM m, TypeResolver r) =>
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
    ("No filters imply " ++ show n2 ++ " <- " ++ show t1 ++ " in " ++ show v ++ " contexts")
  where
    checkInstanceToConstraint (TypeFilter FilterRequires t) =
      -- x -> F implies x -> T only if F -> T
      checkGeneralMatch r f v (singleType $ JustTypeInstance t1) t
    checkInstanceToConstraint f2 =
      -- F -> x cannot imply x -> T
      -- DefinesInstance cannot be converted to TypeInstance
      compileErrorM $ "Constraint " ++ viewTypeFilter n2 f2 ++
                      " does not imply " ++ show n2 ++ " <- " ++ show t1
checkInstanceToParam r f v@Covariant t1@(TypeInstance _ _) n2 = do
  cs2 <- fmap (filter isTypeFilter) $ f `filterLookup` n2
  mergeAnyM (map checkInstanceToConstraint cs2) <!!
    ("No filters imply " ++ show t1 ++ " -> " ++ show n2 ++ " in " ++ show v ++ " contexts")
  where
    checkInstanceToConstraint (TypeFilter FilterAllows t) =
      -- F -> x implies T -> x only if T -> F
      checkGeneralMatch r f v (singleType $ JustTypeInstance t1) t
    checkInstanceToConstraint f2 =
      -- x -> F cannot imply T -> x
      compileErrorM $ "Constraint " ++ viewTypeFilter n2 f2 ++
                      " does not imply " ++ show t1 ++ " -> " ++ show n2

checkParamToParam :: (CompileErrorM m, TypeResolver r) =>
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
      ("No filters imply " ++ show n1 ++ " -> " ++ show n2)
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
          compileErrorM $ "Infinite recursion in " ++ show n1 ++ " -> " ++ show n2
        -- x -> F1, F2 -> y implies x -> y only if F1 -> F2
        | otherwise = checkGeneralMatch r f Covariant t1 t2
      checkConstraintToConstraint Covariant f1 f2 =
        -- x -> F1, y -> F2 cannot imply x -> y
        -- F1 -> x, F1 -> y cannot imply x -> y
        -- F1 -> x, y -> F2 cannot imply x -> y
        compileErrorM $ "Constraints " ++ viewTypeFilter n1 f1 ++ " and " ++
                        viewTypeFilter n2 f2 ++ " do not imply " ++
                        show n1 ++ " -> " ++ show n2
      checkConstraintToConstraint Contravariant (TypeFilter FilterAllows t1) (TypeFilter FilterRequires t2)
        | t1 == selfParam1 && t2 == selfParam2 =
          compileErrorM $ "Infinite recursion in " ++ show n1 ++ " <- " ++ show n2
        -- x <- F1, F2 <- y implies x <- y only if F1 <- F2
        | otherwise = checkGeneralMatch r f Contravariant t1 t2
      checkConstraintToConstraint Contravariant f1 f2 =
        -- x <- F1, y <- F2 cannot imply x <- y
        -- F1 <- x, F1 <- y cannot imply x <- y
        -- F1 <- x, y <- F2 cannot imply x <- y
        compileErrorM $ "Constraints " ++ viewTypeFilter n1 f1 ++ " and " ++
                        viewTypeFilter n2 f2 ++ " do not imply " ++
                        show n1 ++ " <- " ++ show n2
      checkConstraintToConstraint _ _ _ = undefined

validateGeneralInstance :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> GeneralInstance -> m ()
validateGeneralInstance r f = reduceMergeTree collectAllM_ collectAllM_ validateSingle where
  validateSingle (JustTypeInstance t) = validateTypeInstance r f t
  validateSingle (JustParamName _ n) = when (not $ n `Map.member` f) $
      compileErrorM $ "Param " ++ show n ++ " does not exist"
  validateSingle (JustInferredType n) = compileErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

validateTypeInstance :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> TypeInstance -> m ()
validateTypeInstance r f t@(TypeInstance _ ps) = do
  fa <- trTypeFilters r t
  processPairs_ (validateAssignment r f) ps fa
  mapErrorsM_ (validateGeneralInstance r f) (pValues ps) <?? ("In " ++ show t)

validateDefinesInstance :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> DefinesInstance -> m ()
validateDefinesInstance r f t@(DefinesInstance _ ps) = do
  fa <- trDefinesFilters r t
  processPairs_ (validateAssignment r f) ps fa
  mapErrorsM_ (validateGeneralInstance r f) (pValues ps) <?? ("In " ++ show t)

validateTypeFilter :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> TypeFilter -> m ()
validateTypeFilter r f (TypeFilter _ t)  = validateGeneralInstance r f t
validateTypeFilter r f (DefinesFilter t) = validateDefinesInstance r f t

validateAssignment :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> GeneralInstance -> [TypeFilter] -> m ()
validateAssignment r f t fs = mapErrorsM_ checkWithMessage fs where
  checkWithMessage f2 = checkFilter t f2 <?? ("In verification of filter " ++ show t ++ " " ++ show f2)
  checkFilter t1 (TypeFilter FilterRequires t2) =
    noInferredTypes $ checkGeneralMatch r f Covariant t1 t2
  checkFilter t1 (TypeFilter FilterAllows t2) =
    noInferredTypes $ checkGeneralMatch r f Contravariant t1 t2
  checkFilter t1 (DefinesFilter t2) = do
    t1' <- matchOnlyLeaf t1 <!! ("Merged type " ++ show t1 ++ " cannot satisfy defines constraint " ++ show t2)
    checkDefinesFilter t2 t1'
  checkDefinesFilter f2@(DefinesInstance n2 _) (JustTypeInstance t1) = do
    ps1' <- trDefines r t1 n2
    checkDefinesMatch r f f2 (DefinesInstance n2 ps1')
  checkDefinesFilter f2 (JustParamName _ n1) = do
      fs1 <- fmap (map dfType . filter isDefinesFilter) $ f `filterLookup` n1
      (collectFirstM_ $ map (checkDefinesMatch r f f2) fs1) <!!
        ("No filters imply " ++ show n1 ++ " defines " ++ show f2)
  checkDefinesFilter _ (JustInferredType n) =
    compileErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

checkDefinesMatch :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> DefinesInstance -> DefinesInstance -> m ()
checkDefinesMatch r f f2@(DefinesInstance n2 ps2) f1@(DefinesInstance n1 ps1)
  | n1 == n2 = do
    paired <- processPairs alwaysPair ps1 ps2
    variance <- trVariance r n2
    processPairs_ (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) variance (Positional paired)
  | otherwise = compileErrorM $ "Constraint " ++ show f1 ++ " does not imply " ++ show f2

validateInstanceVariance :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamVariances -> Variance -> GeneralInstance -> m ()
validateInstanceVariance r vm v = reduceMergeTree collectAllM_ collectAllM_ validateSingle where
  validateSingle (JustTypeInstance (TypeInstance n ps)) = do
    vs <- trVariance r n
    paired <- processPairs alwaysPair vs ps
    mapErrorsM_ (\(v2,p) -> validateInstanceVariance r vm (v `composeVariance` v2) p) paired
  validateSingle (JustParamName _ n) =
    case n `Map.lookup` vm of
        Nothing -> compileErrorM $ "Param " ++ show n ++ " is undefined"
        (Just v0) -> when (not $ v0 `allowsVariance` v) $
                          compileErrorM $ "Param " ++ show n ++ " cannot be " ++ show v
  validateSingle (JustInferredType n) =
    compileErrorM $ "Inferred param " ++ show n ++ " is not allowed here"

validateDefinesVariance :: (CompileErrorM m, TypeResolver r) =>
  r -> ParamVariances -> Variance -> DefinesInstance -> m ()
validateDefinesVariance r vm v (DefinesInstance n ps) = do
  vs <- trVariance r n
  paired <- processPairs alwaysPair vs ps
  mapErrorsM_ (\(v2,p) -> validateInstanceVariance r vm (v `composeVariance` v2) p) paired

uncheckedSubValueType :: CompileErrorM m =>
  (ParamName -> m GeneralInstance) -> ValueType -> m ValueType
uncheckedSubValueType replace (ValueType s t) = do
  t' <- uncheckedSubInstance replace t
  return $ ValueType s t'

uncheckedSubInstance :: CompileErrorM m => (ParamName -> m GeneralInstance) ->
  GeneralInstance -> m GeneralInstance
uncheckedSubInstance replace = reduceMergeTree subAny subAll subSingle where
  -- NOTE: Don't use mergeAnyM because it will fail if the union is empty.
  subAny = fmap mergeAny . sequence
  subAll = fmap mergeAll . sequence
  subSingle p@(JustParamName True _) = return $ singleType p
  subSingle (JustParamName _ n)      = replace n
  subSingle (JustInferredType n)     = replace n
  subSingle (JustTypeInstance t)     = fmap (singleType . JustTypeInstance) $ uncheckedSubSingle replace t

uncheckedSubSingle :: CompileErrorM m => (ParamName -> m GeneralInstance) ->
  TypeInstance -> m TypeInstance
uncheckedSubSingle replace (TypeInstance n (Positional ts)) = do
  ts' <- mapErrorsM (uncheckedSubInstance replace) ts
  return $ TypeInstance n (Positional ts')

uncheckedSubFilter :: CompileErrorM m =>
  (ParamName -> m GeneralInstance) -> TypeFilter -> m TypeFilter
uncheckedSubFilter replace (TypeFilter d t) = do
  t' <- uncheckedSubInstance replace t
  return (TypeFilter d t')
uncheckedSubFilter replace (DefinesFilter (DefinesInstance n ts)) = do
  ts' <- mapErrorsM (uncheckedSubInstance replace) (pValues ts)
  return (DefinesFilter (DefinesInstance n (Positional ts')))

uncheckedSubFilters :: CompileErrorM m =>
  (ParamName -> m GeneralInstance) -> ParamFilters -> m ParamFilters
uncheckedSubFilters replace fa = do
  fa' <- mapErrorsM subParam $ Map.toList fa
  return $ Map.fromList fa'
  where
    subParam (n,fs) = do
      fs' <- mapErrorsM (uncheckedSubFilter replace) fs
      return (n,fs')
