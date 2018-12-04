{-# LANGUAGE Safe #-}

module TypeInstance (
  AssignedParams,
  GeneralInstance,
  InstanceParams,
  ParamFilters,
  ParamName,
  TypeCategoryInstance(..),
  TypeFilter(..),
  TypeName,
  TypeParam(..),
  TypeResolver(..),
  checkGeneralMatch,
  composeVariance,
  paramAllowsVariance,
) where

import qualified Data.Map as Map

import TypesBase


type GeneralInstance = GeneralType TypeCategoryInstance

newtype TypeName =
  TypeName {
    tnName :: String
  }
  deriving (Eq,Ord,Show)

newtype ParamName =
  ParamName {
    pnName :: String
  }
  deriving (Eq,Ord,Show)

data TypeFilter =
  TypeFilter {
    tfVariance :: Variance,
    tfType :: TypeCategoryInstance
  }
  deriving (Eq,Show)

data TypeParam =
  TypeParam {
    tpName :: ParamName
  }
  deriving (Eq,Show)

type InstanceParams = ParamSet GeneralInstance

data TypeCategoryInstance =
  TypeCategoryInstance {
    tciName :: TypeName,
    tciOptional :: Bool,
    tciParams :: InstanceParams
  } |
  TypeCategoryParam {
    tcpParam :: TypeParam
  }
  deriving (Eq,Show)

type AssignedParams = Map.Map ParamName GeneralInstance

type ParamFilters = Map.Map ParamName [TypeFilter]

data TypeResolver m p =
  TypeResolver {
    -- Validates an instance's param args against required filters.
    tfValidate :: ParamFilters -> TypeCategoryInstance -> m(),
    -- Convert an instance of one category to an instance of the other.
    trFind :: TypeName -> TypeName -> InstanceParams -> m (p,InstanceParams),
    -- Labels params for an instance using the category's param names.
    trParams :: TypeName -> InstanceParams -> m AssignedParams,
    -- Get the parameter variances for the category.
    trVariance :: TypeName -> m [Variance]
  }

filterLookup :: (CompileErrorM m, Monad m) =>
  ParamFilters -> ParamName -> m [TypeFilter]
filterLookup ps n = resolve $ n `Map.lookup` ps where
  resolve (Just x) = return x
  resolve _        = compileError $ "Param " ++ show n ++ " not found"

-- NOTE: This doesn't verify the filters required to create an instance of a
-- type category. That should be done during instantiation of the instances and
-- during validation of the category system. (This does verify filters imposed
-- by individual free params, though.)
checkGeneralMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> GeneralInstance ->
  GeneralInstance -> m p
checkGeneralMatch r f v ts1 ts2 = checkGeneralType (checkSingleMatch r f v) ts1 ts2

checkSingleMatch :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> TypeCategoryInstance ->
  TypeCategoryInstance -> m p
checkSingleMatch r f v (TypeCategoryInstance n1 o1 ps1) (TypeCategoryInstance n2 o2 ps2)
  | o1 && not o2 = compileError $ "Cannot convert from optional to non-optional"
  | otherwise = checkInstanceToInstance r f v (n1,ps1) (n2,ps2)
checkSingleMatch r f v (TypeCategoryParam p1) (TypeCategoryInstance n2 _ ps2) =
  -- Can convert to optional if the types are compatible.
  checkParamToInstance r f v p1 (n2,ps2)
checkSingleMatch r f v (TypeCategoryInstance n1 o1 ps1) (TypeCategoryParam p2)
  | o1 = compileError $ "Cannot convert from optional to param " ++ show p2
  | otherwise = checkInstanceToParam r f v (n1,ps1) p2
checkSingleMatch r f v (TypeCategoryParam p1) (TypeCategoryParam p2) =
  checkParamToParam r f v p1 p2

checkInstanceToInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> (TypeName,InstanceParams) ->
  (TypeName,InstanceParams) -> m p
checkInstanceToInstance r f Invariant (n1,ps1) (n2,ps2) = fixMessage check where
  check = do
    -- Check for isomorphism, but not necessarily equality.
    checkInstanceToInstance r f Covariant     (n1,ps1) (n2,ps2)
    checkInstanceToInstance r f Contravariant (n1,ps1) (n2,ps2)
    mergeDefault
  fixMessage p
    | isCompileError p =
      compileError $ "Not isomorphic (" ++ show n1 ++ " / " ++ show n2 ++ ")"
    | otherwise = p
checkInstanceToInstance r f Contravariant (n1,ps1) (n2,ps2) =
  checkInstanceToInstance r f Covariant (n2,ps2) (n1,ps1)
checkInstanceToInstance r f Covariant (n1,ps1) (n2,ps2)
  | n1 == n2 = do
    checkParamsMatch (\_ _ -> return ()) ps1 ps2
    zipped <- return $ ParamSet $ zip (psParams ps1) (psParams ps2)
    variance <- trVariance r n1
    -- NOTE: Covariant is identity, so v2 has technically been composed with it.
    checkParamsMatch (\v2 (p1,p2) -> checkGeneralMatch r f v2 p1 p2) (ParamSet variance) zipped
  | otherwise = do
    (p2,ps1') <- (trFind r) n2 n1 ps1
    (return p2) `mergeNested` (checkInstanceToInstance r f Covariant (n2,ps1') (n2,ps2))

checkParamToInstance :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> TypeParam ->
  (TypeName,InstanceParams) -> m p
checkParamToInstance r _ Invariant (TypeParam n1) (n2,ps2) =
  compileError $ "Not isomorphic (" ++ show n1 ++ " / " ++ show n2 ++ ")"
checkParamToInstance r f Contravariant p1 (n2,ps2) =
  checkInstanceToParam r f Covariant (n2,ps2) p1
checkParamToInstance r f Covariant (TypeParam n1) (n2,ps2) = checked where
  checked = do
    cs1 <- f `filterLookup` n1
    mergeAll $ map (\c -> checkConstraintToInstance c (n2,ps2)) cs1
  checkConstraintToInstance (TypeFilter Covariant t) (n,ps) =
    -- x -> F implies x -> T only if F -> T
    checkSingleMatch r f Covariant t (TypeCategoryInstance n False ps)
  checkConstraintToInstance (TypeFilter _ t) _ =
    -- F -> x cannot imply x -> T
    compileError $ "Cannot convert param to instance (" ++ show n1 ++ " -> " ++ show n2 ++ ")"

checkInstanceToParam :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> (TypeName,InstanceParams) ->
  TypeParam -> m p
checkInstanceToParam _ _ Invariant (n1,ps1) (TypeParam n2) =
  compileError $ "Not isomorphic (" ++ show n1 ++ " / " ++ show n2 ++ ")"
checkInstanceToParam r f Contravariant (n1,ps1) p2 =
  checkParamToInstance r f Covariant p2 (n1,ps1)
checkInstanceToParam r f Covariant (n1,ps1) (TypeParam n2) = checked where
  checked = do
    cs2 <- f `filterLookup` n2
    mergeAny $ map (\c -> checkInstanceToConstraint (n1,ps1) c) cs2
  checkInstanceToConstraint (n,ps) (TypeFilter Contravariant t) =
    -- F -> x implies T -> x only if T -> F
    checkSingleMatch r f Contravariant (TypeCategoryInstance n False ps) t
  checkInstanceToConstraint _ (TypeFilter _ t) =
    -- x -> F cannot imply T -> x
    compileError $ "Cannot convert instance to param (" ++ show n1 ++ " -> " ++ show n2 ++ ")"

checkParamToParam :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> Variance -> TypeParam -> TypeParam -> m p
checkParamToParam r f Invariant (TypeParam n1) (TypeParam n2)
    | n1 == n2 = mergeDefault
    | otherwise =
      -- Even with identical fiters, if the names are different then it's
      -- possible that the substituted types will be different.
      compileError $ "Cannot convert param to param (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
checkParamToParam r f Contravariant p1 p2 =
  checkParamToParam r f Covariant p1 p2
checkParamToParam r f Covariant (TypeParam n1) (TypeParam n2) = checked where
  checked
    | n1 == n2 = mergeDefault
    | otherwise = do
      cs1 <- f `filterLookup` n1
      cs2 <- f `filterLookup` n2
      mergeAll $ map (\c1 -> mergeAny $ map (\c2 -> checkConstraintToConstraint c1 c2) cs2) cs1
  checkConstraintToConstraint (TypeFilter Covariant t1) (TypeFilter Contravariant t2) =
    -- x -> F1, F2 -> y implies x -> y only if F1 -> F2
    checkSingleMatch r f Covariant t1 t2
  checkConstraintToConstraint (TypeFilter _ _) (TypeFilter _ _) =
    -- x -> F1, y -> F2 cannot imply x -> y
    -- F1 -> x, F1 -> y cannot imply x -> y
    -- F1 -> x, y -> F2 cannot imply x -> y
    compileError $ "Cannot convert param to param (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
