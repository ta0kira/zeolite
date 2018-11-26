{-# LANGUAGE Safe #-}

module TypeInstance (
  AssignedParams,
  GeneralInstance,
  InstanceParams,
  ParamName,
  TypeCategoryInstance(..),
  TypeFilter(..),
  TypeName,
  TypeParam(..),
  TypeResolver(..),
  TypeSystem(..),
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
    tpName :: ParamName,
    -- TODO: Store filters outside of the type instance.
    tpConstraint :: [TypeFilter]
  }
  deriving (Eq,Show)

type InstanceParams = ParamSet GeneralInstance

data TypeCategoryInstance =
  TypeCategoryInstance {
    tciName :: TypeName,
    tciParams :: InstanceParams
  } |
  TypeCategoryParam {
    tcpParam :: TypeParam
  }
  deriving (Eq,Show)

type AssignedParams = Map.Map ParamName GeneralInstance

data TypeResolver m p =
  TypeResolver {
    -- Convert an instance of one category to an instance of the other.
    trFind :: TypeName -> TypeName -> InstanceParams -> m (p,InstanceParams),
    -- Labels params for an instance using the category's param names.
    trParams :: TypeName -> InstanceParams -> m AssignedParams,
    -- Get the parameter variances for the category.
    trVariance :: TypeName -> m [Variance]
  }

data TypeSystem m p =
  TypeSystem {
    tsValidate :: [TypeParam] -> GeneralInstance -> m GeneralInstance,
    tsConvert :: Variance -> GeneralInstance -> GeneralInstance -> m p
  }

-- NOTE: This doesn't verify the filters required to create an instance of a
-- type category. That should be done during instantiation of the instances and
-- during validation of the category system. (This does verify filters imposed
-- by individual free params, though.)
checkGeneralMatch :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> Variance -> GeneralInstance -> GeneralInstance -> m p
checkGeneralMatch r v ts1 ts2 = checkGeneralType (checkSingleMatch r v) ts1 ts2

checkSingleMatch :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> Variance -> TypeCategoryInstance -> TypeCategoryInstance -> m p
checkSingleMatch r v (TypeCategoryInstance n1 ps1) (TypeCategoryInstance n2 ps2) =
  checkInstanceToInstance r v (n1,ps1) (n2,ps2)
checkSingleMatch r v (TypeCategoryParam p1) (TypeCategoryInstance n2 ps2) =
  checkParamToInstance r v p1 (n2,ps2)
checkSingleMatch r v (TypeCategoryInstance n1 ps1) (TypeCategoryParam p2) =
  checkInstanceToParam r v (n1,ps1) p2
checkSingleMatch r v (TypeCategoryParam p1) (TypeCategoryParam p2) =
  checkParamToParam r v p1 p2

checkInstanceToInstance :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> Variance -> (TypeName,InstanceParams) -> (TypeName,InstanceParams) -> m p
checkInstanceToInstance r Invariant (n1,ps1) (n2,ps2) = fixMessage check where
  check = do
    -- Check for isomorphism, but not necessarily equality.
    checkInstanceToInstance r Covariant     (n1,ps1) (n2,ps2)
    checkInstanceToInstance r Contravariant (n1,ps1) (n2,ps2)
    mergeDefault
  fixMessage p
    | isCompileError p =
      compileError $ "Not isomorphic (" ++ show n1 ++ " / " ++ show n2 ++ ")"
    | otherwise = p
checkInstanceToInstance r Contravariant (n1,ps1) (n2,ps2) =
  checkInstanceToInstance r Covariant (n2,ps2) (n1,ps1)
checkInstanceToInstance r Covariant (n1,ps1) (n2,ps2)
  | n1 == n2 = do
    checkParamsMatch (\_ _ -> return ()) ps1 ps2
    zipped <- return $ ParamSet $ zip (psParams ps1) (psParams ps2)
    variance <- trVariance r n1
    -- NOTE: Covariant is identity, so v2 has technically been composed with it.
    checkParamsMatch (\v2 (p1,p2) -> checkGeneralMatch r v2 p1 p2) (ParamSet variance) zipped
  | otherwise = do
    (p2,ps1') <- (trFind r) n2 n1 ps1
    (return p2) `mergeNested` (checkInstanceToInstance r Covariant (n2,ps1') (n2,ps2))

checkParamToInstance :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> Variance -> TypeParam -> (TypeName,InstanceParams) -> m p
checkParamToInstance _ Invariant (TypeParam n1 _) (n2,ps2) =
  compileError $ "Not isomorphic (" ++ show n1 ++ " / " ++ show n2 ++ ")"
checkParamToInstance r Contravariant p1 (n2,ps2) =
  checkInstanceToParam r Covariant (n2,ps2) p1
checkParamToInstance r Covariant (TypeParam n1 cs1) (n2,ps2) = checked where
  checked = mergeAny $ map (\c -> checkConstraintToInstance c (n2,ps2)) cs1
  checkConstraintToInstance (TypeFilter Covariant t) (n,ps) =
    -- x -> F implies x -> T only if F -> T
    checkSingleMatch r Covariant t (TypeCategoryInstance n ps)
  checkConstraintToInstance (TypeFilter _ t) _ =
    compileError $ "Cannot convert param to instance (" ++ show n1 ++ " -> " ++ show n2 ++ ")"

checkInstanceToParam :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> Variance -> (TypeName,InstanceParams) -> TypeParam -> m p
checkInstanceToParam _ Invariant (n1,ps1) (TypeParam n2 _) =
  compileError $ "Not isomorphic (" ++ show n1 ++ " / " ++ show n2 ++ ")"
checkInstanceToParam r Contravariant (n1,ps1) p2 =
  checkParamToInstance r Covariant p2 (n1,ps1)
checkInstanceToParam r Covariant (n1,ps1) (TypeParam n2 cs2) = checked where
  checked = mergeAll $ map (\c -> checkInstanceToConstraint (n1,ps1) c) cs2
  checkInstanceToConstraint (n,ps) (TypeFilter Contravariant t) =
    -- F -> x implies T -> x only if T -> F
    checkSingleMatch r Contravariant t (TypeCategoryInstance n ps)
  checkInstanceToConstraint _ (TypeFilter _ t) =
    compileError $ "Cannot convert instance to param (" ++ show n1 ++ " -> " ++ show n2 ++ ")"

checkParamToParam :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> Variance -> TypeParam -> TypeParam -> m p
checkParamToParam r Invariant p1 p2 = fixMessage check where
  check = do
    -- Check for isomorphism, but not necessarily equality.
    checkParamToParam r Covariant     p1 p2
    checkParamToParam r Contravariant p1 p2
    mergeDefault
  fixMessage p
    | isCompileError p =
      compileError $ "Not isomorphic (" ++ show (tpName p1) ++ " -> " ++ show (tpName p2) ++ ")"
    | otherwise = p
checkParamToParam r Contravariant p1 p2 =
  checkParamToParam r Covariant p1 p2
checkParamToParam r Covariant (TypeParam n1 cs1) (TypeParam n2 cs2) = checked where
  checked
    | n1 /= n2 =
      -- TODO: Need to account for checking x to y when x -> y.
      compileError $ "Param conflict (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
    | otherwise =
      mergeAny $ map (\c1 -> mergeAll $ map (\c2 -> checkConstraintToConstraint c1 c2) cs2) cs1
  checkConstraintToConstraint (TypeFilter Covariant t1) (TypeFilter Covariant t2) =
    -- x -> F1 implies x -> F2 only if F1 -> F2
    checkSingleMatch r Covariant t1 t2
  checkConstraintToConstraint (TypeFilter Contravariant t1) (TypeFilter Contravariant t2) =
    -- F1 -> x implies F2 -> x only if F2 -> F1
    checkSingleMatch r Contravariant t1 t2
  checkConstraintToConstraint (TypeFilter _ _) (TypeFilter _ _) =
    compileError $ "Cannot convert param to param (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
