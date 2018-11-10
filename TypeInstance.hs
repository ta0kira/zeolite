module TypeInstance (
  GeneralInstance,
  InstanceParams,
  Missingness(..),
  TypeCategoryInstance(..),
  TypeConstraint(..),
  TypeName(..),
  TypeParam(..),
  TypeResolver(..),
  Variance(..),
  checkGeneralMatch,
) where

import TypesBase


data Variance = Covariant | Contravariant | Invariant deriving (Eq,Ord,Show)

data Missingness = AllowMissing | DisallowMissing deriving (Eq,Ord,Show)

type GeneralInstance = TypeInstance TypeCategoryInstance

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

data TypeConstraint =
  TypeFilter {
    tfType :: GeneralInstance
  } |
  TypeMissing {
    tmMissing :: Missingness
  }
  deriving (Eq,Show)

data TypeParam =
  TypeParam {
    tpName :: ParamName,
    tpConstraint :: [TypeConstraint]
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

data TypeResolver m p =
  TypeResolver {
    -- Convert an instance of one category to an instance of the other.
    trFind :: TypeName -> TypeName -> InstanceParams -> m (p,InstanceParams),
    -- Get the missingness for the category.
    trMissing :: TypeName -> m Missingness,
    -- Get the parameter variances for the category.
    trVariance :: TypeName -> m [Variance]
  }

checkGeneralMatch :: (Mergeable (m ()), CompileError (m ()),
                      Mergeable (m p), CompileError (m p), Monad m) =>
  TypeResolver m p -> Variance ->
  GeneralInstance -> GeneralInstance -> m p
checkGeneralMatch r v ts1 ts2 = checkTypeInstance checkSingleMatch ts1 ts2 where
  checkSingleMatch (TypeCategoryInstance n1 ps1) (TypeCategoryInstance n2 ps2) =
    checkInstanceToInstance r v (n1,ps1) (n2,ps2)
  checkSingleMatch (TypeCategoryParam p1) (TypeCategoryInstance n2 ps2) =
    checkParamToInstance r v p1 (n2,ps2)
  checkSingleMatch (TypeCategoryInstance n1 ps1) (TypeCategoryParam p2) =
    checkInstanceToParam r v (n1,ps1) p2
  checkSingleMatch (TypeCategoryParam p1) (TypeCategoryParam p2) =
    checkParamToParam r v p1 p2

checkInstanceToInstance :: (Mergeable (m ()), CompileError (m ()),
                            Mergeable (m p), CompileError (m p), Monad m) =>
  TypeResolver m p -> Variance ->
  (TypeName,InstanceParams) -> (TypeName,InstanceParams) -> m p
checkInstanceToInstance r Contravariant (n1,ps1) (n2,ps2) =
  checkInstanceToInstance r Covariant (n2,ps2) (n1,ps1)
checkInstanceToInstance r v (n1,ps1) (n2,ps2)
  | n1 == n2 = do
    checkParamsMatch (\_ _ -> return ()) ps1 ps2
    zipped <- return $ ParamSet $ zip (psParams ps1) (psParams ps2)
    variance <- trVariance r n1
    checkParamsMatch (\v2 (p1,p2) -> checkGeneralMatch r v2 p1 p2) (ParamSet variance) zipped
  | v == Invariant = compileError $ "No path found (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
  | otherwise = do
    (p2,ps1') <- (trFind r) n2 n1 ps1
    (return p2) `mergeNested` (checkInstanceToInstance r v (n2,ps1') (n2,ps2))

checkParamToInstance :: (Mergeable (m ()), CompileError (m ()),
                         Mergeable (m p), CompileError (m p), Monad m) =>
  TypeResolver m p -> Variance ->
  TypeParam -> (TypeName,InstanceParams) -> m p
checkParamToInstance _ Invariant (TypeParam n1 _) (n2,ps2) =
  compileError $ "No path found (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
checkParamToInstance r Contravariant p1 (n2,ps2) =
  checkInstanceToParam r Covariant (n2,ps2) p1
checkParamToInstance r v (TypeParam _ cs1) (n2,ps2) = checked where
  checked = mergeAny $ map (\c -> checkConstraintToInstance c (n2,ps2)) cs1
  checkConstraintToInstance (TypeFilter t) (n,ps) =
    checkGeneralMatch r v t (TypeInstance $ TypeCategoryInstance n ps)
  checkConstraintToInstance (TypeMissing q1) (n,_) = do
    q2 <- trMissing r n
    q1 `canBecomeMissing` q2

checkInstanceToParam :: (Mergeable (m ()), CompileError (m ()),
                         Mergeable (m p), CompileError (m p), Monad m) =>
  TypeResolver m p -> Variance ->
  (TypeName,InstanceParams) -> TypeParam -> m p
checkInstanceToParam _ Invariant (n1,ps1) (TypeParam n2 _) =
  compileError $ "No path found (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
checkInstanceToParam r Contravariant (n1,ps1) p2 =
  checkParamToInstance r Covariant p2 (n1,ps1)
checkInstanceToParam r v (n1,ps1) (TypeParam _ cs2) = checked where
  checked = mergeAll $ map (\c -> checkInstanceToConstraint (n1,ps1) c) cs2
  checkInstanceToConstraint (n,ps) (TypeFilter t) =
    checkGeneralMatch r v (TypeInstance $ TypeCategoryInstance n ps) t
  checkInstanceToConstraint (n,_) (TypeMissing q2) = do
    q1 <- trMissing r n
    q1 `canBecomeMissing` q2

checkParamToParam :: (Mergeable (m ()), CompileError (m ()),
                      Mergeable (m p), CompileError (m p), Monad m) =>
  TypeResolver m p -> Variance ->
  TypeParam -> TypeParam -> m p
checkParamToParam r Contravariant p1 p2 =
  checkParamToParam r Covariant p1 p2
checkParamToParam r v (TypeParam n1 cs1) (TypeParam n2 cs2)
  -- Substitution should have happened already => names should be the same.
  | n1 /= n2 = compileError $ "No path found (" ++ show n1 ++ " -> " ++ show n2 ++ ")"
  | otherwise = checked where
    checked = mergeAny $ map (\c1 -> mergeAll $ map (\c2 -> checkConstraintToConstraint c1 c2) cs2) cs1
    checkConstraintToConstraint (TypeFilter t1) (TypeFilter t2) =
      checkGeneralMatch r v t1 t2
    checkConstraintToConstraint m1@(TypeMissing _) (TypeFilter t2) =
      checkGeneralMatch r v (TypeInstance $ TypeCategoryParam $ TypeParam n1 [m1]) t2
    checkConstraintToConstraint (TypeFilter  t1) m2@(TypeMissing _) =
      checkGeneralMatch r v t1 (TypeInstance $ TypeCategoryParam $ TypeParam n2 [m2])
    checkConstraintToConstraint (TypeMissing q1) (TypeMissing q2) =
      q1 `canBecomeMissing` q2

canBecomeMissing :: (Mergeable (m p), CompileError (m p), Monad m) =>  Missingness -> Missingness -> m p
AllowMissing `canBecomeMissing` DisallowMissing =
  compileError $ "Missingness disallowed (" ++ show AllowMissing ++ " -> " ++ show DisallowMissing ++ ")"
_ `canBecomeMissing` _ = mergeDefault
