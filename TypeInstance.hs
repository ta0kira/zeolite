module TypeInstance (
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

data TypeName =
  TypeName {
    tnName :: String
  }
  deriving (Eq,Ord)

data TypeConstraint =
  TypeFilter {
    tfType :: TypeInstance TypeCategoryInstance
  } |
  TypeMissing {
    tmMissing :: Missingness
  }
  deriving (Eq)

data TypeParam =
  TypeParam {
    tpName :: String,
    tpConstraint :: [TypeConstraint]
  }
  deriving (Eq)

type GeneralInstance = TypeInstance TypeCategoryInstance

type InstanceParams = ParamSet GeneralInstance

data TypeCategoryInstance =
  TypeCategoryInstance {
    tciName :: TypeName,
    tciParams :: InstanceParams
  } |
  TypeCategoryParam {
    tcpParam :: TypeParam
  }
  deriving (Eq)

data TypeResolver m p =
  TypeResolver {
    -- Default path type for finds.
    trPathDefault :: p,
    -- Convert an instance of one category to an instance of the other.
    trFind :: TypeName -> TypeName -> InstanceParams -> m (p,InstanceParams),
    -- Get the missingness for the category.
    trMissing :: TypeName -> m Missingness,
    -- Get the parameter variances for the category.
    trVariance :: TypeName -> m [Variance]
  }

checkGeneralMatch :: (Mergeable (m p), CompileError (m p), Monad m) =>
  Variance -> TypeResolver m p ->
  GeneralInstance -> GeneralInstance -> m p
checkGeneralMatch v r ts1 ts2 = checkTypeInstance checkSingleMatch ts1 ts2 where
  checkSingleMatch (TypeCategoryInstance n1 ps1) (TypeCategoryInstance n2 ps2) =
    checkInstanceToInstance v r (n1,ps1) (n2,ps2)
  checkSingleMatch (TypeCategoryParam p1) (TypeCategoryInstance n2 ps2) =
    checkParamToInstance v r p1 (n2,ps2)
  checkSingleMatch (TypeCategoryInstance n1 ps1) (TypeCategoryParam p2) =
    checkInstanceToParam v r (n1,ps1) p2
  checkSingleMatch (TypeCategoryParam p1) (TypeCategoryParam p2) =
    checkParamToParam v r p1 p2

checkInstanceToInstance :: (Mergeable (m p), CompileError (m p), Monad m) =>
  Variance -> TypeResolver m p ->
  (TypeName,InstanceParams) -> (TypeName,InstanceParams) -> m p
checkInstanceToInstance Contravariant r (n1,ps1) (n2,ps2) =
  checkInstanceToInstance Covariant r (n2,ps2) (n1,ps1)
checkInstanceToInstance v r (n1,ps1) (n2,ps2)
  | n1 == n2 = do
    checkParamsMatch (\_ _ -> return $ trPathDefault r) ps1 ps2
    zipped <- return $ ParamSet $ zip (psParams ps1) (psParams ps2)
    variance <- trVariance r n1
    checkParamsMatch (\v2 (p1,p2) -> checkGeneralMatch v2 r p1 p2) (ParamSet variance) zipped
  | v == Invariant = compileError "No path found (instance -> instance)"
  | otherwise = do
    (p2,ps1') <- (trFind r) n2 n1 ps1
    (return p2) `mergeNested` (checkInstanceToInstance v r (n2,ps1') (n2,ps2))

checkParamToInstance :: (Mergeable (m p), CompileError (m p), Monad m) =>
  Variance -> TypeResolver m p ->
  TypeParam -> (TypeName,InstanceParams) -> m p
checkParamToInstance Invariant _ _ _ = compileError "No path found (param -> instance)"
checkParamToInstance Contravariant r p1 (n2,ps2) =
  checkInstanceToParam Covariant r (n2,ps2) p1
checkParamToInstance v r (TypeParam _ cs1) (n2,ps2) = checked where
  checked = mergeAny $ map (\c -> checkConstraintToInstance c (n2,ps2)) cs1
  checkConstraintToInstance (TypeFilter t) (n,ps) =
    checkGeneralMatch v r t (TypeInstance $ TypeCategoryInstance n ps)
  checkConstraintToInstance (TypeMissing q1) (n,_) = do
    q2 <- trMissing r n
    canBecomeMissing (trPathDefault r) q1 q2

checkInstanceToParam :: (Mergeable (m p), CompileError (m p), Monad m) =>
  Variance -> TypeResolver m p ->
  (TypeName,InstanceParams) -> TypeParam -> m p
checkInstanceToParam Invariant _ _ _ = compileError "No path found (instance -> param)"
checkInstanceToParam Contravariant r (n1,ps1) p2 =
  checkParamToInstance Covariant r p2 (n1,ps1)
checkInstanceToParam v r (n1,ps1) (TypeParam _ cs2) = checked where
  checked = mergeAll $ map (\c -> checkInstanceToConstraint (n1,ps1) c) cs2
  checkInstanceToConstraint (n,ps) (TypeFilter t) =
    checkGeneralMatch v r (TypeInstance $ TypeCategoryInstance n ps) t
  checkInstanceToConstraint (n,_) (TypeMissing q2) = do
    q1 <- trMissing r n
    canBecomeMissing (trPathDefault r) q1 q2

checkParamToParam :: (Mergeable (m p), CompileError (m p), Monad m) =>
  Variance -> TypeResolver m p ->
  TypeParam -> TypeParam -> m p
checkParamToParam Contravariant r p1 p2 =
  checkParamToParam Covariant r p1 p2
checkParamToParam v r (TypeParam n1 cs1) (TypeParam n2 cs2)
  | n1 /= n2 && v == Invariant = compileError "No path found (param -> param)"
  | otherwise = checked where
    checked = mergeAny $ map (\c1 -> mergeAll $ map (\c2 -> checkConstraintToConstraint c1 c2) cs2) cs1
    checkConstraintToConstraint (TypeFilter t1) (TypeFilter t2) =
      checkGeneralMatch v r t1 t2
    checkConstraintToConstraint m1@(TypeMissing _) (TypeFilter t2) =
      checkGeneralMatch v r (TypeInstance $ TypeCategoryParam $ TypeParam n1 [m1]) t2
    checkConstraintToConstraint (TypeFilter  t1) m2@(TypeMissing _) =
      checkGeneralMatch v r t1 (TypeInstance $ TypeCategoryParam $ TypeParam n2 [m2])
    checkConstraintToConstraint (TypeMissing q1) (TypeMissing q2) =
      canBecomeMissing (trPathDefault r) q1 q2

canBecomeMissing :: (CompileError (m p), Monad m) => p -> Missingness -> Missingness -> m p
canBecomeMissing _ AllowMissing DisallowMissing = compileError "Missingness disallowed"
canBecomeMissing p _ _ = return p
