{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module TypeInstance (
  AssignedParams,
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

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.List (intercalate)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.String
import qualified Data.Map as Map

import ParserBase
import TypesBase


type GeneralInstance = GeneralType TypeInstanceOrParam

instance ParseFromSource (GeneralType TypeInstanceOrParam) where
  sourceParser = try intersect <|> try union <|> single where
    single = do
      t <- sourceParser
      return $ SingleType t
    intersect = do
      ts <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string "&"))
      return $ TypeMerge MergeIntersect ts
    union = do
      ts <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string "|"))
      return $ TypeMerge MergeUnion ts


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

instance ParseFromSource ValueType where
  sourceParser = try weak <|> try optional <|> required where
    weak = do
      keyword "weak"
      t <- sourceParser
      return $ ValueType WeakValue t
    optional = do
      keyword "optional"
      t <- sourceParser
      return $ ValueType OptionalValue t
    required = do
      t <- sourceParser
      return $ ValueType RequiredValue t


newtype TypeName =
  TypeName {
    tnName :: String
  }
  deriving (Eq,Ord)

instance Show TypeName where
  show (TypeName n) = n

instance ParseFromSource TypeName where
  sourceParser = do
    noKeywords
    b <- upper
    e <- sepAfter $ many alphaNum
    return $ TypeName (b:e)


newtype ParamName =
  ParamName {
    pnName :: String
  }
  deriving (Eq,Ord)

instance Show ParamName where
  show (ParamName n) = n

instance ParseFromSource ParamName where
  sourceParser = do
    noKeywords
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ ParamName (b:e)


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

instance ParseFromSource TypeInstance where
  sourceParser = parsed where
    args = between (sepAfter $ string "<")
                   (sepAfter $ string ">")
                   (sepBy sourceParser (sepAfter $ string ","))
    parsed = do
      n <- sourceParser
      as <- try args <|> return []
      return $ TypeInstance n (ParamSet as)


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

instance ParseFromSource TypeInstanceOrParam where
  sourceParser = try param <|> inst <?> "type or param" where
    param = do
      n <- sourceParser
      return $ JustParamName n
    inst = do
      t <- sourceParser
      return $ JustTypeInstance t


data TypeFilter =
  TypeFilter {
    tfVariance :: Variance,
    tfType :: TypeInstanceOrParam
  }
  deriving (Eq)

viewTypeFilter :: ParamName -> TypeFilter -> String
viewTypeFilter n (TypeFilter Covariant t)     = show n ++ " -> " ++ show t
viewTypeFilter n (TypeFilter Contravariant t) = show n ++ " <- " ++ show t
viewTypeFilter n (TypeFilter Invariant t)     = show n ++ " = "  ++ show t

instance ParseFromSource TypeFilter where
  sourceParser = try requires <|> allows where
    requires = do
      keyword "requires"
      t <- sourceParser
      return $ TypeFilter Covariant t
    allows = do
      keyword "allows"
      t <- sourceParser
      return $ TypeFilter Contravariant t


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
checkGeneralMatch r f v ts1 ts2 = checkGeneralType (checkSingleMatch r f v) ts1 ts2

-- TODO: An error message here should include both type names in full.
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
  checkConstraintToInstance (TypeFilter Covariant t) =
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
  checkInstanceToConstraint (TypeFilter Contravariant t) =
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
  self1 = TypeFilter Covariant     (JustParamName n1)
  self2 = TypeFilter Contravariant (JustParamName n2)
  checked
    | n1 == n2 = mergeDefault
    | otherwise = do
      cs1 <- f `filterLookup` n1
      cs2 <- f `filterLookup` n2
      pairs <- return $ [(c1,c2) | c1 <- cs1, c2 <- cs2] ++
                        [(self1,c2) | c2 <- cs2] ++
                        [(c1,self2) | c1 <- cs1]
      mergeAny $ map (\(c1,c2) -> checkConstraintToConstraint c1 c2) pairs
  checkConstraintToConstraint (TypeFilter Covariant t1) (TypeFilter Contravariant t2)
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
