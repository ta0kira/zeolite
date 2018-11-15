{-# LANGUAGE Safe #-}

module TypeCategory (
) where

import Control.Monad (join,(>=>))
import Control.Monad.Fix (MonadFix,mfix)
import Data.List (group,intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import TypeInstance
import TypesBase


newtype CategoryConnect a =
  CategoryConnect {
    ccMap :: Map.Map TypeName a
  }

newtype CategoryRefine =
  CategoryRefine {
    crRefines :: [TypeCategoryInstance]
  }

type CategoryMain = CategoryConnect (Set.Set TypeName)

type Refinements = CategoryConnect CategoryRefine

type CategoryMissing = CategoryConnect Missingness

type CategoryVariance = CategoryConnect (ParamSet Variance)

type CategoryParams = CategoryConnect (ParamSet TypeParam)

type CategoryConcrete = CategoryConnect Bool

categoryLookup :: (CompileErrorM m, Monad m) => TypeName -> CategoryConnect a -> m a
categoryLookup n (CategoryConnect cs) = resolve $ n `Map.lookup` cs where
  resolve (Just x) = return x
  resolve _        = compileError $ "Category " ++ show n ++ " not found"

paramLookup :: (CompileErrorM m, Monad m) =>
  AssignedParams -> TypeParam -> m GeneralInstance
paramLookup ps (TypeParam n _ _) = resolve $ n `Map.lookup` ps where
  -- TODO: Should this check constraints?
  resolve (Just x) = return x
  resolve _        = compileError $ "Param " ++ show n ++ " not found"

checkCategory :: (Mergeable b) => (TypeName -> a -> b) -> CategoryConnect a -> b
checkCategory f (CategoryConnect cs) = mergeAll $ map (\(k,v) -> f k v) (Map.toList cs)

data CategorySystem =
  CategorySystem {
    csMain :: CategoryMain,
    csRefine :: Refinements,
    csMissing :: CategoryMissing,
    csVariance :: CategoryVariance,
    csParams :: CategoryParams,
    csConcrete :: CategoryConcrete
  }

validateCategory :: (Mergeable (m ()), Mergeable (m p),
                     Mergeable p, CompileErrorM m, Monad m, MonadFix m) =>
  TypeResolver m p -> CategorySystem -> m CategorySystem
validateCategory r cs = do
  -- Basic structural checks.
  checkCycles (csMain cs)
  checkConcrete (csConcrete cs) (csMain cs)
  checkMissing (csMissing cs) (csMain cs)
  -- Refine the structure.
  refine <- flattenRefines r (csRefine cs)
  checkRefines refine
  -- Check finer structural semantics.
  labeledVars <- labelParamVals (csParams cs) (csVariance cs)
  checkVariances labeledVars (csVariance cs) refine
  return $ CategorySystem {
      csMain = csMain cs,
      -- TODO: Check refines w.r.t. param filters.
      csRefine = refine,
      csMissing = csMissing cs,
      csVariance = csVariance cs,
      -- TODO: Check params w.r.t. compatible missingness requirements.
      csParams = csParams cs,
      csConcrete = csConcrete cs
    }

labelParamVals :: (Mergeable (m ()), CompileErrorM m, Monad m) =>
  CategoryParams -> CategoryConnect (ParamSet a) ->
  m (CategoryConnect (Map.Map ParamName a))
labelParamVals (CategoryConnect pa) va@(CategoryConnect _) = paired where
  paired = do
    pairs <- collectAllOrErrorM $ map pairType (Map.toList pa)
    return $ CategoryConnect $ Map.fromList pairs
  pairType (n,ps) = do
    vs <- n `categoryLookup` va
    checkParamsMatch (\_ _ -> return ()) ps vs
    return (n,Map.fromList $ zip (map tpName $ psParams ps) (psParams vs))

checkVariances :: (Mergeable (m ()), CompileErrorM m, Monad m) =>
  (CategoryConnect (Map.Map ParamName Variance)) ->
  CategoryVariance -> Refinements -> m ()
checkVariances va vs = checkCategory checkAll where
  checkAll n (CategoryRefine gs) = do
    as <- n `categoryLookup` va
    mergeAll $ map (checkSingle as Covariant . SingleType) gs
  checkSingle as v (SingleType (TypeCategoryInstance t ps)) = do
    vs2 <- t `categoryLookup` vs
    checkParamsMatch (\_ _ -> return ()) vs2 ps
    mergeAll $ map (\(v2,p) -> checkSingle as (v `composeVariance` v2) p) (zip (psParams vs2) (psParams ps))
  checkSingle as v (TypeMerge MergeUnion     ts) = mergeAll $ map (checkSingle as v) ts
  checkSingle as v (TypeMerge MergeIntersect ts) = mergeAll $ map (checkSingle as v) ts
  checkSingle as v (SingleType (TypeCategoryParam (TypeParam n _ _))) = check (n `Map.lookup` as) where
    check Nothing   = compileError $ "Param " ++ show n ++ " is undefined"
    check (Just v0) =
      if v0 `paramAllowsVariance` v
         then return ()
         else compileError $ "Param " ++ show n ++ " does not allow variance " ++ show v

checkCycles :: (Mergeable (m ()), CompileErrorM m, Monad m) => CategoryMain -> m ()
checkCycles ca = checkCategory (checker []) ca where
  checker ns n ts
    | n `Set.member` (Set.fromList ns) =
      compileError $ "Cycle found: " ++ intercalate " -> " (map show (ns ++ [n]))
    | otherwise =
      mergeAll $ map (\(n2,ts2) -> ts2 >>= checker (ns ++ [n]) n2) (map find $ Set.toList ts) where
        find t = (t,suppress $ t `categoryLookup` ca)
        -- The error is suppressed, since a missing reference will be detected
        -- during later checks. This allows incremental construction of the
        -- type resolver, e.g., using multiple modules.
        suppress cs
          | isCompileError cs = return Set.empty
          | otherwise         = cs

checkConcrete :: (Mergeable (m ()), CompileErrorM m, Monad m) =>
  CategoryConcrete -> CategoryMain -> m ()
checkConcrete cc = checkCategory checkAll where
  checkAll n = mergeAll . map (checkSingle n) . Set.toList
  checkSingle n n2 = do
    concrete <- n2 `categoryLookup` cc
    if concrete
       then compileError $ "Category " ++ show n ++ " cannot refine concrete " ++ show n2
       else return ()

checkMissing :: (Mergeable (m ()), CompileErrorM m, Monad m) =>
  CategoryMissing -> CategoryMain -> m ()
checkMissing ms ca = checked where
  checked = do
    checkCategory checkSpecified ms
    checkCategory checkAllowed ca
  checkSpecified n m
    | m == AllowsMissing    = return ()
    | m == DisallowsMissing = return ()
    | otherwise =
      compileError $ "Category " ++ show n ++ " cannot have missing type " ++ show m
  checkAllowed n ts = do
    m <- n `categoryLookup` ms
    mergeAll $ map (checkSingle n m) $ Set.toList ts
  checkSingle n m1 n2 = do
    m2 <- n2 `categoryLookup` ms
    m2 `canBecomeMissing` m1

mergeInstances :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> [TypeCategoryInstance] -> [TypeCategoryInstance]
mergeInstances r gs = merge [] gs where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    checker x2 = checkGeneralMatch r Covariant (SingleType x2) (SingleType x)
    ys = if isCompileError $ mergeAny $ map checker (cs ++ xs)
       then [x] -- x is not redundant => keep.
       else []  -- x is redundant => remove.

flattenRefines :: (Mergeable (m ()), Mergeable (m p),
                   Mergeable p, CompileErrorM m, Monad m, MonadFix m) =>
  TypeResolver m p -> Refinements -> m Refinements
flattenRefines r (CategoryConnect gs) = mfix flattenAll where
  flattenAll ca@(CategoryConnect _) = do
    items <- collectAllOrErrorM $ map (flattenCategory ca) (Map.toList gs)
    return $ CategoryConnect $ Map.fromList items
  flattenCategory ca (n,(CategoryRefine gs)) = do
    gs2 <- collectAllOrErrorM $ map (flattenSingle ca n) gs
    return (n,CategoryRefine (mergeInstances r $ join gs2))
  flattenSingle ca _ ta@(TypeCategoryInstance t ps) = do
    params <- (trParams r) t ps
    refines <- t `categoryLookup` ca
    collectAllOrErrorM $ map (substitute params) (crRefines refines)
  substitute params t = do
    -- TODO: This should preserve the path (fst from the sub call) since it
    -- might be needed to keep track of conversion information.
    ((),SingleType x) <- uncheckedSubAllParams (paramLookup params) (SingleType t)
    return x

checkRefines :: (Mergeable (m ()), CompileErrorM m, Monad m) => Refinements -> m ()
checkRefines = checkCategory checkAll where
  checkAll n (CategoryRefine gs) = do
    ts <- collectAllOrErrorM $ map (getTypeName n) gs
    mergeAll $ map (checkGroup n) $ group ts
  getTypeName _ (TypeCategoryInstance t _) = return t
  getTypeName n (TypeCategoryParam _) =
    compileError $ "Type " ++ show n ++ " cannot refine a param"
  checkGroup n (t:t2:ts) =
    compileError $ "Type " ++ show n ++ " has conflicting refinements of type " ++ show t
  checkGroup _ _ = return ()

checkedSubAllParams :: (Mergeable (m ()), Mergeable (m p),
                        Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> (TypeParam -> m GeneralInstance) ->
  GeneralInstance -> m (p,GeneralInstance)
checkedSubAllParams r = subAllParams (checkGeneralMatch r Covariant)

uncheckedSubAllParams :: (Mergeable (m ()), Mergeable (m p),
                          Mergeable p, CompileErrorM m, Monad m) =>
  (TypeParam -> m GeneralInstance) -> GeneralInstance -> m (p,GeneralInstance)
uncheckedSubAllParams = subAllParams (\_ _ -> return mergeDefault)

subAllParams :: (Mergeable (m ()), Mergeable (m p),
                 Mergeable p, CompileErrorM m, Monad m) =>
  (GeneralInstance -> GeneralInstance -> m p) ->
  (TypeParam -> m GeneralInstance) -> GeneralInstance -> m (p,GeneralInstance)
subAllParams find replace = subAll where
  subAll (TypeMerge MergeUnion ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeUnion $ map snd gs)
  subAll (TypeMerge MergeIntersect ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeIntersect $ map snd gs)
  subAll (SingleType t) = subInstance t
  subInstance (TypeCategoryInstance n (ParamSet ts)) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,SingleType $ TypeCategoryInstance n $ (ParamSet $ map snd gs))
  subInstance (TypeCategoryParam t) = subParam t
  subParam pa@(TypeParam _ _ _) = do
    -- NOTE: No need to handle replacement in filters, since we require that
    -- *all* params be substituted.
    -- TODO: Maybe this should sub t into the filters and then check t against
    -- those filters? Maybe not, since they should be checked where the param
    -- is actually assigned a value.
    t <- replace pa
    p <- find t (SingleType $ TypeCategoryParam pa)
    return (p,t)

typeSystemFromResolver :: (Mergeable (m ()), Mergeable (m p),
                           Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> TypeSystem m p
typeSystemFromResolver r = system where
  system = TypeSystem {
      tsValidate = validate,
      tsConvert = convert
    }
  convert = checkGeneralMatch r
  validate ps = checkedSubAllParams r (paramLookup $ toMap ps) >=>  return . snd
  toMap = Map.fromList . map (\p -> (tpName p,SingleType $ TypeCategoryParam p))
