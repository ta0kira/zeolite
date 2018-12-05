{-# LANGUAGE Safe #-}

module TypeCategory (
  CategoryConcrete,
  CategoryConnect(..),
  CategoryFilters,
  CategoryMain,
  CategoryParams,
  CategoryRefines,
  CategorySystem(..),
  CategoryVariance,
  validateCategory,
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
    crRefines :: [TypeInstance]
  }

type CategoryMain = CategoryConnect (Set.Set TypeName)

type CategoryRefines = CategoryConnect CategoryRefine

type CategoryVariance = CategoryConnect (ParamSet Variance)

type CategoryParams = CategoryConnect (ParamSet ParamName)

type CategoryFilters = CategoryConnect (ParamSet [TypeFilter])

type CategoryConcrete = CategoryConnect Bool

data CategorySystem =
  CategorySystem {
    csMain :: CategoryMain,
    csRefine :: CategoryRefines,
    csVariance :: CategoryVariance,
    csParams :: CategoryParams,
    csFilters :: CategoryFilters,
    csConcrete :: CategoryConcrete
  }

categoryLookup :: (CompileErrorM m, Monad m) => TypeName -> CategoryConnect a -> m a
categoryLookup n (CategoryConnect cs) = resolve $ n `Map.lookup` cs where
  resolve (Just x) = return x
  resolve _        = compileError $ "Category " ++ show n ++ " not found"

paramLookup :: (CompileErrorM m, Monad m) =>
  AssignedParams -> ParamName -> m GeneralInstance
paramLookup ps n = resolve $ n `Map.lookup` ps where
  -- TODO: Should this check constraints?
  resolve (Just x) = return x
  resolve _        = compileError $ "Param " ++ show n ++ " not found"

checkCategory :: (Mergeable b) => (TypeName -> a -> b) -> CategoryConnect a -> b
checkCategory f (CategoryConnect cs) = mergeAll $ map (\(k,v) -> f k v) (Map.toList cs)

validateCategory :: (MergeableM m, Mergeable p, CompileErrorM m,
                     Monad m, MonadFix m) =>
  TypeResolver m p -> CategorySystem -> m CategorySystem
validateCategory r cs = do
  -- Basic structural checks.
  checkCycles (csMain cs)
  checkConcrete (csConcrete cs) (csMain cs)
  -- TODO: Need to check that filters are actually valid type instances. Before
  -- checking refines w.r.t. filters: just ensure that all categories and params
  -- are defined. After that: ensure that the filters themselves have valid type
  -- instances w.r.t. the category of the filter itself. Also, disallow optional
  -- in filters.
  -- Refine the structure.
  refine <- flattenRefines r (csParams cs) (csFilters cs) (csRefine cs)
  checkRefines r (csParams cs) (csFilters cs) refine
  -- Check finer structural semantics.
  labeledVars <- labelParamVals (csParams cs) (csVariance cs)
  checkVariances labeledVars (csVariance cs) refine
  return $ CategorySystem {
      csMain = csMain cs,
      csRefine = refine,
      csVariance = csVariance cs,
      -- TODO: Check params w.r.t. compatible missingness requirements.
      csParams = csParams cs,
      csFilters = csFilters cs,
      csConcrete = csConcrete cs
    }

uncheckedZipFilters ::
  ParamSet ParamName -> ParamSet [TypeFilter] -> Map.Map ParamName [TypeFilter]
uncheckedZipFilters (ParamSet pa) (ParamSet fa) =
  Map.fromList $ zip pa fa

labelParamVals :: (MergeableM m, CompileErrorM m, Monad m) =>
  CategoryParams -> CategoryConnect (ParamSet a) ->
  m (CategoryConnect (Map.Map ParamName a))
labelParamVals (CategoryConnect pa) va@(CategoryConnect _) = paired where
  paired = do
    pairs <- collectAllOrErrorM $ map pairType (Map.toList pa)
    return $ CategoryConnect $ Map.fromList pairs
  pairType (n,ps) = do
    vs <- n `categoryLookup` va
    checkParamsMatch (\_ _ -> return ()) ps vs
    return (n,Map.fromList $ zip (psParams ps) (psParams vs))

checkVariances :: (MergeableM m, CompileErrorM m, Monad m) =>
  (CategoryConnect (Map.Map ParamName Variance)) ->
  CategoryVariance -> CategoryRefines -> m ()
checkVariances va vs = checkCategory checkAll where
  checkAll n (CategoryRefine gs) = do
    as <- n `categoryLookup` va
    mergeAll $ map (checkSingle as Covariant . SingleType . JustTypeInstance) gs
  checkSingle as v (SingleType (JustTypeInstance (TypeInstance t ps))) = do
    vs2 <- t `categoryLookup` vs
    checkParamsMatch (\_ _ -> return ()) vs2 ps
    mergeAll $ map (\(v2,p) -> checkSingle as (v `composeVariance` v2) p) (zip (psParams vs2) (psParams ps))
  checkSingle as v (TypeMerge MergeUnion     ts) = mergeAll $ map (checkSingle as v) ts
  checkSingle as v (TypeMerge MergeIntersect ts) = mergeAll $ map (checkSingle as v) ts
  checkSingle as v (SingleType (JustParamName n)) = check (n `Map.lookup` as) where
    check Nothing   = compileError $ "Param " ++ show n ++ " is undefined"
    check (Just v0) =
      if v0 `paramAllowsVariance` v
         then return ()
         else compileError $ "Param " ++ show n ++ " does not allow variance " ++ show v

checkCycles :: (MergeableM m, CompileErrorM m, Monad m) => CategoryMain -> m ()
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

checkConcrete :: (MergeableM m, CompileErrorM m, Monad m) =>
  CategoryConcrete -> CategoryMain -> m ()
checkConcrete cc = checkCategory checkAll where
  checkAll n = mergeAll . map (checkSingle n) . Set.toList
  checkSingle n n2 = do
    concrete <- n2 `categoryLookup` cc
    if concrete
       then compileError $ "Category " ++ show n ++ " cannot refine concrete " ++ show n2
       else return ()

mergeInstances :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> [TypeInstance] -> [TypeInstance]
mergeInstances r f gs = merge [] gs where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    checker x2 = checkGeneralMatch r f Covariant
                                   (SingleType $ JustTypeInstance x2)
                                   (SingleType $ JustTypeInstance x)
    ys = if isCompileError $ mergeAny $ map checker (cs ++ xs)
       then [x] -- x is not redundant => keep.
       else []  -- x is redundant => remove.

flattenRefines :: (MergeableM m, Mergeable p, CompileErrorM m,
                   Monad m, MonadFix m) =>
  TypeResolver m p -> CategoryParams ->
  CategoryFilters -> CategoryRefines -> m CategoryRefines
flattenRefines r ps fs (CategoryConnect gs) = mfix flattenAll where
  flattenAll ca@(CategoryConnect _) = do
    items <- collectAllOrErrorM $ map (flattenCategory ca) (Map.toList gs)
    return $ CategoryConnect $ Map.fromList items
  flattenCategory ca (n,(CategoryRefine gs)) = do
    gs2 <- collectAllOrErrorM $ map (flattenSingle ca n) gs
    params <- n `categoryLookup` ps
    filters <- n `categoryLookup` fs
    mapped <- return $ uncheckedZipFilters params filters
    return (n,CategoryRefine (mergeInstances r mapped $ join gs2))
  flattenSingle ca _ ta@(TypeInstance t ps) = do
    params <- (trParams r) t ps
    refines <- t `categoryLookup` ca
    collectAllOrErrorM $ map (substitute params) (crRefines refines)
  substitute params t = do
    ((),SingleType (JustTypeInstance t2)) <-
        uncheckedSubAllParams (paramLookup params) (SingleType $ JustTypeInstance t)
    return t2

checkRefines :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m p -> CategoryParams -> CategoryFilters -> CategoryRefines -> m ()
checkRefines r ps fs = checkCategory checkAll where
  checkAll n (CategoryRefine gs) = do
    ts <- collectAllOrErrorM $ map (getTypeName n) gs
    mergeAll $ map (checkGroup n) $ group ts
  getTypeName n ta@(TypeInstance t _) = do
    params <- n `categoryLookup` ps
    filters <- n `categoryLookup` fs
    mapped <- return $ uncheckedZipFilters params filters
    tfValidate r mapped ta
    return t
  checkGroup n (t:t2:ts) =
    compileError $ "Type " ++ show n ++ " has conflicting refinements of type " ++ show t
  checkGroup _ _ = return ()

uncheckedSubFilterParams :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> ParamSet [TypeFilter] ->
  m (ParamSet [TypeFilter])
uncheckedSubFilterParams replace pa = subbed where
  subbed = do
    updated <- collectAllOrErrorM $ map subAllFilters (psParams pa)
    return (ParamSet updated)
  subAllFilters ps = collectAllOrErrorM $ map subSingleFilter ps
  subSingleFilter (TypeFilter v t) = do
    ((),SingleType t2) <- uncheckedSubAllParams replace (SingleType t)
    return (TypeFilter v t2)

-- NOTE: The param filters here correspond to the *new* instances.
checkedSubAllParams :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> (ParamName -> m GeneralInstance) ->
  GeneralInstance -> m (p,GeneralInstance)
checkedSubAllParams r f = subAllParams (checkGeneralMatch r f Covariant)

uncheckedSubAllParams :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> GeneralInstance -> m (p,GeneralInstance)
uncheckedSubAllParams = subAllParams (\_ _ -> return mergeDefault)

subAllParams :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  (GeneralInstance -> GeneralInstance -> m p) ->
  (ParamName -> m GeneralInstance) -> GeneralInstance -> m (p,GeneralInstance)
subAllParams find replace = subAll where
  subAll (TypeMerge MergeUnion ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeUnion $ map snd gs)
  subAll (TypeMerge MergeIntersect ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeIntersect $ map snd gs)
  subAll (SingleType t) = subInstance t
  subInstance (JustTypeInstance (TypeInstance n (ParamSet ts))) = do
    gs <- collectAllOrErrorM $ map subAll ts
    p <- return $ mergeAll $ map fst gs
    t2 <- return $ SingleType $ JustTypeInstance $ TypeInstance n (ParamSet $ map snd gs)
    return (p,t2)
  subInstance (JustParamName n) = do
    t <- replace n
    p <- find t (SingleType $ JustParamName n)
    return (p,t)
