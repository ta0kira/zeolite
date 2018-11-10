{-# LANGUAGE Safe #-}

module TypeCategory (
) where

import Control.Monad (join,(>=>))
import Control.Monad.Fix (MonadFix,mfix)
import Data.List (group,intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CompileInfo
import TypeInstance
import TypesBase


newtype CategoryConnect a =
  CategoryConnect {
    ccMap :: Map.Map TypeName a
  }

newtype CategoryRefine =
  CategoryRefine {
    crRefines :: [GeneralInstance]
  }

type CategoryMain = CategoryConnect (Set.Set TypeName)

type Refinements = CategoryConnect CategoryRefine

type CategoryMissing = CategoryConnect Missingness

type CategoryVariance = CategoryConnect (ParamSet Variance)

type CategoryParams = CategoryConnect (ParamSet TypeParam)

categoryLookup :: (CompileErrorM m, Monad m) => TypeName -> CategoryConnect a -> m a
categoryLookup n (CategoryConnect cs) = resolve $ n `Map.lookup` cs where
  resolve (Just x) = return x
  resolve _        = compileError $ "Category " ++ show n ++ " not found"

checkCategory :: (Mergeable b) => (TypeName -> a -> b) -> CategoryConnect a -> b
checkCategory f (CategoryConnect cs) = mergeAll $ map (\(k,v) -> f k v) (Map.toList cs)


data CategorySystem =
  CategorySystem {
    csMain :: CategoryMain,
    csRefine :: Refinements,
    csMissing :: CategoryMissing,
    csVariance :: CategoryVariance,
    csParams :: CategoryParams
  }

validateCategory :: (Mergeable (m ()), Mergeable (m p),
                     Mergeable p, CompileErrorM m, Monad m, MonadFix m) =>
  TypeResolver m p -> CategorySystem -> m CategorySystem
validateCategory r cs = do
  checkCycles (csMain cs)
  refine <- flattenRefines r (csRefine cs)
  checkRefines refine
  labeledVars <- labelParamVals (csParams cs) (csVariance cs)
  checkVariances labeledVars (csVariance cs) refine
  return $ CategorySystem {
      csMain = csMain cs,
      -- TODO: Check refines w.r.t. missingness and param filters.
      csRefine = refine,
      csMissing = csMissing cs,
      csVariance = csVariance cs,
      csParams = csParams cs
    }


labelParamVals :: (Mergeable (m ()), CompileErrorM m, Monad m) =>
  CategoryParams -> CategoryConnect (ParamSet a) ->
  m (CategoryConnect (Map.Map ParamName a))
labelParamVals (CategoryConnect pa) va@(CategoryConnect _) = paired where
  paired = do
    pairs <- collectOrErrorM $ map pairType (Map.toList pa)
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
    mergeAll $ map (checkSingle as Covariant) gs
  checkSingle as v (TypeInstance (TypeCategoryInstance t ps)) = do
    vs2 <- t `categoryLookup` vs
    checkParamsMatch (\_ _ -> return ()) vs2 ps
    mergeAll $ map (\(v2,p) -> checkSingle as (v `composeVariance` v2) p) (zip (psParams vs2) (psParams ps))
  checkSingle as v (TypeMerge MergeUnion     ts) = mergeAll $ map (checkSingle as v) ts
  checkSingle as v (TypeMerge MergeIntersect ts) = mergeAll $ map (checkSingle as v) ts
  checkSingle as v (TypeInstance (TypeCategoryParam (TypeParam n _))) = check (n `Map.lookup` as) where
    check Nothing   = compileError $ "Param " ++ show n ++ " is undefined"
    check (Just v0) =
      if v0 `paramAllowsVariance` v
         then return ()
         else compileError $ "Param " ++ show n ++ " does not allow variance " ++ show v

checkCycles :: (Mergeable (m ()), CompileErrorM m, Monad m) => CategoryMain -> m ()
checkCycles ca@(CategoryConnect cs) = checkCategory (checker []) ca where
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

mergeInstances :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> [GeneralInstance] -> [GeneralInstance]
mergeInstances r gs = merge [] gs where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    ys = if isCompileError $ mergeAny $ map (\x2 -> checkGeneralMatch r Covariant x2 x) (cs ++ xs)
       then [x] -- x is not redundant => keep.
       else []  -- x is redundant => remove.

flattenRefines :: (Mergeable (m ()), Mergeable (m p),
                   Mergeable p, CompileErrorM m, Monad m, MonadFix m) =>
  TypeResolver m p -> Refinements -> m Refinements
flattenRefines r (CategoryConnect gs) = mfix flattenAll where
  flattenAll ca@(CategoryConnect _) = do
    items <- collectOrErrorM $ map (flattenCategory ca) (Map.toList gs)
    return $ CategoryConnect $ Map.fromList items
  flattenCategory ca (n,(CategoryRefine gs)) = do
    gs2 <- collectOrErrorM $ map (flattenSingle ca n) gs
    return (n,CategoryRefine (mergeInstances r $ join gs2))
  flattenSingle ca _ ta@(TypeInstance (TypeCategoryInstance t ps)) = do
    params <- (trParams r) t ps
    refines <- t `categoryLookup` ca
    -- TODO: This should preserve the path (fst from the sub call) since it
    -- might be needed to keep track of conversion information.
    -- TODO: Should substitution be unchecked here?
    collectOrErrorM $ map (checkedSubAllParams r params >=> return . snd) (crRefines refines)
  flattenSingle _ n (TypeMerge MergeUnion _) =
    compileError $ "Type " ++ show n ++ " cannot refine a union"
  flattenSingle _ n (TypeMerge MergeIntersect _) =
    compileError $ "Type " ++ show n ++ " cannot refine an intersection"
  flattenSingle _ n (TypeInstance (TypeCategoryParam _)) =
    compileError $ "Type " ++ show n ++ " cannot refine a param"

checkRefines :: (Mergeable (m ()), CompileErrorM m, Monad m) => Refinements -> m ()
checkRefines = checkCategory checkAll where
  checkAll n (CategoryRefine gs) = do
    ts <- collectOrErrorM $ map (getTypeName n) gs
    mergeAll $ map (checkGroup n) $ group ts
  getTypeName _ (TypeInstance (TypeCategoryInstance t _)) = return t
  getTypeName n (TypeMerge MergeUnion _) =
    compileError $ "Type " ++ show n ++ " cannot refine a union"
  getTypeName n (TypeMerge MergeIntersect _) =
    compileError $ "Type " ++ show n ++ " cannot refine an intersection"
  getTypeName n (TypeInstance (TypeCategoryParam _)) =
    compileError $ "Type " ++ show n ++ " cannot refine a param"
  checkGroup n (t:t2:ts) =
    compileError $ "Type " ++ show n ++ " has conflicting refinements of type " ++ show t
  checkGroup _ _ = return ()

checkedSubAllParams :: (Mergeable (m ()), Mergeable (m p),
                        Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> AssignedParams -> GeneralInstance -> m (p,GeneralInstance)
checkedSubAllParams r = subAllParams (checkGeneralMatch r Covariant)

uncheckedSubAllParams :: (Mergeable (m ()), Mergeable (m p),
                          Mergeable p, CompileErrorM m, Monad m) =>
  AssignedParams -> GeneralInstance -> m (p,GeneralInstance)
uncheckedSubAllParams = subAllParams (\_ _ -> return mergeDefault)

subAllParams :: (Mergeable (m ()), Mergeable (m p),
                 Mergeable p, CompileErrorM m, Monad m) =>
  (GeneralInstance -> GeneralInstance -> m p) ->
  AssignedParams -> GeneralInstance -> m (p,GeneralInstance)
subAllParams find ps = subAll where
  subAll (TypeMerge MergeUnion ts) = do
    gs <- collectOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeUnion $ map snd gs)
  subAll (TypeMerge MergeIntersect ts) = do
    gs <- collectOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeIntersect $ map snd gs)
  subAll (TypeInstance t) = subInstance t
  subInstance (TypeCategoryInstance n (ParamSet ts)) = do
    gs <- collectOrErrorM $ map subAll ts
    return (mergeAll $ map fst gs,TypeInstance $ TypeCategoryInstance n $ (ParamSet $ map snd gs))
  subInstance (TypeCategoryParam t) = subParam t
  subParam pa@(TypeParam n cs) = maybeSub (n `Map.lookup` ps) where
    -- Found => check and substitute.
    maybeSub (Just t) = do
      p <- find t (TypeInstance $ TypeCategoryParam pa)
      -- TODO: This might not WAI if there is a name clash between old params
      -- and new params, e.g., one x is replaced by a term containing another x.
      return (p,t)
    -- Not found => replace within the constraints.
    maybeSub _ = do
      gs <- collectOrErrorM $ map subConstraint cs
      return (mergeAll $ map fst gs,TypeInstance $ TypeCategoryParam $ TypeParam n $ map snd gs)
  subConstraint (TypeFilter t) = do
    (p,t2) <- subAll t
    return (p,TypeFilter t2)
  subConstraint f = return (mergeDefault,f)
