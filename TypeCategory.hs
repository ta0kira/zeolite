{-# LANGUAGE Safe #-}

module TypeCategory (
) where

import Data.List (group, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CompileInfo
import TypeInstance
import TypesBase


newtype CategoryConnect a =
  CategoryConnect {
    ccMap :: Map.Map TypeName a
  }

data CategoryRefine =
  CategoryRefine {
    crName :: TypeName,
    crRefines :: [GeneralInstance]
  }

type CategoryMain = CategoryConnect (Set.Set TypeName)

type Refinements = CategoryConnect CategoryRefine

categoryLookup :: (CompileErrorM m, Monad m) => CategoryConnect a -> TypeName -> m a
categoryLookup (CategoryConnect cs) n = resolve $ n `Map.lookup` cs where
  resolve (Just x) = return x
  resolve _        = compileErrorM $ "Category " ++ show n ++ " not found"

checkCategory :: (Mergeable b) => (TypeName -> a -> b) -> CategoryConnect a -> b
checkCategory f (CategoryConnect cs) = mergeAll $ map (\(k,v) -> f k v) (Map.toList cs)


checkCycles :: (Mergeable (m ()), CompileErrorM m, Monad m) => CategoryMain -> m ()
checkCycles ca@(CategoryConnect cs) = checkCategory (checker []) ca where
  checker ns n ts
    | n `Set.member` (Set.fromList ns) =
      compileError $ "Cycle found: " ++ intercalate " -> " (map show (ns ++ [n]))
    | otherwise =
      mergeAll $ map (\(n2,ts2) -> ts2 >>= checker (ns ++ [n]) n2) (map find $ Set.toList ts) where
        find t = (t,categoryLookup ca t)


mergeInstances :: (Mergeable (m ()), Mergeable (m p), CompileErrorM m, Monad m) =>
  TypeResolver m p -> [GeneralInstance] -> [GeneralInstance]
mergeInstances r gs = merge [] gs where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    ys = if isCompileError $ mergeAny $ map (\x2 -> checkGeneralMatch r Covariant x2 x) (cs ++ xs)
       then [x] -- x is not redundant => keep.
       else []  -- x is redundant => remove.


checkedSubAllParams :: (Mergeable (m ()), Mergeable (m p),
                        Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> Map.Map ParamName GeneralInstance ->
  GeneralInstance -> m (p,GeneralInstance)
checkedSubAllParams r = subAllParams (checkGeneralMatch r Covariant)

uncheckedSubAllParams :: (Mergeable (m ()), Mergeable (m p),
                          Mergeable p, CompileErrorM m, Monad m) =>
  Map.Map ParamName GeneralInstance -> GeneralInstance -> m (p,GeneralInstance)
uncheckedSubAllParams = subAllParams (\_ _ -> return mergeDefault)

subAllParams :: (Mergeable (m ()), Mergeable (m p),
                 Mergeable p, CompileErrorM m, Monad m) =>
  (GeneralInstance -> GeneralInstance -> m p) ->
  Map.Map ParamName GeneralInstance ->
  GeneralInstance -> m (p,GeneralInstance)
subAllParams find ps = subAll where
  subAll (TypeMerge MergeUnion ts) = do
    gs <- sequence $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeUnion $ map snd gs)
  subAll (TypeMerge MergeIntersect ts) = do
    gs <- sequence $ map subAll ts
    return (mergeAll $ map fst gs,TypeMerge MergeIntersect $ map snd gs)
  subAll (TypeInstance t) = subInstance t
  subInstance (TypeCategoryInstance n (ParamSet ts)) = do
    gs <- sequence $ map subAll ts
    return (mergeAll $ map fst gs,TypeInstance $ TypeCategoryInstance n $ (ParamSet $ map snd gs))
  subInstance (TypeCategoryParam t) = subParam t
  subParam pa@(TypeParam n cs) = maybeSub (n `Map.lookup` ps) where
    -- Found => check and substitute.
    maybeSub (Just t) = do
      p <- find t (TypeInstance $ TypeCategoryParam pa)
      return (p,t)
    -- Not found => replace within the constraints.
    maybeSub _ = do
      gs <- sequence $ map subConstraint cs
      return (mergeAll $ map fst gs,TypeInstance $ TypeCategoryParam $ TypeParam n $ map snd gs)
  subConstraint (TypeFilter t) = do
    (p,t2) <- subAll t
    return (p,TypeFilter t2)
  subConstraint f = return (mergeDefault,f)
