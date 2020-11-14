{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Base.MergeTree (
  MergeTree,
  PreserveMerge(..),
  matchOnlyLeaf,
  mergeAllM,
  mergeAnyM,
  mergeLeaf,
  pairMergeTree,
  reduceMergeTree,
) where

import Data.List (intercalate)

import Base.CompileError
import Base.Mergeable


data MergeTree a =
  MergeAny [MergeTree a] |
  MergeAll [MergeTree a] |
  MergeLeaf a
  deriving (Eq)

mergeLeaf :: a -> MergeTree a
mergeLeaf = MergeLeaf

instance Show a => Show (MergeTree a) where
  show = reduceMergeTree anyOp allOp leafOp where
    anyOp xs = "mergeAny [" ++ intercalate "," xs ++ "]"
    allOp xs = "mergeAll [" ++ intercalate "," xs ++ "]"
    leafOp x = "mergeLeaf " ++ show x

class (Bounded t, Mergeable t) => PreserveMerge t where
  type T t :: *
  toMergeTree :: t -> MergeTree (T t)

instance PreserveMerge (MergeTree a) where
  type T (MergeTree a) = a
  toMergeTree = id

reduceMergeTree :: PreserveMerge t => ([b] -> b) -> ([b] -> b) -> (T t -> b) -> t -> b
reduceMergeTree anyOp allOp leafOp = reduce . toMergeTree where
  reduce (MergeAny xs) = anyOp $ map reduce xs
  reduce (MergeAll xs) = allOp $ map reduce xs
  reduce (MergeLeaf x) = leafOp x

pairMergeTree :: (PreserveMerge t1, PreserveMerge t2) =>
  ([c] -> c) -> ([c] -> c) -> (T t1 -> T t2 -> c) -> t1 -> t2 -> c
pairMergeTree anyOp allOp leafOp x y = pair (toMergeTree x) (toMergeTree y) where
  pair (MergeLeaf t1) (MergeLeaf t2) = t1 `leafOp` t2
  -- NOTE: allOp is expanded first so that anyOp is ignored when either both
  -- sides are minBound or both sides are maxBound. This allows
  -- pairMergeTree mergeAny mergeAll (==) to be a partial order.
  pair (MergeAny t1) ti2 = allOp $ map (`pair` ti2) t1
  pair ti1 (MergeAll t2) = allOp $ map (ti1 `pair`) t2
  pair (MergeAll t1) ti2 = anyOp $ map (`pair` ti2) t1
  pair ti1 (MergeAny t2) = anyOp $ map (ti1 `pair`) t2

instance Functor MergeTree where
  fmap f = reduceMergeTree mergeAny mergeAll (mergeLeaf . f)

instance Applicative MergeTree where
  pure = mergeLeaf
  f <*> x = reduceMergeTree mergeAny mergeAll (<$> x) f

instance Monad MergeTree where
  return = pure
  x >>= f = reduceMergeTree mergeAny mergeAll f x

instance Foldable MergeTree where
  foldr f y = foldr f y . reduceMergeTree concat concat (:[])

instance Traversable MergeTree where
  traverse f = reduceMergeTree anyOp allOp leafOp where
    anyOp = (mergeAny <$>) . foldr (<*>) (pure []) . (map (fmap (:)))
    allOp = (mergeAll <$>) . foldr (<*>) (pure []) . (map (fmap (:)))
    leafOp = (mergeLeaf <$>) . f

instance Mergeable (MergeTree a) where
  mergeAny = unnest . foldr ((++) . flattenAny) [] where
    flattenAny (MergeAny xs) = xs
    flattenAny x             = [x]
    unnest [x] = x
    unnest xs  = MergeAny xs
  mergeAll = unnest . foldr ((++) . flattenAll) [] where
    flattenAll (MergeAll xs) = xs
    flattenAll x             = [x]
    unnest [x] = x
    unnest xs  = MergeAll xs

instance Bounded (MergeTree a) where
  minBound = mergeAny Nothing
  maxBound = mergeAll Nothing

mergeAnyM :: (PreserveMerge t, CompileErrorM m) => [m t] -> m t
mergeAnyM xs = do
  collectFirstM_ xs
  fmap mergeAny $ collectAnyM xs

mergeAllM :: (PreserveMerge t, CompileErrorM m) => [m t] -> m t
mergeAllM = fmap mergeAll . collectAllM

matchOnlyLeaf :: (PreserveMerge t, CompileErrorM m) => t -> m (T t)
matchOnlyLeaf = reduceMergeTree (const $ compileErrorM "") (const $ compileErrorM "") return
