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
  show = reduceMergeCommon anyOp allOp leafOp where
    anyOp xs = "mergeAny [" ++ intercalate "," xs ++ "]"
    allOp xs = "mergeAll [" ++ intercalate "," xs ++ "]"
    leafOp x = "mergeLeaf " ++ show x

instance PreserveMerge (MergeTree a) where
  type T (MergeTree a) = a
  convertMerge = reduceMergeCommon mergeAny mergeAll

reduceMergeTree :: PreserveMerge a => ([b] -> b) -> ([b] -> b) -> (T a -> b) -> a -> b
reduceMergeTree anyOp allOp leafOp = reduceMergeCommon anyOp allOp leafOp . toMergeTree

toMergeTree :: PreserveMerge a => a -> MergeTree (T a)
toMergeTree = convertMerge mergeLeaf

reduceMergeCommon :: ([b] -> b) -> ([b] -> b) -> (a -> b) -> MergeTree a -> b
reduceMergeCommon anyOp allOp leafOp = reduce where
  reduce (MergeAny xs) = anyOp $ map reduce xs
  reduce (MergeAll xs) = allOp $ map reduce xs
  reduce (MergeLeaf x) = leafOp x

pairMergeTree :: (PreserveMerge a, PreserveMerge b) =>
  ([c] -> c) -> ([c] -> c) -> (T a -> T b -> c) -> a -> b -> c
pairMergeTree anyOp allOp leafOp x y = pair (toMergeTree x) (toMergeTree y) where
  pair (MergeLeaf x2) (MergeLeaf y2) = x2 `leafOp` y2
  pair x2@(MergeAll xs) y2@(MergeAny ys) = anyOp $ map (`pair` y2) xs ++ map (x2 `pair`) ys
  -- NOTE: allOp is expanded first so that anyOp is ignored when either both
  -- sides are minBound or both sides are maxBound. This allows
  -- pairMergeTree mergeAny mergeAll (==) to be a partial order.
  pair (MergeAny xs) y2 = allOp $ map (`pair` y2) xs
  pair x2 (MergeAll ys) = allOp $ map (x2 `pair`) ys
  pair (MergeAll xs) y2 = anyOp $ map (`pair` y2) xs
  pair x2 (MergeAny ys) = anyOp $ map (x2 `pair`) ys

instance Functor MergeTree where
  fmap f = reduceMergeCommon mergeAny mergeAll (mergeLeaf . f)

instance Applicative MergeTree where
  pure = mergeLeaf
  f <*> x = reduceMergeCommon mergeAny mergeAll (<$> x) f

instance Monad MergeTree where
  return = pure
  x >>= f = reduceMergeCommon mergeAny mergeAll f x

instance Foldable MergeTree where
  foldr f y = foldr f y . reduceMergeCommon concat concat (:[])

instance Traversable MergeTree where
  traverse f = reduceMergeCommon anyOp allOp leafOp where
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

mergeAnyM :: (PreserveMerge a, CompileErrorM m) => [m a] -> m a
mergeAnyM xs = do
  collectFirstM_ xs
  fmap mergeAny $ collectAnyM xs

mergeAllM :: (PreserveMerge a, CompileErrorM m) => [m a] -> m a
mergeAllM = fmap mergeAll . collectAllM

matchOnlyLeaf :: (PreserveMerge a, CompileErrorM m) => a -> m (T a)
matchOnlyLeaf = reduceMergeTree (const $ compileErrorM "") (const $ compileErrorM "") return
