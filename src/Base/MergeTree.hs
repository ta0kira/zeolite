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

module Base.MergeTree (
  MergeTree,
  mergeLeaf,
  reduceMergeTree,
) where

import Base.Mergeable


data MergeTree a =
  MergeAny [MergeTree a] |
  MergeAll [MergeTree a] |
  MergeLeaf a
  deriving (Eq)

mergeLeaf :: a -> MergeTree a
mergeLeaf = MergeLeaf

instance Show a => Show (MergeTree a) where
  show (MergeAny xs) = "mergeAny " ++ show xs
  show (MergeAll xs) = "mergeAll " ++ show xs
  show (MergeLeaf x) = "mergeLeaf " ++ show x

reduceMergeTree :: ([b] -> b) -> ([b] -> b) -> (a -> b) -> MergeTree a -> b
reduceMergeTree anyOp allOp leafOp = reduce where
  reduce (MergeAny xs) = anyOp $ map reduce xs
  reduce (MergeAll xs) = allOp $ map reduce xs
  reduce (MergeLeaf x) = leafOp x

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

instance (Eq a, Ord a) => Bounded (MergeTree a) where
  minBound = mergeAny Nothing
  maxBound = mergeAll Nothing
