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
  MergeTree(..),
  reduceMergeTree,
) where

import Base.Mergeable


data MergeTree a =
  MergeAny [MergeTree a] |
  MergeAll [MergeTree a] |
  MergeLeaf a
  deriving (Eq,Show)

instance Functor MergeTree where
  fmap f (MergeAny xs) = MergeAny (map (fmap f) xs)
  fmap f (MergeAll xs) = MergeAll (map (fmap f) xs)
  fmap f (MergeLeaf x) = MergeLeaf (f x)

instance Foldable MergeTree where
  foldr f y xa = foldr f y (flatten xa) where
    flatten (MergeAny xs) = concat $ map flatten xs
    flatten (MergeAll xs) = concat $ map flatten xs
    flatten (MergeLeaf x) = [x]

instance Traversable MergeTree where
  traverse f (MergeAny xs) = MergeAny <$> foldr (<*>) (pure []) (map (fmap (:) . traverse f) xs)
  traverse f (MergeAll xs) = MergeAll <$> foldr (<*>) (pure []) (map (fmap (:) . traverse f) xs)
  traverse f (MergeLeaf x) = fmap MergeLeaf (f x)

instance Mergeable (MergeTree a) where
  mergeAny = MergeAny . foldr (:) []
  mergeAll = MergeAll . foldr (:) []

reduceMergeTree :: (Mergeable b, MergeableM m) => (b -> m b) -> (b -> m b) ->
  (a -> m b) -> MergeTree a -> m b
reduceMergeTree anyOp allOp leafOp xa = reduce xa where
  reduce (MergeAny xs) = do
    xs' <- mergeAnyM $ map reduce xs
    anyOp xs'
  reduce (MergeAll xs) = do
    xs' <- mergeAllM $ map reduce xs
    allOp xs'
  reduce (MergeLeaf x) = leafOp x
