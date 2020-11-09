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
  evalMergeTree,
  mergeLeaf,
  pruneMergeTree,
  reduceMergeTree,
) where

import Base.Mergeable


data MergeTree a =
  MergeAny [MergeTree a] |
  MergeAll [MergeTree a] |
  MergeLeaf a
  deriving (Eq)

instance Show a => Show (MergeTree a) where
  show (MergeAny xs) = "mergeAny " ++ show xs
  show (MergeAll xs) = "mergeAll " ++ show xs
  show (MergeLeaf x) = "mergeLeaf " ++ show x

mergeLeaf :: a -> MergeTree a
mergeLeaf = MergeLeaf

reduceMergeTree :: MergeableM m => ([b] -> m b) -> ([b] -> m b) -> (a -> m b) -> MergeTree a -> m b
reduceMergeTree anyOp allOp leafOp = reduce where
  reduce (MergeAny xs) = do
    xs' <- mergeAnyM $ map (fmap (:[]) . reduce) xs
    anyOp xs'
  reduce (MergeAll xs) = do
    xs' <- mergeAllM $ map (fmap (:[]) . reduce) xs
    allOp xs'
  reduce (MergeLeaf x) = leafOp x

evalMergeTree :: (Mergeable b, MergeableM m) => (a -> m b) -> MergeTree a -> m b
evalMergeTree f = reduceMergeTree (return . mergeAny) (return . mergeAll) f

pruneMergeTree :: MergeableM m => MergeTree (m a) -> m (MergeTree a)
pruneMergeTree = evalMergeTree (fmap mergeLeaf)

instance Functor MergeTree where
  fmap f (MergeAny xs) = mergeAny (map (fmap f) xs)
  fmap f (MergeAll xs) = mergeAll (map (fmap f) xs)
  fmap f (MergeLeaf x) = mergeLeaf (f x)

instance Applicative MergeTree where
  pure = mergeLeaf
  (MergeAny fs) <*> x = mergeAny $ map (<*> x) fs
  (MergeAll fs) <*> x = mergeAll $ map (<*> x) fs
  (MergeLeaf f) <*> x = f <$> x

instance Monad MergeTree where
  return = pure
  (MergeAny xs) >>= f = mergeAny $ map (>>= f) xs
  (MergeAll xs) >>= f = mergeAll $ map (>>= f) xs
  (MergeLeaf x) >>= f = f x

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

instance MergeableM MergeTree where
