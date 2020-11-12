{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

module Types.GeneralType (
  GeneralType,
  dualGeneralType,
  mapGeneralType,
  matchSingleType,
  pairGeneralType,
  reduceGeneralType,
  singleType,
) where

import Data.List (nub,sort)

import Base.CompileError
import Base.MergeTree
import Base.Mergeable


data GeneralType a =
  SingleType {
    stType :: a
  } |
  AllowAnyOf {
    aaoTypes :: [GeneralType a]
  } |
  RequireAllOf {
    raoTypes :: [GeneralType a]
  }
  deriving (Eq,Ord)

singleType :: a -> GeneralType a
singleType = SingleType

instance (Eq a, Ord a) => Mergeable (GeneralType a) where
  mergeAny = unnest . nub . sort . foldr ((++) . flattenAny) [] where
    flattenAny (AllowAnyOf xs) = xs
    flattenAny x               = [x]
    unnest [x] = x
    unnest xs  = AllowAnyOf xs
  mergeAll = unnest . nub . sort . foldr ((++) . flattenAll) [] where
    flattenAll (RequireAllOf xs) = xs
    flattenAll x                 = [x]
    unnest [x] = x
    unnest xs  = RequireAllOf xs

instance (Eq a, Ord a) => Bounded (GeneralType a) where
  minBound = mergeAny Nothing  -- all
  maxBound = mergeAll Nothing  -- any

matchSingleType :: CompileErrorM m => GeneralType a -> m a
matchSingleType = reduceGeneralType (const $ compileErrorM "") (const $ compileErrorM "") return

dualGeneralType :: (Eq a, Ord a) => GeneralType a -> GeneralType a
dualGeneralType = reduceGeneralType mergeAll mergeAny singleType

mapGeneralType :: (Eq b, Ord b) => (a -> b) -> GeneralType a -> GeneralType b
mapGeneralType = reduceGeneralType mergeAny mergeAll . (singleType .)

toMergeTree :: GeneralType a -> MergeTree a
toMergeTree (AllowAnyOf   xs) = mergeAny $ map toMergeTree xs
toMergeTree (RequireAllOf xs) = mergeAll $ map toMergeTree xs
toMergeTree (SingleType x)    = mergeLeaf x

reduceGeneralType :: ([b] -> b) -> ([b] -> b) -> (a -> b) -> GeneralType a -> b
reduceGeneralType anyOp allOp singleOp = reduceMergeTree anyOp allOp singleOp . toMergeTree

pairGeneralType :: ([c] -> c) -> ([c] -> c) -> (a -> b -> c) -> GeneralType a -> GeneralType b -> c
pairGeneralType anyOp allOp singleOp x y =
  pairMergeTree anyOp allOp singleOp (toMergeTree x) (toMergeTree y)
