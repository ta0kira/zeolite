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
  checkGeneralType,
  dualGeneralType,
  mapGeneralType,
  matchSingleType,
  reduceGeneralType,
  singleType,
) where

import Data.List (nub,sort)

import Base.CompileError
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

reduceGeneralType :: ([b] -> b) -> ([b] -> b) -> (a -> b) -> GeneralType a -> b
reduceGeneralType anyOp allOp singleOp = reduce where
  reduce (AllowAnyOf   xs) = anyOp $ map reduce xs
  reduce (RequireAllOf xs) = allOp $ map reduce xs
  reduce (SingleType x) = singleOp x

checkGeneralType :: (MergeableM m, Mergeable c) => (a -> b -> m c) -> GeneralType a -> GeneralType b -> m c
checkGeneralType f = singleCheck where
  singleCheck (SingleType t1) (SingleType t2) = t1 `f` t2
  -- NOTE: The merge-alls must be expanded strictly before the merge-anys.
  singleCheck ti1 (RequireAllOf t2) = mergeAllM $ map (ti1 `singleCheck`) t2
  singleCheck (AllowAnyOf   t1) ti2 = mergeAllM $ map (`singleCheck` ti2) t1
  singleCheck (RequireAllOf t1) ti2 = mergeAnyM $ map (`singleCheck` ti2) t1
  singleCheck ti1 (AllowAnyOf   t2) = mergeAnyM $ map (ti1 `singleCheck`) t2
