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
  GeneralType(..),
  MergeType(..),
  checkGeneralType,
  dualGeneralType,
  mapGeneralType,
) where

import Data.List (nub,sort)

import Base.Mergeable


data MergeType =
  AllowAnyOf |
  RequireAllOf
  deriving (Eq,Ord)

data GeneralType a =
  SingleType {
    stType :: a
  } |
  TypeMerge {
    tmMerge :: MergeType,
    tmTypes :: [GeneralType a]
  }
  deriving (Eq,Ord)

instance (Eq a,Ord a) => Mergeable (GeneralType a) where
  mergeAny = unnest . nub . sort . foldr ((++) . flattenAny) [] where
    flattenAny (TypeMerge AllowAnyOf xs) = xs
    flattenAny x                         = [x]
    unnest [x] = x
    unnest xs  = TypeMerge AllowAnyOf xs
  mergeAll = unnest . nub . sort . foldr ((++) . flattenAll) [] where
    flattenAll (TypeMerge RequireAllOf xs) = xs
    flattenAll x                           = [x]
    unnest [x] = x
    unnest xs  = TypeMerge RequireAllOf xs

instance (Eq a,Ord a) => Bounded (GeneralType a) where
  minBound = mergeAny Nothing  -- all
  maxBound = mergeAll Nothing  -- any

dualGeneralType :: GeneralType a -> GeneralType a
dualGeneralType (TypeMerge AllowAnyOf xs) =
  TypeMerge RequireAllOf $ map dualGeneralType xs
dualGeneralType (TypeMerge RequireAllOf xs) =
  TypeMerge AllowAnyOf $ map dualGeneralType xs
dualGeneralType x = x

mapGeneralType :: (Eq b,Ord b) => (a -> b) -> GeneralType a -> GeneralType b
mapGeneralType f (TypeMerge AllowAnyOf   xs) = mergeAny $ map (mapGeneralType f) xs
mapGeneralType f (TypeMerge RequireAllOf xs) = mergeAll $ map (mapGeneralType f) xs
mapGeneralType f (SingleType x) = SingleType $ f x

checkGeneralType :: (MergeableM m, Mergeable c) => (a -> b -> m c) -> GeneralType a -> GeneralType b -> m c
checkGeneralType f = singleCheck where
  singleCheck (SingleType t1) (SingleType t2) = t1 `f` t2
  -- NOTE: The merge-alls must be expanded strictly before the merge-anys.
  singleCheck ti1 (TypeMerge RequireAllOf t2) = mergeAllM $ map (ti1 `singleCheck`) t2
  singleCheck (TypeMerge AllowAnyOf   t1) ti2 = mergeAllM $ map (`singleCheck` ti2) t1
  singleCheck (TypeMerge RequireAllOf t1) ti2 = mergeAnyM $ map (`singleCheck` ti2) t1
  singleCheck ti1 (TypeMerge AllowAnyOf   t2) = mergeAnyM $ map (ti1 `singleCheck`) t2
