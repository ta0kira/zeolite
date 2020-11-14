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
{-# LANGUAGE TypeFamilies #-}

module Types.GeneralType (
  GeneralType,
  dualGeneralType,
  mapGeneralType,
  singleType,
) where

import qualified Data.Set as Set

import Base.MergeTree
import Base.Mergeable


data GeneralType a =
  SingleType {
    stType :: a
  } |
  AllowAnyOf {
    aaoTypes :: Set.Set (GeneralType a)
  } |
  RequireAllOf {
    raoTypes :: Set.Set (GeneralType a)
  }
  deriving (Eq,Ord)

singleType :: (Eq a, Ord a) => a -> GeneralType a
singleType = SingleType

instance (Eq a, Ord a) => Mergeable (GeneralType a) where
  mergeAny = unnest . foldr (Set.union . flattenAny) Set.empty where
    flattenAny (AllowAnyOf xs) = xs
    flattenAny x               = Set.fromList [x]
    unnest xs = case Set.toList xs of
                     [x] -> x
                     _ -> AllowAnyOf xs
  mergeAll = unnest . foldr (Set.union . flattenAll) Set.empty where
    flattenAll (RequireAllOf xs) = xs
    flattenAll x                 = Set.fromList [x]
    unnest xs = case Set.toList xs of
                     [x] -> x
                     _ -> RequireAllOf xs

instance (Eq a, Ord a) => PreserveMerge (GeneralType a) where
  type T (GeneralType a) = a
  toMergeTree (AllowAnyOf   xs) = mergeAny $ map toMergeTree $ Set.toList xs
  toMergeTree (RequireAllOf xs) = mergeAll $ map toMergeTree $ Set.toList xs
  toMergeTree (SingleType x)    = mergeLeaf x

instance (Eq a, Ord a) => Bounded (GeneralType a) where
  minBound = mergeAny Nothing  -- all
  maxBound = mergeAll Nothing  -- any

dualGeneralType :: (Eq a, Ord a) => GeneralType a -> GeneralType a
dualGeneralType = reduceMergeTree mergeAll mergeAny singleType

mapGeneralType :: (Eq a, Ord a, Eq b, Ord b) => (a -> b) -> GeneralType a -> GeneralType b
mapGeneralType = reduceMergeTree mergeAny mergeAll . (singleType .)
