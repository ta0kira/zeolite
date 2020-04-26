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
) where

import Base.CompileError
import Base.Mergeable


data MergeType =
  MergeUnion |
  MergeIntersect
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

checkGeneralType :: (MergeableM m, Mergeable c) => (a -> b -> m c) -> GeneralType a -> GeneralType b -> m c
checkGeneralType f ti1 ti2 = singleCheck ti1 ti2 where
  singleCheck (SingleType t1) (SingleType t2) = t1 `f` t2
  -- NOTE: The merge-alls must be expanded strictly before the merge-anys.
  singleCheck ti1 (TypeMerge MergeIntersect t2) = mergeAllM $ map (ti1 `singleCheck`) t2
  singleCheck (TypeMerge MergeUnion     t1) ti2 = mergeAllM $ map (`singleCheck` ti2) t1
  singleCheck (TypeMerge MergeIntersect t1) ti2 = mergeAnyM $ map (`singleCheck` ti2) t1
  singleCheck ti1 (TypeMerge MergeUnion     t2) = mergeAnyM $ map (ti1 `singleCheck`) t2
