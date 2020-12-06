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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Base.Mergeable (
  Mergeable(..),
  PreserveMerge(..),
  (<||>),
  (<&&>),
) where

import Data.Map as Map hiding (foldr)


class Mergeable a where
  mergeAny :: Foldable f => f a -> a
  mergeAll :: Foldable f => f a -> a

class (Bounded a, Mergeable a) => PreserveMerge a where
  type T a :: *
  convertMerge :: Mergeable b => (T a -> b) -> a -> b

(<||>) :: Mergeable a => a -> a -> a
(<||>) x y = mergeAny [x,y]
infixl 2 <||>

(<&&>) :: Mergeable a => a -> a -> a
(<&&>) x y = mergeAll [x,y]
infixl 2 <&&>

instance Mergeable Bool where
  mergeAny = foldr (||) False
  mergeAll = foldr (&&) True

instance (Ord k, Mergeable a) => Mergeable (Map k a) where
  mergeAny = Map.fromListWith (<||>) . foldr ((++) . Map.toList) []
  mergeAll = Map.fromListWith (<&&>) . foldr ((++) . Map.toList) []
