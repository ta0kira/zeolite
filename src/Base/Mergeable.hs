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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Base.Mergeable (
  Mergeable(..),
  MergeableM(..),
) where

import Control.Monad (Monad(..))
import Data.Foldable
import Data.Functor


class Mergeable a where
  mergeAny :: Foldable f => f a -> a
  mergeAll :: Foldable f => f a -> a
  mergeNested :: a -> a -> a
  mergeNested x y = mergeAll [x,y]
  mergeDefault :: a
  mergeDefault = mergeAll Nothing

class (Functor m, Monad m) => MergeableM m where
  mergeAnyM :: (Mergeable a, Foldable f) => f (m a) -> m a
  mergeAllM :: (Mergeable a, Foldable f) => f (m a) -> m a
  mergeNestedM :: Mergeable a => m a -> m a -> m a
  mergeNestedM x y = mergeAllM [x,y]
  mergeDefaultM :: Mergeable a => m a
  mergeDefaultM = mergeAllM Nothing

instance Mergeable () where
  mergeAny = const ()
  mergeAll = const ()
