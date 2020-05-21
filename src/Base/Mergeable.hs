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
  mergeDefault,
  mergeDefaultM,
) where

import Data.Functor.Identity
import Data.Map as Map hiding (foldr)

#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif


class Mergeable a where
  mergeAny :: Foldable f => f a -> a
  mergeAll :: Foldable f => f a -> a

class Monad m => MergeableM m where
  mergeAnyM :: (Mergeable a, Foldable f) => f (m a) -> m a
  mergeAllM :: (Mergeable a, Foldable f) => f (m a) -> m a

mergeDefault :: Mergeable a => a
mergeDefault = mergeAll Nothing

mergeDefaultM :: (MergeableM m, Mergeable a) => m a
mergeDefaultM = mergeAllM Nothing

instance Mergeable () where
  mergeAny = const ()
  mergeAll = const ()

instance Mergeable [a] where
  mergeAny = foldr (++) []
  mergeAll = foldr (++) []

instance (Ord k, Mergeable a) => Mergeable (Map k a) where
  mergeAny = Map.fromListWith (\x y -> mergeAny [x,y]) . foldr ((++) . Map.toList) []
  mergeAll = Map.fromListWith (\x y -> mergeAll [x,y]) . foldr ((++) . Map.toList) []

instance MergeableM Maybe where
  mergeAnyM = fmap mergeAny . foldr ((<>) . fmap (:[])) Nothing
  mergeAllM = fmap mergeAll . sequence . foldr (:) []

instance MergeableM Identity where
  mergeAnyM = Identity . mergeAny . foldr ((:) . runIdentity) []
  mergeAllM = Identity . mergeAll . foldr ((:) . runIdentity) []
