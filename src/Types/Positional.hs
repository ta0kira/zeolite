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

module Types.Positional (
  Positional(..),
  alwaysPair,
  processPairs,
  processPairs_,
  processPairsM,
  processPairsT,
) where

import Control.Monad.Trans (MonadTrans(..))

import Base.CompileError
import Base.Mergeable


newtype Positional a =
  Positional {
    pValues :: [a]
  }
  deriving (Eq,Ord,Show)

instance Functor Positional where
  fmap f = Positional . fmap f . pValues

alwaysPair :: Monad m => a -> b -> m (a,b)
alwaysPair x y = return (x,y)

processPairs :: (Show a, Show b, CompileErrorM m) =>
  (a -> b -> m c) -> Positional a -> Positional b -> m [c]
processPairs f (Positional ps1) (Positional ps2)
  | length ps1 == length ps2 =
    mapErrorsM (uncurry f) (zip ps1 ps2)
  | otherwise = mismatchError ps1 ps2

processPairsM :: (Show a, Show b, Mergeable c, CompileErrorM m) =>
  (a -> b -> m c) -> Positional a -> Positional b -> m c
processPairsM f x y = fmap mergeAll $ processPairs f x y

processPairs_ :: (Show a, Show b, CompileErrorM m) =>
  (a -> b -> m c) -> Positional a -> Positional b -> m ()
processPairs_ f xs ys = processPairs f xs ys >> return ()

processPairsT :: (MonadTrans t, Monad (t m), Show a, Show b, CompileErrorM m) =>
  (a -> b -> t m c) -> Positional a -> Positional b -> t m [c]
processPairsT f (Positional ps1) (Positional ps2)
  | length ps1 == length ps2 =
    sequence $ map (uncurry f) (zip ps1 ps2)
  | otherwise = lift $ mismatchError ps1 ps2

mismatchError :: (Show a, Show b, CompileErrorM m) => [a] -> [b] -> m c
mismatchError ps1 ps2 = compileErrorM $ "Count mismatch: " ++ show ps1 ++
                                       " (expected) vs. " ++ show ps2 ++ " (actual)"
