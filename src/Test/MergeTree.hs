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

module Test.MergeTree (tests) where

import Control.Monad (when)
import Data.Char (toUpper)

import Data.Functor.Identity

import Base.CompileError
import Base.CompileInfo
import Base.MergeTree
import Base.Mergeable


tests :: [IO (CompileInfo ())]
tests = [
   checkMatch (mergeAny $ fmap mergeLeaf [2,4,6]) (fmap (*2))
              (mergeAny $ map mergeLeaf [1..3] :: MergeTree Int),
   checkMatch (mergeAll $ fmap mergeLeaf [2,4,6]) (fmap (*2))
              (mergeAll $ map mergeLeaf [1..3] :: MergeTree Int),

   checkMatch (mergeLeaf 1) id (mergeAny [mergeLeaf 1,minBound] :: MergeTree Int),
   checkMatch (mergeLeaf 1) id (mergeAll [mergeLeaf 1,maxBound] :: MergeTree Int),

   checkMatch2 (mergeAny [mergeLeaf 1,mergeLeaf 2,mergeAll [mergeLeaf 3,mergeLeaf 4]])
               (mergeAny [mergeLeaf 1,mergeLeaf 2,mergeLeaf 3,mergeLeaf 4])
               (mergeAny [mergeAny [mergeLeaf 1],mergeLeaf 2,mergeAll [mergeLeaf 3,mergeLeaf 4]] :: MergeTree Int),
   checkMatch2 (mergeAll [mergeLeaf 1,mergeLeaf 2,mergeAny [mergeLeaf 3,mergeLeaf 4]])
               (mergeAll [mergeLeaf 1,mergeLeaf 2,mergeLeaf 3,mergeLeaf 4])
               (mergeAll [mergeAll [mergeLeaf 1],mergeLeaf 2,mergeAny [mergeLeaf 3,mergeLeaf 4]] :: MergeTree Int),

   checkSuccess (mergeAny $ map mergeLeaf [2,4]) (pruneMergeTree . fmap oddError)
                (mergeAny $ map mergeLeaf [1..4] :: MergeTree Int),
   checkError "1 is odd\n3 is odd\n" (pruneMergeTree . fmap oddError)
              (mergeAll $ map mergeLeaf [1..4] :: MergeTree Int),
   checkSuccess (mergeAny $ map mergeLeaf [1..4]) (pruneMergeTree . fmap return)
                (mergeAny $ map mergeLeaf [1..4] :: MergeTree Int),
   checkSuccess (mergeAll $ map mergeLeaf [1..4]) (pruneMergeTree . fmap return)
                (mergeAll $ map mergeLeaf [1..4] :: MergeTree Int),

   checkSuccess [2,4]
                (reduceMergeTree (return . mergeAny) (\xs -> compileErrorM $ "mergeAll " ++ show xs) oddError2)
                (mergeAny $ map mergeLeaf [1..4] :: MergeTree Int),
   checkError "1 is odd\n3 is odd\n"
              (reduceMergeTree (\xs -> compileErrorM $ "mergeAny " ++ show xs) (return . mergeAll) oddError2)
              (mergeAll $ map mergeLeaf [1..4] :: MergeTree Int),

   checkMatch False (runIdentity . evalMergeTree (return . odd)) (mergeAll [mergeLeaf 1,mergeLeaf 2] :: MergeTree Int),
   checkMatch True  (runIdentity . evalMergeTree (return . odd)) (mergeAll [mergeLeaf 1,mergeLeaf 1] :: MergeTree Int),
   checkMatch True  (runIdentity . evalMergeTree (return . odd)) (mergeAny [mergeLeaf 1,mergeLeaf 2] :: MergeTree Int),
   checkMatch False (runIdentity . evalMergeTree (return . odd)) (mergeAny [mergeLeaf 2,mergeLeaf 2] :: MergeTree Int),
   checkMatch True  (runIdentity . evalMergeTree (return . odd)) (maxBound :: MergeTree Int),
   checkMatch False (runIdentity . evalMergeTree (return . odd)) (minBound :: MergeTree Int),

   -- a*(b&c)*(d|e) = (a*b&a*c)*(d|e) = (a*b*(d|e)&a*c*(d|e)) = (a*b*d|a*b*e)&(a*c*d|a*c*e)
   checkMatch (mergeAll [mergeAny [mergeLeaf "abd",mergeLeaf "abe"],mergeAny [mergeLeaf "acd",mergeLeaf "ace"]]) sequence
              [mergeLeaf 'a',mergeAll [mergeLeaf 'b',mergeLeaf 'c'],mergeAny [mergeLeaf 'd',mergeLeaf 'e']],

   checkMatch (mergeAny [mergeAll [mergeLeaf False,mergeLeaf False],mergeAll [mergeLeaf False,mergeLeaf True]]) mergeAllM
              ([mergeAny [mergeLeaf False,mergeLeaf True],mergeAll [mergeLeaf False,mergeLeaf True]]),
   checkMatch (mergeAny [mergeAll [mergeLeaf False,mergeLeaf True],mergeAll [mergeLeaf True,mergeLeaf True]]) mergeAnyM
              ([mergeAny [mergeLeaf False,mergeLeaf True],mergeAll [mergeLeaf False,mergeLeaf True]]),

   checkMatch (mergeAll [mergeAll [mergeLeaf 'a',mergeLeaf 'A'],
                         mergeAny [mergeAll [mergeLeaf 'b',mergeLeaf 'B'],
                                   mergeAll [mergeLeaf 'c',mergeLeaf 'C']]])
              (\x -> do
                x' <- x
                mergeAll [return x',return (toUpper x')])
              (mergeAll [mergeLeaf 'a',mergeAny [mergeLeaf 'b',mergeLeaf 'c']])
 ]

oddError :: Int -> CompileInfo Int
oddError x = do
  when (odd x) $ compileErrorM $ show x ++ " is odd"
  return x

oddError2 :: Int -> CompileInfo [Int]
oddError2 = fmap (:[]) . oddError

checkMatch :: (Eq b, Show b) => b -> (a -> b) -> a -> IO (CompileInfo ())
checkMatch x f y = let y' = f y in
  return $ if x /= y'
              then compileErrorM $ "Expected " ++ show x ++ " but got " ++ show y'
              else return ()

checkMatch2 :: (Eq a, Show a) => a -> a -> a -> IO (CompileInfo ())
checkMatch2 x y z = return $ do
  when (x /= z) $ compileErrorM $ "Expected " ++ show x ++ " but got " ++ show z
  when (y == z) $ compileErrorM $ "Expected something besides " ++ show y

checkSuccess :: (Eq b, Show b) => b -> (a -> CompileInfo b) -> a -> IO (CompileInfo ())
checkSuccess x f y = let y' = f y in
  return $ if isCompileError y' || getCompileSuccess y' == x
              then y' >> return ()
              else compileErrorM $ "Expected value " ++ show x ++ " but got value " ++ show (getCompileSuccess y')

checkError :: Show b => String -> (a -> CompileInfo b) -> a -> IO (CompileInfo ())
checkError e f y = let y' = f y in
  return $ if not (isCompileError y')
              then compileErrorM $ "Expected error \"" ++ e ++ "\" but got value " ++ show (getCompileSuccess y')
              else if show (getCompileError y') == e
                      then return ()
                      else compileErrorM $ "Expected error \"" ++ e ++ "\" but got error \"" ++ show (getCompileError y') ++ "\""
