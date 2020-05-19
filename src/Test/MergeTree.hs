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

import Base.CompileError
import Base.CompileInfo
import Base.MergeTree
import Base.Mergeable


tests :: [IO (CompileInfo ())]
tests = [
   checkMatch (mergeAny $ fmap MergeLeaf [2,4,6]) (fmap (*2))
              (mergeAny $ map MergeLeaf [1..3] :: MergeTree Int),
   checkMatch (mergeAll $ fmap MergeLeaf [2,4,6]) (fmap (*2))
              (mergeAll $ map MergeLeaf [1..3] :: MergeTree Int),

   checkMatch (MergeLeaf 1) id (mergeAny [MergeLeaf 1,mergeAny []] :: MergeTree Int),
   checkMatch (MergeLeaf 1) id (mergeAll [MergeLeaf 1,mergeAll []] :: MergeTree Int),

   checkMatch ([1,2]) (foldr (:) [])
              (mergeAny [MergeLeaf 1,mergeAll [MergeLeaf 2]] :: MergeTree Int),
   checkMatch ([1,2]) (foldr (:) [])
              (mergeAll [MergeLeaf 1,mergeAny [MergeLeaf 2]] :: MergeTree Int),

   checkSuccess (mergeAny $ fmap MergeLeaf [1,2,3]) (sequence . fmap return)
                (mergeAny $ map MergeLeaf [1..3] :: MergeTree Int),
   checkSuccess (mergeAll $ fmap MergeLeaf [1,2,3]) (sequence . fmap return)
                (mergeAll $ map MergeLeaf [1..3] :: MergeTree Int),

   checkError "1 is odd\n" (sequence . fmap oddError)
              (mergeAny $ map MergeLeaf [1..3] :: MergeTree Int),
   checkError "1 is odd\n" (sequence . fmap oddError)
              (mergeAll $ map MergeLeaf [1..3] :: MergeTree Int),

   checkSuccess [2,4]
                (reduceMergeTree return (\xs -> compileErrorM $ "mergeAll " ++ show xs) oddError)
                (mergeAny $ map MergeLeaf [1..4] :: MergeTree Int),
   checkError "1 is odd\n3 is odd\n"
              (reduceMergeTree (\xs -> compileErrorM $ "mergeAny " ++ show xs) return oddError)
              (mergeAll $ map MergeLeaf [1..4] :: MergeTree Int)
 ]

oddError :: Int -> CompileInfo [Int]
oddError x = do
  when (x `mod` 2 == 1) $ compileErrorM $ show x ++ " is odd"
  return [x]

checkMatch :: (Eq b, Show b) => b -> (a -> b) -> a -> IO (CompileInfo ())
checkMatch x f y = let y' = f y in
  return $ if x /= y'
              then compileErrorM $ "Expected " ++ show x ++ " but got " ++ show y'
              else return ()

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
