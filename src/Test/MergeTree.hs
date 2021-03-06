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

module Test.MergeTree (tests) where

import Control.Monad (when)
import Data.Char (toUpper)

import Base.CompilerError
import Base.TrackedErrors
import Base.MergeTree
import Base.Mergeable


tests :: [IO (TrackedErrors ())]
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

   -- a*(b&c)*(d|e) = (a*b&a*c)*(d|e) = (a*b*(d|e)&a*c*(d|e)) = (a*b*d|a*b*e)&(a*c*d|a*c*e)
   checkMatch (mergeAll [mergeAny [mergeLeaf "abd",mergeLeaf "abe"],mergeAny [mergeLeaf "acd",mergeLeaf "ace"]]) sequence
              [mergeLeaf 'a',mergeAll [mergeLeaf 'b',mergeLeaf 'c'],mergeAny [mergeLeaf 'd',mergeLeaf 'e']],

   checkMatch (mergeAll [mergeAll [mergeLeaf 'a',mergeLeaf 'A'],
                         mergeAny [mergeAll [mergeLeaf 'b',mergeLeaf 'B'],
                                   mergeAll [mergeLeaf 'c',mergeLeaf 'C']]])
              (\x -> do
                x' <- x
                mergeAll [return x',return (toUpper x')])
              (mergeAll [mergeLeaf 'a',mergeAny [mergeLeaf 'b',mergeLeaf 'c']]),

    checkMatch [mergeAll [mergeAny [mergeLeaf 'a',mergeLeaf 'b'],
                          mergeAny [mergeLeaf 'c',
                                    mergeAll [mergeLeaf 'd',mergeLeaf 'e']],
                          mergeLeaf 'f']]
               sequence  -- MergeTree [Char] -> [MergeTree Char]
               (mergeAll [mergeAny [mergeLeaf "a",mergeLeaf "b"],
                          mergeAny [mergeLeaf "c",
                                    mergeAll [mergeLeaf "d",mergeLeaf "e"]],
                          mergeLeaf "f"]),

    checkMatch (mergeAll [mergeAny [mergeLeaf "a",mergeLeaf "b"],
                          mergeAny [mergeLeaf "c",
                                    mergeAll [mergeLeaf "d",mergeLeaf "e"]],
                          mergeLeaf "f"])
               sequence  -- [MergeTree Char] -> MergeTree [Char]
               [mergeAll [mergeAny [mergeLeaf 'a',mergeLeaf 'b'],
                          mergeAny [mergeLeaf 'c',
                                    mergeAll [mergeLeaf 'd',mergeLeaf 'e']],
                          mergeLeaf 'f']],

    checkMatch (mergeAll [mergeAny [mergeLeaf 'A',mergeLeaf 'B'],
                          mergeAny [mergeLeaf 'C',
                                    mergeAll [mergeLeaf 'D',mergeLeaf 'E']],
                          mergeLeaf 'F'])
               (toUpper <$>)
               (mergeAll [mergeAny [mergeLeaf 'a',mergeLeaf 'b'],
                          mergeAny [mergeLeaf 'c',
                                    mergeAll [mergeLeaf 'd',mergeLeaf 'e']],
                          mergeLeaf 'f']),

    checkMatch (mergeAll [mergeAny [mergeLeaf 'A',mergeLeaf 'B'],
                          mergeAny [mergeLeaf 'C',
                                    mergeAll [mergeLeaf 'D',mergeLeaf 'E']],
                          mergeLeaf 'F'])
               ((mergeAll [mergeAny [mergeLeaf ($'a'),mergeLeaf ($'b')],
                           mergeAny [mergeLeaf ($'c'),
                                     mergeAll [mergeLeaf ($'d'),mergeLeaf ($'e')]],
                           mergeLeaf ($'f')]) <*>)
               (mergeLeaf toUpper),

    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (minBound :: MergeTree (),minBound :: MergeTree ()),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (maxBound :: MergeTree (),maxBound :: MergeTree ()),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (minBound :: MergeTree (),maxBound :: MergeTree ()),
    checkMatch False
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (maxBound :: MergeTree (),minBound :: MergeTree ()),

    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAll [mergeLeaf 'a',mergeLeaf 'b'],
                mergeAny [mergeLeaf 'a',mergeLeaf 'b',mergeLeaf 'c']),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAll [mergeLeaf 'a',mergeLeaf 'b',mergeLeaf 'c'],
                mergeAny [mergeLeaf 'a',mergeLeaf 'b']),
    checkMatch False
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAny [mergeLeaf 'a',mergeLeaf 'b'],
                mergeAll [mergeLeaf 'a',mergeLeaf 'b',mergeLeaf 'c']),
    checkMatch False
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAny [mergeLeaf 'a',mergeLeaf 'b',mergeLeaf 'c'],
                mergeAll [mergeLeaf 'a',mergeLeaf 'b']),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAny [mergeLeaf 'a',mergeLeaf 'b'],
                mergeAny [mergeLeaf 'a',mergeLeaf 'b']),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAll [mergeLeaf 'a',mergeLeaf 'b'],
                mergeAll [mergeLeaf 'a',mergeLeaf 'b']),

    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAll [mergeLeaf 'a',mergeLeaf 'b'],
                mergeAny [mergeAll [mergeLeaf 'a',mergeLeaf 'b'],mergeLeaf 'c']),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAll [mergeAny [mergeLeaf 'a',mergeLeaf 'b'],mergeLeaf 'c'],
                mergeAny [mergeLeaf 'a',mergeLeaf 'b']),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAny [mergeAll [mergeLeaf 'a',mergeLeaf 'b'],mergeLeaf 'c'],
                mergeAny [mergeAll [mergeLeaf 'a',mergeLeaf 'b'],mergeLeaf 'c']),
    checkMatch True
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAll [mergeAny [mergeLeaf 'a',mergeLeaf 'b'],mergeLeaf 'c'],
                mergeAll [mergeAny [mergeLeaf 'a',mergeLeaf 'b'],mergeLeaf 'c']),
    checkMatch False
               (uncurry $ pairMergeTree mergeAny mergeAll (==))
               (mergeAny [mergeLeaf 'a',mergeLeaf 'b'],
                mergeAll [mergeLeaf 'a',mergeLeaf 'b'])
 ]

oddError :: Int -> TrackedErrors Int
oddError x = do
  when (odd x) $ compilerErrorM $ show x ++ " is odd"
  return x

oddError2 :: Int -> TrackedErrors [Int]
oddError2 = fmap (:[]) . oddError

checkMatch :: (Eq b, Show b) => b -> (a -> b) -> a -> IO (TrackedErrors ())
checkMatch x f y = let y' = f y in
  return $ if x /= y'
              then compilerErrorM $ "Expected " ++ show x ++ " but got " ++ show y'
              else return ()

checkMatch2 :: (Eq a, Show a) => a -> a -> a -> IO (TrackedErrors ())
checkMatch2 x y z = return $ do
  when (x /= z) $ compilerErrorM $ "Expected " ++ show x ++ " but got " ++ show z
  when (y == z) $ compilerErrorM $ "Expected something besides " ++ show y

checkSuccess :: (Eq b, Show b) => b -> (a -> TrackedErrors b) -> a -> IO (TrackedErrors ())
checkSuccess x f y = let y' = f y in
  return $ if isCompilerError y' || getCompilerSuccess y' == x
              then y' >> return ()
              else compilerErrorM $ "Expected value " ++ show x ++ " but got value " ++ show (getCompilerSuccess y')

checkError :: Show b => String -> (a -> TrackedErrors b) -> a -> IO (TrackedErrors ())
checkError e f y = let y' = f y in
  return $ if not (isCompilerError y')
              then compilerErrorM $ "Expected error \"" ++ e ++ "\" but got value " ++ show (getCompilerSuccess y')
              else if show (getCompilerError y') == e
                      then return ()
                      else compilerErrorM $ "Expected error \"" ++ e ++ "\" but got error \"" ++ show (getCompilerError y') ++ "\""
