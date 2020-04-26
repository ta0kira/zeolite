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

module Test.Common (
  checkDefinesFail,
  checkDefinesSuccess,
  checkEquals,
  checkTypeFail,
  checkTypeSuccess,
  containsAtLeast,
  containsAtMost,
  containsExactly,
  containsNoDuplicates,
  forceParse,
  parseFilterMap,
  parseTheTest,
  readMulti,
  readSingle,
  readSingleWith,
  runAllTests,
  showParams,
) where

import Data.Either
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Compilation.CompileInfo
import Parser.Common
import Parser.TypeInstance
import Types.TypeInstance


runAllTests :: [IO (CompileInfo ())] -> IO ()
runAllTests ts = do
  results <- sequence ts
  let (es,ps) = partitionEithers $ zipWith numberError [1..] results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e ++ "\n")) es
  hPutStr stderr $ show (length ps) ++ " tests passed + " ++
                   show (length es) ++ " tests failed\n"

numberError :: a -> CompileInfo b -> Either (a,CompileMessage) b
numberError n c
  | isCompileError c = Left (n,getCompileError c)
  | otherwise        = Right (getCompileSuccess c)

forceParse :: ParseFromSource a => String -> a
forceParse s = force $ parse sourceParser "(string)" s where
  force (Right x) = x

readSingle :: (ParseFromSource a, CompileErrorM m) => String -> String -> m a
readSingle = readSingleWith (optionalSpace >> sourceParser)

readSingleWith :: CompileErrorM m => Parser a -> String -> String -> m a
readSingleWith p f s =
  unwrap $ parse (between nullParse endOfDoc p) f s
  where
    unwrap (Left e)  = compileError (show e)
    unwrap (Right t) = return t

readMulti :: CompileErrorM m => ParseFromSource a => String -> String -> m [a]
readMulti f s =
  unwrap $ parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  where
    unwrap (Left e)  = compileError (show e)
    unwrap (Right t) = return t

parseFilterMap :: CompileErrorM m => [(String,[String])] -> m ParamFilters
parseFilterMap pa = do
  pa2 <- collectAllOrErrorM $ map parseFilters pa
  return $ Map.fromList pa2
  where
    parseFilters (n,fs) = do
      fs2 <- collectAllOrErrorM $ map (readSingle "(string)") fs
      return (ParamName n,fs2)

parseTheTest :: (ParseFromSource a, CompileErrorM m) =>
  [(String,[String])] -> [String] -> m ([a],ParamFilters)
parseTheTest pa xs = do
  ts <- collectAllOrErrorM $ map (readSingle "(string)") xs
  pa2 <- parseFilterMap pa
  return (ts,pa2)

showParams :: [(String,[String])] -> String
showParams pa = "[" ++ intercalate "," (concat $ map expand pa) ++ "]" where
  expand (n,ps) = map (\p -> n ++ " " ++ p) ps

checkTypeSuccess :: (TypeResolver r) =>
  r -> [(String,[String])] -> String -> CompileInfo ()
checkTypeSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check = flip reviseError (prefix ++ ":")

checkTypeFail :: (TypeResolver r) =>
  r -> [(String,[String])] -> String -> CompileInfo ()
checkTypeFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check :: CompileInfo a -> CompileInfo ()
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ prefix ++ ": Expected failure\n"

checkDefinesSuccess :: (TypeResolver r) =>
  r -> [(String,[String])] -> String -> CompileInfo ()
checkDefinesSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check = flip reviseError (prefix ++ ":")

checkDefinesFail :: (TypeResolver r) =>
  r -> [(String,[String])] -> String -> CompileInfo ()
checkDefinesFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check :: CompileInfo a -> CompileInfo ()
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ prefix ++ ": Expected failure\n"

containsExactly :: (Ord a, Show a, MergeableM m, CompileErrorM m) =>
  [a] -> [a] -> m ()
containsExactly actual expected = do
  containsNoDuplicates actual
  containsAtLeast actual expected
  containsAtMost actual expected

containsNoDuplicates :: (Ord a, Show a, MergeableM m, CompileErrorM m) =>
  [a] -> m ()
containsNoDuplicates expected =
  (mergeAllM $ map checkSingle $ group $ sort expected) `reviseError` (show expected)
  where
    checkSingle xa@(x:_:_) =
      compileError $ "Item " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkSingle _ = return ()

containsAtLeast :: (Ord a, Show a, MergeableM m, CompileErrorM m) =>
  [a] -> [a] -> m ()
containsAtLeast actual expected =
  (mergeAllM $ map (checkInActual $ Set.fromList actual) expected) `reviseError`
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInActual va v =
      if v `Set.member` va
         then return ()
         else compileError $ "Item " ++ show v ++ " was expected but not present"

containsAtMost :: (Ord a, Show a, MergeableM m, CompileErrorM m) =>
  [a] -> [a] -> m ()
containsAtMost actual expected =
  (mergeAllM $ map (checkInExpected $ Set.fromList expected) actual) `reviseError`
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInExpected va v =
      if v `Set.member` va
         then return ()
         else compileError $ "Item " ++ show v ++ " is unexpected"

checkEquals :: (Eq a, Show a, MergeableM m, CompileErrorM m) =>
  a -> a -> m ()
checkEquals actual expected
  | actual == expected = return ()
  | otherwise = compileError $ "Expected " ++ show expected ++ " but got " ++ show actual
