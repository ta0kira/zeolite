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
  loadFile,
  parseFilterMap,
  parseTheTest,
  readMulti,
  readSingle,
  readSingleWith,
  runAllTests,
  showParams,
) where

import Control.Monad (when)
import Data.Either
import Data.List
import System.Exit
import System.FilePath
import System.IO
import Text.Megaparsec
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.CompilerMessage
import Base.TrackedErrors
import Parser.Common
import Parser.TextParser
import Parser.TypeInstance ()
import Types.TypeInstance


runAllTests :: [IO (TrackedErrors ())] -> IO ()
runAllTests ts = do
  results <- sequence ts
  let (es,ps) = partitionEithers $ zipWith numberError ([1..] :: [Int]) results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e ++ "\n")) es
  hPutStr stderr $ show (length ps) ++ " tests passed + " ++
                   show (length es) ++ " tests failed\n"
  when (not $ null es) exitFailure

numberError :: a -> TrackedErrors b -> Either (a,CompilerMessage) b
numberError n c
  | isCompilerError c = Left (n,getCompilerError c)
  | otherwise        = Right (getCompilerSuccess c)

forceParse :: ParseFromSource a => String -> a
forceParse s = getCompilerSuccess $ runTextParser sourceParser "(string)" s

readSingle :: ParseFromSource a => String -> String -> TrackedErrors a
readSingle  = readSingleWith sourceParser

readSingleWith :: TextParser a -> String -> String -> TrackedErrors a
readSingleWith p = runTextParser (between nullParse endOfDoc p)

readMulti :: ParseFromSource a => String -> String -> TrackedErrors [a]
readMulti f s = runTextParser (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s

parseFilterMap :: [(String,[String])] -> TrackedErrors ParamFilters
parseFilterMap pa = do
  pa2 <- mapErrorsM parseFilters pa
  return $ Map.fromList pa2
  where
    parseFilters (n,fs) = do
      fs2 <- mapErrorsM (readSingle "(string)") fs
      return (ParamName n,fs2)

parseTheTest :: ParseFromSource a => [(String,[String])] -> [String] -> TrackedErrors ([a],ParamFilters)
parseTheTest pa xs = do
  ts <- mapErrorsM (readSingle "(string)") xs
  pa2 <- parseFilterMap pa
  return (ts,pa2)

showParams :: [(String,[String])] -> String
showParams pa = "[" ++ intercalate "," (concat $ map expand pa) ++ "]" where
  expand (n,ps) = map (\p -> n ++ " " ++ p) ps

checkTypeSuccess :: TypeResolver r => r -> [(String,[String])] -> String -> TrackedErrors ()
checkTypeSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check x2 = x2 <!! prefix ++ ":"

checkTypeFail :: TypeResolver r => r -> [(String,[String])] -> String -> TrackedErrors ()
checkTypeFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check :: TrackedErrors a -> TrackedErrors ()
    check c
      | isCompilerError c = return ()
      | otherwise = compilerErrorM $ prefix ++ ": Expected failure\n"

checkDefinesSuccess :: TypeResolver r => r -> [(String,[String])] -> String -> TrackedErrors ()
checkDefinesSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check x2 = x2 <!! prefix ++ ":"

checkDefinesFail :: TypeResolver r => r -> [(String,[String])] -> String -> TrackedErrors ()
checkDefinesFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check :: TrackedErrors a -> TrackedErrors ()
    check c
      | isCompilerError c = return ()
      | otherwise = compilerErrorM $ prefix ++ ": Expected failure\n"

containsExactly :: (Ord a, Show a) => [a] -> [a] -> TrackedErrors ()
containsExactly actual expected = do
  containsNoDuplicates actual
  containsAtLeast actual expected
  containsAtMost actual expected

containsNoDuplicates :: (Ord a, Show a) => [a] -> TrackedErrors ()
containsNoDuplicates expected =
  (mapErrorsM_ checkSingle $ group $ sort expected) <!! show expected
  where
    checkSingle xa@(x:_:_) =
      compilerErrorM $ "Item " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkSingle _ = return ()

containsAtLeast :: (Ord a, Show a) => [a] -> [a] -> TrackedErrors ()
containsAtLeast actual expected =
  (mapErrorsM_ (checkInActual $ Set.fromList actual) expected) <!!
        show actual ++ " (actual) vs. " ++ show expected ++ " (expected)"
  where
    checkInActual va v =
      if v `Set.member` va
         then return ()
         else compilerErrorM $ "Item " ++ show v ++ " was expected but not present"

containsAtMost :: (Ord a, Show a) => [a] -> [a] -> TrackedErrors ()
containsAtMost actual expected =
  (mapErrorsM_ (checkInExpected $ Set.fromList expected) actual) <!!
        show actual ++ " (actual) vs. " ++ show expected ++ " (expected)"
  where
    checkInExpected va v =
      if v `Set.member` va
         then return ()
         else compilerErrorM $ "Item " ++ show v ++ " is unexpected"

checkEquals :: (Eq a, Show a) => a -> a -> TrackedErrors ()
checkEquals actual expected
  | actual == expected = return ()
  | otherwise = compilerErrorM $ "Expected " ++ show expected ++ " but got " ++ show actual

loadFile :: String -> IO String
loadFile f = readFile ("src" </> "Test" </> f)
