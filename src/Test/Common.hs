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
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Base.CompileInfo
import Parser.Common
import Parser.TypeInstance ()
import Types.TypeInstance


runAllTests :: [IO (CompileInfo ())] -> IO ()
runAllTests ts = do
  results <- sequence ts
  let (es,ps) = partitionEithers $ zipWith numberError ([1..] :: [Int]) results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e ++ "\n")) es
  hPutStr stderr $ show (length ps) ++ " tests passed + " ++
                   show (length es) ++ " tests failed\n"
  when (not $ null es) exitFailure

numberError :: a -> CompileInfo b -> Either (a,CompileMessage) b
numberError n c
  | isCompileError c = Left (n,getCompileError c)
  | otherwise        = Right (getCompileSuccess c)

forceParse :: ParseFromSource a => String -> a
forceParse s = force $ parse sourceParser "(string)" s where
  force (Right x) = x
  force _         = undefined

readSingle :: ParseFromSource a => String -> String -> CompileInfo a
readSingle = readSingleWith (optionalSpace >> sourceParser)

readSingleWith :: Parser a -> String -> String -> CompileInfo a
readSingleWith p f s =
  unwrap $ parse (between nullParse endOfDoc p) f s
  where
    unwrap (Left e)  = compileErrorM (show e)
    unwrap (Right t) = return t

readMulti :: ParseFromSource a => String -> String -> CompileInfo [a]
readMulti f s =
  unwrap $ parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  where
    unwrap (Left e)  = compileErrorM (show e)
    unwrap (Right t) = return t

parseFilterMap :: [(String,[String])] -> CompileInfo ParamFilters
parseFilterMap pa = do
  pa2 <- mapErrorsM parseFilters pa
  return $ Map.fromList pa2
  where
    parseFilters (n,fs) = do
      fs2 <- mapErrorsM (readSingle "(string)") fs
      return (ParamName n,fs2)

parseTheTest :: ParseFromSource a => [(String,[String])] -> [String] -> CompileInfo ([a],ParamFilters)
parseTheTest pa xs = do
  ts <- mapErrorsM (readSingle "(string)") xs
  pa2 <- parseFilterMap pa
  return (ts,pa2)

showParams :: [(String,[String])] -> String
showParams pa = "[" ++ intercalate "," (concat $ map expand pa) ++ "]" where
  expand (n,ps) = map (\p -> n ++ " " ++ p) ps

checkTypeSuccess :: TypeResolver r => r -> [(String,[String])] -> String -> CompileInfo ()
checkTypeSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check x2 = x2 <?? (prefix ++ ":")

checkTypeFail :: TypeResolver r => r -> [(String,[String])] -> String -> CompileInfo ()
checkTypeFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check :: CompileInfo a -> CompileInfo ()
    check c
      | isCompileError c = return ()
      | otherwise = compileErrorM $ prefix ++ ": Expected failure\n"

checkDefinesSuccess :: TypeResolver r => r -> [(String,[String])] -> String -> CompileInfo ()
checkDefinesSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check x2 = x2 <?? (prefix ++ ":")

checkDefinesFail :: TypeResolver r => r -> [(String,[String])] -> String -> CompileInfo ()
checkDefinesFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check :: CompileInfo a -> CompileInfo ()
    check c
      | isCompileError c = return ()
      | otherwise = compileErrorM $ prefix ++ ": Expected failure\n"

containsExactly :: (Ord a, Show a) => [a] -> [a] -> CompileInfo ()
containsExactly actual expected = do
  containsNoDuplicates actual
  containsAtLeast actual expected
  containsAtMost actual expected

containsNoDuplicates :: (Ord a, Show a) => [a] -> CompileInfo ()
containsNoDuplicates expected =
  (mergeAllM $ map checkSingle $ group $ sort expected) <?? (show expected)
  where
    checkSingle xa@(x:_:_) =
      compileErrorM $ "Item " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkSingle _ = return ()

containsAtLeast :: (Ord a, Show a) => [a] -> [a] -> CompileInfo ()
containsAtLeast actual expected =
  (mergeAllM $ map (checkInActual $ Set.fromList actual) expected) <??
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInActual va v =
      if v `Set.member` va
         then return ()
         else compileErrorM $ "Item " ++ show v ++ " was expected but not present"

containsAtMost :: (Ord a, Show a) => [a] -> [a] -> CompileInfo ()
containsAtMost actual expected =
  (mergeAllM $ map (checkInExpected $ Set.fromList expected) actual) <??
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInExpected va v =
      if v `Set.member` va
         then return ()
         else compileErrorM $ "Item " ++ show v ++ " is unexpected"

checkEquals :: (Eq a, Show a) => a -> a -> CompileInfo ()
checkEquals actual expected
  | actual == expected = return ()
  | otherwise = compileErrorM $ "Expected " ++ show expected ++ " but got " ++ show actual

loadFile :: String -> IO String
loadFile f = readFile ("src" </> "Test" </> f)
