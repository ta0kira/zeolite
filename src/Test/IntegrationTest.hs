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

module Test.IntegrationTest (tests) where

import Control.Monad (when)
import System.FilePath
import Text.Parsec

import Base.CompileError
import Base.CompileInfo
import Parser.Common
import Parser.IntegrationTest ()
import Test.Common
import Types.DefinedCategory
import Types.IntegrationTest
import Types.TypeCategory


tests :: [IO (CompileInfo ())]
tests = [
    checkFileContents
      ("testfiles" </> "basic_compiles_test.0rt")
      (\t -> do
        let h = itHeader t
        when (not $ isExpectCompiles $ ithResult h) $ compileErrorM "Expected ExpectCompiles"
        checkEquals (ithTestName h) "basic compiles test"
        containsExactly (getRequirePattern $ ithResult h) [
            OutputPattern OutputCompiler "pattern in output 1",
            OutputPattern OutputAny      "pattern in output 2"
          ]
        containsExactly (getExcludePattern $ ithResult h) [
            OutputPattern OutputStderr "pattern not in output 1",
            OutputPattern OutputStdout "pattern not in output 2"
          ]
        containsExactly (extractCategoryNames t) ["Test"]
        containsExactly (extractDefinitionNames t) ["Test"]
        ),

    checkFileContents
      ("testfiles" </> "basic_error_test.0rt")
      (\t -> do
        let h = itHeader t
        when (not $ isExpectCompileError $ ithResult h) $ compileErrorM "Expected ExpectCompileError"
        checkEquals (ithTestName h) "basic error test"
        containsExactly (getRequirePattern $ ithResult h) [
            OutputPattern OutputCompiler "pattern in output 1",
            OutputPattern OutputAny      "pattern in output 2"
          ]
        containsExactly (getExcludePattern $ ithResult h) [
            OutputPattern OutputStderr "pattern not in output 1",
            OutputPattern OutputStdout "pattern not in output 2"
          ]
        containsExactly (extractCategoryNames t) ["Test"]
        containsExactly (extractDefinitionNames t) ["Test"]
        ),

    checkFileContents
      ("testfiles" </> "basic_crash_test.0rt")
      (\t -> do
        let h = itHeader t
        when (not $ isExpectRuntimeError $ ithResult h) $ compileErrorM "Expected ExpectRuntimeError"
        checkEquals (ithTestName h) "basic crash test"
        containsExactly (getRequirePattern $ ithResult h) [
            OutputPattern OutputAny "pattern in output 1",
            OutputPattern OutputAny "pattern in output 2"
          ]
        containsExactly (getExcludePattern $ ithResult h) [
            OutputPattern OutputAny "pattern not in output 1",
            OutputPattern OutputAny "pattern not in output 2"
          ]
        containsExactly (extractCategoryNames t) ["Test"]
        containsExactly (extractDefinitionNames t) ["Test"]
        ),

    checkFileContents
      ("testfiles" </> "basic_success_test.0rt")
      (\t -> do
        let h = itHeader t
        when (not $ isExpectRuntimeSuccess $ ithResult h) $ compileErrorM "Expected ExpectRuntimeSuccess"
        checkEquals (ithTestName h) "basic success test"
        containsExactly (getRequirePattern $ ithResult h) [
            OutputPattern OutputAny "pattern in output 1",
            OutputPattern OutputAny "pattern in output 2"
          ]
        containsExactly (getExcludePattern $ ithResult h) [
            OutputPattern OutputAny "pattern not in output 1",
            OutputPattern OutputAny "pattern not in output 2"
          ]
        containsExactly (extractCategoryNames t) ["Test"]
        containsExactly (extractDefinitionNames t) ["Test"]
        )
  ]

checkFileContents ::
  String -> (IntegrationTest SourcePos -> CompileInfo ()) -> IO (CompileInfo ())
checkFileContents f o = toCompileInfo $ do
  s <- errorFromIO $ loadFile f
  t <- runParserE (between optionalSpace endOfDoc sourceParser) f s
  fromCompileInfo $ ("Check " ++ f ++ ":") ??> o t

extractCategoryNames :: IntegrationTest c -> [String]
extractCategoryNames = map (show . getCategoryName) . itCategory

extractDefinitionNames :: IntegrationTest c -> [String]
extractDefinitionNames = map (show . dcName) . itDefinition
