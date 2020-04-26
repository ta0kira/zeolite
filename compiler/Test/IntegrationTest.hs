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
import Text.Parsec

import Base.CompileInfo
import Base.TypesBase
import Parser.Base
import Parser.IntegrationTest
import Test.Base
import Types.DefinedCategory
import Types.IntegrationTest
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


tests :: [IO (CompileInfo ())]
tests = [
    checkFileContents
      "testfiles/basic_error_test.0rt"
      (\t -> return $ do
        let h = itHeader t
        when (not $ isExpectCompileError $ ithResult h) $ compileError "Expected ExpectCompileError"
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
      "testfiles/basic_crash_test.0rt"
      (\t -> return $ do
        let h = itHeader t
        when (not $ isExpectRuntimeError $ ithResult h) $ compileError "Expected ExpectRuntimeError"
        checkEquals (ithTestName h) "basic crash test"
        let match = case ereExpression $ ithResult h of
                         (Expression _
                           (TypeCall _
                             (JustTypeInstance (TypeInstance (CategoryName "Test") (ParamSet [])))
                             (FunctionCall _ (FunctionName "execute") (ParamSet []) (ParamSet []))) []) -> True
                         _ -> False
        when (not match) $ compileError "Expected test expression \"Test$execute()\""
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
      "testfiles/basic_success_test.0rt"
      (\t -> return $ do
        let h = itHeader t
        when (not $ isExpectRuntimeSuccess $ ithResult h) $ compileError "Expected ExpectRuntimeSuccess"
        checkEquals (ithTestName h) "basic success test"
        let match = case ersExpression $ ithResult h of
                         (Expression _
                           (TypeCall _
                             (JustTypeInstance (TypeInstance (CategoryName "Test") (ParamSet [])))
                             (FunctionCall _ (FunctionName "execute") (ParamSet []) (ParamSet []))) []) -> True
                         _ -> False
        when (not match) $ compileError "Expected test expression \"Test$execute()\""
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
  String -> (IntegrationTest SourcePos -> IO (CompileInfo ())) -> IO (CompileInfo ())
checkFileContents f o = do
  s <- readFile f
  unwrap $ parse (between optionalSpace endOfDoc sourceParser) f s
  where
    unwrap (Left e)  = return $ compileError (show e)
    unwrap (Right t) = fmap (flip reviseError ("Check " ++ f ++ ":")) $ o t

extractCategoryNames :: IntegrationTest c -> [String]
extractCategoryNames = map (show . getCategoryName) . itCategory

extractDefinitionNames :: IntegrationTest c -> [String]
extractDefinitionNames = map (show . dcName) . itDefinition
