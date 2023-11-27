{- -----------------------------------------------------------------------------
Copyright 2020,2023 Kevin P. Barry

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

{-# LANGUAGE FlexibleInstances #-}

module Parser.IntegrationTest (
) where

import Control.Applicative.Permutations

import Parser.Common
import Parser.DefinedCategory ()
import Parser.Procedure ()
import Parser.TextParser
import Parser.TypeCategory ()
import Parser.TypeInstance ()
import Types.IntegrationTest


instance ParseFromSource (IntegrationTestHeader SourceContext) where
  sourceParser = labeled "testcase" $ do
    c <- getSourceContext
    sepAfter kwTestcase
    string_ "\""
    name <- manyTill stringChar (string_ "\"")
    optionalSpace
    sepAfter (string_ "{")
    result <- resultCompiles <|> resultError <|> resultCrash <|> resultSuccess
    header <- runPermutation $ build c name result
      <$> toPermutationWithDefault [] parseArgs
      <*> toPermutationWithDefault Nothing parseTimeout
      <*> toPermutation requireOrExclude
    sepAfter (string_ "}")
    return header where
      build c name result args timeout (req,exc) =
        IntegrationTestHeader [c] name args timeout (result req exc)
      resultCompiles = labeled "compiles expectation" $ do
        c <- getSourceContext
        keyword "compiles"
        return $ ExpectCompiles [c]
      resultError = labeled "error expectation" $ do
        c <- getSourceContext
        keyword "error"
        return $ ExpectCompilerError [c]
      resultCrash = labeled "crash expectation" $ do
        c <- getSourceContext
        keyword "crash"
        t <- fmap Just parseTestcaseType <|> return Nothing
        return $ ExpectRuntimeError [c] t
      resultSuccess = labeled "success expectation" $ do
        c <- getSourceContext
        keyword "success"
        t <- fmap Just parseTestcaseType <|> return Nothing
        return $ ExpectRuntimeSuccess [c] t
      parseTestcaseType = do
        c <- getSourceContext
        t <- sourceParser
        return ([c],t)
      parseArgs = labeled "testcase args" $ do
        keyword "args"
        many (sepAfter quotedString)
      parseTimeout = labeled "testcase timeout" $ do
        keyword "timeout"
        fmap (Just . snd) (sepAfter parseDec)
      requireOrExclude = parseAny2 require exclude where
        require = do
          keyword "require"
          s <- outputScope
          string_ "\""
          r <- fmap concat $ manyTill regexChar (string_ "\"")
          optionalSpace
          return $ OutputPattern s r
        exclude = do
          keyword "exclude"
          s <- outputScope
          string_ "\""
          e <- fmap concat $ manyTill regexChar (string_ "\"")
          optionalSpace
          return $ OutputPattern s e
      outputScope = try anyScope <|>
                    try compilerScope <|>
                    try stderrScope <|>
                    try stdoutScope <|>
                    return OutputAny
      anyScope      = keyword "any"      >> return OutputAny
      compilerScope = keyword "compiler" >> return OutputCompiler
      stderrScope   = keyword "stderr"   >> return OutputStderr
      stdoutScope   = keyword "stdout"   >> return OutputStdout

instance ParseFromSource (IntegrationTest SourceContext) where
  sourceParser = labeled "integration test" $ do
    h <- sourceParser
    (cs,ds,ts) <- parseAny3 sourceParser sourceParser sourceParser
    return $ IntegrationTest h cs ds ts
