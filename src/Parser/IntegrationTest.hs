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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Parser.IntegrationTest (
) where

import Text.Megaparsec

import Parser.Common
import Parser.DefinedCategory ()
import Parser.Procedure ()
import Parser.TypeCategory ()
import Types.IntegrationTest


instance ParseFromSource (IntegrationTestHeader SourcePos) where
  sourceParser = labeled "testcase" $ do
    c <- getSourcePos
    sepAfter kwTestcase
    string_ "\""
    name <- manyTill stringChar (string_ "\"")
    optionalSpace
    sepAfter (string_ "{")
    result <- resultCompiles <|> resultError <|> resultCrash <|> resultSuccess
    args <- parseArgs <|> return []
    sepAfter (string_ "}")
    return $ IntegrationTestHeader [c] name args result where
      resultCompiles = labeled "compiles expectation" $ do
        c <- getSourcePos
        try $ sepAfter (keyword "compiles")
        (req,exc) <- requireOrExclude
        return $ ExpectCompiles [c] req exc
      resultError = labeled "error expectation" $ do
        c <- getSourcePos
        sepAfter (keyword "error")
        (req,exc) <- requireOrExclude
        return $ ExpectCompilerError [c] req exc
      resultCrash = labeled "crash expectation" $ do
        c <- getSourcePos
        try $ sepAfter (keyword "crash")
        (req,exc) <- requireOrExclude
        return $ ExpectRuntimeError [c] req exc
      resultSuccess = labeled "success expectation" $ do
        c <- getSourcePos
        sepAfter (keyword "success")
        (req,exc) <- requireOrExclude
        return $ ExpectRuntimeSuccess [c] req exc
      parseArgs = labeled "testcase args" $ do
        sepAfter (keyword "args")
        many (sepAfter quotedString)
      requireOrExclude = parseAny2 require exclude where
        require = do
          sepAfter (keyword "require")
          s <- outputScope
          string_ "\""
          r <- fmap concat $ manyTill regexChar (string_ "\"")
          optionalSpace
          return $ OutputPattern s r
        exclude = do
          sepAfter (keyword "exclude")
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
      anyScope      = sepAfter (keyword "any")      >> return OutputAny
      compilerScope = sepAfter (keyword "compiler") >> return OutputCompiler
      stderrScope   = sepAfter (keyword "stderr")   >> return OutputStderr
      stdoutScope   = sepAfter (keyword "stdout")   >> return OutputStdout

instance ParseFromSource (IntegrationTest SourcePos) where
  sourceParser = labeled "integration test" $ do
    h <- sourceParser
    (cs,ds,ts) <- parseAny3 sourceParser sourceParser sourceParser
    return $ IntegrationTest h cs ds ts
