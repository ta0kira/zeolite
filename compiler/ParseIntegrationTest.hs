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

module ParseIntegrationTest (
) where

import Text.Parsec
import Text.Parsec.String

import IntegrationTest
import ParseCategory
import ParseDefinition
import ParseProcedure
import ParserBase


instance ParseFromSource (IntegrationTestHeader SourcePos) where
  sourceParser = labeled "testcase" $ do
    c <- getPosition
    sepAfter kwTestcase
    string "\""
    name <- fmap concat $ manyTill stringChar (string "\"")
    optionalSpace
    sepAfter $ string "{"
    result <- resultError <|> resultCrash <|> resultSuccess
    sepAfter $ string "}"
    return $ IntegrationTestHeader [c] name result where
      resultError = labeled "error expectation" $ do
        c <- getPosition
        sepAfter (keyword "error")
        (req,exc) <- requireOrExclude
        return $ ExpectCompileError [c] req exc
      resultCrash = labeled "crash expectation" $ do
        c <- getPosition
        sepAfter (keyword "crash")
        e <- labeled "test expression" sourceParser
        (req,exc) <- requireOrExclude
        return $ ExpectRuntimeError [c] e req exc
      resultSuccess = labeled "success expectation" $ do
        c <- getPosition
        sepAfter (keyword "success")
        e <- labeled "test expression" sourceParser
        (req,exc) <- requireOrExclude
        return $ ExpectRuntimeSuccess [c] e req exc
      requireOrExclude = parsed >>= return . foldr merge empty where
        empty = ([],[])
        merge (cs1,ds1) (cs2,ds2) = (cs1++cs2,ds1++ds2)
        parsed = sepBy anyType optionalSpace
        anyType = require <|> exclude where
          require = do
            sepAfter (keyword "require")
            string "\""
            r <- fmap concat $ manyTill stringChar (string "\"")
            optionalSpace
            return ([r],[])
          exclude = do
            sepAfter (keyword "exclude")
            string "\""
            e <- fmap concat $ manyTill stringChar (string "\"")
            optionalSpace
            return ([],[e])

instance ParseFromSource (IntegrationTest SourcePos) where
  sourceParser = labeled "integration test" $ do
    h <- sourceParser
    (cs,ds) <- parseAnySource
    return $ IntegrationTest h cs ds
