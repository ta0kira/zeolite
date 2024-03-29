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

{-# LANGUAGE Safe #-}

module Types.IntegrationTest (
  ExpectedResult(..),
  IntegrationTest(..),
  IntegrationTestHeader(..),
  OutputPattern(..),
  OutputScope(..),
  getExcludePattern,
  getRequirePattern,
  isExpectCompilerError,
  isExpectCompiles,
  isExpectRuntimeError,
  isExpectRuntimeSuccess,
) where

import Types.TypeCategory
import Types.TypeInstance
import Types.DefinedCategory
import Types.Procedure


data IntegrationTestHeader c =
  IntegrationTestHeader {
    ithContext :: [c],
    ithTestName :: String,
    ithArgs :: [String],
    ithTimeout :: Maybe Integer,
    ithResult :: ExpectedResult c
  }

data IntegrationTest c =
  IntegrationTest {
    itHeader :: IntegrationTestHeader c,
    itCategory :: [AnyCategory c],
    itDefinition :: [DefinedCategory c],
    itTests :: [TestProcedure c]
  }

data ExpectedResult c =
  ExpectCompilerError {
    eceContext :: [c],
    eceRequirePattern :: [OutputPattern],
    eceExcludePattern :: [OutputPattern]
  } |
  ExpectCompiles {
    eceContext :: [c],
    eceRequirePattern :: [OutputPattern],
    eceExcludePattern :: [OutputPattern]
  } |
  ExpectRuntimeError {
    ereContext :: [c],
    ereCategory :: Maybe ([c],TypeInstance),
    ereRequirePattern :: [OutputPattern],
    ereExcludePattern :: [OutputPattern]
  } |
  ExpectRuntimeSuccess {
    ersContext :: [c],
    ersCategory :: Maybe ([c],TypeInstance),
    ersRequirePattern :: [OutputPattern],
    ersExcludePattern :: [OutputPattern]
  }

data OutputPattern =
  OutputPattern {
    opScope :: OutputScope,
    opPattern :: String
  }
  deriving (Eq,Ord,Show)

data OutputScope = OutputAny | OutputCompiler | OutputStderr | OutputStdout deriving (Eq,Ord,Show)

isExpectCompiles :: ExpectedResult c -> Bool
isExpectCompiles (ExpectCompiles _ _ _) = True
isExpectCompiles _                      = False

isExpectCompilerError :: ExpectedResult c -> Bool
isExpectCompilerError (ExpectCompilerError _ _ _) = True
isExpectCompilerError _                          = False

isExpectRuntimeError :: ExpectedResult c -> Bool
isExpectRuntimeError (ExpectRuntimeError _ _ _ _) = True
isExpectRuntimeError _                            = False

isExpectRuntimeSuccess :: ExpectedResult c -> Bool
isExpectRuntimeSuccess (ExpectRuntimeSuccess _ _ _ _) = True
isExpectRuntimeSuccess _                              = False

getRequirePattern :: ExpectedResult c -> [OutputPattern]
getRequirePattern (ExpectCompiles _ rs _)         = rs
getRequirePattern (ExpectCompilerError _ rs _)     = rs
getRequirePattern (ExpectRuntimeError _ _ rs _)   = rs
getRequirePattern (ExpectRuntimeSuccess _ _ rs _) = rs

getExcludePattern :: ExpectedResult c -> [OutputPattern]
getExcludePattern (ExpectCompiles _ _ es)         = es
getExcludePattern (ExpectCompilerError _ _ es)    = es
getExcludePattern (ExpectRuntimeError _ _ _ es)   = es
getExcludePattern (ExpectRuntimeSuccess _ _ _ es) = es
