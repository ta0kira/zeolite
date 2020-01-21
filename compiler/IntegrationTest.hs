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

module IntegrationTest (
  ExpectedResult(..),
  IntegrationTest(..),
  IntegrationTestHeader(..),
  OutputPattern(..),
  OutputScope(..),
  getExcludePattern,
  getRequirePattern,
  isExpectCompileError,
  isExpectRuntimeError,
  isExpectRuntimeSuccess,
) where

import TypeCategory
import DefinedCategory
import Procedure (Expression)


data IntegrationTestHeader c =
  IntegrationTestHeader {
    ithContext :: [c],
    ithTestName :: String,
    ithResult :: ExpectedResult c
  }

data IntegrationTest c =
  IntegrationTest {
    itHeader :: IntegrationTestHeader c,
    itCategory :: [AnyCategory c],
    itDefinition :: [DefinedCategory c]
  }

data ExpectedResult c =
  ExpectCompileError {
    eceContext :: [c],
    eceRequirePattern :: [OutputPattern],
    eceExcludePattern :: [OutputPattern]
  } |
  ExpectRuntimeError {
    ereContext :: [c],
    ereExpression :: Expression c,
    ereRequirePattern :: [OutputPattern],
    ereExcludePattern :: [OutputPattern]
  } |
  ExpectRuntimeSuccess {
    ersContext :: [c],
    ersExpression :: Expression c,
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

isExpectCompileError (ExpectCompileError _ _ _) = True
isExpectCompileError _                          = False

isExpectRuntimeError (ExpectRuntimeError _ _ _ _) = True
isExpectRuntimeError _                            = False

isExpectRuntimeSuccess (ExpectRuntimeSuccess _ _ _ _) = True
isExpectRuntimeSuccess _                              = False

getRequirePattern (ExpectCompileError _ rs _)     = rs
getRequirePattern (ExpectRuntimeError _ _ rs _)   = rs
getRequirePattern (ExpectRuntimeSuccess _ _ rs _) = rs

getExcludePattern (ExpectCompileError _ _ es)     = es
getExcludePattern (ExpectRuntimeError _ _ _ es)   = es
getExcludePattern (ExpectRuntimeSuccess _ _ _ es) = es
