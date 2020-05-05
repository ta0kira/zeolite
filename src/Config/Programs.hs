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

module Config.Programs (
  CompilerBackend(..),
  CxxCommand(..),
  TestCommand(..),
  TestCommandResult(..),
) where


class CompilerBackend b where
  runCxxCommand :: b -> CxxCommand -> IO String
  runTestCommand :: b -> TestCommand -> IO TestCommandResult
  getCompilerHash :: b -> String

data CxxCommand =
  CompileToObject {
    ctoSource :: String,
    ctoPath :: String,
    ctoNamespaceMacro :: String,
    ctoNamespace :: String,
    ctoPaths :: [String],
    ctoExtra :: Bool
  } |
  CompileToBinary {
    ctbMain :: String,
    ctbSources :: [String],
    ctbOutput :: String,
    ctbPaths :: [String],
    ctbLinkFlags :: [String]
  }
  deriving (Show)

data TestCommand =
  TestCommand {
    tcBinary :: String,
    tcPath :: String
  }
  deriving (Show)

data TestCommandResult =
  TestCommandResult {
    tcrSuccess :: Bool,
    tcrOutput :: [String],
    tcrError :: [String]
  }
  deriving (Show)
