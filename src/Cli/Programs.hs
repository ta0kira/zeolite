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

module Cli.Programs (
  CompilerBackend(..),
  CxxCommand(..),
  TestCommand(..),
  TestCommandResult(..),
  VersionHash(..),
) where

import Control.Monad.IO.Class

import Base.CompilerError


class CompilerBackend b where
  runCxxCommand   :: (MonadIO m, ErrorContextM m) => b -> CxxCommand -> m FilePath
  runTestCommand  :: (MonadIO m, ErrorContextM m) => b -> TestCommand -> m TestCommandResult
  getCompilerHash :: b -> VersionHash

newtype VersionHash = VersionHash String deriving (Eq)

instance Show VersionHash where
  show (VersionHash h) = h

data CxxCommand =
  CompileToObject {
    ctoSource :: FilePath,
    ctoPath :: FilePath,
    ctoMacros :: [(String,Maybe String)],
    ctoPaths :: [FilePath],
    ctoExtra :: Bool
  } |
  CompileToShared {
    ctsSources :: [FilePath],
    ctsOutput :: FilePath,
    ctsLinkFlags :: [String]
  } |
  CompileToBinary {
    ctbMain :: FilePath,
    ctbSources :: [FilePath],
    ctbMacros :: [(String,Maybe String)],
    ctbOutput :: FilePath,
    ctbPaths :: [FilePath],
    ctbLinkFlags :: [String]
  }
  deriving (Show)

data TestCommand =
  TestCommand {
    tcBinary :: FilePath,
    tcPath :: FilePath,
    tcArgs :: [String]
  }
  deriving (Show)

data TestCommandResult =
  TestCommandResult {
    tcrSuccess :: Bool,
    tcrOutput :: [String],
    tcrError :: [String]
  }
  deriving (Show)
