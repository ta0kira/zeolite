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

module Cli.CompileOptions (
  CompileOptions(..),
  CompileMode(..),
  HelpMode(..),
  emptyCompileOptions,
  isCompileBinary,
  maybeDisableHelp,
) where


data CompileOptions =
  CompileOptions {
    coHelp :: HelpMode,
    coIncludes :: [String],
    coSources :: [String],
    coExtraFiles :: [String],
    coSourcePrefix :: String,
    coMode :: CompileMode
  } deriving (Show)

emptyCompileOptions :: CompileOptions
emptyCompileOptions =
  CompileOptions {
    coHelp = HelpUnspecified,
    coIncludes = [],
    coSources = [],
    coExtraFiles = [],
    coSourcePrefix = "",
    coMode = CompileUnspecified
  }

data HelpMode = HelpNeeded | HelpNotNeeded | HelpUnspecified deriving (Eq,Show)

data CompileMode =
  CompileBinary {
    cbCategory :: String,
    cbFile :: String
  } | CompileIncremental | CompileUnspecified
  deriving (Eq,Show)

isCompileBinary (CompileBinary _ _) = True
isCompileBinary _                   = False

maybeDisableHelp HelpUnspecified = HelpNotNeeded
maybeDisableHelp h               = h
