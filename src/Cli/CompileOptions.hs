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
  ExtraSource(..),
  ExternalCategory(..),
  ForceMode(..),
  HelpMode(..),
  emptyCompileOptions,
  isCompileBinary,
  isCompileIncremental,
  isCompileRecompile,
  isCreateTemplates,
  isExecuteTests,
  maybeDisableHelp,
) where


data CompileOptions =
  CompileOptions {
    coHelp :: HelpMode,
    coPublicDeps :: [String],
    coPrivateDeps :: [String],
    coSources :: [String],
    coExtraFiles :: [ExtraSource],
    coExtraPaths :: [String],
    coExtraRequires :: [String],
    coSourcePrefix :: String,
    coExternalDefs :: [ExternalCategory],
    coMode :: CompileMode,
    coOutputName :: String,
    coForce :: ForceMode
  } deriving (Show)

emptyCompileOptions :: CompileOptions
emptyCompileOptions =
  CompileOptions {
    coHelp = HelpUnspecified,
    coPublicDeps = [],
    coPrivateDeps = [],
    coSources = [],
    coExtraFiles = [],
    coExtraPaths = [],
    coExtraRequires = [],
    coSourcePrefix = "",
    coExternalDefs = [],
    coMode = CompileUnspecified,
    coOutputName = "",
    coForce = DoNotForce
  }

data ExternalCategory =
  ExternalCategory {
    ecName :: String,
    ecSource :: String
  }
  deriving (Eq,Show)

data ExtraSource =
  ExtraSource {
    esSource :: String,
    esDepCategories :: [String]
  }
  deriving (Eq,Show)

data HelpMode = HelpNeeded | HelpNotNeeded | HelpUnspecified deriving (Eq,Show)

data ForceMode = DoNotForce | AllowRecompile | ForceRecompile | ForceAll deriving (Eq,Ord,Show)

data CompileMode =
  CompileBinary {
    cbCategory :: String,
    cbFunction :: String
  } |
  ExecuteTests {
    etInclude :: [String]
  } |
  CompileIncremental |
  CompileRecompile |
  CreateTemplates |
  CompileUnspecified
  deriving (Eq,Show)

isCompileBinary :: CompileMode -> Bool
isCompileBinary (CompileBinary _ _) = True
isCompileBinary _                   = False

isCompileIncremental :: CompileMode -> Bool
isCompileIncremental CompileIncremental = True
isCompileIncremental _                  = False

isCompileRecompile :: CompileMode -> Bool
isCompileRecompile CompileRecompile = True
isCompileRecompile _                = False

isExecuteTests :: CompileMode -> Bool
isExecuteTests (ExecuteTests _) = True
isExecuteTests _                = False

isCreateTemplates :: CompileMode -> Bool
isCreateTemplates CreateTemplates = True
isCreateTemplates _               = False

maybeDisableHelp :: HelpMode -> HelpMode
maybeDisableHelp HelpUnspecified = HelpNotNeeded
maybeDisableHelp h               = h
