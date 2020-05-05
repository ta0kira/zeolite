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
  ForceMode(..),
  HelpMode(..),
  emptyCompileOptions,
  getLinkFlags,
  getSourceCategories,
  getSourceDeps,
  getSourceFile,
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
    coSourcePrefix :: String,
    coMode :: CompileMode,
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
    coSourcePrefix = "",
    coMode = CompileUnspecified,
    coForce = DoNotForce
  }

data ExtraSource =
  CategorySource {
    csSource :: String,
    csCategories :: [String],
    csDepCategories :: [String]
  } |
  OtherSource {
    osSource :: String
  }
  deriving (Eq,Show)

getSourceFile :: ExtraSource -> String
getSourceFile (CategorySource s _ _) = s
getSourceFile (OtherSource s)        = s

getSourceCategories :: ExtraSource -> [String]
getSourceCategories (CategorySource _ cs _) = cs
getSourceCategories (OtherSource _)         = []

getSourceDeps :: ExtraSource -> [String]
getSourceDeps (CategorySource _ _ ds) = ds
getSourceDeps (OtherSource _)         = []

data HelpMode = HelpNeeded | HelpNotNeeded | HelpUnspecified deriving (Eq,Show)

data ForceMode = DoNotForce | AllowRecompile | ForceRecompile | ForceAll deriving (Eq,Ord,Show)

data CompileMode =
  CompileBinary {
    cbCategory :: String,
    cbFunction :: String,
    cbOutputName :: String,
    cbLinkFlags :: [String]
  } |
  ExecuteTests {
    etInclude :: [String]
  } |
  CompileIncremental {
    ciLinkFlags :: [String]
  } |
  CompileRecompile |
  CompileRecompileRecursive |
  CreateTemplates |
  CompileUnspecified
  deriving (Eq,Show)

isCompileBinary :: CompileMode -> Bool
isCompileBinary (CompileBinary _ _ _ _) = True
isCompileBinary _                       = False

isCompileIncremental :: CompileMode -> Bool
isCompileIncremental (CompileIncremental _) = True
isCompileIncremental _                      = False

isCompileRecompile :: CompileMode -> Bool
isCompileRecompile CompileRecompile          = True
isCompileRecompile CompileRecompileRecursive = True
isCompileRecompile _                         = False

isExecuteTests :: CompileMode -> Bool
isExecuteTests (ExecuteTests _) = True
isExecuteTests _                = False

isCreateTemplates :: CompileMode -> Bool
isCreateTemplates CreateTemplates = True
isCreateTemplates _               = False

maybeDisableHelp :: HelpMode -> HelpMode
maybeDisableHelp HelpUnspecified = HelpNotNeeded
maybeDisableHelp h               = h

getLinkFlags :: CompileMode -> [String]
getLinkFlags (CompileBinary _ _ _ lf) = lf
getLinkFlags (CompileIncremental lf)  = lf
getLinkFlags _                        = []
