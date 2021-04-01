{- -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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
  isCompileFast,
  isCompileIncremental,
  isCompileRecompile,
  isCompileUnspecified,
  isCreateTemplates,
  isExecuteTests,
  maybeDisableHelp,
) where

import Types.TypeCategory (FunctionName)
import Types.TypeInstance (CategoryName)


data CompileOptions =
  CompileOptions {
    coHelp :: HelpMode,
    coPublicDeps :: [FilePath],
    coPrivateDeps :: [FilePath],
    coPaths :: [FilePath],
    coExtraFiles :: [ExtraSource],
    coExtraPaths :: [FilePath],
    coSourcePrefix :: FilePath,
    coMode :: CompileMode,
    coForce :: ForceMode
  }
  deriving (Show)

emptyCompileOptions :: CompileOptions
emptyCompileOptions =
  CompileOptions {
    coHelp = HelpUnspecified,
    coPublicDeps = [],
    coPrivateDeps = [],
    coPaths = [],
    coExtraFiles = [],
    coExtraPaths = [],
    coSourcePrefix = "",
    coMode = CompileUnspecified,
    coForce = DoNotForce
  }

data ExtraSource =
  CategorySource {
    csSource :: FilePath,
    csCategories :: [CategoryName],
    csRequires :: [CategoryName]
  } |
  OtherSource {
    osSource :: FilePath
  }
  deriving (Eq,Show)

getSourceFile :: ExtraSource -> String
getSourceFile (CategorySource s _ _) = s
getSourceFile (OtherSource s)        = s

getSourceCategories :: ExtraSource -> [CategoryName]
getSourceCategories (CategorySource _ cs _) = cs
getSourceCategories (OtherSource _)         = []

getSourceDeps :: ExtraSource -> [CategoryName]
getSourceDeps (CategorySource _ _ ds) = ds
getSourceDeps (OtherSource _)         = []

data HelpMode = HelpNeeded | HelpNotNeeded | HelpUnspecified deriving (Eq,Show)

data ForceMode = DoNotForce | ForceAll deriving (Eq,Ord,Show)

data CompileMode =
  CompileBinary {
    cbCategory :: CategoryName,
    cbFunction :: FunctionName,
    cbOutputName :: FilePath,
    cbLinkFlags :: [String]
  } |
  CompileFast {
    cfCategory :: CategoryName,
    cfFunction :: FunctionName,
    cfSource :: FilePath
  } |
  ExecuteTests {
    etInclude :: [FilePath],
    etCallLog :: Maybe FilePath
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

isCompileFast :: CompileMode -> Bool
isCompileFast (CompileFast _ _ _) = True
isCompileFast _                   = False

isCompileIncremental :: CompileMode -> Bool
isCompileIncremental (CompileIncremental _) = True
isCompileIncremental _                      = False

isCompileRecompile :: CompileMode -> Bool
isCompileRecompile CompileRecompile          = True
isCompileRecompile CompileRecompileRecursive = True
isCompileRecompile _                         = False

isExecuteTests :: CompileMode -> Bool
isExecuteTests (ExecuteTests _ _) = True
isExecuteTests _                  = False

isCreateTemplates :: CompileMode -> Bool
isCreateTemplates CreateTemplates = True
isCreateTemplates _               = False

isCompileUnspecified :: CompileMode -> Bool
isCompileUnspecified CompileUnspecified = True
isCompileUnspecified _                  = False

maybeDisableHelp :: HelpMode -> HelpMode
maybeDisableHelp HelpUnspecified = HelpNotNeeded
maybeDisableHelp h               = h

getLinkFlags :: CompileMode -> [String]
getLinkFlags (CompileBinary _ _ _ lf) = lf
getLinkFlags (CompileIncremental lf)  = lf
getLinkFlags _                        = []
