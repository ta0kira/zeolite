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

module Cli.CompileMetadata (
  CategoryIdentifier(..),
  CompileMetadata(..),
  ModuleConfig(..),
  ObjectFile(..),
  isCategoryObjectFile,
  mergeObjectFiles,
) where

import Data.List (nub)

import Cli.CompileOptions


data CompileMetadata =
  CompileMetadata {
    cmVersionHash :: String,
    cmPath :: FilePath,
    cmNamespace :: String, -- TODO: Use Namespace here?
    cmPublicDeps :: [FilePath],
    cmPrivateDeps :: [FilePath],
    cmCategories :: [String],
    cmSubdirs :: [FilePath],
    cmPublicFiles :: [FilePath],
    cmPrivateFiles :: [FilePath],
    cmTestFiles :: [FilePath],
    cmHxxFiles :: [FilePath],
    cmCxxFiles :: [FilePath],
    cmLinkFlags :: [FilePath],
    cmObjectFiles :: [ObjectFile]
  }
  deriving (Eq,Show)

data ObjectFile =
  CategoryObjectFile {
    cofCategory :: CategoryIdentifier,
    cofRequires :: [CategoryIdentifier],
    cofFiles :: [FilePath]
  } |
  OtherObjectFile {
    oofFile :: FilePath
  }
  deriving (Eq,Show)

data CategoryIdentifier =
  CategoryIdentifier {
    ciPath :: String,
    ciCategory :: String,
    ciNamespace :: String
  } |
  UnresolvedCategory {
    ucCategory :: String
  }
  deriving (Eq,Ord,Show)

mergeObjectFiles :: ObjectFile -> ObjectFile -> ObjectFile
mergeObjectFiles (CategoryObjectFile c rs1 fs1) (CategoryObjectFile _ rs2 fs2) =
  CategoryObjectFile c (nub $ rs1 ++ rs2) (nub $ fs1 ++ fs2)
mergeObjectFiles o _ = o

isCategoryObjectFile :: ObjectFile -> Bool
isCategoryObjectFile (CategoryObjectFile _ _ _) = True
isCategoryObjectFile (OtherObjectFile _)        = False

data ModuleConfig =
  ModuleConfig {
    rmRoot :: FilePath,
    rmPath :: FilePath,
    rmPublicDeps :: [FilePath],
    rmPrivateDeps :: [FilePath],
    rmExtraFiles :: [ExtraSource],
    rmExtraPaths :: [FilePath],
    rmMode :: CompileMode
  }
  deriving (Eq,Show)
