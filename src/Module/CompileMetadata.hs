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

module Module.CompileMetadata (
  CategoryIdentifier(..),
  CategorySpec(..),
  CompileMetadata(..),
  ModuleConfig(..),
  ObjectFile(..),
  isCategoryObjectFile,
  getObjectFiles,
  mergeObjectFiles,
) where

import Data.List (nub)

import Cli.CompileOptions
import Cli.Programs (VersionHash)
import Parser.TextParser (SourceContext)
import Types.Procedure (Expression,MacroName)
import Types.TypeCategory
import Types.TypeInstance (CategoryName)


data CompileMetadata =
  CompileMetadata {
    cmVersionHash :: VersionHash,
    cmRoot :: FilePath,
    cmPath :: FilePath,
    cmExtra :: [FilePath],
    cmPublicNamespace :: Namespace,
    cmPrivateNamespace :: Namespace,
    cmPublicDeps :: [FilePath],
    cmPrivateDeps :: [FilePath],
    cmPublicCategories :: [CategoryName],
    cmPrivateCategories :: [CategoryName],
    cmPublicSubdirs :: [FilePath],
    cmPrivateSubdirs :: [FilePath],
    cmPublicFiles :: [FilePath],
    cmPrivateFiles :: [FilePath],
    cmTestFiles :: [FilePath],
    cmHxxFiles :: [FilePath],
    cmCxxFiles :: [FilePath],
    cmBinaries :: [FilePath],
    cmLibraries :: [FilePath],
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
    ciPath :: FilePath,
    ciCategory :: CategoryName,
    ciNamespace :: Namespace
  } |
  UnresolvedCategory {
    ucCategory :: CategoryName
  }
  deriving (Eq,Ord,Show)

getIdentifierCategory :: CategoryIdentifier -> CategoryName
getIdentifierCategory (CategoryIdentifier _ n _) = n
getIdentifierCategory (UnresolvedCategory n)     = n

mergeObjectFiles :: ObjectFile -> ObjectFile -> ObjectFile
mergeObjectFiles (CategoryObjectFile c rs1 fs1) (CategoryObjectFile _ rs2 fs2) =
  CategoryObjectFile c (nub $ rs1 ++ rs2) (nub $ fs1 ++ fs2)
mergeObjectFiles o _ = o

isCategoryObjectFile :: ObjectFile -> Bool
isCategoryObjectFile (CategoryObjectFile _ _ _) = True
isCategoryObjectFile (OtherObjectFile _)        = False

getObjectFiles :: ObjectFile -> [FilePath]
getObjectFiles (CategoryObjectFile _ _ os) = os
getObjectFiles (OtherObjectFile o)         = [o]

data CategorySpec c =
  CategorySpec {
    csContext :: [c],
    csRefines :: [ValueRefine c],
    csDefines :: [ValueDefine c]
  }
  deriving (Show)

data ModuleConfig =
  ModuleConfig {
    mcRoot :: FilePath,
    mcPath :: FilePath,
    mcExtra :: [FilePath],
    mcExprMap :: [(MacroName,Expression SourceContext)],
    mcPublicDeps :: [FilePath],
    mcPrivateDeps :: [FilePath],
    mcExtraFiles :: [ExtraSource],
    mcCategories :: [(CategoryName,CategorySpec SourceContext)],
    mcExtraPaths :: [FilePath],
    mcMode :: CompileMode
  }
  deriving (Show)

instance Eq ModuleConfig where
  (ModuleConfig pA dA eeA _ isA is2A esA cA epA mA) == (ModuleConfig pB dB eeB _ isB is2B esB cB epB mB) =
    all id [
        pA == pB,
        eeA == eeB,
        dA == dB,
        isA == isB,
        is2A == is2B,
        esA == esB,
        map fst cA == map fst cB,
        map (map vrType . csRefines . snd) cA == map (map vrType . csRefines . snd) cB,
        map (map vdType . csDefines . snd) cA == map (map vdType . csDefines . snd) cB,
        epA == epB,
        mA == mB
      ]
