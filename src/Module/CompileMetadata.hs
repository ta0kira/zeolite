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
  CompileMetadata(..),
  ModuleConfig(..),
  ObjectFile(..),
  isCategoryObjectFile,
  mergeObjectFiles,
) where

import Data.List (nub)
import qualified Data.Map as Map

import Cli.CompileOptions
import Cli.Programs (VersionHash)
import Parser.TextParser (SourceContext)
import Types.Procedure (Expression,MacroName)
import Types.TypeCategory (Namespace)
import Types.TypeInstance (CategoryName)


data CompileMetadata =
  CompileMetadata {
    cmVersionHash :: VersionHash,
    cmPath :: FilePath,
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
  CategoryObjectFile c (uniqueCategoryIdentifiers $ rs1 ++ rs2) (nub $ fs1 ++ fs2)
mergeObjectFiles o _ = o

uniqueCategoryIdentifiers :: [CategoryIdentifier] -> [CategoryIdentifier]
uniqueCategoryIdentifiers = concat . Map.elems . Map.fromListWith tryMerge . map (\i -> (getIdentifierCategory i,[i])) where
  tryMerge [(UnresolvedCategory _)] is = is
  tryMerge is [(UnresolvedCategory _)] = is
  tryMerge is1 is2 = nub (is1 ++ is2)

isCategoryObjectFile :: ObjectFile -> Bool
isCategoryObjectFile (CategoryObjectFile _ _ _) = True
isCategoryObjectFile (OtherObjectFile _)        = False

data ModuleConfig =
  ModuleConfig {
    mcRoot :: FilePath,
    mcPath :: FilePath,
    mcExprMap :: [(MacroName,Expression SourceContext)],
    mcPublicDeps :: [FilePath],
    mcPrivateDeps :: [FilePath],
    mcExtraFiles :: [ExtraSource],
    mcExtraPaths :: [FilePath],
    mcMode :: CompileMode
  }
  deriving (Show)

instance Eq ModuleConfig where
  (ModuleConfig pA dA _ isA is2A esA epA mA) == (ModuleConfig pB dB _ isB is2B esB epB mB) =
    all id [
        pA == pB,
        dA == dB,
        isA == isB,
        is2A == is2B,
        esA == esB,
        epA == epB,
        mA == mB
      ]
