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

module Test.ParseMetadata (tests) where

import Control.Monad (when)
import Text.Regex.TDFA -- Not safe!

import Base.CompileError
import Compilation.CompileInfo
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.ParseMetadata

tests :: [IO (CompileInfo ())]
tests = [
    checkWriteThenRead $ CompileMetadata {
      cmVersionHash = "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = "public_ABCDEF",
      cmPublicDeps = [
        "/home/project/public-dep1",
        "/home/project/public-dep2"
      ],
      cmPrivateDeps = [
        "/home/project/private-dep1",
        "/home/project/private-dep2"
      ],
      cmExtraRequires = [
        CategoryIdentifier {
          ciPath = "/home/project/private-dep1",
          ciCategory = "PrivateCategory",
          ciNamespace = "private_123456"
        },
        UnresolvedCategory {
          ucCategory = "UnresolvedCategory"
        }
      ],
      cmCategories = [
        "MyCategory",
        "MyOtherCategory"
      ],
      cmSubdirs = [
        "/home/project/special/subdir1",
        "/home/project/special/subdir2"
      ],
      cmPublicFiles = [
        "/home/project/special/category1.0rp",
        "/home/project/special/category2.0rp"
      ],
      cmPrivateFiles = [
        "/home/project/special/category1.0rx",
        "/home/project/special/category2.0rx"
      ],
      cmTestFiles = [
        "/home/project/special/category1.0rt",
        "/home/project/special/category2.0rt"
      ],
      cmHxxFiles = [
        "/home/project/special/category1.hpp",
        "/home/project/special/category2.hpp"
      ],
      cmCxxFiles = [
        "/home/project/special/category1.cpp",
        "/home/project/special/category2.cpp"
      ],
      cmObjectFiles = [
        CategoryObjectFile {
          cofCategory = CategoryIdentifier {
            ciPath = "/home/project/special",
            ciCategory = "SpecialCategory",
            ciNamespace = "public_ABCDEF"
          },
          cofRequires = [
            CategoryIdentifier {
              ciPath = "/home/project/private-dep1",
              ciCategory = "PrivateCategory",
              ciNamespace = "private_123456"
            },
            UnresolvedCategory {
              ucCategory = "UnresolvedCategory"
            }
          ],
          cofFiles = [
            "/home/project/special/object1.o",
            "/home/project/special/object1.o"
          ]
        }
      ]
    },

    checkWriteFail "bad hash" $ CompileMetadata {
      cmVersionHash = "bad hash",
      cmPath = "/home/project/special",
      cmNamespace = "public_ABCDEF",
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmExtraRequires = [],
      cmCategories = [],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad namespace" $ CompileMetadata {
      cmVersionHash = "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = "bad namespace",
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmExtraRequires = [],
      cmCategories = [],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad category" $ CompileMetadata {
      cmVersionHash = "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = "public_ABCDEF",
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmExtraRequires = [],
      cmCategories = [
        "bad category"
      ],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmObjectFiles = []
    },

    checkWriteThenRead $ ModuleConfig {
      rmRoot = "/home/projects",
      rmPath = "special",
      rmPublicDeps = [
        "/home/project/public-dep1",
        "/home/project/public-dep2"
      ],
      rmPrivateDeps = [
        "/home/project/private-dep1",
        "/home/project/private-dep2"
      ],
      rmExtraFiles = [
        "extra1.cpp",
        "extra2.cpp"
      ],
      rmExtraPaths = [
        "extra1",
        "extra2"
      ],
      rmExtraRequires = [
        "Extra1",
        "Extra2"
      ],
      rmMode = CompileIncremental,
      rmOutputName = "binary"
    },

    checkWriteFail "bad category" $ ModuleConfig {
      rmRoot = "/home/projects",
      rmPath = "special",
      rmPublicDeps = [],
      rmPrivateDeps = [],
      rmExtraFiles = [],
      rmExtraPaths = [],
      rmExtraRequires = [
        "bad category"
      ],
      rmMode = CompileIncremental,
      rmOutputName = ""
    },

    checkWriteFail "bad category" $ CategoryIdentifier {
      ciPath = "/home/project/special",
      ciCategory = "bad category",
      ciNamespace = "public_ABCDEF"
    },

    checkWriteFail "bad namespace" $ CategoryIdentifier {
      ciPath = "/home/project/special",
      ciCategory = "bad namespace",
      ciNamespace = "SpecialCategory"
    },

    checkWriteFail "bad category" $ UnresolvedCategory {
      ucCategory = "bad category"
    },

    checkWriteThenRead $ CompileBinary {
      cbCategory = "SpecialCategory",
      cbFunction = "specialFunction"
    },

    checkWriteFail "bad category" $ CompileBinary {
      cbCategory = "bad category",
      cbFunction = "specialFunction"
    },

    checkWriteFail "bad function" $ CompileBinary {
      cbCategory = "SpecialCategory",
      cbFunction = "bad function"
    },

    checkWriteThenRead $ CompileIncremental,

    checkWriteFail "compile mode" $ ExecuteTests { etInclude = [] },
    checkWriteFail "compile mode" $ CompileRecompile,
    checkWriteFail "compile mode" $ CreateTemplates,
    checkWriteFail "compile mode" $ CompileUnspecified
  ]

checkWriteThenRead :: (Eq a, Show a, ConfigFormat a) => a -> IO (CompileInfo ())
checkWriteThenRead m = return $ do
  text <- fmap spamComments $ autoWriteConfig m
  m' <- autoReadConfig "(string)" text
  when (m' /= m) $
    compileError $ "Failed to match after write/read\n" ++
                   "Before:\n" ++ show m ++ "\n" ++
                   "After:\n" ++ show m' ++ "\n" ++
                   "Intermediate:\n" ++ text where
   spamComments = unlines . map (++ " // spam") . lines

checkWriteFail :: ConfigFormat a => String -> a -> IO (CompileInfo ())
checkWriteFail p m = return $ do
  let m' = autoWriteConfig m
  check m'
  where
    check c
      | isCompileError c = do
          let text = show (getCompileError c)
          when (not $ text =~ p) $
            compileError $ "Expected pattern " ++ show p ++ " in error output but got\n" ++ text
      | otherwise =
          compileError $ "Expected write failure but got\n" ++ getCompileSuccess c
