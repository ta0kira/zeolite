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
import Config.Programs (VersionHash(..))
import Types.TypeCategory (FunctionName(..),Namespace(..))
import Types.TypeInstance (CategoryName(..))

tests :: [IO (CompileInfo ())]
tests = [
    checkWriteThenRead $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = StaticNamespace "public_ABCDEF",
      cmPublicDeps = [
        "/home/project/public-dep1",
        "/home/project/public-dep2"
      ],
      cmPrivateDeps = [
        "/home/project/private-dep1",
        "/home/project/private-dep2"
      ],
      cmPublicCategories = [
        CategoryName "MyCategory",
        CategoryName "MyOtherCategory"
      ],
      cmPrivateCategories = [
        CategoryName "PrivateCategory",
        CategoryName "PrivateOtherCategory"
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
      cmBinaries = [
        "/home/project/special/binary1",
        "/home/project/special/binary2"
      ],
      cmLinkFlags = [
        "-lm",
        "-ldl"
      ],
      cmObjectFiles = [
        CategoryObjectFile {
          cofCategory = CategoryIdentifier {
            ciPath = "/home/project/special",
            ciCategory = CategoryName "SpecialCategory",
            ciNamespace = StaticNamespace "public_ABCDEF"
          },
          cofRequires = [
            CategoryIdentifier {
              ciPath = "/home/project/private-dep1",
              ciCategory = CategoryName "PrivateCategory",
              ciNamespace = NoNamespace
            },
            UnresolvedCategory {
              ucCategory = CategoryName "UnresolvedCategory"
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
      cmVersionHash = VersionHash "bad hash",
      cmPath = "/home/project/special",
      cmNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad namespace" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = StaticNamespace "bad namespace",
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad category" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [
        CategoryName "bad category"
      ],
      cmPrivateCategories = [],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad category" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmPath = "/home/project/special",
      cmNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [
        CategoryName "bad category"
      ],
      cmSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLinkFlags = [],
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
        CategorySource {
          csSource = "extra1.cpp",
          csCategories = [
            CategoryName "Category1",
            CategoryName "Category2"
          ],
          csDepCategories = [
            CategoryName "DepCategory1",
            CategoryName "DepCategory2"
          ]
        },
        OtherSource {
          osSource = "extra2.cpp"
        }
      ],
      rmExtraPaths = [
        "extra1",
        "extra2"
      ],
      rmMode = CompileIncremental {
        ciLinkFlags = [
          "-lm",
          "-ldl"
        ]
      }
    },

    checkWriteFail "bad category" $ CategorySource {
      csSource = "extra1.cpp",
      csCategories = [
        CategoryName "bad category"
      ],
      csDepCategories = []
    },

    checkWriteFail "bad category" $ CategorySource {
      csSource = "extra1.cpp",
      csCategories = [],
      csDepCategories = [
        CategoryName "bad category"
      ]
    },

    checkWriteFail "bad category" $ CategoryIdentifier {
      ciPath = "/home/project/special",
      ciCategory = CategoryName "bad category",
      ciNamespace = NoNamespace
    },

    checkWriteFail "bad namespace" $ CategoryIdentifier {
      ciPath = "/home/project/special",
      ciCategory = CategoryName "SpecialCategory",
      ciNamespace = StaticNamespace "bad namespace"
    },

    checkWriteFail "bad category" $ UnresolvedCategory {
      ucCategory = CategoryName "bad category"
    },

    checkWriteThenRead $ CompileBinary {
      cbCategory = CategoryName "SpecialCategory",
      cbFunction = FunctionName "specialFunction",
      cbOutputName = "binary",
      cbLinkFlags = []
    },

    checkWriteFail "bad category" $ CompileBinary {
      cbCategory = CategoryName "bad category",
      cbFunction = FunctionName "specialFunction",
      cbOutputName = "binary",
      cbLinkFlags = []
    },

    checkWriteFail "bad function" $ CompileBinary {
      cbCategory = CategoryName "SpecialCategory",
      cbFunction = FunctionName "bad function",
      cbOutputName = "binary",
      cbLinkFlags = []
    },

    checkWriteFail "compile mode" $ CompileFast {
      cfCategory = CategoryName "SpecialCategory",
      cfFunction = FunctionName "specialFunction",
      cfSource = "source.0rx"
    },

    checkWriteThenRead $ CompileIncremental {
      ciLinkFlags = [
        "-lm",
        "-ldl"
      ]
    },

    checkWriteFail "compile mode" $ ExecuteTests { etInclude = [] },
    checkWriteFail "compile mode" $ CompileRecompile,
    checkWriteFail "compile mode" $ CompileRecompileRecursive,
    checkWriteFail "compile mode" $ CreateTemplates
  ]

checkWriteThenRead :: (Eq a, Show a, ConfigFormat a) => a -> IO (CompileInfo ())
checkWriteThenRead m = return $ do
  text <- fmap spamComments $ autoWriteConfig m
  m' <- autoReadConfig "(string)" text `reviseError` ("Serialized >>>\n\n" ++ text ++ "\n<<< Serialized\n\n")
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
