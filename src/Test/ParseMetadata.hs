{- -----------------------------------------------------------------------------
Copyright 2020-2021,2023 Kevin P. Barry

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

import Base.CompilerError
import Base.Positional
import Base.TrackedErrors
import Cli.CompileOptions
import Cli.Programs (VersionHash(..))
import Module.CompileMetadata
import Module.ParseMetadata
import System.FilePath
import Test.Common
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


hugeCompileMetadata :: CompileMetadata  -- testfiles/module-cache.txt
hugeCompileMetadata = CompileMetadata {
    cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
    cmRoot = "/home/project",
    cmPath = "/home/project/special",
    cmExtra = [
      "extra1",
      "extra2"
    ],
    cmPublicNamespace = StaticNamespace "public_ABCDEF",
    cmPrivateNamespace = StaticNamespace "private_ABCDEF",
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
    cmPublicSubdirs = [
      "/home/project/special/subdir1",
      "/home/project/special/subdir2"
    ],
    cmPrivateSubdirs = [
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
    cmLibraries = [
      "/home/project/special/library1",
      "/home/project/special/library2"
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
  }

hugeModuleConfig :: ModuleConfig  -- testfiles/module-config.txt
hugeModuleConfig = ModuleConfig {
    mcRoot = "/home/projects",
    mcPath = "special",
    mcExtra = [
      "extra1",
      "extra2"
    ],
    mcExprMap = [],
    mcPublicDeps = [
      "/home/project/public-dep1",
      "/home/project/public-dep2"
    ],
    mcPrivateDeps = [
      "/home/project/private-dep1",
      "/home/project/private-dep2"
    ],
    mcExtraFiles = [
      CategorySource {
        csSource = "extra1.cpp",
        csCategories = [
          CategoryName "Category1",
          CategoryName "Category2"
        ],
        csRequires = [
          CategoryName "DepCategory1",
          CategoryName "DepCategory2"
        ]
      },
      OtherSource {
        osSource = "extra2.cpp"
      }
    ],
    mcCategories = [
      (CategoryName "Category1",CategorySpec {
        csContext = [],
        csRefines = [
          ValueRefine [] (TypeInstance (CategoryName "Base1") (Positional [])),
          ValueRefine [] (TypeInstance (CategoryName "Base2") (Positional []))
        ],
        csDefines = [
          ValueDefine [] (DefinesInstance (CategoryName "Base3") (Positional [])),
          ValueDefine [] (DefinesInstance (CategoryName "Base4") (Positional []))
        ]
      }),
      (CategoryName "Category2",CategorySpec {
        csContext = [],
        csRefines = [
          ValueRefine [] (TypeInstance (CategoryName "Base1") (Positional [])),
          ValueRefine [] (TypeInstance (CategoryName "Base2") (Positional []))
        ],
        csDefines = [
          ValueDefine [] (DefinesInstance (CategoryName "Base3") (Positional [])),
          ValueDefine [] (DefinesInstance (CategoryName "Base4") (Positional []))
        ]
      })
    ],
    mcExtraPaths = [
      "extra1",
      "extra2"
    ],
    mcMode = CompileIncremental {
      ciLinkFlags = [
        "-lm",
        "-ldl"
      ]
    }
  }

tests :: [IO (TrackedErrors ())]
tests = [
    checkWriteThenRead hugeCompileMetadata,

    checkWriteFail "bad hash" $ CompileMetadata {
      cmVersionHash = VersionHash "bad hash",
      cmRoot = "/home/project",
      cmPath = "/home/project/special",
      cmExtra = [],
      cmPublicNamespace = NoNamespace,
      cmPrivateNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [],
      cmPublicSubdirs = [],
      cmPrivateSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLibraries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad namespace" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmRoot = "/home/project",
      cmPath = "/home/project/special",
      cmExtra = [],
      cmPublicNamespace = StaticNamespace "bad namespace",
      cmPrivateNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [],
      cmPublicSubdirs = [],
      cmPrivateSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLibraries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad namespace" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmRoot = "/home/project",
      cmPath = "/home/project/special",
      cmExtra = [],
      cmPublicNamespace = NoNamespace,
      cmPrivateNamespace = StaticNamespace "bad namespace",
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [],
      cmPublicSubdirs = [],
      cmPrivateSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLibraries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad category" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmRoot = "/home/project",
      cmPath = "/home/project/special",
      cmExtra = [],
      cmPublicNamespace = NoNamespace,
      cmPrivateNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [
        CategoryName "bad category"
      ],
      cmPrivateCategories = [],
      cmPublicSubdirs = [],
      cmPrivateSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLibraries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteFail "bad category" $ CompileMetadata {
      cmVersionHash = VersionHash "0123456789ABCDEFabcdef",
      cmRoot = "/home/project",
      cmPath = "/home/project/special",
      cmExtra = [],
      cmPublicNamespace = NoNamespace,
      cmPrivateNamespace = NoNamespace,
      cmPublicDeps = [],
      cmPrivateDeps = [],
      cmPublicCategories = [],
      cmPrivateCategories = [
        CategoryName "bad category"
      ],
      cmPublicSubdirs = [],
      cmPrivateSubdirs = [],
      cmPublicFiles = [],
      cmPrivateFiles = [],
      cmTestFiles = [],
      cmHxxFiles = [],
      cmCxxFiles = [],
      cmBinaries = [],
      cmLibraries = [],
      cmLinkFlags = [],
      cmObjectFiles = []
    },

    checkWriteThenRead hugeModuleConfig,

    checkWriteFail "empty.+map" $ ModuleConfig {
      mcRoot = "/home/projects",
      mcPath = "special",
      mcExtra = [],
      mcExprMap = [(MacroName "MACRO",Literal (StringLiteral [] "something"))],
      mcPublicDeps = [],
      mcPrivateDeps = [],
      mcExtraFiles = [],
      mcCategories = [],
      mcExtraPaths = [],
      mcMode = CompileIncremental {
        ciLinkFlags = []
      }
    },

    checkWriteFail "bad category" $ CategorySource {
      csSource = "extra1.cpp",
      csCategories = [
        CategoryName "bad category"
      ],
      csRequires = []
    },

    checkWriteFail "bad category" $ CategorySource {
      csSource = "extra1.cpp",
      csCategories = [],
      csRequires = [
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
      cbLinker = LinkStatic,
      cbOutputName = "binary",
      cbLinkFlags = []
    },

    checkWriteThenRead $ CompileBinary {
      cbCategory = CategoryName "SpecialCategory",
      cbFunction = FunctionName "specialFunction",
      cbLinker = LinkDynamic,
      cbOutputName = "binary",
      cbLinkFlags = []
    },

    checkWriteFail "bad category" $ CompileBinary {
      cbCategory = CategoryName "bad category",
      cbFunction = FunctionName "specialFunction",
      cbLinker = LinkDynamic,
      cbOutputName = "binary",
      cbLinkFlags = []
    },

    checkWriteFail "bad function" $ CompileBinary {
      cbCategory = CategoryName "SpecialCategory",
      cbFunction = FunctionName "bad function",
      cbLinker = LinkDynamic,
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

    checkWriteFail "compile mode" $ ExecuteTests { etInclude = [], etCallLog = Nothing },
    checkWriteFail "compile mode" $ CompileRecompile,
    checkWriteFail "compile mode" $ CompileRecompileRecursive,
    checkWriteFail "compile mode" $ CreateTemplates,

    checkParsesAs ("testfiles" </> "macro-config.txt")
      (\m -> case mcExprMap m of
                  [(MacroName "MY_MACRO",
                    Expression _ (BuiltinCall _
                      (FunctionCall _ BuiltinRequire (Positional [])
                        (Positional [(Nothing,Literal (EmptyLiteral _))]))) []),
                   (MacroName "MY_OTHER_MACRO",
                    Expression _
                      (TypeCall _ _
                        (FunctionCall _ (FunctionName "execute") (Positional [])
                        (Positional [(Nothing,Expression _ (UnambiguousLiteral (StringLiteral _ "this is a string\n")) [])]))) [])
                    ] -> True
                  _ -> False),

    checkParsesAs ("testfiles" </> "module-config.txt") (== hugeModuleConfig),

    checkParsesAs ("testfiles" </> "module-cache.txt") (== hugeCompileMetadata)
  ]

checkParsesAs :: (Show a, ConfigFormat a) => String -> (a -> Bool) -> IO (TrackedErrors ())
checkParsesAs f m = do
  contents <- loadFile f
  let parsed = autoReadConfig f contents
  return $ check parsed contents
  where
    check x contents = do
      x' <- x <!! "While parsing " ++ f
      when (not $ m x') $
        compilerErrorM $ "Failed to match after write/read\n" ++
                       "Unparsed:\n" ++ contents ++ "\n" ++
                       "Parsed:\n" ++ show x' ++ "\n"
