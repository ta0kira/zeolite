{- -----------------------------------------------------------------------------
Copyright 2021 Kevin P. Barry

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

module Test.ParseConfig (tests) where

import Base.TrackedErrors
import Config.CompilerConfig
import Config.ParseConfig ()
import Test.Common


compilerConfig :: LocalConfig
compilerConfig = LocalConfig {
    lcBackend = UnixBackend {
      ucCxxBinary = "/usr/bin/clang++",
      ucCompileFlags = [
        "-O2",
        "-std=c++11",
        "-fPIC"
      ],
      ucLibraryFlags = [
        "-shared",
        "-fpic"
      ],
      ucBinaryFlags = [
        "-02",
        "-std=c++11"
      ],
      ucArBinary = "/usr/bin/ar"
    },
    lcResolver = SimpleResolver {
      srVisibleSystem = [
        "lib",
        "testing"
      ],
      srExtraPaths = [
        "extra1",
        "extra2"
      ]
    }
  }

tests :: [IO (TrackedErrors ())]
tests = [
    checkWriteThenRead compilerConfig
  ]
