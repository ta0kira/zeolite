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

module Test.SourceFile (tests) where

import System.FilePath

import Base.CompilerError
import Base.TrackedErrors
import Parser.SourceFile
import Test.Common


tests :: [IO (TrackedErrors ())]
tests = [
    checkParseSuccess ("testfiles" </> "public.0rp")   parsePublicSource,
    checkParseSuccess ("testfiles" </> "internal.0rx") parseInternalSource,
    checkParseSuccess ("testfiles" </> "test.0rt")     parseTestSource
  ]

checkParseSuccess :: String -> ((FilePath,String) -> TrackedErrors a) -> IO (TrackedErrors ())
checkParseSuccess f p = do
  contents <- loadFile f
  let parsed = p (f,contents)
  return $ check parsed
  where
    check c
      | isCompilerError c = compilerErrorM $ "Parse " ++ f ++ ":\n" ++ show (getCompilerError c)
      | otherwise = return ()
