{- -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

module Test.DefinedCategory (tests) where

import System.FilePath

import Base.CompilerError
import Base.TrackedErrors
import Parser.DefinedCategory ()
import Parser.TextParser (SourceContext)
import Test.Common
import Types.DefinedCategory


tests :: [IO (TrackedErrors ())]
tests = [
    checkParseSuccess ("testfiles" </> "definitions.0rx"),
    checkParseSuccess ("testfiles" </> "internal_inheritance.0rx")
  ]

checkParseSuccess :: String -> IO (TrackedErrors ())
checkParseSuccess f = do
  contents <- loadFile f
  let parsed = readMulti f contents :: TrackedErrors [DefinedCategory SourceContext]
  return $ check parsed
  where
  check c
    | isCompilerError c = compilerErrorM $ "Parse " ++ f ++ ":\n" ++ show (getCompilerError c)
    | otherwise = return ()

checkParseFail :: String -> IO (TrackedErrors ())
checkParseFail f = do
  contents <- loadFile f
  let parsed = readMulti f contents :: TrackedErrors [DefinedCategory SourceContext]
  return $ check parsed
  where
  check c
    | isCompilerError c = return ()
    | otherwise = compilerErrorM $ "Parse " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompilerSuccess c) ++ "\n"
