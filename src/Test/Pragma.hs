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

module Test.Pragma (tests) where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String
import Text.Regex.TDFA -- Not safe!

import Base.CompileError
import Compilation.CompileInfo
import Parser.Pragma
import Test.Common
import Types.Pragma


tests :: [IO (CompileInfo ())]
tests = [
    checkParsesAs "$ModuleOnly$" (fmap (:[]) pragmaModuleOnly)
      (\e -> case e of
                  [PragmaVisibility _ ModuleOnly] -> True
                  _ -> False),

    checkParsesAs "$TestsOnly$" (fmap (:[]) pragmaTestsOnly)
      (\e -> case e of
                  [PragmaVisibility _ TestsOnly] -> True
                  _ -> False),

    checkParsesAs "$NoTrace$" (fmap (:[]) pragmaNoTrace)
      (\e -> case e of
                  [PragmaTracing _ NoTrace] -> True
                  _ -> False),

    checkParsesAs "$TraceCreation$" (fmap (:[]) pragmaTraceCreation)
      (\e -> case e of
                  [PragmaTracing _ TraceCreation] -> True
                  _ -> False),

    checkParsesAs "$Comment[ \"this is a pragma with args\" ]$" (fmap (:[]) pragmaComment)
      (\e -> case e of
                  [PragmaComment _ "this is a pragma with args"] -> True
                  _ -> False),

    checkParsesAs "$ExprLookup[ \nMODULE_PATH /*comment*/\n ]$" (fmap (:[]) pragmaExprLookup)
      (\e -> case e of
                  [PragmaExprLookup _ "MODULE_PATH"] -> True
                  _ -> False),

    checkParsesAs "/*only comments*/" (parsePragmas [pragmaModuleOnly,pragmaTestsOnly])
      (\e -> case e of
                  [] -> True
                  _ -> False),

    checkParsesAs "$ModuleOnly$  // comment" (parsePragmas [pragmaTestsOnly,pragmaModuleOnly])
      (\e -> case e of
                  [PragmaVisibility _ ModuleOnly] -> True
                  _ -> False),

    checkParsesAs "$TestsOnly$  /*comment*/" (parsePragmas [pragmaModuleOnly,pragmaTestsOnly])
      (\e -> case e of
                  [PragmaVisibility _ TestsOnly] -> True
                  _ -> False),

    checkParsesAs "$TestsOnly$\n$TestsOnly$\n$ModuleOnly$" (parsePragmas [pragmaModuleOnly,pragmaTestsOnly])
      (\e -> case e of
                  [PragmaVisibility _ TestsOnly,
                   PragmaVisibility _ TestsOnly,
                   PragmaVisibility _ ModuleOnly] -> True
                  _ -> False),

    checkParseError "$ModuleOnly[ extra ]$" "does not allow arguments" pragmaModuleOnly,

    checkParseError "$TestsOnly[ extra ]$" "does not allow arguments" pragmaTestsOnly,

    checkParseError "$Comment$" "requires arguments" pragmaComment,

    checkParseError "$ExprLookup[ \"bad stuff\" ]$" "macro name" pragmaExprLookup
  ]

checkParsesAs :: String -> Parser [Pragma SourcePos] -> ([Pragma SourcePos] -> Bool) -> IO (CompileInfo ())
checkParsesAs s p m = return $ do
  let parsed = readSingleWith p "(string)" s
  check parsed
  e <- parsed
  when (not $ m e) $
    compileErrorM $ "No match in '" ++ s ++ "':\n" ++ show e
  where
    check c
      | isCompileErrorM c = compileErrorM $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkParseError :: String -> String -> Parser (Pragma SourcePos) -> IO (CompileInfo ())
checkParseError s m p = return $ do
  let parsed = readSingleWith p "(string)" s
  check parsed
  where
    check c
      | isCompileErrorM c = do
          let text = show (getCompileError c)
          when (not $ text =~ m) $
            compileErrorM $ "Expected pattern " ++ show m ++ " in error output but got\n" ++ text
      | otherwise =
          compileErrorM $ "Expected write failure but got\n" ++ show (getCompileSuccess c)
