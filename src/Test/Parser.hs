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

module Test.Parser (tests) where

import Control.Monad (when)

import Base.CompileError
import Base.CompileInfo
import Parser.Common
import Test.Common
import Text.Parsec.String


tests :: [IO (CompileInfo ())]
tests = [
    checkParsesAs stringChar "\\'" '\'',
    checkParsesAs stringChar "\\\"" '"',
    checkParsesAs stringChar "\\?" '?',
    checkParsesAs stringChar "\\\\" '\\',
    checkParsesAs stringChar "\\a" '\a',
    checkParsesAs stringChar "\\b" '\b',
    checkParsesAs stringChar "\\f" '\f',
    checkParsesAs stringChar "\\n" '\n',
    checkParsesAs stringChar "\\r" '\r',
    checkParsesAs stringChar "\\t" '\t',
    checkParsesAs stringChar "\\v" '\v',
    checkParsesAs stringChar "\n" '\n',
    checkParsesAs stringChar "\\x0A" '\n',
    checkParsesAs stringChar "\\012" '\n',
    checkParseFail stringChar "\"",
    checkParseFail stringChar "\\q",
    checkParseFail stringChar "\\00",
    checkParseFail stringChar "\\x0",

    checkParsesAs regexChar "\\\\" "\\\\",
    checkParsesAs regexChar "\\n" "\\n",
    checkParsesAs regexChar "\n" "\n",
    checkParsesAs regexChar "\\\"" "\"",
    checkParseFail regexChar "\""
  ]

checkParsesAs :: (Eq a, Show a) => ParserE CompileInfo a -> [Char] -> a -> IO (CompileInfo ())
checkParsesAs p s m = return $ do
  let parsed = readSingleWith p "(string)" s
  check parsed
  e <- parsed
  when (e /= m) $
    compileErrorM $ show s ++ " does not parse as " ++ show m ++ ":\n" ++ show e
  where
    check c
      | isCompileError c = compileErrorM $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkParseFail :: Show a => ParserE CompileInfo a -> [Char] -> IO (CompileInfo ())
checkParseFail p s = do
  let parsed = readSingleWith p "(string)" s
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileErrorM $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"
