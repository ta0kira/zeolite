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

module ParserTest where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParserBase
import TestBase
import TypesBase


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

checkParsesAs p s m = return $ do
  let parsed = readSingleWith p "(string)" s
  check parsed
  e <- parsed
  when (e /= m) $
    compileError $ show s ++ " does not parse as " ++ show m ++ ":\n" ++ show e
  where
    check c
      | isCompileError c = compileError $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkParseFail p s = do
  let parsed = readSingleWith p "(string)" s
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"
