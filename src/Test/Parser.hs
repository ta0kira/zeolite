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

module Test.Parser (tests) where

import Control.Monad (when)

import Base.CompilerError
import Base.TrackedErrors
import Parser.Common
import Parser.TextParser
import Test.Common


tests :: [IO (TrackedErrors ())]
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
    checkParseFail regexChar "\"",

    checkParsesAs (keyword "keyword" >> some asciiChar) "keyword   string" "string",
    checkParseFail (keyword "keyword" >> some asciiChar) "keywordstring",
    checkParseFail (keyword "keyword" >> some asciiChar) "keyword_string",
    checkParsesAs
      ((fmap Left $ keyword "keyword" >> some asciiChar) <|> (fmap Right $ some asciiChar))
      "keywordstring"
      (Right "keywordstring"),

    checkParsesAs (operator ">>??!" >> many asciiChar) ">>??!   !!" "!!",
    checkParseFail (operator ">>??!" >> many asciiChar) ">>??!!!"
  ]

checkParsesAs :: (Eq a, Show a) => TextParser a -> String -> a -> IO (TrackedErrors ())
checkParsesAs p s m = return $ do
  let parsed = readSingleWith p "(string)" s
  check parsed
  e <- parsed
  when (e /= m) $
    compilerErrorM $ show s ++ " does not parse as " ++ show m ++ ":\n" ++ show e
  where
    check c
      | isCompilerError c = compilerErrorM $ "Parse '" ++ s ++ "':\n" ++ show (getCompilerError c)
      | otherwise = return ()

checkParseFail :: Show a => TextParser a -> String -> IO (TrackedErrors ())
checkParseFail p s = do
  let parsed = readSingleWith p "(string)" s
  return $ check parsed
  where
    check c
      | isCompilerError c = return ()
      | otherwise = compilerErrorM $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                     show (getCompilerSuccess c) ++ "\n"
