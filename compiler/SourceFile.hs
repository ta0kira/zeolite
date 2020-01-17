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

module SourceFile (
  parseInternalSource,
  parsePublicSource,
) where

import Text.Parsec
import Text.Parsec.String

import DefinedCategory
import TypeCategory
import ParseCategory
import ParseDefinition
import ParserBase
import TypesBase


parseInternalSource :: (CompileErrorM m, Monad m) =>
  (String,String) -> m ([AnyCategory SourcePos],[DefinedCategory SourcePos])
parseInternalSource (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc parseAny) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

parsePublicSource :: (CompileErrorM m, Monad m) =>
  (String,String) -> m [AnyCategory SourcePos]
parsePublicSource (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

parseAny :: Parser ([AnyCategory SourcePos],[DefinedCategory SourcePos])
parseAny = parsed >>= return . foldr merge empty where
  empty = ([],[])
  merge (cs1,ds1) (cs2,ds2) = (cs1++cs2,ds1++ds2)
  parsed = sepBy anyType optionalSpace
  anyType = singleCategory <|> singleDefine
  singleCategory = do
    c <- sourceParser
    return ([c],[])
  singleDefine = do
    d <- sourceParser
    return ([],[d])
