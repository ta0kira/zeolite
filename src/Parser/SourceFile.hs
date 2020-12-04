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

module Parser.SourceFile (
  parseInternalSource,
  parsePublicSource,
  parseTestSource,
) where

import Text.Parsec
import Text.Parsec.String

import Base.CompileError
import Parser.Common
import Parser.DefinedCategory ()
import Parser.IntegrationTest ()
import Parser.Pragma
import Parser.TypeCategory ()
import Types.DefinedCategory
import Types.IntegrationTest
import Types.Pragma
import Types.TypeCategory


parseInternalSource :: CompileErrorM m =>
  (FilePath,String) -> m ([Pragma SourcePos],[AnyCategory SourcePos],[DefinedCategory SourcePos])
parseInternalSource (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc withPragmas) f s
  unwrap (Left e)  = compileErrorM (show e)
  unwrap (Right t) = return t
  withPragmas = do
    pragmas <- parsePragmas internalSourcePragmas
    optionalSpace
    (cs,ds) <- parseAny2 sourceParser sourceParser
    return (pragmas,cs,ds)

parsePublicSource :: CompileErrorM m => (FilePath,String) -> m ([Pragma SourcePos],[AnyCategory SourcePos])
parsePublicSource (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc withPragmas) f s
  unwrap (Left e)  = compileErrorM (show e)
  unwrap (Right t) = return t
  withPragmas = do
    pragmas <- parsePragmas publicSourcePragmas
    optionalSpace
    cs <- sepBy sourceParser optionalSpace
    return (pragmas,cs)

parseTestSource :: CompileErrorM m => (FilePath,String) -> m ([Pragma SourcePos],[IntegrationTest SourcePos])
parseTestSource (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc withPragmas) f s
  unwrap (Left e)  = compileErrorM (show e)
  unwrap (Right t) = return t
  withPragmas = do
    pragmas <- parsePragmas testSourcePragmas
    optionalSpace
    ts <- sepBy sourceParser optionalSpace
    return (pragmas,ts)

publicSourcePragmas :: [Parser (Pragma SourcePos)]
publicSourcePragmas = [pragmaModuleOnly,pragmaTestsOnly]

internalSourcePragmas :: [Parser (Pragma SourcePos)]
internalSourcePragmas = [pragmaTestsOnly]

testSourcePragmas :: [Parser (Pragma SourcePos)]
testSourcePragmas = []
