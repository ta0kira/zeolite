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

module Parser.SourceFile (
  CodeVisibility(..),
  PragmaSource(..),
  WithVisibility(..),
  hasCodeVisibility,
  isModuleOnly,
  isTestsOnly,
  mapCodeVisibility,
  parseInternalSource,
  parsePublicSource,
  parseTestSource,
  pragmaModuleOnly,
  pragmaTestsOnly,
  updateCodeVisibility,
) where

import qualified Data.Set as Set

import Base.CompilerError
import Parser.Common
import Parser.DefinedCategory ()
import Parser.IntegrationTest ()
import Parser.Pragma
import Parser.TextParser
import Parser.TypeCategory ()
import Types.DefinedCategory
import Types.IntegrationTest
import Types.TypeCategory


parseInternalSource :: ErrorContextM m =>
  (FilePath,String) -> m ([PragmaSource SourceContext],[AnyCategory SourceContext],[DefinedCategory SourceContext])
parseInternalSource (f,s) = runTextParser (between optionalSpace endOfDoc withPragmas) f s where
  withPragmas = do
    pragmas <- parsePragmas internalSourcePragmas
    optionalSpace
    (cs,ds) <- parseAny2 sourceParser sourceParser
    return (pragmas,cs,ds)

parsePublicSource :: ErrorContextM m => (FilePath,String) -> m ([PragmaSource SourceContext],[AnyCategory SourceContext])
parsePublicSource (f,s) = runTextParser (between optionalSpace endOfDoc withPragmas) f s where
  withPragmas = do
    pragmas <- parsePragmas publicSourcePragmas
    optionalSpace
    cs <- sepBy sourceParser optionalSpace
    return (pragmas,cs)

parseTestSource :: ErrorContextM m => (FilePath,String) -> m ([PragmaSource SourceContext],[IntegrationTest SourceContext])
parseTestSource (f,s) = runTextParser (between optionalSpace endOfDoc withPragmas) f s where
  withPragmas = do
    pragmas <- parsePragmas testSourcePragmas
    optionalSpace
    ts <- sepBy sourceParser optionalSpace
    return (pragmas,ts)

publicSourcePragmas :: [TextParser (PragmaSource SourceContext)]
publicSourcePragmas = [pragmaModuleOnly,pragmaTestsOnly]

internalSourcePragmas :: [TextParser (PragmaSource SourceContext)]
internalSourcePragmas = [pragmaTestsOnly]

testSourcePragmas :: [TextParser (PragmaSource SourceContext)]
testSourcePragmas = []

pragmaModuleOnly :: TextParser (PragmaSource SourceContext)
pragmaModuleOnly = autoPragma "ModuleOnly" $ Left parseAt where
  parseAt c = PragmaVisibility [c] ModuleOnly

pragmaTestsOnly :: TextParser (PragmaSource SourceContext)
pragmaTestsOnly = autoPragma "TestsOnly" $ Left parseAt where
  parseAt c = PragmaVisibility [c] TestsOnly

data CodeVisibility = ModuleOnly | TestsOnly | FromDependency deriving (Eq,Ord,Show)

data WithVisibility a =
  WithVisibility {
    wvVisibility :: Set.Set CodeVisibility,
    wvData :: a
  }
  deriving (Show)

hasCodeVisibility :: CodeVisibility -> WithVisibility a -> Bool
hasCodeVisibility v = Set.member v . wvVisibility

mapCodeVisibility :: (a -> b) -> WithVisibility a -> WithVisibility b
mapCodeVisibility f (WithVisibility v x) = WithVisibility v (f x)

updateCodeVisibility :: (Set.Set CodeVisibility -> Set.Set CodeVisibility) ->
  WithVisibility a -> WithVisibility a
updateCodeVisibility f (WithVisibility v x) = WithVisibility (f v) x

data PragmaSource c =
  PragmaVisibility {
    pvContext :: [c],
    pvScopes :: CodeVisibility
  }
  deriving (Show)

isModuleOnly :: PragmaSource c -> Bool
isModuleOnly (PragmaVisibility _ ModuleOnly) = True
isModuleOnly _                               = False

isTestsOnly :: PragmaSource c -> Bool
isTestsOnly (PragmaVisibility _ TestsOnly) = True
isTestsOnly _                              = False
