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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- Older versions of Text.Megaparsec are Unsafe. This makes sure that the
-- compiler catches imports into Safe sources when a newer version is used.
{-# LANGUAGE Unsafe #-}

module Parser.TextParser (
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  SourceContext,
  TextParser,
  getSourceContext,
  runTextParser,
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set

import Base.CompilerError
import Base.CompilerMessage


type TextParser = Parsec CompilerMessage String

instance ErrorContextM TextParser where
  compilerErrorM = customFailure . compilerMessage
  withContextM     x e = region (mapParseError (pushErrorScope e) . promoteError) x
  summarizeErrorsM x e = region (mapParseError (pushErrorScope e) . promoteError) x

instance ShowErrorComponent CompilerMessage where
  showErrorComponent = show

runTextParser :: ErrorContextM m => TextParser a -> String -> String -> m a
runTextParser p n s = case parse p n s of
                           Left e  -> compilerErrorM $ dropWhileEnd isSpace $ errorBundlePretty e
                           Right x -> return x

newtype SourceContext = SourceContext SourcePos deriving (Eq,Ord)

instance Show SourceContext where
  show (SourceContext (SourcePos f l c)) =
    "line " ++ show (unPos l) ++ " column " ++ show (unPos c) ++ " of " ++ "\"" ++ f ++ "\""

getSourceContext :: TextParser SourceContext
getSourceContext = fmap SourceContext getSourcePos

promoteError :: ParseError String CompilerMessage -> ParseError String CompilerMessage
promoteError e@(TrivialError i _ _) = FancyError i $ Set.fromList [ErrorCustom $ compilerMessage $ parseErrorTextPretty e]
promoteError e = e
