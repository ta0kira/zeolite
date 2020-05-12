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

module Parser.Pragma (
  parsePragmas,
  pragmaComment,
  pragmaExprLookup,
  pragmaModuleOnly,
  pragmaTestsOnly
) where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String

import Parser.Common
import Types.Pragma


parsePragmas :: [Parser a] -> Parser [a]
parsePragmas = many . foldr ((<|>)) unknownPragma

pragmaModuleOnly :: Parser (Pragma SourcePos)
pragmaModuleOnly = autoPragma "ModuleOnly" $ Left parseAt where
  parseAt c = PragmaVisibility [c] ModuleOnly

pragmaExprLookup :: Parser (Pragma SourcePos)
pragmaExprLookup = autoPragma "ExprLookup" $ Right parseAt where
  parseAt c = do
    name <- labeled "macro name" $ many1 (upper <|> char '_')
    optionalSpace
    return $ PragmaExprLookup [c] name

pragmaTestsOnly :: Parser (Pragma SourcePos)
pragmaTestsOnly = autoPragma "TestsOnly" $ Left parseAt where
  parseAt c = PragmaVisibility [c] TestsOnly

pragmaComment :: Parser (Pragma SourcePos)
pragmaComment = autoPragma "Comment" $ Right parseAt where
  parseAt c = do
    string_ "\""
    ss <- manyTill stringChar (string_ "\"")
    optionalSpace
    return $ PragmaComment [c] ss

unknownPragma :: Parser a
unknownPragma = do
  try pragmaStart
  p <- many1 alphaNum
  fail $ "Pragma " ++ p ++ " is not supported in this context"

autoPragma :: String -> Either (SourcePos -> a) (SourcePos -> Parser a) -> Parser a
autoPragma p f = do
  c <- getPosition
  try $ pragmaStart >> string_ p
  hasArgs <- (pragmaArgsStart >> optionalSpace >> return True) <|> return False
  x <- delegate hasArgs f c
  if hasArgs
     then do
       extra <- manyTill anyChar (string_ "]$")
       when (not $ null extra) $ fail $ "Content unused by pragma " ++ p ++ ": " ++ extra
     else sepAfter pragmaEnd
  return x where
    delegate False (Left f2)  c = return $ f2 c
    delegate True  (Right f2) c = f2 c
    delegate _     (Left _)   _ = fail $ "Pragma " ++ p ++ " does not allow arguments with []"
    delegate _     (Right _)  _ = fail $ "Pragma " ++ p ++ " requires arguments with []"
