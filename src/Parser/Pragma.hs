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

module Parser.Pragma (
  autoPragma,
  parsePragmas,
  unknownPragma,
) where

import Control.Monad (when)

import Base.CompilerError
import Parser.Common
import Parser.TextParser


parsePragmas :: [TextParser a] -> TextParser [a]
parsePragmas = many . foldr ((<|>)) (try unknownPragma)

unknownPragma :: TextParser a
unknownPragma = do
  try pragmaStart
  p <- some alphaNumChar
  compilerErrorM $ "pragma " ++ p ++ " is not supported in this context"

autoPragma :: String -> Either (SourceContext -> a) (SourceContext -> TextParser a) -> TextParser a
autoPragma p f = do
  c <- getSourceContext
  try $ pragmaStart >> string_ p >> notFollowedBy alphaNumChar
  hasArgs <- (pragmaArgsStart >> optionalSpace >> return True) <|> return False
  x <- delegate hasArgs f c
  if hasArgs
     then do
       extra <- manyTill asciiChar (string_ "]$")
       when (not $ null extra) $ compilerErrorM $ "content unused by pragma " ++ p ++ ": " ++ extra
       optionalSpace
     else sepAfter pragmaEnd
  return x where
    delegate False (Left f2)  c = return $ f2 c
    delegate True  (Right f2) c = f2 c
    delegate _     (Left _)   _ = compilerErrorM $ "pragma " ++ p ++ " does not allow arguments using []"
    delegate _     (Right _)  _ = compilerErrorM $ "pragma " ++ p ++ " requires arguments using []"
