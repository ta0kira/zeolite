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

module Parser.Pragma (
  parsePragmas,
  pragmaComment,
  pragmaExprLookup,
  pragmaNoTrace,
  pragmaModuleOnly,
  pragmaSourceContext,
  pragmaTestsOnly,
  pragmaTraceCreation,
) where

import Control.Monad (when)

import Base.CompilerError
import Parser.Common
import Parser.TextParser
import Types.Pragma


parsePragmas :: [TextParser a] -> TextParser [a]
parsePragmas = many . foldr ((<|>)) unknownPragma

pragmaModuleOnly :: TextParser (Pragma SourceContext)
pragmaModuleOnly = autoPragma "ModuleOnly" $ Left parseAt where
  parseAt c = PragmaVisibility [c] ModuleOnly

instance ParseFromSource MacroName where
  sourceParser = labeled "macro name" $ do
    h <- upperChar <|> char '_'
    t <- many (upperChar <|> digitChar <|> char '_')
    optionalSpace
    return $ MacroName (h:t)

pragmaExprLookup :: TextParser (Pragma SourceContext)
pragmaExprLookup = autoPragma "ExprLookup" $ Right parseAt where
  parseAt c = do
    name <- sourceParser
    return $ PragmaExprLookup [c] name

pragmaSourceContext :: TextParser (Pragma SourceContext)
pragmaSourceContext = autoPragma "SourceContext" $ Left parseAt where
  parseAt c = PragmaSourceContext c

pragmaNoTrace :: TextParser (Pragma SourceContext)
pragmaNoTrace = autoPragma "NoTrace" $ Left parseAt where
  parseAt c = PragmaTracing [c] NoTrace

pragmaTraceCreation :: TextParser (Pragma SourceContext)
pragmaTraceCreation = autoPragma "TraceCreation" $ Left parseAt where
  parseAt c = PragmaTracing [c] TraceCreation

pragmaTestsOnly :: TextParser (Pragma SourceContext)
pragmaTestsOnly = autoPragma "TestsOnly" $ Left parseAt where
  parseAt c = PragmaVisibility [c] TestsOnly

pragmaComment :: TextParser (Pragma SourceContext)
pragmaComment = autoPragma "Comment" $ Right parseAt where
  parseAt c = do
    string_ "\""
    ss <- manyTill stringChar (string_ "\"")
    optionalSpace
    return $ PragmaComment [c] ss

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
