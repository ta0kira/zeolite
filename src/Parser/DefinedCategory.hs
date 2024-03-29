{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

module Parser.DefinedCategory (
) where

import Prelude hiding (pi)

import Base.CompilerError
import Parser.Common
import Parser.Pragma (autoPragma)
import Parser.Procedure ()
import Parser.TextParser hiding (hidden)
import Parser.TypeCategory
import Parser.TypeInstance ()
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


instance ParseFromSource (DefinedCategory SourceContext) where
  sourceParser = labeled "defined concrete category" $ do
    c <- getSourceContext
    kwDefine
    n <- sourceParser
    sepAfter (string_ "{")
    pragmas <- sepBy singlePragma optionalSpace
    (ds,rs) <- parseRefinesDefines
    (ms,ps,fs) <- parseMemberProcedureFunction n
    sepAfter (string_ "}")
    return $ DefinedCategory [c] n pragmas ds rs ms ps fs
    where
      parseRefinesDefines = fmap merge2 $ sepBy refineOrDefine optionalSpace
      singlePragma = readOnly <|> readOnlyExcept <|> hidden <|> flatCleanup
      flatCleanup = autoPragma "FlatCleanup" $ Right parseAt where
        parseAt c = do
          v <- labeled "variable name" sourceParser
          return $ FlatCleanup [c] v
      readOnly = autoPragma "ReadOnly" $ Right parseAt where
        parseAt c = do
          vs <- labeled "variable names" $ sepBy sourceParser (sepAfter $ string ",")
          return $ MembersReadOnly [c] vs
      readOnlyExcept = autoPragma "ReadOnlyExcept" $ Right parseAt where
        parseAt c = do
          vs <- labeled "variable names" $ sepBy sourceParser (sepAfter $ string ",")
          return $ MembersReadOnlyExcept [c] vs
      hidden = autoPragma "Hidden" $ Right parseAt where
        parseAt c = do
          vs <- labeled "variable names" $ sepBy sourceParser (sepAfter $ string ",")
          return $ MembersHidden [c] vs
      refineOrDefine = labeled "refine or define" $ put12 singleRefine <|> put22 singleDefine

instance ParseFromSource (DefinedMember SourceContext) where
  sourceParser = labeled "defined member" $ do
    c <- getSourceContext
    (s,t) <- try parseType
    n <- sourceParser
    e <- if s == ValueScope
            then return Nothing
            else parseInit
    return $ DefinedMember [c] s t n e
    where
      parseInit = labeled "member initializer" $ do
        assignOperator
        e <- sourceParser
        return $ Just e
      parseType = do
        s <- parseScope
        t <- sourceParser
        return (s,t)

parseMemberProcedureFunction ::
  CategoryName -> TextParser ([DefinedMember SourceContext],
                             [ExecutableProcedure SourceContext],
                             [ScopedFunction SourceContext])
parseMemberProcedureFunction n = do
  (ms,ps,fs) <- parseAny3 (catchUnscopedType <|> sourceParser) sourceParser singleFunction
  let ps2 = ps ++ map snd fs
  let fs2 = map fst fs
  return (ms,ps2,fs2) where
    singleFunction = labeled "function" $ do
      f <- parseScopedFunction parseScope (return n)
      lookAhead (sepAfter $ string_ $ show (sfName f)) <|>
        (compilerErrorM $ "expected definition of function " ++ show (sfName f))
      p <- labeled ("definition of function " ++ show (sfName f)) $ sourceParser
      return (f,p)
    catchUnscopedType = labeled "" $ do
      _ <- try sourceParser :: TextParser ValueType
      compilerErrorM "members must have an explicit @value or @category scope"
