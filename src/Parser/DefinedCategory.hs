{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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
{-# LANGUAGE Safe #-}

module Parser.DefinedCategory (
) where

import Control.Monad (when)
import Prelude hiding (pi)
import Text.Parsec
import Text.Parsec.String

import Parser.Common
import Parser.Procedure ()
import Parser.TypeCategory
import Parser.TypeInstance ()
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


instance ParseFromSource (DefinedCategory SourcePos) where
  sourceParser = labeled "defined concrete category" $ do
    c <- getPosition
    kwDefine
    n <- sourceParser
    sepAfter (string_ "{")
    (ds,rs) <- parseRefinesDefines
    (pi,fi) <- parseInternalParams <|> return ([],[])
    (ms,ps,fs) <- parseMemberProcedureFunction n
    sepAfter (string_ "}")
    return $ DefinedCategory [c] n pi ds rs fi ms ps fs
    where
      parseRefinesDefines = fmap merge2 $ sepBy refineOrDefine optionalSpace
      refineOrDefine = labeled "refine or define" $ put12 singleRefine <|> put22 singleDefine
      parseInternalParams = labeled "internal params" $ do
        try kwTypes
        pi <- between (sepAfter $ string_ "<")
                      (sepAfter $ string_ ">")
                      (sepBy singleParam (sepAfter $ string_ ","))
        fi <- parseInternalFilters
        return (pi,fi)
      parseInternalFilters = do
        try $ sepAfter (string_ "{")
        fi <- parseFilters
        sepAfter (string_ "}")
        return fi
      singleParam = labeled "param declaration" $ do
        c <- getPosition
        n <- sourceParser
        return $ ValueParam [c] n Invariant

instance ParseFromSource (DefinedMember SourcePos) where
  sourceParser = labeled "defined member" $ do
    c <- getPosition
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
  CategoryName ->
  Parser ([DefinedMember SourcePos],[ExecutableProcedure SourcePos],[ScopedFunction SourcePos])
parseMemberProcedureFunction n = do
  (ms,ps,fs) <- parseAny3 (catchUnscopedType <|> sourceParser) sourceParser singleFunction
  let ps2 = ps ++ map snd fs
  let fs2 = map fst fs
  return (ms,ps2,fs2) where
    singleFunction = labeled "function" $ do
      f <- parseScopedFunction parseScope (return n)
      p <- labeled ("definition of function " ++ show (sfName f)) $ sourceParser
      when (sfName f /= epName p) $
        fail $ "expecting definition of function " ++ show (sfName f) ++
              " but got definition of " ++ show (epName p)
      return (f,p)
    catchUnscopedType = labeled "" $ do
      _ <- try sourceParser :: Parser ValueType
      fail $ "members must have an explicit @value or @category scope"
