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
  parseAnySource,
) where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String

import Parser.Common
import Parser.Procedure
import Parser.TypeCategory
import Parser.TypeInstance
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
    sepAfter (string "{")
    (ds,rs) <- parseRefinesDefines
    (pi,fi) <- parseInternalParams <|> return ([],[])
    (ms,ps,fs) <- parseMemberProcedureFunction n
    sepAfter (string "}")
    return $ DefinedCategory [c] n pi ds rs fi ms ps fs
    where
      parseRefinesDefines = fmap merge2 $ sepBy refineOrDefine optionalSpace
      refineOrDefine = labeled "refine or define" $ put12 singleRefine <|> put22 singleDefine
      parseInternalParams = labeled "internal params" $ do
        try kwTypes
        pi <- between (sepAfter $ string "<")
                      (sepAfter $ string ">")
                      (sepBy singleParam (sepAfter $ string ","))
        fi <- parseInternalFilters
        return (pi,fi)
      parseInternalFilters = do
        try $ sepAfter (string "{")
        fi <- parseFilters
        sepAfter (string "}")
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
parseMemberProcedureFunction n = parsed >>= return . foldr merge empty where
  empty = ([],[],[])
  merge (ms1,ps1,fs1) (ms2,ps2,fs2) = (ms1++ms2,ps1++ps2,fs1++fs2)
  parsed = sepBy anyType optionalSpace
  anyType = labeled "" $ catchUnscopedType <|> singleMember <|> singleProcedure <|> singleFunction
  singleMember = labeled "member" $ do
    m <- sourceParser
    return ([m],[],[])
  singleProcedure = labeled "procedure" $ do
    p <- sourceParser
    return ([],[p],[])
  singleFunction = labeled "function" $ do
    f <- try $ parseScopedFunction parseScope (return n)
    p <- labeled ("definition of function " ++ show (sfName f)) $ sourceParser
    when (sfName f /= epName p) $
      fail $ "expecting definition of function " ++ show (sfName f) ++
             " but got definition of " ++ show (epName p)
    return ([],[p],[f])
  catchUnscopedType = do
    t <- try sourceParser :: Parser ValueType
    fail $ "members must have an explicit @value or @category scope"

parseAnySource :: Parser ([AnyCategory SourcePos],[DefinedCategory SourcePos])
parseAnySource = parsed >>= return . foldr merge empty where
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
