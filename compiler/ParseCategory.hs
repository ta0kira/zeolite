{- -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

module ParseCategory (
  parseFilters,
  parseScope,
  parseScopedFunction,
  singleDefine,
  singleFilter,
  singleRefine,
) where

import Text.Parsec
import Text.Parsec.String

import ParseInstance
import ParserBase
import TypeCategory
import TypeInstance
import TypesBase


instance ParseFromSource (AnyCategory SourcePos) where
  sourceParser = parseValue <|> parseInstance <|> parseConcrete where
    open = sepAfter $ string "{"
    close = sepAfter $ string "}"
    parseValue = labeled "value interface" $ do
      c <- getPosition
      try $ kwValue >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      (rs,vs) <- parseRefinesFilters
      fs <- flip sepBy optionalSpace $ parseScopedFunction (return ValueScope) (return n)
      close
      return $ ValueInterface [c] NoNamespace n ps rs vs fs
    parseInstance = labeled "type interface" $ do
      c <- getPosition
      try $ kwType >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      vs <- parseFilters
      fs <- flip sepBy optionalSpace $ parseScopedFunction (return TypeScope) (return n)
      close
      return $ InstanceInterface [c] NoNamespace n ps vs fs
    parseConcrete = labeled "concrete type" $ do
      c <- getPosition
      try kwConcrete
      n <- sourceParser
      ps <- parseCategoryParams
      open
      (rs,ds,vs) <- parseRefinesDefinesFilters
      fs <- flip sepBy optionalSpace $ parseScopedFunction parseScope (return n)
      close
      return $ ValueConcrete [c] NoNamespace n ps rs ds vs fs

parseCategoryParams :: Parser [ValueParam SourcePos]
parseCategoryParams = do
  (con,inv,cov) <- none <|> try fixedOnly <|> try noFixed <|> try explicitFixed
  return $ map (apply Contravariant) con ++
           map (apply Invariant) inv ++
           map (apply Covariant) cov
  where
    none = do
      notFollowedBy (string "<")
      return ([],[],[])
    fixedOnly = do -- T<a,b,c>
      inv <- between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy singleParam (sepAfter $ string ","))
      return ([],inv,[])
    noFixed = do -- T<a,b|c,d>
      con <- between (sepAfter $ string "<")
                     (sepAfter $ string "|")
                     (sepBy singleParam (sepAfter $ string ","))
      cov <- between nullParse
                     (sepAfter $ string ">")
                     (sepBy singleParam (sepAfter $ string ","))
      return (con,[],cov)
    explicitFixed = do -- T<a,b|c,d|e,f>
      con <- between (sepAfter $ string "<")
                     (sepAfter $ string "|")
                     (sepBy singleParam (sepAfter $ string ","))
      inv <- between nullParse
                     (sepAfter $ string "|")
                     (sepBy singleParam (sepAfter $ string ","))
      cov <- between nullParse
                     (sepAfter $ string ">")
                     (sepBy singleParam (sepAfter $ string ","))
      return (con,inv,cov)
    singleParam = labeled "param declaration" $ do
      c <- getPosition
      n <- sourceParser
      return (c,n)
    apply v (c,n) = ValueParam [c] n v

singleRefine :: Parser (ValueRefine SourcePos)
singleRefine = do
  c <- getPosition
  try kwRefines
  t <- sourceParser
  return $ ValueRefine [c] t

singleDefine :: Parser (ValueDefine SourcePos)
singleDefine = do
  c <- getPosition
  try kwDefines
  t <- sourceParser
  return $ ValueDefine [c] t

singleFilter :: Parser (ParamFilter SourcePos)
singleFilter = try $ do
  c <- getPosition
  n <- sourceParser
  f <- sourceParser
  return $ ParamFilter [c] n f

parseCategoryRefines :: Parser [ValueRefine SourcePos]
parseCategoryRefines = sepAfter $ sepBy singleRefine optionalSpace

parseFilters :: Parser [ParamFilter SourcePos]
parseFilters = sepBy singleFilter optionalSpace

parseRefinesFilters :: Parser ([ValueRefine SourcePos],[ParamFilter SourcePos])
parseRefinesFilters = parsed >>= return . merge2 where
  parsed = sepBy anyType optionalSpace
  anyType = labeled "refine or param filter" $ put12 singleRefine <|> put22 singleFilter

parseRefinesDefinesFilters :: Parser ([ValueRefine SourcePos],
                                      [ValueDefine SourcePos],
                                      [ParamFilter SourcePos])
parseRefinesDefinesFilters = parsed >>= return . merge3 where
  parsed = sepBy anyType optionalSpace
  anyType =
    labeled "refine or define or param filter" $ put13 singleRefine <|> put23 singleDefine <|> put33 singleFilter

instance ParseFromSource FunctionName where
  sourceParser = labeled "function name" $ do
    noKeywords
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ FunctionName (b:e)

parseScopedFunction :: Parser SymbolScope -> Parser CategoryName ->
                       Parser (ScopedFunction SourcePos)
parseScopedFunction sp tp = labeled "function" $ do
  c <- getPosition
  s <- try sp -- Could be a constant, i.e., nothing consumed.
  t <- try tp -- Same here.
  n <- try sourceParser
  ps <- fmap ParamSet $ noParams <|> someParams
  fa <- parseFilters
  as <- fmap ParamSet $ typeList "argument type"
  sepAfter $ string "->"
  rs <- fmap ParamSet $ typeList "return type"
  return $ ScopedFunction [c] n t s as rs ps fa []
  where
    noParams = notFollowedBy (string "<") >> return []
    someParams = between (sepAfter $ string "<")
                         (sepAfter $ string ">")
                         (sepBy singleParam (sepAfter $ string ","))
    singleParam = labeled "param declaration" $ do
      c <- getPosition
      n <- sourceParser
      return $ ValueParam [c] n Invariant
    typeList l = between (sepAfter $ string "(")
                         (sepAfter $ string ")")
                         (sepBy (labeled l $ singleType) (sepAfter $ string ","))
    singleType = do
      c <- getPosition
      t <- sourceParser
      return $ PassedValue [c] t

parseScope :: Parser SymbolScope
parseScope = try categoryScope <|> try typeScope <|> valueScope
categoryScope = kwCategory >> return CategoryScope
typeScope     = kwType     >> return TypeScope
valueScope    = kwValue    >> return ValueScope
