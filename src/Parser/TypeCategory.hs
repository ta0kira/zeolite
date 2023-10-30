{- -----------------------------------------------------------------------------
Copyright 2019,2021,2023 Kevin P. Barry

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

module Parser.TypeCategory (
  parseFilters,
  parseScope,
  parseScopedFunction,
  singleDefine,
  singleFilter,
  singleRefine,
) where

import Base.Positional
import Parser.Common
import Parser.TextParser
import Parser.TypeInstance ()
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


instance ParseFromSource (AnyCategory SourceContext) where
  sourceParser = parseValue <|> parseInstance <|> parseConcrete where
    open = sepAfter (string_ "{")
    close = sepAfter (string_ "}")
    parseValue = labeled "value interface" $ do
      c <- getSourceContext
      try $ kwValue >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      pg <- pragmas
      rs <- parseCategoryRefines
      fs <- flip sepBy optionalSpace $ parseScopedFunction (return ValueScope) (return n)
      close
      return $ ValueInterface [c] NoNamespace n pg ps rs fs
    parseInstance = labeled "type interface" $ do
      c <- getSourceContext
      try $ kwType >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      pg <- pragmas
      fs <- flip sepBy optionalSpace $ parseScopedFunction (return TypeScope) (return n)
      close
      return $ InstanceInterface [c] NoNamespace n pg ps fs
    parseConcrete = labeled "concrete type" $ do
      c <- getSourceContext
      kwConcrete
      n <- sourceParser
      ps <- parseCategoryParams
      open
      pg <- pragmas
      (rs,ds,vs) <- parseRefinesDefinesFilters
      fs <- flip sepBy optionalSpace $ parseScopedFunction parseScope (return n)
      close
      return $ ValueConcrete [c] NoNamespace n pg ps rs ds vs fs
    pragmas = fmap (:[]) immutable <|> return []
    immutable = do
      c <- getSourceContext
      kwImmutable
      return $ CategoryImmutable [c]

parseCategoryParams :: TextParser [ValueParam SourceContext]
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
      inv <- between (sepAfter $ string_ "<")
                     (sepAfter $ string_ ">")
                     (sepBy singleParam (sepAfter $ string_ ","))
      return ([],inv,[])
    noFixed = do -- T<a,b|c,d>
      con <- between (sepAfter $ string_ "<")
                     (sepAfter $ string_ "|")
                     (sepBy singleParam (sepAfter $ string_ ","))
      cov <- between nullParse
                     (sepAfter $ string_ ">")
                     (sepBy singleParam (sepAfter $ string_ ","))
      return (con,[],cov)
    explicitFixed = do -- T<a,b|c,d|e,f>
      con <- between (sepAfter $ string_ "<")
                     (sepAfter $ string_ "|")
                     (sepBy singleParam (sepAfter $ string_ ","))
      inv <- between nullParse
                     (sepAfter $ string_ "|")
                     (sepBy singleParam (sepAfter $ string_ ","))
      cov <- between nullParse
                     (sepAfter $ string_ ">")
                     (sepBy singleParam (sepAfter $ string_ ","))
      return (con,inv,cov)
    singleParam = labeled "param declaration" $ do
      noParamSelf
      c <- getSourceContext
      n <- sourceParser
      return (c,n)
    apply v (c,n) = ValueParam [c] n v

singleRefine :: TextParser (ValueRefine SourceContext)
singleRefine = do
  c <- getSourceContext
  kwRefines
  t <- sourceParser
  return $ ValueRefine [c] t

singleDefine :: TextParser (ValueDefine SourceContext)
singleDefine = do
  c <- getSourceContext
  kwDefines
  t <- sourceParser
  return $ ValueDefine [c] t

singleFilter :: TextParser (ParamFilter SourceContext)
singleFilter = do
  c <- getSourceContext
  noParamSelf
  n <- sourceParser
  f <- sourceParser
  return $ ParamFilter [c] n f

parseCategoryRefines :: TextParser [ValueRefine SourceContext]
parseCategoryRefines = sepAfter $ sepBy singleRefine optionalSpace

parseFilters :: TextParser [ParamFilter SourceContext]
parseFilters = sepBy singleFilter optionalSpace

parseRefinesDefinesFilters ::
  TextParser ([ValueRefine SourceContext],[ValueDefine SourceContext],[ParamFilter SourceContext])
parseRefinesDefinesFilters = parsed >>= return . merge3 where
  parsed = sepBy anyType optionalSpace
  anyType =
    labeled "refine or define or param filter" $ put13 singleRefine <|> put23 singleDefine <|> put33 singleFilter

instance ParseFromSource FunctionName where
  sourceParser = labeled "function name" $ do
    noKeywords
    b <- lowerChar
    e <- sepAfter $ many alphaNumChar
    return $ FunctionName (b:e)

instance ParseFromSource (CallArgLabel SourceContext) where
  sourceParser = labeled "arg label" $ do
    c <- getSourceContext
    b <- lowerChar
    e <- many alphaNumChar
    sepAfter $ char_ ':'
    return $ CallArgLabel [c] (b:e ++ ":")

parseScopedFunction ::
  TextParser SymbolScope -> TextParser CategoryName -> TextParser (ScopedFunction SourceContext)
parseScopedFunction sp tp = labeled "function" $ do
  c <- getSourceContext
  (s,t,n) <- try parseName
  ps <- fmap Positional $ noParams <|> someParams
  fa <- parseFilters
  as <- fmap Positional $ typeList "arg label" singleArg
  sepAfter_ (string "->")
  rs <- fmap Positional $ typeList "return type" singleReturn
  return $ ScopedFunction [c] n t s as rs ps fa []
  where
    parseName = do
      s <- sp -- Could be a constant, i.e., nothing consumed.
      t <- tp -- Same here.
      n <- sourceParser
      return (s,t,n)
    noParams = notFollowedBy (string "<") >> return []
    someParams = between (sepAfter $ string_ "<")
                         (sepAfter $ string_ ">")
                         (sepBy singleParam (sepAfter $ string ","))
    singleParam = labeled "param declaration" $ do
      noParamSelf
      c <- getSourceContext
      n <- sourceParser
      return $ ValueParam [c] n Invariant
    typeList l parseVal = between (sepAfter $ string_ "(")
                                  (sepAfter $ string_ ")")
                                  (sepBy (labeled l $ parseVal) (sepAfter $ string ","))
    singleArg = do
      c <- getSourceContext
      t <- sourceParser
      optionalSpace
      n <- fmap Just sourceParser <|> return Nothing
      return $ (PassedValue [c] t,n)
    singleReturn = do
      c <- getSourceContext
      t <- sourceParser
      return $ PassedValue [c] t

parseScope :: TextParser SymbolScope
parseScope = try categoryScope <|> try typeScope <|> valueScope

categoryScope :: TextParser SymbolScope
categoryScope = kwCategory >> return CategoryScope

typeScope :: TextParser SymbolScope
typeScope = kwType >> return TypeScope

valueScope :: TextParser SymbolScope
valueScope = kwValue >> return ValueScope
