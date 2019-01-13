{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseCategory (
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
      return $ ValueInterface [c] n ps rs vs fs
    parseInstance = labeled "type interface" $ do
      c <- getPosition
      try $ kwType >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      vs <- parseFilters
      fs <- flip sepBy optionalSpace $ parseScopedFunction (return TypeScope) (return n)
      close
      return $ InstanceInterface [c] n ps vs fs
    parseConcrete = labeled "concrete type" $ do
      c <- getPosition
      try kwConcrete
      n <- sourceParser
      ps <- parseCategoryParams
      open
      (rs,ds,vs) <- parseRefinesDefinesFilters
      fs <- flip sepBy optionalSpace $ parseScopedFunction parseScope (return n)
      close
      return $ ValueConcrete [c] n ps rs ds vs fs
    parseScope = try categoryScope <|> try typeScope <|> valueScope
    categoryScope = kwCategory >> return CategoryScope
    typeScope     = kwType     >> return TypeScope
    valueScope    = kwValue    >> return ValueScope

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

parseCategoryRefines :: Parser [ValueRefine SourcePos]
parseCategoryRefines = sepAfter $ sepBy singleRefine optionalSpace where
  singleRefine = do
    c <- getPosition
    try kwRefines
    t <- sourceParser
    return $ ValueRefine [c] t

parseFilters :: Parser [ParamFilter SourcePos]
parseFilters = sepBy singleFilter optionalSpace where
  singleFilter = labeled "param filter" $ try $ do
    c <- getPosition
    n <- sourceParser
    f <- sourceParser
    return $ ParamFilter [c] n f

parseRefinesFilters :: Parser ([ValueRefine SourcePos],[ParamFilter SourcePos])
parseRefinesFilters = parsed >>= return . foldr merge empty where
  empty = ([],[])
  merge (rs1,vs1) (rs2,vs2) = (rs1++rs2,vs1++vs2)
  parsed = sepBy anyType optionalSpace
  anyType = labeled "refine or param filter" $ singleRefine <|> singleFilter
  singleRefine = do
    c <- getPosition
    try kwRefines
    t <- sourceParser
    return ([ValueRefine [c] t],[])
  singleFilter = try $ do
    c <- getPosition
    n <- sourceParser
    f <- sourceParser
    return ([],[ParamFilter [c] n f])

parseRefinesDefinesFilters :: Parser ([ValueRefine SourcePos],
                                      [ValueDefine SourcePos],
                                      [ParamFilter SourcePos])
parseRefinesDefinesFilters = parsed >>= return . foldr merge empty where
  empty = ([],[],[])
  merge (rs1,ds1,vs1) (rs2,ds2,vs2) = (rs1++rs2,ds1++ds2,vs1++vs2)
  parsed = sepBy anyType optionalSpace
  anyType =
    labeled "refine or define or param filter" $ singleRefine <|> singleDefine <|> singleFilter
  singleRefine = do
    c <- getPosition
    try kwRefines
    t <- sourceParser
    return ([ValueRefine [c] t],[],[])
  singleDefine = do
    c <- getPosition
    try kwDefines
    t <- sourceParser
    return ([],[ValueDefine [c] t],[])
  singleFilter = try $ do
    c <- getPosition
    n <- sourceParser
    f <- sourceParser
    return ([],[],[ParamFilter [c] n f])

instance ParseFromSource FunctionName where
  sourceParser = labeled "function name" $ do
    noKeywords
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ FunctionName (b:e)

parseScopedFunction :: Parser SymbolScope -> Parser TypeName ->
                       Parser (ScopedFunction SourcePos)
parseScopedFunction sp tp = labeled "function" $ do
  c <- getPosition
  s <- try sp -- Could be a constant, i.e., nothing consumed.
  t <- try tp -- Same here.
  n <- try sourceParser
  ps <- noParams <|> someParams
  fa <- parseFilters
  as <- typeList "argument type"
  sepAfter $ string "->"
  rs <- typeList "return type"
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
