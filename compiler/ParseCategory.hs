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
      close
      return $ ValueInterface [c] n ps rs vs
    parseInstance = labeled "type interface" $ do
      c <- getPosition
      try $ kwType >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      vs <- parseFilters
      close
      return $ InstanceInterface [c] n ps vs
    parseConcrete = labeled "concrete type" $ do
      c <- getPosition
      try kwConcrete
      n <- sourceParser
      ps <- parseCategoryParams
      open
      (rs,ds,vs) <- parseRefinesDefinesFilters
      close
      return $ ValueConcrete [c] n ps rs ds vs

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
