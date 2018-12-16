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
      rs <- parseCategoryRefines
      notAllowed parseRefinesDefinesFilters
                 "defines and filters not allowed in value interfaces"
      close
      return $ ValueInterface [c] n ps rs
    parseInstance = labeled "type interface" $ do
      c <- getPosition
      try $ kwType >> kwInterface
      n <- sourceParser
      ps <- parseCategoryParams
      open
      notAllowed parseRefinesDefinesFilters
                 "refines, defines and filters not allowed in type interfaces"
      close
      return $ InstanceInterface [c] n ps
    parseConcrete = labeled "concrete type" $ do
      c <- getPosition
      try kwConcrete
      n <- sourceParser
      ps <- parseCategoryParams
      open
      (rs,ds,vs,is) <- parseRefinesDefinesFilters
      close
      return $ ValueConcrete [c] n ps rs ds vs is

parseCategoryParams :: Parser [ValueParam SourcePos]
parseCategoryParams = parsed where
  parsed = do
    (con,inv,cov) <- none <|> try fixedOnly <|> try noFixed <|> try explicitFixed
    return $ map (apply Contravariant) con ++
             map (apply Invariant) inv ++
             map (apply Covariant) cov
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
parseCategoryRefines = parsed where
  parsed = sepAfter $ sepBy singleRefine optionalSpace
  singleRefine = do
    c <- getPosition
    try kwRefines
    t <- sourceParser
    return $ ValueRefine [c] t

parseRefinesDefinesFilters :: Parser ([ValueRefine SourcePos],
                                      [ValueRefine SourcePos],
                                      [ParamValueFilter SourcePos],
                                      [ParamInstanceFilter SourcePos])
parseRefinesDefinesFilters = parsed >>= return . foldr merge empty where
  empty = ([],[],[],[])
  merge (rs1,ds1,vs1,is1) (rs2,ds2,vs2,is2) = (rs1++rs2,ds1++ds2,vs1++vs2,is1++is2)
  parsed = sepBy anyType optionalSpace
  anyType = labeled "" $ singleRefine <|> singleDefine <|> singleValue <|> singleInstance
  singleRefine = do
    c <- getPosition
    try kwRefines
    t <- sourceParser
    return ([ValueRefine [c] t],[],[],[])
  singleDefine = do
    c <- getPosition
    try kwDefines
    t <- sourceParser
    return ([],[ValueRefine [c] t],[],[])
  singleValue = try $ do
    c <- getPosition
    n <- sourceParser
    f <- sourceParser
    return ([],[],[ParamValueFilter [c] n f],[])
  singleInstance = do
    c <- getPosition
    n <- try $ sourceParser
    try kwDefines
    notAllowed (sourceParser :: Parser ParamName) $
               "param " ++ show n ++ " cannot define another param"
    t <- sourceParser
    return ([],[],[],[ParamInstanceFilter [c] n t])
