{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseCategory () where

import Text.Parsec
import Text.Parsec.String

import ParseInstance
import ParserBase
import TypeInstance
import TypesBase


data AnyCategory c =
  ValueInterface {
    viContext :: [c],
    viName :: TypeName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c]
  } |
  InstanceInterface {
    iiContext :: [c],
    iiName :: TypeName,
    iiParams :: [ValueParam c]
  } |
  ValueConcrete {
    vcContext :: [c],
    vcName :: TypeName,
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcDefines :: [ValueRefine c],
    vcParamValue :: [ParamValueFilter c],
    vcParamInstance :: [ParamInstanceFilter c]
  }
  deriving (Eq,Show)

instance ParseFromSource (AnyCategory SourcePos) where
  sourceParser = parseValue <|> parseInstance <|> parseConcrete where
    open = sepAfter $ string "{"
    close = sepAfter $ string "}"
    parseValue = do
      c <- getPosition
      try $ keyword "value" >> keyword "interface"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      rs <- parseCategoryRefines
      notAllowed parseRefinesDefinesFilters
                 "defines and filters only allowed in concrete categories"
      close
      return $ ValueInterface [c] n ps rs
    parseInstance = do
      c <- getPosition
      try $ keyword "type" >> keyword "interface"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      notAllowed parseRefinesDefinesFilters
                 "refines, defines and filters only allowed in concrete categories"
      close
      return $ InstanceInterface [c] n ps
    parseConcrete = do
      c <- getPosition
      try $ keyword "concrete"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      (rs,ds,vs,is) <- parseRefinesDefinesFilters
      close
      return $ ValueConcrete [c] n ps rs ds vs is


data DefineConcrete c =
  DefineConcrete {
    dcContext :: [c],
    dcName :: TypeName
  }
  deriving (Eq,Show)

data ValueRefine c =
  ValueRefine {
    vrContext :: [c],
    vrType :: TypeInstance
  }
  deriving (Eq,Show)

data ValueParam c =
  ValueParam {
    vpContext :: [c],
    vpParam :: ParamName,
    vpVariance :: Variance
  }
  deriving (Eq,Show)

data ParamValueFilter c =
  ParamValueFilter {
    pfContext :: [c],
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }
  deriving (Eq,Show)

data ParamInstanceFilter c =
  ParamInstanceFilter {
    pdContext :: [c],
    pdParam :: ParamName,
    pdDefines :: TypeInstance
  }
  deriving (Eq,Show)


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
    try $ keyword "refines"
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
    try $ keyword "refines"
    t <- sourceParser
    return ([ValueRefine [c] t],[],[],[])
  singleDefine = do
    c <- getPosition
    try $ keyword "defines"
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
    try $ keyword "defines"
    notAllowed (sourceParser :: Parser ParamName) $
               "param " ++ show n ++ " cannot define another param"
    t <- sourceParser
    return ([],[],[],[ParamInstanceFilter [c] n t])
