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
    viContext :: c,
    viName :: TypeName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c]
  } |
  InstanceInterface {
    iiContext :: c,
    iiName :: TypeName,
    iiParams :: [ValueParam c],
    iiRefines :: [ValueRefine c]
  } |
  ValueConcrete {
    vcContext :: c,
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
      try $ optional (keyword "value") >> keyword "interface"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      rs <- parseCategoryRefines
      notAllowed parseCategoryDefines      "defines not allowed in interfaces"
      notAllowed parseParamValueFilters    "filters not allowed in interfaces"
      notAllowed parseParamInstanceFilters "filters not allowed in interfaces"
      close
      return $ ValueInterface c n ps rs
    parseInstance = do
      c <- getPosition
      try $ keyword "type" >> keyword "interface"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      rs <- parseCategoryRefines
      notAllowed parseCategoryDefines      "defines not allowed in interfaces"
      notAllowed parseParamValueFilters    "filters not allowed in interfaces"
      notAllowed parseParamInstanceFilters "filters not allowed in interfaces"
      close
      return $ InstanceInterface c n ps rs
    parseConcrete = do
      c <- getPosition
      try $ keyword "concrete"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      -- TODO: Allow arbitrary ordering here?
      rs <- parseCategoryRefines
      ds <- parseCategoryDefines
      vs <- parseParamValueFilters
      is <- parseParamInstanceFilters
      close
      return $ ValueConcrete c n ps rs ds vs is


data DefineConcrete c =
  DefineConcrete {
    dcContext :: c,
    dcName :: TypeName
  }
  deriving (Eq,Show)

data ValueRefine c =
  ValueRefine {
    vrContext :: c,
    vrType :: TypeInstance
  }
  deriving (Eq,Show)

data ValueParam c =
  ValueParam {
    vpContext :: c,
    vpParam :: ParamName,
    vpVariance :: Variance
  }
  deriving (Eq,Show)

data ParamValueFilter c =
  ParamValueFilter {
    pfContext :: c,
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }
  deriving (Eq,Show)

data ParamInstanceFilter c =
  ParamInstanceFilter {
    pdContext :: c,
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
  apply v (c,n) = ValueParam c n v

parseCategoryRefines :: Parser [ValueRefine SourcePos]
parseCategoryRefines = parsed where
  parsed = sepAfter $ sepBy singleRefine optionalSpace
  singleRefine = do
    c <- getPosition
    try $ keyword "refines"
    t <- sourceParser
    return $ ValueRefine c t

parseCategoryDefines :: Parser [ValueRefine SourcePos]
parseCategoryDefines = parsed where
  parsed = sepAfter $ sepBy singleDefine optionalSpace
  singleDefine = do
    c <- getPosition
    try $ keyword "defines"
    t <- sourceParser
    return $ ValueRefine c t

parseParamValueFilters :: Parser [ParamValueFilter SourcePos]
parseParamValueFilters = parsed where
  parsed = sepAfter $ sepBy singleFilter optionalSpace
  singleFilter = try $ do
    c <- getPosition
    n <- sourceParser
    f <- sourceParser
    return $ ParamValueFilter c n f

parseParamInstanceFilters :: Parser [ParamInstanceFilter SourcePos]
parseParamInstanceFilters = parsed where
  parsed = sepAfter $ sepBy singleFilter optionalSpace
  singleFilter = do
    c <- getPosition
    n <- try $ sourceParser
    try $ keyword "defines"
    notAllowed (sourceParser :: Parser ParamName) $
               "param " ++ show n ++ " cannot define another param"
    t <- sourceParser
    return $ ParamInstanceFilter c n t
