{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseCategory () where

import Text.Parsec
import Text.Parsec.String

import ParseInstance
import ParserBase
import TypeInstance
import TypesBase


data ValueInterface c =
  ValueInterface {
    viContext :: c,
    viName :: TypeName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c]
  }
  deriving (Eq,Show)

instance ParseFromSource (ValueInterface SourcePos) where
  sourceParser = parsed where
    open = sepAfter $ string "{"
    close = sepAfter $ string "}"
    parsed = do
      c <- getPosition
      optional (keyword "value") >> keyword "interface"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      rs <- parseCategoryRefines
      notAllowed parseCategoryDefines "defines not allowed in interfaces"
      notAllowed parseParamFilters    "filters not allowed in interfaces"
      notAllowed parseParamDefines    "param defines not allowed in interfaces"
      close
      return $ ValueInterface c n ps rs


data ValueConcrete c =
  ValueConcrete {
    vcContext :: c,
    vcName :: TypeName,
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcCategoryDefines :: [ValueRefine c],
    vcFilters :: [ParamFilter c],
    vcParamDefines :: [ParamDefines c]
  }
  deriving (Eq,Show)

instance ParseFromSource (ValueConcrete SourcePos) where
  sourceParser = parsed where
    open = sepAfter $ string "{"
    close = sepAfter $ string "}"
    parsed = do
      c <- getPosition
      keyword "concrete"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      -- TODO: Allow arbitrary ordering here?
      rs <- parseCategoryRefines
      cds <- parseCategoryDefines
      fs <- parseParamFilters
      pds <- parseParamDefines
      close
      return $ ValueConcrete c n ps rs cds fs pds


data InstanceInterface c =
  InstanceInterface {
    iiContext :: c,
    iiName :: TypeName,
    iiParams :: [ValueParam c],
    iiRefines :: [ValueRefine c]
  }
  deriving (Eq,Show)

instance ParseFromSource (InstanceInterface SourcePos) where
  sourceParser = parsed where
    open = sepAfter $ string "{"
    close = sepAfter $ string "}"
    parsed = do
      c <- getPosition
      keyword "type" >> keyword "interface"
      n <- sourceParser
      ps <- parseCategoryParams
      open
      rs <- parseCategoryRefines
      notAllowed parseCategoryDefines "defines not allowed in interfaces"
      notAllowed parseParamFilters    "filters not allowed in interfaces"
      notAllowed parseParamDefines    "param defines not allowed in interfaces"
      close
      return $ InstanceInterface c n ps rs


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

data ParamFilter c =
  ParamFilter {
    pfContext :: c,
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }
  deriving (Eq,Show)

data ParamDefines c =
  ParamDefines {
    pdContext :: c,
    pdParam :: ParamName,
    pdDefines :: TypeInstance
  }
  deriving (Eq,Show)


parseCategoryParams :: Parser [ValueParam SourcePos]
parseCategoryParams = parsed where
  parsed = do
    (con,inv,cov) <- none <|> try fixedOnly <|> try noFixed <|> explicitFixed
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

parseParamFilters :: Parser [ParamFilter SourcePos]
parseParamFilters = parsed where
  parsed = sepAfter $ sepBy singleFilter optionalSpace
  singleFilter = try $ do
    c <- getPosition
    n <- sourceParser
    f <- sourceParser
    return $ ParamFilter c n f

parseParamDefines :: Parser [ParamDefines SourcePos]
parseParamDefines = parsed where
  parsed = sepAfter $ sepBy singleDefine optionalSpace
  singleDefine = do
    c <- getPosition
    n <- try $ sourceParser
    try $ keyword "defines"
    notAllowed (sourceParser :: Parser ParamName) "param cannot define another param"
    t <- sourceParser
    return $ ParamDefines c n t
