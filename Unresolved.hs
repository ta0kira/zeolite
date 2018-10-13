module Unresolved (
  unresolvedParser,
  UnresolvedParamFilter(..),
  UnresolvedType(..),
  UnresolvedTypeClass(..),
  UnresolvedTypeParam(..),
  -- For testing...
  UnresolvedTypeClassFunction
) where

import Control.Applicative ((<|>))
import Control.Monad.Fix (fix)
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Data.Set as Set

import Variance


class UnresolvedParsable a where
    unresolvedParser :: ReadP a


reservedWords = Set.fromList $ [
    "maps",
    "to",
    "inherits",
    "requires",
    "interface",
    "concrete",
    "allows",
    "disallows",
    "missing"
  ]

checkReserved w =
  if (w `Set.member` reservedWords)
     then pfail
     else return w

nullParse = return () :: ReadP ()
whitespace = satisfy (\c -> c == ' ' || c == '\n' || c == '\t')
upperChar = satisfy (\c -> c >= 'A' && c <= 'Z')
lowerChar = satisfy (\c -> c >= 'a' && c <= 'z')
digit = satisfy (\c -> c >= '0' && c <= '9')
alphaNumChar = upperChar <|> lowerChar <|> digit

lineComment = between (skipSpaces >> string "//")
                      (char '\n' >> skipSpaces)
                      (many $ satisfy (/= '\n'))

blockComment = between (skipSpaces >> string "/*")
                       (string "*/" >> skipSpaces)
                       (many $ satisfy $ const True)

comment = lineComment <|> blockComment

separator = skipMany1 whitespace <|> skipMany1 comment

deadSpace = skipSpaces <|> skipMany1 comment

typeClassName = do
  c1 <- upperChar
  rest <- many alphaNumChar
  checkReserved (c1:rest)

typeParamName = do
  c1 <- lowerChar
  rest <- many alphaNumChar
  checkReserved (c1:rest)

functionName = do
  c1 <- lowerChar
  rest <- many alphaNumChar
  checkReserved (c1:rest)

listOf p = sepBy p (deadSpace >> string "," >> deadSpace)


data TypeClassType = InterfaceTypeClass | ConcreteTypeClass deriving (Eq, Show)

data UnresolvedTypeClass =
  UnresolvedTypeClass {
    utcName :: String,
    -- TODO: Use this in type resolution.
    utcType :: TypeClassType,
    utcMissing :: Missingness,
    utcParams :: [UnresolvedTypeParam],
    utcInherits :: [UnresolvedType],
    utcFilters :: [UnresolvedParamFilter],
    utcFunctions :: [UnresolvedTypeClassFunction]
  }
  deriving (Eq, Show)

instance UnresolvedParsable UnresolvedTypeClass where
  unresolvedParser = do
    classType <- interfaceType <|> concreteType
    name <- between deadSpace nullParse typeClassName
    params <- option [] typeParamList
    between deadSpace deadSpace (string "{")
    missing <- allowsMissing <|> return DisallowsMissing
    inherits <- sepBy singleInherit deadSpace
    filters <- sepBy (singleFilter <|> singleMissing) deadSpace
    functions <- sepBy (unresolvedParser :: ReadP UnresolvedTypeClassFunction) deadSpace
    between deadSpace deadSpace (string "}")
    return $ UnresolvedTypeClass {
        utcName = name,
        utcType = classType,
        utcMissing = missing,
        utcParams = params,
        utcInherits = inherits,
        utcFilters = filters,
        utcFunctions = functions
      }

interfaceType = do
  between deadSpace separator (string "interface")
  return InterfaceTypeClass

concreteType = do
  between deadSpace separator (string "concrete")
  return ConcreteTypeClass

allowsMissing = do
  between deadSpace separator (string "allows")
  between nullParse deadSpace (string "missing")
  return AllowsMissing

requiresMissing = do
  between deadSpace separator (string "allows")
  between nullParse deadSpace (string "missing")
  return RequiresMissing

disallowsMissing = do
  between deadSpace separator (string "disallows")
  between nullParse deadSpace (string "missing")
  return DisallowsMissing

singleInherit :: ReadP UnresolvedType
singleInherit = do
  between deadSpace separator (string "inherits")
  unresolvedParser :: ReadP UnresolvedType


data UnresolvedParamFilter =
  UnresolvedParamFilter {
    upfName :: String,
    upfType :: UnresolvedType
  } |
  UnresolvedParamMissing {
    upmName :: String,
    upmMissing :: Missingness
  }
  deriving (Eq, Show)

singleFilter :: ReadP UnresolvedParamFilter
singleFilter = do
  deadSpace
  name <- typeParamName
  between separator separator (string "requires")
  requires <- unresolvedParser :: ReadP UnresolvedType
  return $ UnresolvedParamFilter {
      upfName = name,
      upfType = requires
    }

singleMissing :: ReadP UnresolvedParamFilter
singleMissing = do
  deadSpace
  name <- typeParamName
  missing <- requiresMissing <|> disallowsMissing
  return $ UnresolvedParamMissing {
      upmName = name,
      upmMissing = missing
    }


data UnresolvedType =
  UnresolvedType {
    utTypeClass :: String,
    utParamArgs :: [UnresolvedType]
  } |
  UnresolvedTypeArg {
    utaName :: String
  }
  deriving (Eq, Show)

instance UnresolvedParsable UnresolvedType where
  unresolvedParser = typeInstance <|> typeArg where
    typeInstance = do
      name <- between deadSpace nullParse typeClassName
      args <- option [] $ between (deadSpace >> string "<")
                          (deadSpace >> string ">")
                          (listOf (unresolvedParser :: ReadP UnresolvedType))
      return $ UnresolvedType {
          utTypeClass = name,
          utParamArgs = args
        }
    typeArg = do
      name <- between deadSpace nullParse typeParamName
      return $ UnresolvedTypeArg {
          utaName = name
        }

data UnresolvedTypeParam =
  UnresolvedTypeParam {
    utpName :: String,
    utpVariance :: Variance
  }
  deriving (Eq, Show)

typeParamList = types where
  types = do
    (con, fixed, cov) <- between (deadSpace >> string "<")
                                 (deadSpace >> string ">")
                                 split
    return $ (map (createParam Contravariant) con)   ++
             (map (createParam Invariant)     fixed) ++
             (map (createParam Covariant)     cov)
  createParam v n = UnresolvedTypeParam {
      utpName = n,
      utpVariance = v
    }
  split = fixedOnly <|> noFixed <|> explicitFixed
  fixedOnly = do -- T<a,b,c>
    fixed <- between deadSpace nullParse (listOf typeParamName)
    return ([], fixed, [])
  noFixed = do -- T<a,b|c,d>
    con   <- between deadSpace (deadSpace >> string "|") (listOf typeParamName)
    cov   <- between deadSpace nullParse                 (listOf typeParamName)
    return (con, [], cov)
  explicitFixed = do -- T<a,b|c,d|e,f>
    con   <- between deadSpace (deadSpace >> string "|") (listOf typeParamName)
    fixed <- between deadSpace (deadSpace >> string "|") (listOf typeParamName)
    cov   <- between deadSpace nullParse                 (listOf typeParamName)
    return (con, fixed, cov)

data UnresolvedTypeClassFunction =
  UnresolvedTypeClassFunction {
    utcfName :: String,
    utcfParams :: [UnresolvedTypeParam],
    utcfFilters :: [UnresolvedParamFilter],
    utcfArgs :: [UnresolvedType],
    utcfReturns :: [UnresolvedType]
  }
  deriving (Eq, Show)

instance UnresolvedParsable UnresolvedTypeClassFunction where
  unresolvedParser = function where
    function = do
      name <- between deadSpace nullParse functionName
      params <- option [] functionParamList
      args <- getArgs
      returns <- getReturns
      filters <- sepBy (singleFilter <|> singleMissing) deadSpace
      return $ UnresolvedTypeClassFunction {
          utcfName = name,
          utcfParams = map (\n -> UnresolvedTypeParam n IgnoreVariance) params,
          utcfFilters = filters,
          utcfArgs = args,
          utcfReturns= returns
        }
    functionParamList = between (deadSpace >> string "<")
                                (deadSpace >> string ">")
                                getParams
    getParams = between deadSpace nullParse (listOf typeParamName)
    getArgs = between deadSpace nullParse (string "maps" >> collectTypes)
    getReturns = between deadSpace nullParse (string "to" >> collectTypes)
    collectTypes = between (deadSpace >> string "(")
                           (string ")")
                           (listOf unresolvedParser :: ReadP [UnresolvedType])
