module Unresolved {-(
  unresolvedParser,
  UnresolvedParamFilter(..),
  UnresolvedType(..),
  UnresolvedTypeClass(..),
  UnresolvedTypeParam(..),
)-} where

import Control.Applicative ((<|>))
import Control.Monad.Fix (fix)
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Data.Set as Set

import Variance

class UnresolvedParsable a where
    unresolvedParser :: ReadP a

reservedWords = Set.fromList $ [
    "inherits",
    "requires",
    "interface",
    "concrete"
  ]

checkReserved w =
  if (w `Set.member` reservedWords)
     then pfail
     else return w

parseNull = return () :: ReadP ()
whitespace = satisfy (\c -> c == ' ' || c == '\n' || c == '\t')
upperChar = satisfy (\c -> c >= 'A' && c <= 'Z')
lowerChar = satisfy (\c -> c >= 'a' && c <= 'z')
digit = satisfy (\c -> c >= '0' && c <= '9')
alphaNumChar = upperChar <|> lowerChar <|> digit

typeClassName = do
  c1 <- upperChar
  rest <- many alphaNumChar
  checkReserved (c1:rest)

typeParamName = do
  c1 <- lowerChar
  rest <- many alphaNumChar
  checkReserved (c1:rest)

listOf p = sepBy p (skipSpaces >> string ",")


data UnresolvedTypeClass =
  UnresolvedTypeClass {
    utcName :: String,
    utcParams :: [UnresolvedTypeParam],
    utcInherits :: [UnresolvedType],
    utcFilters :: [UnresolvedParamFilter]
  }
  deriving (Eq, Show)

instance UnresolvedParsable UnresolvedTypeClass where
  unresolvedParser = do
    name <- between skipSpaces parseNull typeClassName
    params <- option [] typeParamList
    skipSpaces >> string "{"
    inherits <- between skipSpaces parseNull $ sepBy singleInherit skipSpaces
    filters <- between skipSpaces parseNull $ sepBy singleFilter skipSpaces
    skipSpaces >> string "}"
    return $ UnresolvedTypeClass {
        utcName = name,
        utcParams = params,
        utcInherits = inherits,
        utcFilters = filters
      }

singleInherit :: ReadP UnresolvedType
singleInherit = do
  between skipSpaces whitespace (string "inherits")
  unresolvedParser :: ReadP UnresolvedType


data UnresolvedParamFilter =
  UnresolvedParamFilter {
    upfName :: String,
    upfType :: UnresolvedType
  }
  deriving (Eq, Show)

singleFilter :: ReadP UnresolvedParamFilter
singleFilter = do
  name <- between skipSpaces parseNull typeParamName
  between whitespace whitespace (string "requires")
  requires <- skipSpaces >> unresolvedParser :: ReadP UnresolvedType
  return $ UnresolvedParamFilter {
      upfName = name,
      upfType = requires
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
      name <- between skipSpaces parseNull typeClassName
      args <- option [] $ between (skipSpaces >> string "<")
                          (skipSpaces >> string ">")
                          (listOf (unresolvedParser :: ReadP UnresolvedType))
      return $ UnresolvedType {
          utTypeClass = name,
          utParamArgs = args
        }
    typeArg = do
      name <- between skipSpaces parseNull typeParamName
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
    (con, fixed, cov) <- between (skipSpaces >> string "<")
                                 (skipSpaces >> string ">")
                                 split
    return $ (map (createParam Contravariant) con) ++
             (map (createParam Invariant) fixed) ++
             (map (createParam Covariant) cov)
  createParam v n = UnresolvedTypeParam {
      utpName = n,
      utpVariance = v
    }
  split = fixedOnly <|> noFixed <|> explicitFixed
  fixedOnly = do -- T<a,b,c>
    fixed <- between skipSpaces parseNull (listOf typeParamName)
    return ([], fixed, [])
  noFixed = do -- T<a,b|c,d>
    con   <- between skipSpaces (skipSpaces >> string "|") (listOf typeParamName)
    cov   <- between skipSpaces parseNull                  (listOf typeParamName)
    return (con, [], cov)
  explicitFixed = do -- T<a,b|c,d|e,f>
    con   <- between skipSpaces (skipSpaces >> string "|") (listOf typeParamName)
    fixed <- between skipSpaces (skipSpaces >> string "|") (listOf typeParamName)
    cov   <- between skipSpaces parseNull                  (listOf typeParamName)
    return (con, fixed, cov)
