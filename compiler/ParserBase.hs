{-# LANGUAGE Safe #-}

module ParserBase (
  ParseFromSource(..),
  anyComment,
  assignOperator,
  blockComment,
  binaryOperator,
  builtinValues,
  categorySymbolGet,
  endOfDoc,
  initSeparator,
  keyword,
  kwAll,
  kwAllows,
  kwAny,
  kwBreak,
  kwCategory,
  kwConcrete,
  kwDefine,
  kwDefines,
  kwElif,
  kwElse,
  kwEmpty,
  kwIf,
  kwIn,
  kwIgnore,
  kwInterface,
  kwOptional,
  kwPresent,
  kwReduce,
  kwRefines,
  kwRequire,
  kwRequires,
  kwReturn,
  kwSelf,
  kwScoped,
  kwStrong,
  kwType,
  kwTypes,
  kwValue,
  kwWeak,
  kwWhile,
  labeled,
  lineComment,
  noKeywords,
  notAllowed,
  nullParse,
  optionalSpace,
  requiredSpace,
  sepAfter,
  sepAfter1,
  statementEnd,
  statementStart,
  typeSymbolGet,
  unaryOperator,
  valueSymbolGet,
) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as Set


class ParseFromSource a where
  -- Should never prune whitespace/comments from front, but always from back.
  sourceParser :: Parser a

labeled = flip label

statementStart    = sepAfter (string "~")
statementEnd      = sepAfter (string "")
valueSymbolGet    = sepAfter (string ".")
categorySymbolGet = sepAfter (string "$$")
typeSymbolGet     = sepAfter (string "$" >> notFollowedBy (string "$"))
initSeparator     = sepAfter (string ":")
assignOperator    = operator "<-"

-- TODO: Maybe this should not use strings.
builtinValues :: Parser String
builtinValues = foldr (<|>) (fail "empty") $ map try [
    kwEmpty >> return "empty",
    kwFalse >> return "false",
    kwSelf >> return "self",
    kwTrue >> return "true"
  ]

-- TODO: Maybe this should not use strings.
unaryOperator :: Parser String
unaryOperator =
  labeled "unary operator" $ foldr (<|>) (fail "empty") $ map (try . operator) [
      "!", "-"
    ]

-- TODO: Maybe this should not use strings.
binaryOperator :: Parser String
binaryOperator =
  labeled "binary operator" $ foldr (<|>) (fail "empty") $ map (try . operator) [
      "+","-","*","/","%","==","!=","<","<=",">",">=","&&","||"
    ]

kwAll = keyword "all"
kwAllows = keyword "allows"
kwAny = keyword "any"
kwBreak = keyword "break"
kwCategory = keyword "@category"
kwConcrete = keyword "concrete"
kwDefine = keyword "define"
kwDefines = keyword "defines"
kwElif = keyword "elif"
kwElse = keyword "else"
kwEmpty = keyword "empty"
kwFalse = keyword "false"
kwIf = keyword "if"
kwIn = keyword "in"
kwIgnore = keyword "_"
kwInterface = keyword "interface"
kwOptional = keyword "optional"
kwPresent = keyword "present"
kwReduce = keyword "reduce"
kwRefines = keyword "refines"
kwRequire = keyword "require"
kwRequires = keyword "requires"
kwReturn = keyword "return"
kwSelf = keyword "self"
kwScoped = keyword "scoped"
kwStrong = keyword "strong"
kwTrue = keyword "true"
kwType = keyword "@type"
kwTypes = keyword "types"
kwValue = keyword "@value"
kwWeak = keyword "weak"
kwWhile = keyword "while"

operatorSymbol = labeled "operator symbol" $ satisfy (`Set.member` Set.fromList "+-*/%=!<>&|")

isKeyword :: Parser ()
isKeyword = foldr (<|>) nullParse $ map try [
    kwAll,
    kwAllows,
    kwAny,
    kwBreak,
    kwCategory,
    kwConcrete,
    kwDefine,
    kwDefines,
    kwElif,
    kwElse,
    kwEmpty,
    kwFalse,
    kwIf,
    kwIn,
    kwIgnore,
    kwInterface,
    kwOptional,
    kwPresent,
    kwReduce,
    kwRefines,
    kwRequire,
    kwRequires,
    kwReturn,
    kwSelf,
    kwScoped,
    kwStrong,
    kwTrue,
    kwType,
    kwTypes,
    kwValue,
    kwWeak,
    kwWhile
  ]

nullParse :: Parser ()
nullParse = return ()

lineComment :: Parser String
lineComment = between (string "//")
                      endOfLine
                      (many $ satisfy (/= '\n'))

blockComment :: Parser String
blockComment = between (string "/*")
                       (string "*/")
                       (many $ notFollowedBy (string "*/") >> anyChar)

anyComment :: Parser String
anyComment = try blockComment <|> try lineComment

optionalSpace :: Parser ()
optionalSpace = labeled "" $ many (anyComment <|> many1 space) >> nullParse

requiredSpace :: Parser ()
requiredSpace = labeled "break" $ eof <|> (many1 (anyComment <|> many1 space) >> nullParse)

sepAfter :: Parser a -> Parser a
sepAfter = between nullParse optionalSpace

sepAfter1 :: Parser a -> Parser a
sepAfter1 = between nullParse requiredSpace

keyword :: String -> Parser ()
keyword s = labeled s $ sepAfter $ string s >> (labeled "" $ notFollowedBy (many alphaNum))

noKeywords :: Parser ()
noKeywords = notFollowedBy isKeyword

endOfDoc :: Parser ()
endOfDoc = labeled "" $ optionalSpace >> eof

notAllowed :: Parser a -> String -> Parser ()
-- Based on implementation of notFollowedBy.
notAllowed p s = (try p >> fail s) <|> return ()

operator :: String -> Parser String
operator o = labeled o $ do
  string o
  notFollowedBy operatorSymbol
  optionalSpace
  return o
