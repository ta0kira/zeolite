{-# LANGUAGE Safe #-}

module ParserBase (
  ParseFromSource(..),
  anyComment,
  blockComment,
  builtinFunctions,
  builtinValues,
  endOfDoc,
  initSeparator,
  keyword,
  kwAll,
  kwAllows,
  kwAny,
  kwCategory,
  kwConcrete,
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
  kwScoped,
  kwStrong,
  kwType,
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
  valueSymbolGet,
) where

import Text.Parsec
import Text.Parsec.String


class ParseFromSource a where
  -- Should never prune whitespace/comments from front, but always from back.
  sourceParser :: Parser a

labeled = flip label

statementStart = sepAfter (string "~")
statementEnd   = sepAfter (string "")
valueSymbolGet = sepAfter (string ".")
typeSymbolGet  = sepAfter (string "$")
initSeparator  = sepAfter (string ":")

kwAll = keyword "all"
kwAllows = keyword "allows"
kwAny = keyword "any"
kwCategory = keyword "@category"
kwConcrete = keyword "concrete"
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
kwScoped = keyword "scoped"
kwStrong = keyword "strong"
kwTrue = keyword "true"
kwType = keyword "@type"
kwValue = keyword "@value"
kwWeak = keyword "weak"
kwWhile = keyword "while"

isKeyword :: Parser ()
isKeyword = foldr (<|>) nullParse $ map try [
    kwAll,
    kwAllows,
    kwAny,
    kwCategory,
    kwConcrete,
    kwElif,
    kwElse,
    kwEmpty,
    kwFalse,
    kwIf,
    kwIn,
    kwDefines,
    kwIgnore,
    kwInterface,
    kwOptional,
    kwPresent,
    kwReduce,
    kwRefines,
    kwRequire,
    kwRequires,
    kwReturn,
    kwScoped,
    kwStrong,
    kwTrue,
    kwType,
    kwValue,
    kwWeak,
    kwWhile
  ]

-- TODO: Maybe this should not use strings.
builtinFunctions :: Parser String
builtinFunctions = foldr (<|>) (fail "empty") $ map try [
    kwPresent >> return "present",
    kwReduce >> return "reduce",
    kwRequire >> return "require",
    kwStrong >> return "strong"
  ]

-- TODO: Maybe this should not use strings.
builtinValues :: Parser String
builtinValues = foldr (<|>) (fail "empty") $ map try [
    kwEmpty >> return "empty",
    kwFalse >> return "false",
    kwTrue >> return "true"
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
