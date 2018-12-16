{-# LANGUAGE Safe #-}

module ParserBase (
  ParseFromSource(..),
  anyComment,
  blockComment,
  endOfDoc,
  keyword,
  kwAll,
  kwAllows,
  kwAny,
  kwCategory,
  kwConcrete,
  kwDefines,
  kwInterface,
  kwOptional,
  kwRefines,
  kwRequires,
  kwType,
  kwValue,
  kwWeak,
  labeled,
  lineComment,
  noKeywords,
  notAllowed,
  nullParse,
  optionalSpace,
  requiredSpace,
  sepAfter,
  sepAfter1,
) where

import Text.Parsec
import Text.Parsec.String


class ParseFromSource a where
  -- Should never prune whitespace/comments from front, but always from back.
  sourceParser :: Parser a

labeled = flip label

kwAll = keyword "all"
kwAllows = keyword "allows"
kwAny = keyword "any"
kwCategory = keyword "@category"
kwConcrete = keyword "concrete"
kwDefines = keyword "defines"
kwInterface = keyword "interface"
kwOptional = keyword "optional"
kwRefines = keyword "refines"
kwRequires = keyword "requires"
kwType = keyword "@type"
kwValue = keyword "@value"
kwWeak = keyword "weak"

isKeyword :: Parser ()
isKeyword = foldr (<|>) nullParse $ map try [
    kwAll,
    kwAllows,
    kwAny,
    kwCategory,
    kwConcrete,
    kwDefines,
    kwInterface,
    kwOptional,
    kwRefines,
    kwRequires,
    kwType,
    kwValue,
    kwWeak
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
