{-# LANGUAGE Safe #-}

module ParserBase (
  ParseFromSource(..),
  anyComment,
  blockComment,
  endOfDoc,
  keyword,
  lineComment,
  noKeywords,
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

isKeyword :: Parser ()
isKeyword = foldr (<|>) nullParse $ map (try . keyword) [
    "allows",
    "optional",
    "requires",
    "weak"
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
optionalSpace = many (anyComment <|> many1 space) >> nullParse

requiredSpace :: Parser ()
requiredSpace = eof <|> (many1 (anyComment <|> many1 space) >> nullParse)

sepAfter :: Parser a -> Parser a
sepAfter = between nullParse optionalSpace

sepAfter1 :: Parser a -> Parser a
sepAfter1 = between nullParse requiredSpace

keyword :: String -> Parser ()
keyword s = sepAfter $ string s >> notFollowedBy (many alphaNum)

noKeywords :: Parser ()
noKeywords = notFollowedBy isKeyword

endOfDoc :: Parser ()
endOfDoc = optionalSpace >> eof
