{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

{-# LANGUAGE Safe #-}

module Parser.Common (
  ParseFromSource(..),
  anyComment,
  assignOperator,
  blockComment,
  builtinValues,
  categorySymbolGet,
  char_,
  endOfDoc,
  escapeStart,
  infixFuncEnd,
  infixFuncStart,
  keyword,
  kwAll,
  kwAllows,
  kwAny,
  kwBreak,
  kwCategory,
  kwCleanup,
  kwConcrete,
  kwContinue,
  kwDefine,
  kwDefines,
  kwElif,
  kwElse,
  kwEmpty,
  kwFail,
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
  kwTestcase,
  kwTrue,
  kwType,
  kwTypename,
  kwTypes,
  kwUpdate,
  kwValue,
  kwWeak,
  kwWhile,
  labeled,
  lineComment,
  merge2,
  merge3,
  noKeywords,
  notAllowed,
  nullParse,
  operator,
  optionalSpace,
  parseBin,
  parseDec,
  parseHex,
  parseOct,
  parseSubOne,
  put12,
  put13,
  put22,
  put23,
  put33,
  regexChar,
  requiredSpace,
  sepAfter,
  sepAfter_,
  sepAfter1,
  string_,
  stringChar,
  statementEnd,
  statementStart,
  typeSymbolGet,
  valueSymbolGet,
) where

import Data.Char
import Data.Foldable
import Data.Functor
import Data.Monoid
import Prelude hiding (foldl,foldr)
import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as Set


class ParseFromSource a where
  -- Should never prune whitespace/comments from front, but always from back.
  sourceParser :: Parser a

labeled :: String -> Parser a -> Parser a
labeled = flip label

escapeStart :: Parser ()
escapeStart = sepAfter (string_ "\\")

statementStart :: Parser ()
statementStart = sepAfter (string_ "\\")

statementEnd :: Parser ()
statementEnd = sepAfter (string_ "")

valueSymbolGet :: Parser ()
valueSymbolGet = sepAfter (string_ ".")

categorySymbolGet :: Parser ()
categorySymbolGet = sepAfter (string_ "$$")

typeSymbolGet :: Parser ()
typeSymbolGet = sepAfter (string_ "$" >> notFollowedBy (string_ "$"))

assignOperator :: Parser ()
assignOperator = operator "<-" >> return ()

infixFuncStart :: Parser ()
infixFuncStart = sepAfter (string_ "`")

infixFuncEnd :: Parser ()
infixFuncEnd = sepAfter (string_ "`")

-- TODO: Maybe this should not use strings.
builtinValues :: Parser String
builtinValues = foldr (<|>) (fail "empty") $ map try [
    kwSelf >> return "self"
  ]

kwAll :: Parser ()
kwAll = keyword "all"

kwAllows :: Parser ()
kwAllows = keyword "allows"

kwAny :: Parser ()
kwAny = keyword "any"

kwBreak :: Parser ()
kwBreak = keyword "break"

kwCategory :: Parser ()
kwCategory = keyword "@category"

kwCleanup :: Parser ()
kwCleanup = keyword "cleanup"

kwConcrete :: Parser ()
kwConcrete = keyword "concrete"

kwContinue :: Parser ()
kwContinue = keyword "continue"

kwDefine :: Parser ()
kwDefine = keyword "define"

kwDefines :: Parser ()
kwDefines = keyword "defines"

kwElif :: Parser ()
kwElif = keyword "elif"

kwElse :: Parser ()
kwElse = keyword "else"

kwEmpty :: Parser ()
kwEmpty = keyword "empty"

kwFail :: Parser ()
kwFail = keyword "fail"

kwFalse :: Parser ()
kwFalse = keyword "false"

kwIf :: Parser ()
kwIf = keyword "if"

kwIn :: Parser ()
kwIn = keyword "in"

kwIgnore :: Parser ()
kwIgnore = keyword "_"

kwInterface :: Parser ()
kwInterface = keyword "interface"

kwOptional :: Parser ()
kwOptional = keyword "optional"

kwPresent :: Parser ()
kwPresent = keyword "present"

kwReduce :: Parser ()
kwReduce = keyword "reduce"

kwRefines :: Parser ()
kwRefines = keyword "refines"

kwRequire :: Parser ()
kwRequire = keyword "require"

kwRequires :: Parser ()
kwRequires = keyword "requires"

kwReturn :: Parser ()
kwReturn = keyword "return"

kwSelf :: Parser ()
kwSelf = keyword "self"

kwScoped :: Parser ()
kwScoped = keyword "scoped"

kwStrong :: Parser ()
kwStrong = keyword "strong"

kwTestcase :: Parser ()
kwTestcase = keyword "testcase"

kwTrue :: Parser ()
kwTrue = keyword "true"

kwType :: Parser ()
kwType = keyword "@type"

kwTypename :: Parser ()
kwTypename = keyword "typename"

kwTypes :: Parser ()
kwTypes = keyword "types"

kwUpdate :: Parser ()
kwUpdate = keyword "update"

kwValue :: Parser ()
kwValue = keyword "@value"

kwWeak :: Parser ()
kwWeak = keyword "weak"

kwWhile :: Parser ()
kwWhile = keyword "while"

operatorSymbol :: Parser Char
operatorSymbol = labeled "operator symbol" $ satisfy (`Set.member` Set.fromList "+-*/%=!<>&|")

isKeyword :: Parser ()
isKeyword = foldr (<|>) nullParse $ map try [
    kwAll,
    kwAllows,
    kwAny,
    kwBreak,
    kwCategory,
    kwCleanup,
    kwConcrete,
    kwContinue,
    kwDefine,
    kwDefines,
    kwElif,
    kwElse,
    kwEmpty,
    kwFail,
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
    kwTestcase,
    kwTrue,
    kwType,
    kwTypename,
    kwTypes,
    kwUpdate,
    kwValue,
    kwWeak,
    kwWhile
  ]

nullParse :: Parser ()
nullParse = return ()

char_ :: Char -> Parser ()
char_ = (>> return ()) . char

string_ :: String -> Parser ()
string_ = (>> return ()) . string

lineComment :: Parser String
lineComment = between (string_ "//")
                      endOfLine
                      (many $ satisfy (/= '\n'))

blockComment :: Parser String
blockComment = between (string_ "/*")
                       (string_ "*/")
                       (many $ notFollowedBy (string_ "*/") >> anyChar)

anyComment :: Parser String
anyComment = try blockComment <|> try lineComment

optionalSpace :: Parser ()
optionalSpace = labeled "" $ many (anyComment <|> many1 space) >> nullParse

requiredSpace :: Parser ()
requiredSpace = labeled "break" $ eof <|> (many1 (anyComment <|> many1 space) >> nullParse)

sepAfter :: Parser a -> Parser a
sepAfter = between nullParse optionalSpace

sepAfter_ :: Parser a -> Parser ()
sepAfter_ = (>> return ()) . between nullParse optionalSpace

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
  string_ o
  notFollowedBy operatorSymbol
  optionalSpace
  return o

stringChar :: Parser Char
stringChar = escaped <|> notEscaped where
  escaped = labeled "escaped char sequence" $ do
    char_ '\\'
    octChar <|> otherEscape where
      otherEscape = do
        v <- anyChar
        case v of
            '\'' -> return '\''
            '"' -> return '"'
            '?' -> return '?'
            '\\' -> return '\\'
            'a' -> return $ chr 7
            'b' -> return $ chr 8
            'f' -> return $ chr 12
            'n' -> return $ chr 10
            'r' -> return $ chr 13
            't' -> return $ chr 9
            'v' -> return $ chr 11
            'x' -> hexChar
            _ -> fail (show v)
      octChar = labeled "3 octal chars" $ do
        o1 <- octDigit >>= return . digitVal
        o2 <- octDigit >>= return . digitVal
        o3 <- octDigit >>= return . digitVal
        return $ chr $ 8*8*o1 + 8*o2 + o3
      hexChar = labeled "2 hex chars" $ do
        h1 <- hexDigit >>= return . digitVal
        h2 <- hexDigit >>= return . digitVal
        return $ chr $ 16*h1 + h2
  notEscaped = noneOf "\""

digitVal :: Char -> Int
digitVal c
  | c >= '0' && c <= '9' = ord(c) - ord('0')
  | c >= 'A' && c <= 'F' = 10 + ord(c) - ord('A')
  | c >= 'a' && c <= 'f' = 10 + ord(c) - ord('a')
  | otherwise = undefined

parseDec :: Parser Integer
parseDec = fmap snd $ parseIntCommon 10 digit

parseHex :: Parser Integer
parseHex = fmap snd $ parseIntCommon 16 hexDigit

parseOct :: Parser Integer
parseOct = fmap snd $ parseIntCommon 8 octDigit

parseBin :: Parser Integer
parseBin = fmap snd $ parseIntCommon 2 (oneOf "01")

parseSubOne :: Parser (Integer,Integer)
parseSubOne = parseIntCommon 10 digit

parseIntCommon :: Integer -> Parser Char -> Parser (Integer,Integer)
parseIntCommon b p = do
  ds <- many1 p
  return $ foldl (\(n,x) y -> (n+1,b*x + (fromIntegral $ digitVal y :: Integer))) (0,0) ds

regexChar :: Parser String
regexChar = escaped <|> notEscaped where
  escaped = do
    char_ '\\'
    v <- anyChar
    case v of
         '"' -> return "\""
         _ -> return ['\\',v]
  notEscaped = fmap (:[]) $ noneOf "\""

put12 :: (Functor m, Monad m) => m a -> m ([a],[b])
put12 = fmap put where put x = ([x],[])

put22 :: (Functor m, Monad m) => m b -> m ([a],[b])
put22 = fmap put where put x = ([],[x])

merge2 :: (Foldable f, Monoid a, Monoid b) => f (a,b) -> (a,b)
merge2 = foldr merge (mempty,mempty) where
  merge (xs1,ys1) (xs2,ys2) = (xs1<>xs2,ys1<>ys2)

put13 :: (Functor m, Monad m) => m a -> m ([a],[b],[c])
put13 = fmap put where put x = ([x],[],[])

put23 :: (Functor m, Monad m) => m b -> m ([a],[b],[c])
put23 = fmap put where put x = ([],[x],[])

put33 :: (Functor m, Monad m) => m c -> m ([a],[b],[c])
put33 = fmap put where put x = ([],[],[x])

merge3 :: (Foldable f, Monoid a, Monoid b, Monoid c) => f (a,b,c) -> (a,b,c)
merge3 = foldr merge (mempty,mempty,mempty) where
  merge (xs1,ys1,zs1) (xs2,ys2,zs2) = (xs1<>xs2,ys1<>ys2,zs1<>zs2)
