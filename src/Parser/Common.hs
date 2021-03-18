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

{-# LANGUAGE FlexibleInstances #-}

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
  inferredParam,
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
  kwIgnore,
  kwIn,
  kwInterface,
  kwOptional,
  kwPresent,
  kwReduce,
  kwRefines,
  kwRequire,
  kwRequires,
  kwReturn,
  kwScoped,
  kwSelf,
  kwStrong,
  kwTestcase,
  kwTrue,
  kwType,
  kwTypename,
  kwTypes,
  kwUnittest,
  kwUpdate,
  kwValue,
  kwWeak,
  kwWhile,
  labeled,
  lineComment,
  lineEnd,
  merge2,
  merge3,
  noKeywords,
  noParamSelf,
  notAllowed,
  nullParse,
  operator,
  optionalSpace,
  paramSelf,
  parseAny2,
  parseAny3,
  parseBin,
  parseDec,
  parseHex,
  parseOct,
  parseSubOne,
  pragmaArgsEnd,
  pragmaArgsStart,
  pragmaEnd,
  pragmaStart,
  put12,
  put13,
  put22,
  put23,
  put33,
  quotedString,
  regexChar,
  sepAfter,
  sepAfter_,
  statementEnd,
  statementStart,
  stringChar,
  string_,
  typeSymbolGet,
  valueSymbolGet,
) where

import Data.Char
import Data.Foldable
import Data.Functor
import Data.Monoid
import Prelude hiding (foldl,foldr)
import qualified Data.Set as Set

import Base.CompilerError
import Parser.TextParser
import Types.TypeInstance (ParamName(ParamSelf))


class ParseFromSource a where
  -- Always prune whitespace and comments from back, but never from front!
  sourceParser :: TextParser a

labeled :: String -> TextParser a -> TextParser a
labeled = label

escapeStart :: TextParser ()
escapeStart = sepAfter (string_ "\\")

statementStart :: TextParser ()
statementStart = sepAfter (string_ "\\")

statementEnd :: TextParser ()
statementEnd = sepAfter (string_ "")

valueSymbolGet :: TextParser ()
valueSymbolGet = sepAfter (string_ ".")

categorySymbolGet :: TextParser ()
categorySymbolGet = labeled ":" $ useNewOperators <|> sepAfter (string_ ":")

typeSymbolGet :: TextParser ()
typeSymbolGet = labeled "." $ useNewOperators <|> sepAfter (string_ ".")

-- TODO: Remove this after a reasonable amount of time.
useNewOperators :: TextParser ()
useNewOperators = newCategory <|> newType where
  newCategory = do
    string_ "$$"
    compilerErrorM "use \":\" instead of \"$$\" to call @category functions"
  newType = do
    string_ "$"
    compilerErrorM "use \".\" instead of \"$\" to call @type functions"

assignOperator :: TextParser ()
assignOperator = operator "<-" >> return ()

infixFuncStart :: TextParser ()
infixFuncStart = sepAfter (string_ "`")

infixFuncEnd :: TextParser ()
infixFuncEnd = sepAfter (string_ "`")

-- TODO: Maybe this should not use strings.
builtinValues :: TextParser String
builtinValues = foldr (<|>) empty $ map try [
    kwSelf >> return "self"
  ]

kwAll :: TextParser ()
kwAll = keyword "all"

kwAllows :: TextParser ()
kwAllows = keyword "allows"

kwAny :: TextParser ()
kwAny = keyword "any"

kwBreak :: TextParser ()
kwBreak = keyword "break"

kwCategory :: TextParser ()
kwCategory = keyword "@category"

kwCleanup :: TextParser ()
kwCleanup = keyword "cleanup"

kwConcrete :: TextParser ()
kwConcrete = keyword "concrete"

kwContinue :: TextParser ()
kwContinue = keyword "continue"

kwDefine :: TextParser ()
kwDefine = keyword "define"

kwDefines :: TextParser ()
kwDefines = keyword "defines"

kwElif :: TextParser ()
kwElif = keyword "elif"

kwElse :: TextParser ()
kwElse = keyword "else"

kwEmpty :: TextParser ()
kwEmpty = keyword "empty"

kwFail :: TextParser ()
kwFail = keyword "fail"

kwFalse :: TextParser ()
kwFalse = keyword "false"

kwIf :: TextParser ()
kwIf = keyword "if"

kwIn :: TextParser ()
kwIn = keyword "in"

kwIgnore :: TextParser ()
kwIgnore = keyword "_"

kwInterface :: TextParser ()
kwInterface = keyword "interface"

kwOptional :: TextParser ()
kwOptional = keyword "optional"

kwPresent :: TextParser ()
kwPresent = keyword "present"

kwReduce :: TextParser ()
kwReduce = keyword "reduce"

kwRefines :: TextParser ()
kwRefines = keyword "refines"

kwRequire :: TextParser ()
kwRequire = keyword "require"

kwRequires :: TextParser ()
kwRequires = keyword "requires"

kwReturn :: TextParser ()
kwReturn = keyword "return"

kwSelf :: TextParser ()
kwSelf = keyword "self"

kwScoped :: TextParser ()
kwScoped = keyword "scoped"

kwStrong :: TextParser ()
kwStrong = keyword "strong"

kwTestcase :: TextParser ()
kwTestcase = keyword "testcase"

kwTrue :: TextParser ()
kwTrue = keyword "true"

kwType :: TextParser ()
kwType = keyword "@type"

kwTypename :: TextParser ()
kwTypename = keyword "typename"

kwTypes :: TextParser ()
kwTypes = keyword "types"

kwUnittest :: TextParser ()
kwUnittest = keyword "unittest"

kwUpdate :: TextParser ()
kwUpdate = keyword "update"

kwValue :: TextParser ()
kwValue = keyword "@value"

kwWeak :: TextParser ()
kwWeak = keyword "weak"

kwWhile :: TextParser ()
kwWhile = keyword "while"

paramSelf :: TextParser ()
paramSelf = keyword (show ParamSelf)

noParamSelf :: TextParser ()
noParamSelf = (<|> return ()) $ do
    try paramSelf
    compilerErrorM "#self is not allowed here"

operatorSymbol :: TextParser Char
operatorSymbol = labeled "operator symbol" $ satisfy (`Set.member` Set.fromList "+-*/%=!<>&|?")

isKeyword :: TextParser ()
isKeyword = foldr (<|>) empty $ map try [
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
    kwUnittest,
    kwUpdate,
    kwValue,
    kwWeak,
    kwWhile
  ]

nullParse :: TextParser ()
nullParse = return ()

char_ :: Char -> TextParser ()
char_ = (>> return ()) . char

string_ :: String -> TextParser ()
string_ = (>> return ()) . string

lineEnd :: TextParser ()
lineEnd = (eol >> return ()) <|> eof

lineComment :: TextParser String
lineComment = labeled "line comment" $
  between (string_ "//")
          lineEnd
          (many $ satisfy (/= '\n'))

blockComment :: TextParser String
blockComment = labeled "block comment" $
  between (string_ "/*")
          (string_ "*/")
          (many $ notFollowedBy (string_ "*/") >> asciiChar)

anyComment :: TextParser ()
anyComment = labeled "comment" $ (blockComment <|> lineComment) $> ()

optionalSpace :: TextParser ()
optionalSpace = hidden $ many (anyComment <|> space1) $> ()

sepAfter :: TextParser a -> TextParser a
sepAfter = between nullParse optionalSpace

sepAfter_ :: TextParser a -> TextParser ()
sepAfter_ = (>> return ()) . between nullParse optionalSpace

keyword :: String -> TextParser ()
keyword s = labeled s $ try $ do
  string_ s
  notFollowedBy (alphaNumChar <|> char '_')
  optionalSpace

noKeywords :: TextParser ()
noKeywords = notFollowedBy isKeyword

endOfDoc :: TextParser ()
endOfDoc = labeled "end of input" $ optionalSpace >> eof

notAllowed :: TextParser a -> String -> TextParser ()
-- Based on implementation of notFollowedBy.
notAllowed p s = (try p >> fail s) <|> return ()

pragmaStart :: TextParser ()
pragmaStart = string_ "$"

pragmaEnd :: TextParser ()
pragmaEnd = string_ "$"

pragmaArgsStart :: TextParser ()
pragmaArgsStart = string_ "["

pragmaArgsEnd :: TextParser ()
pragmaArgsEnd = string_ "]"

inferredParam :: TextParser ()
inferredParam = string_ "?"

operator :: String -> TextParser String
operator o = labeled o $ do
  string_ o
  notFollowedBy operatorSymbol
  optionalSpace
  return o

stringChar :: TextParser Char
stringChar = escaped <|> notEscaped where
  escaped = labeled "escaped char sequence" $ do
    char_ '\\'
    octChar <|> otherEscape where
      otherEscape = do
        v <- asciiChar
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
        o1 <- octDigitChar >>= return . digitCharVal
        o2 <- octDigitChar >>= return . digitCharVal
        o3 <- octDigitChar >>= return . digitCharVal
        return $ chr $ 8*8*o1 + 8*o2 + o3
      hexChar = labeled "2 hex chars" $ do
        h1 <- hexDigitChar >>= return . digitCharVal
        h2 <- hexDigitChar >>= return . digitCharVal
        return $ chr $ 16*h1 + h2
  notEscaped = noneOf "\""

quotedString :: TextParser String
quotedString = do
  string_ "\""
  manyTill stringChar (string_ "\"")

digitCharVal :: Char -> Int
digitCharVal c
  | c >= '0' && c <= '9' = ord(c) - ord('0')
  | c >= 'A' && c <= 'F' = 10 + ord(c) - ord('A')
  | c >= 'a' && c <= 'f' = 10 + ord(c) - ord('a')
  | otherwise = undefined

parseDec :: TextParser Integer
parseDec = fmap snd $ parseIntCommon 10 digitChar

parseHex :: TextParser Integer
parseHex = fmap snd $ parseIntCommon 16 hexDigitChar

parseOct :: TextParser Integer
parseOct = fmap snd $ parseIntCommon 8 octDigitChar

parseBin :: TextParser Integer
parseBin = fmap snd $ parseIntCommon 2 (oneOf "01")

parseSubOne :: TextParser (Integer,Integer)
parseSubOne = parseIntCommon 10 digitChar

parseIntCommon :: Integer -> TextParser Char -> TextParser (Integer,Integer)
parseIntCommon b p = do
  ds <- some p
  return $ foldl (\(n,x) y -> (n+1,b*x + (fromIntegral $ digitCharVal y :: Integer))) (0,0) ds

regexChar :: TextParser String
regexChar = escaped <|> notEscaped where
  escaped = do
    char_ '\\'
    v <- asciiChar
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

parseAny2 :: TextParser a -> TextParser b -> TextParser ([a],[b])
parseAny2 p1 p2 = sepBy anyType optionalSpace >>= return . merge2 where
  anyType = p1' <|> p2'
  p1' = do
    x <- p1
    return ([x],[])
  p2' = do
    y <- p2
    return ([],[y])

parseAny3 :: TextParser a -> TextParser b -> TextParser c -> TextParser ([a],[b],[c])
parseAny3 p1 p2 p3 = sepBy anyType optionalSpace >>= return . merge3 where
  anyType = p1' <|> p2' <|> p3'
  p1' = do
    x <- p1
    return ([x],[],[])
  p2' = do
    y <- p2
    return ([],[y],[])
  p3' = do
    z <- p3
    return ([],[],[z])
