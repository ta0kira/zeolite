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
{-# LANGUAGE Safe #-}

module Parser.Common (
  ParseFromSource(..),
  ParserE,
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
  notAllowed,
  nullParse,
  operator,
  optionalSpace,
  parseAny2,
  parseAny3,
  parseBin,
  parseDec,
  parseHex,
  parseOct,
  parseSubOne,
  parseErrorM,
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
  requiredSpace,
  runParserE,
  sepAfter,
  sepAfter1,
  sepAfter_,
  statementEnd,
  statementStart,
  stringChar,
  string_,
  typeSymbolGet,
  valueSymbolGet,
) where

import Control.Applicative (empty)
import Control.Monad.Trans (lift)
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Monoid
import Prelude hiding (foldl,foldr)
import Text.Parsec
import qualified Data.Set as Set

import Base.CompilerError


type ParserE = ParsecT String ()

class ParseFromSource a where
  -- Must never prune whitespace/comments from front, but always from back.
  sourceParser :: ErrorContextM m => ParserE m a

runParserE :: ErrorContextM m => ParserE m a -> String -> String -> m a
runParserE p n s = do
  result <- runPT p () n s
  case result of
       Left e  -> compilerErrorM (show e)
       Right t -> return t

parseErrorM :: ErrorContextM m => SourcePos -> String -> ParserE m a
parseErrorM c e = lift $ compilerErrorM $ "At " ++ show c ++ ": " ++ e

labeled :: Monad m => String -> ParserE m a -> ParserE m a
labeled = flip label

escapeStart :: Monad m => ParserE m ()
escapeStart = sepAfter (string_ "\\")

statementStart :: Monad m => ParserE m ()
statementStart = sepAfter (string_ "\\")

statementEnd :: Monad m => ParserE m ()
statementEnd = sepAfter (string_ "")

valueSymbolGet :: Monad m => ParserE m ()
valueSymbolGet = sepAfter (string_ ".")

categorySymbolGet :: ErrorContextM m => ParserE m ()
categorySymbolGet = labeled ":" $ useNewOperators <|> sepAfter (string_ ":")

typeSymbolGet :: ErrorContextM m => ParserE m ()
typeSymbolGet = labeled "." $ useNewOperators <|> sepAfter (string_ ".")

-- TODO: Remove this after a reasonable amount of time.
useNewOperators :: ErrorContextM m => ParserE m ()
useNewOperators = newCategory <|> newType where
  newCategory = do
    c <- getPosition
    try $ string_ "$$"
    parseErrorM c "use \":\" instead of \"$$\" to call @category functions"
  newType = do
    c <- getPosition
    try $ string_ "$"
    parseErrorM c "use \".\" instead of \"$\" to call @type functions"

assignOperator :: Monad m => ParserE m ()
assignOperator = operator "<-" >> return ()

infixFuncStart :: Monad m => ParserE m ()
infixFuncStart = sepAfter (string_ "`")

infixFuncEnd :: Monad m => ParserE m ()
infixFuncEnd = sepAfter (string_ "`")

-- TODO: Maybe this should not use strings.
builtinValues :: Monad m => ParserE m String
builtinValues = foldr (<|>) empty $ map try [
    kwSelf >> return "self"
  ]

kwAll :: Monad m => ParserE m ()
kwAll = keyword "all"

kwAllows :: Monad m => ParserE m ()
kwAllows = keyword "allows"

kwAny :: Monad m => ParserE m ()
kwAny = keyword "any"

kwBreak :: Monad m => ParserE m ()
kwBreak = keyword "break"

kwCategory :: Monad m => ParserE m ()
kwCategory = keyword "@category"

kwCleanup :: Monad m => ParserE m ()
kwCleanup = keyword "cleanup"

kwConcrete :: Monad m => ParserE m ()
kwConcrete = keyword "concrete"

kwContinue :: Monad m => ParserE m ()
kwContinue = keyword "continue"

kwDefine :: Monad m => ParserE m ()
kwDefine = keyword "define"

kwDefines :: Monad m => ParserE m ()
kwDefines = keyword "defines"

kwElif :: Monad m => ParserE m ()
kwElif = keyword "elif"

kwElse :: Monad m => ParserE m ()
kwElse = keyword "else"

kwEmpty :: Monad m => ParserE m ()
kwEmpty = keyword "empty"

kwFail :: Monad m => ParserE m ()
kwFail = keyword "fail"

kwFalse :: Monad m => ParserE m ()
kwFalse = keyword "false"

kwIf :: Monad m => ParserE m ()
kwIf = keyword "if"

kwIn :: Monad m => ParserE m ()
kwIn = keyword "in"

kwIgnore :: Monad m => ParserE m ()
kwIgnore = keyword "_"

kwInterface :: Monad m => ParserE m ()
kwInterface = keyword "interface"

kwOptional :: Monad m => ParserE m ()
kwOptional = keyword "optional"

kwPresent :: Monad m => ParserE m ()
kwPresent = keyword "present"

kwReduce :: Monad m => ParserE m ()
kwReduce = keyword "reduce"

kwRefines :: Monad m => ParserE m ()
kwRefines = keyword "refines"

kwRequire :: Monad m => ParserE m ()
kwRequire = keyword "require"

kwRequires :: Monad m => ParserE m ()
kwRequires = keyword "requires"

kwReturn :: Monad m => ParserE m ()
kwReturn = keyword "return"

kwSelf :: Monad m => ParserE m ()
kwSelf = keyword "self"

kwScoped :: Monad m => ParserE m ()
kwScoped = keyword "scoped"

kwStrong :: Monad m => ParserE m ()
kwStrong = keyword "strong"

kwTestcase :: Monad m => ParserE m ()
kwTestcase = keyword "testcase"

kwTrue :: Monad m => ParserE m ()
kwTrue = keyword "true"

kwType :: Monad m => ParserE m ()
kwType = keyword "@type"

kwTypename :: Monad m => ParserE m ()
kwTypename = keyword "typename"

kwTypes :: Monad m => ParserE m ()
kwTypes = keyword "types"

kwUnittest :: Monad m => ParserE m ()
kwUnittest = keyword "unittest"

kwUpdate :: Monad m => ParserE m ()
kwUpdate = keyword "update"

kwValue :: Monad m => ParserE m ()
kwValue = keyword "@value"

kwWeak :: Monad m => ParserE m ()
kwWeak = keyword "weak"

kwWhile :: Monad m => ParserE m ()
kwWhile = keyword "while"

operatorSymbol :: Monad m => ParserE m Char
operatorSymbol = labeled "operator symbol" $ satisfy (`Set.member` Set.fromList "+-*/%=!<>&|")

isKeyword :: Monad m => ParserE m ()
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
    kwUnittest,
    kwUpdate,
    kwValue,
    kwWeak,
    kwWhile
  ]

nullParse :: Monad m => ParserE m ()
nullParse = return ()

char_ :: Monad m => Char -> ParserE m ()
char_ = (>> return ()) . char

string_ :: Monad m => String -> ParserE m ()
string_ = (>> return ()) . string

lineEnd :: Monad m => ParserE m ()
lineEnd = (endOfLine >> return ()) <|> endOfDoc

lineComment :: Monad m => ParserE m String
lineComment = between (string_ "//")
                      lineEnd
                      (many $ satisfy (/= '\n'))

blockComment :: Monad m => ParserE m String
blockComment = between (string_ "/*")
                       (string_ "*/")
                       (many $ notFollowedBy (string_ "*/") >> anyChar)

anyComment :: Monad m => ParserE m String
anyComment = try blockComment <|> try lineComment

optionalSpace :: Monad m => ParserE m ()
optionalSpace = labeled "" $ many (anyComment <|> many1 space) >> nullParse

requiredSpace :: Monad m => ParserE m ()
requiredSpace = labeled "break" $ eof <|> (many1 (anyComment <|> many1 space) >> nullParse)

sepAfter :: Monad m => ParserE m a -> ParserE m a
sepAfter = between nullParse optionalSpace

sepAfter_ :: Monad m => ParserE m a -> ParserE m ()
sepAfter_ = (>> return ()) . between nullParse optionalSpace

sepAfter1 :: Monad m => ParserE m a -> ParserE m a
sepAfter1 = between nullParse requiredSpace

keyword :: Monad m => String -> ParserE m ()
keyword s = labeled s $ sepAfter $ string s >> (labeled "" $ notFollowedBy (many alphaNum))

noKeywords :: Monad m => ParserE m ()
noKeywords = notFollowedBy isKeyword

endOfDoc :: Monad m => ParserE m ()
endOfDoc = labeled "" $ optionalSpace >> eof

notAllowed :: ParserE m a -> String -> ParserE m ()
-- Based on implementation of notFollowedBy.
notAllowed p s = (try p >> fail s) <|> return ()

pragmaStart :: Monad m => ParserE m ()
pragmaStart = string_ "$"

pragmaEnd :: Monad m => ParserE m ()
pragmaEnd = string_ "$"

pragmaArgsStart :: Monad m => ParserE m ()
pragmaArgsStart = string_ "["

pragmaArgsEnd :: Monad m => ParserE m ()
pragmaArgsEnd = string_ "]"

inferredParam :: Monad m => ParserE m ()
inferredParam = string_ "?"

operator :: Monad m => String -> ParserE m String
operator o = labeled o $ do
  string_ o
  notFollowedBy operatorSymbol
  optionalSpace
  return o

stringChar :: Monad m => ParserE m Char
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

quotedString :: Monad m => ParserE m String
quotedString = do
  string_ "\""
  manyTill stringChar (string_ "\"")

digitVal :: Char -> Int
digitVal c
  | c >= '0' && c <= '9' = ord(c) - ord('0')
  | c >= 'A' && c <= 'F' = 10 + ord(c) - ord('A')
  | c >= 'a' && c <= 'f' = 10 + ord(c) - ord('a')
  | otherwise = undefined

parseDec :: Monad m => ParserE m Integer
parseDec = fmap snd $ parseIntCommon 10 digit

parseHex :: Monad m => ParserE m Integer
parseHex = fmap snd $ parseIntCommon 16 hexDigit

parseOct :: Monad m => ParserE m Integer
parseOct = fmap snd $ parseIntCommon 8 octDigit

parseBin :: Monad m => ParserE m Integer
parseBin = fmap snd $ parseIntCommon 2 (oneOf "01")

parseSubOne :: Monad m => ParserE m (Integer,Integer)
parseSubOne = parseIntCommon 10 digit

parseIntCommon :: Monad m => Integer -> ParserE m Char -> ParserE m (Integer,Integer)
parseIntCommon b p = do
  ds <- many1 p
  return $ foldl (\(n,x) y -> (n+1,b*x + (fromIntegral $ digitVal y :: Integer))) (0,0) ds

regexChar :: Monad m => ParserE m String
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

parseAny2 :: Monad m => ParserE m a -> ParserE m b -> ParserE m ([a],[b])
parseAny2 p1 p2 = sepBy anyType optionalSpace >>= return . merge2 where
  anyType = p1' <|> p2'
  p1' = do
    x <- p1
    return ([x],[])
  p2' = do
    y <- p2
    return ([],[y])

parseAny3 :: Monad m => ParserE m a -> ParserE m b -> ParserE m c -> ParserE m ([a],[b],[c])
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
