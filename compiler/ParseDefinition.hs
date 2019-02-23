{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseDefinition (
) where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String

import DefinedCategory
import ParseCategory
import ParseInstance
import ParseProcedure
import ParserBase
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


instance ParseFromSource (DefinedCategory SourcePos) where
  sourceParser = labeled "defined concrete category" $ do
    c <- getPosition
    kwDefine
    n <- sourceParser
    sepAfter (string "{")
    (pi,fi) <- parseInternalParams <|> return ([],[])
    (ms,ps,fs) <- parseMemberProcedureFunction n
    sepAfter (string "}")
    return $ DefinedCategory [c] n pi fi ms ps fs
    where
      parseInternalParams = labeled "internal params" $ do
        try kwTypes
        pi <- between (sepAfter $ string "<")
                      (sepAfter $ string ">")
                      (sepBy singleParam (sepAfter $ string ","))
        fi <- parseInternalFilters
        return (pi,fi)
      parseInternalFilters = do
        try $ sepAfter (string "{")
        fi <- parseFilters
        sepAfter (string "}")
        return fi
      singleParam = labeled "param declaration" $ do
        c <- getPosition
        n <- sourceParser
        return $ ValueParam [c] n Invariant

instance ParseFromSource (DefinedMember SourcePos) where
  sourceParser = labeled "defined member" $ do
    c <- getPosition
    (s,t) <- try parseType
    n <- sourceParser
    e <- if s == ValueScope
            then return Nothing
            else parseInit
    return $ DefinedMember [c] s t n e
    where
      parseInit = labeled "member initializer" $ do
        assignOperator
        e <- sourceParser
        return $ Just e
      parseType = do
        s <- parseScope
        t <- sourceParser
        return (s,t)

parseMemberProcedureFunction ::
  CategoryName ->
  Parser ([DefinedMember SourcePos],[ExecutableProcedure SourcePos],[ScopedFunction SourcePos])
parseMemberProcedureFunction n = parsed >>= return . foldr merge empty where
  empty = ([],[],[])
  merge (ms1,ps1,fs1) (ms2,ps2,fs2) = (ms1++ms2,ps1++ps2,fs1++fs2)
  parsed = sepBy anyType optionalSpace
  anyType = labeled "" $ singleMember <|> singleProcedure <|> singleFunction
  singleMember = labeled "member" $ do
    m <- sourceParser
    return ([m],[],[])
  singleProcedure = labeled "procedure" $ do
    p <- sourceParser
    return ([],[p],[])
  singleFunction = labeled "function" $ do
    f <- try $ parseScopedFunction parseScope (return n)
    p <- labeled ("definition of function " ++ show (sfName f)) $ sourceParser
    when (sfName f /= epName p) $
      fail $ "expecting definition of function " ++ show (sfName f) ++
             " but got definition of " ++ show (epName p)
    return ([],[p],[f])
