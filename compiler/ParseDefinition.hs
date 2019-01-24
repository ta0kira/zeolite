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
    (ms,ps,fs) <- parseMemberProcedureFunction n
    sepAfter (string "}")
    return $ DefinedCategory [c] n ms ps fs

instance ParseFromSource (DefinedMember SourcePos) where
  sourceParser = labeled "defined member" $ do
    c <- getPosition
    s <- parseScope
    t <- sourceParser
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

parseMemberProcedureFunction ::
  CategoryName ->
  Parser ([DefinedMember SourcePos],[ExecutableProcedure SourcePos],[ScopedFunction SourcePos])
parseMemberProcedureFunction n = parsed >>= return . foldr merge empty where
  empty = ([],[],[])
  merge (ms1,ps1,fs1) (ms2,ps2,fs2) = (ms1++ms2,ps1++ps2,fs1++fs2)
  parsed = sepBy anyType optionalSpace
  anyType = labeled "" $ singleMember <|> singleProcedure <|> singleFunction
  singleMember = labeled "member" $ do
    m <- try $ sourceParser
    return ([m],[],[])
  singleProcedure = labeled "procedure" $ do
    p <- try $ sourceParser
    return ([],[p],[])
  singleFunction = labeled "function" $ do
    f <- try $ parseScopedFunction parseScope (return n)
    p <- labeled ("definition of function " ++ show (sfName f)) $ sourceParser
    when (sfName f /= epName p) $
      fail $ "expecting definition of function " ++ show (sfName f) ++
             " but got definition of " ++ show (epName p)
    return ([],[p],[f])
