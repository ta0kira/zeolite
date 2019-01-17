{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseDefinition (
) where

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
    e <- parseInit <|> return Nothing
    return $ DefinedMember [c] s t n e
    where
      parseInit = labeled "member initializer" $ do
        sepAfter (string "=")
        e <- sourceParser
        return $ Just e

parseMemberProcedureFunction ::
  TypeName ->
  Parser ([DefinedMember SourcePos],[ExecutableProcedure SourcePos],[ScopedFunction SourcePos])
parseMemberProcedureFunction n = parsed >>= return . foldr merge empty where
  empty = ([],[],[])
  merge (ms1,ps1,fs1) (ms2,ps2,fs2) = (ms1++ms2,ps1++ps2,fs1++fs2)
  parsed = sepBy anyType optionalSpace
  anyType = labeled "member, procedure, or function" $
              singleMember <|> singleProcedure <|> singleFunction
  singleMember = try $ do
    m <- sourceParser
    return ([m],[],[])
  singleProcedure = try $ do
    p <- sourceParser
    return ([],[p],[])
  singleFunction = try $ do
    f <- parseScopedFunction parseScope (return n)
    return ([],[],[f])
