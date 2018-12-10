{-# LANGUAGE Safe #-}

module TypeCategoryTest where

import System.IO
import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParseCategory
import ParserBase
import TestBase
import TypesBase


main = runAllTests [
    checkSingleParseSuccess "testfiles/value_interface.txt",
    checkSingleParseSuccess "testfiles/type_interface.txt",
    checkSingleParseSuccess "testfiles/concrete.txt"
  ]


checkSingleParseSuccess f = checked where
  checked = do
    parsed <- readSingleCategory f
    return $ check parsed
  check (Left es) = compileError $ "Parse " ++ f ++ ": " ++ show es
  check _ = return ()

checkSingleParseFail f = checked where
  checked = do
    parsed <- readSingleCategory f
    return $ check parsed
  check (Right t) = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t
  check _ = return ()

readSingleCategory :: String -> IO (CompileInfo (AnyCategory SourcePos))
readSingleCategory n = parsed where
  parsed = do
    contents <- readFile n
    attempt <- return $ parse (between optionalSpace endOfDoc sourceParser) n contents
    return $ unwrap attempt
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t
