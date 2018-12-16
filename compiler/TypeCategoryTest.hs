{-# LANGUAGE Safe #-}

module TypeCategoryTest where

import System.IO
import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParseCategory
import ParserBase
import TestBase
import TypeCategory
import TypesBase


main = runAllTests [
    checkSingleParseSuccess "testfiles/value_interface.txt",
    checkSingleParseSuccess "testfiles/type_interface.txt",
    checkSingleParseSuccess "testfiles/concrete.txt",

    checkShortParseSuccess "concrete Type<x> {}",
    checkShortParseSuccess "concrete Type {}",
    checkShortParseFail "concrete Type<T> {}",
    checkShortParseFail "concrete Type<optional> {}",
    checkShortParseFail "concrete Type<optional T> {}",
    checkShortParseFail "concrete Type<T<x>> {}",
    checkShortParseSuccess "concrete Type { refines T }",
    checkShortParseFail "concrete Type { refines x }",
    checkShortParseSuccess "concrete Type { defines T }",
    checkShortParseFail "concrete Type { defines x }",
    checkShortParseFail "concrete Type { refines optional }",
    checkShortParseFail "concrete Type { refines optional T }",
    checkShortParseSuccess "concrete Type<x|y> { x requires y }",
    checkShortParseSuccess "concrete Type<x|y> { x allows y }",
    checkShortParseSuccess "concrete Type<x|y> { x defines T }",
    checkShortParseFail "concrete Type<x|y> { x defines y }",

    checkShortParseSuccess "@type interface Type<x> {}",
    checkShortParseSuccess "@type interface Type {}",
    checkShortParseFail "@type interface Type { refines T }",
    checkShortParseFail "@type interface Type { defines T }",
    checkShortParseFail "@type interface Type<x> { x allows T }",

    checkShortParseSuccess "@value interface Type<x> {}",
    checkShortParseSuccess "@value interface Type {}",
    checkShortParseSuccess "@value interface Type { refines T }",
    checkShortParseFail "@value interface Type { defines T }",
    checkShortParseFail "@value interface Type<x> { x allows T }",

    checkOperationSuccess "testfiles/value_refines_value.txt" checkConnectedTypes,
    checkOperationFail "testfiles/value_refines_instance.txt" checkConnectedTypes,
    checkOperationFail "testfiles/value_refines_concrete.txt" checkConnectedTypes,

    checkOperationSuccess "testfiles/concrete_refines_value.txt" checkConnectedTypes,
    checkOperationFail "testfiles/concrete_refines_instance.txt" checkConnectedTypes,
    checkOperationFail "testfiles/concrete_refines_concrete.txt" checkConnectedTypes,
    checkOperationSuccess "testfiles/concrete_defines_instance.txt" checkConnectedTypes,
    checkOperationFail "testfiles/concrete_defines_value.txt" checkConnectedTypes,
    checkOperationFail "testfiles/concrete_defines_concrete.txt" checkConnectedTypes,

    checkOperationSuccess "testfiles/value_refines_value.txt" checkConnectionCycles,
    checkOperationSuccess "testfiles/concrete_refines_value.txt" checkConnectionCycles,
    checkOperationSuccess "testfiles/concrete_defines_instance.txt" checkConnectionCycles,
    checkOperationFail "testfiles/value_cycle.txt" checkConnectionCycles
  ]


checkOperationSuccess f o = checked where
  checked = do
    contents <- readFile f
    parsed <- readMultiCategory f contents
    return $ check (parsed >>= o)
  check (Left es) = compileError $ "Check " ++ f ++ ": " ++ show es
  check _ = return ()

checkOperationFail f o = checked where
  checked = do
    contents <- readFile f
    parsed <- readMultiCategory f contents
    return $ check (parsed >>= o)
  check (Right x) = compileError $ "Check " ++ f ++ ": Expected failure but got\n" ++ show x
  check _ = return ()

checkSingleParseSuccess f = checked where
  checked = do
    contents <- readFile f
    parsed <- readSingleCategory f contents
    return $ check parsed
  check (Left es) = compileError $ "Parse " ++ f ++ ": " ++ show es
  check _ = return ()

checkSingleParseFail f = checked where
  checked = do
    contents <- readFile f
    parsed <- readSingleCategory f contents
    return $ check parsed
  check (Right t) = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t
  check _ = return ()

checkShortParseSuccess s = checked where
  checked = do
    parsed <- readSingleCategory "(string)" s
    return $ check parsed
  check (Left es) = compileError $ "Parse '" ++ s ++ "': " ++ show es
  check _ = return ()

checkShortParseFail s = checked where
  checked = do
    parsed <- readSingleCategory "(string)" s
    return $ check parsed
  check (Right t) = compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++ show t
  check _ = return ()

readSingleCategory :: String -> String -> IO (CompileInfo (AnyCategory SourcePos))
readSingleCategory f s = parsed where
  parsed =
    return $ unwrap $ parse (between optionalSpace endOfDoc sourceParser) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

readMultiCategory :: String -> String -> IO (CompileInfo [AnyCategory SourcePos])
readMultiCategory f s = parsed where
  parsed =
    return $ unwrap $ parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t
