{-# LANGUAGE Safe #-}

module TypeCategoryTest where

import Control.Arrow
import System.IO
import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as Set

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
    checkOperationFail "testfiles/value_cycle.txt" checkConnectionCycles,

    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections ts
        scrapeAllRefines ts2 `containsExactly` [
            ("Object1","Object3<y>"),
            ("Object1","Object2"),
            ("Object3","Object2"),
            ("Parent","Object1<x,Object3<Object2>>"),
            ("Parent","Object3<Object3<Object2>>"),
            ("Parent","Object2"),
            ("Child","Parent<Child>"),
            ("Child","Object1<Child,Object3<Object2>>"),
            ("Child","Object3<Object3<Object2>>"),
            ("Child","Object2")
          ]
        scrapeAllDefines ts2 `containsExactly` [
            ("Child","Type<Child>")
          ])
  ]


scrapeAllRefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueInterface _ n _ rs) = map ((,) n . vrType) rs
  scrapeSingle (ValueConcrete _ n _ rs _ _ _) = map ((,) n . vrType) rs
  scrapeSingle _ = []

scrapeAllDefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueConcrete _ n _ _ ds _ _) = map ((,) n . vrType) ds
  scrapeSingle _ = []

containsExactly expected actual = do
  containsAtLeast expected actual
  containsAtMost expected actual

containsAtLeast actual expected = checked where
  checked = mergeAll $ map (checkInActual $ Set.fromList actual) expected
  checkInActual va v =
    if v `Set.member` va
       then return ()
       else compileError $ "Item " ++ show v ++ " was expected but not present"

containsAtMost actual expected = checked where
  checked = mergeAll $ map (checkInExpected $ Set.fromList expected) actual
  checkInExpected va v =
    if v `Set.member` va
       then return ()
       else compileError $ "Item " ++ show v ++ " is unexpected"

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
