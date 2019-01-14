{-# LANGUAGE Safe #-}

module ProcedureTest where

import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParseProcedure
import Procedure
import TestBase
import TypesBase


tests :: [IO (CompileInfo ())]
tests = [
    checkParseSuccess "testfiles/procedures.txt",

    checkShortParseSuccess "return",
    checkShortParseSuccess "return var",
    checkShortParseFail "return var var",
    checkShortParseSuccess "return call()",
    checkShortParseSuccess "return var.T<`x>$func()",
    checkShortParseSuccess "return { var, var.T<`x>$func() }",
    checkShortParseFail "return { var  var.T<`x>$func() }",
    checkShortParseFail "return T<`x> var",
    checkShortParseSuccess "return T<`x>{ var: val }",

    checkShortParseSuccess "~ var",
    checkShortParseFail "~ var var",
    checkShortParseSuccess "~ var.T<`x>$func().func2().func3()",
    checkShortParseSuccess "~ T<`x>$func().func2().func3()",
    checkShortParseSuccess "~ `x$func().func2().func3()",
    checkShortParseFail "~ T<`x>$func()$func2()",
    checkShortParseFail "~ var$func2()",
    checkShortParseFail "~ var.T<`x>",
    checkShortParseFail "~ T<`x> var",
    checkShortParseSuccess "~ T<`x>{ var: val x: var.T<`x>$func() }",

    checkShortParseSuccess "x = var.func()",
    checkShortParseFail "x = var.func() var.func()",
    checkShortParseSuccess "x = empty",
    checkShortParseSuccess "x = require(y)",
    checkShortParseSuccess "x = reduce<`x,`y>(z)",
    checkShortParseSuccess "T<`x> x = var.func()",
    checkShortParseSuccess "weak T<`x> x = var.func()",
    checkShortParseFail "~ T<`x> x = var.func()",
    checkShortParseSuccess "{ _, weak T<`x> x } = var.func()",
    checkShortParseFail "{ _, weak T<`x> x } = T<`x> x",

    checkShortParseSuccess "if (var.func()) { ~ val.call() }",
    checkShortParseSuccess "if (present(var)) { ~ val.call() }",
    checkShortParseFail "if (T<`x> x) { ~ val.call() }",
    checkShortParseSuccess "if (var) { ~ val.call() } else { ~ val.call() }",
    checkShortParseFail "if (var) { ~ val.call() } elif { ~ val.call() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() } else { ~ c() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() } elif (v) { ~ c() }",

    checkShortParseSuccess "while (var.func()) { ~ val.call() }",

    checkShortParseSuccess "scoped { T<`x> x = y } in return",
    checkShortParseSuccess "scoped { T<`x> x = y } in return { var, var.T<`x>$func() }",
    checkShortParseSuccess "scoped { T<`x> x = y } in ~ var.T<`x>$func()",
    checkShortParseSuccess "scoped { T<`x> x = y } in { _, weak T<`x> x } = var.func()",

    checkShortParseSuccess "scoped { T<`x> x = y } in if (var.func()) { ~ val.call() }",
    checkShortParseSuccess "scoped { T<`x> x = y } in while (var.func()) { ~ val.call() }"
  ]

checkParseSuccess f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check (Left es) = compileError $ "Parse " ++ f ++ ":\n" ++ show es
    check _ = return ()

checkParseFail f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check (Right t) =
      compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t ++ "\n"
    check _ = return ()

checkShortParseSuccess s = do
  let parsed = readSingle "(string)" s :: CompileInfo (Statement SourcePos)
  return $ check parsed
  where
    check (Left es) = compileError $ "Parse '" ++ s ++ "':\n" ++ show es
    check _ = return ()

checkShortParseFail s = do
  let parsed = readSingle "(string)" s :: CompileInfo (Statement SourcePos)
  return $ check parsed
  where
    check (Right t) =
      compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++ show t ++ "\n"
    check _ = return ()
