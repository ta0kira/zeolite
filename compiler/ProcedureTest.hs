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

module ProcedureTest where

import Control.Monad
import Text.Parsec
import Text.Parsec.String

import CompileInfo
import ParseProcedure
import Procedure
import TestBase
import TypesBase


tests :: [IO (CompileInfo ())]
tests = [
    checkParseSuccess "testfiles/procedures.0rx",

    checkShortParseSuccess "return _",
    checkShortParseSuccess "return var",
    checkShortParseFail "return var var",
    checkShortParseFail "return _ var",
    checkShortParseSuccess "return call()",
    checkShortParseSuccess "return var.T<#x>$func()",
    checkShortParseSuccess "return { var, var.T<#x>$func() }",
    checkShortParseFail "return { var  var.T<#x>$func() }",
    checkShortParseFail "return { var, _ }",
    checkShortParseFail "return T<#x> var",
    checkShortParseSuccess "return T<#x>{ val }",
    checkShortParseSuccess "~ T$$func()",
    checkShortParseFail "~ T<#x>$$func()",
    checkShortParseFail "~ var.T$$func()",
    checkShortParseFail "~ T$ $func()",

    checkShortParseSuccess "break",
    checkShortParseFail "break var",
    checkShortParseFail "break _",
    checkShortParseFail "break { }",

    checkShortParseSuccess "~ var",
    checkShortParseFail "~ var var",
    checkShortParseSuccess "~ var.T<#x>$func().func2().func3()",
    checkShortParseSuccess "~ T<#x>$func().func2().func3()",
    checkShortParseSuccess "~ #x$func().func2().func3()",
    checkShortParseFail "~ var.T<#x>.T<#x>$func()",
    checkShortParseFail "~ var.T<#x>$T<#x>$func()",
    checkShortParseFail "~ T<#x>$func()$func2()",
    checkShortParseFail "~ var$func2()",
    checkShortParseFail "~ var.T<#x>",
    checkShortParseFail "~ T<#x> var",
    checkShortParseSuccess "~ T<#x>{ val, var.T<#x>$func() }",
    checkShortParseFail "~ T<#x>{ val var.T<#x>$func() }",
    checkShortParseFail "~ T<#x>{}.call()",
    checkShortParseSuccess "~ (T<#x>{}).call()",

    checkShortParseSuccess "x <- var.func()",
    checkShortParseFail "x <- var.func() var.func()",
    checkShortParseFail "x <- y <- var.func()",
    checkShortParseSuccess "x <- empty",
    checkShortParseSuccess "x <- true",
    checkShortParseSuccess "x <- false",
    checkShortParseSuccess "x <- require(y)",
    checkShortParseSuccess "x <- reduce<#x,#y>(z)",
    checkShortParseSuccess "x <- self",
    checkShortParseSuccess "x <- self.f()",
    checkShortParseFail "empty <- x",
    checkShortParseFail "true <- x",
    checkShortParseFail "false <- x",
    checkShortParseFail "require <- x",
    checkShortParseFail "reduce <- x",
    checkShortParseFail "self <- x",
    checkShortParseFail "T<#x> empty <- x",
    checkShortParseFail "T<#x> true <- x",
    checkShortParseFail "T<#x> false <- x",
    checkShortParseFail "T<#x> require <- x",
    checkShortParseFail "T<#x> reduce <- x",
    checkShortParseFail "T<#x> self <- x",
    checkShortParseSuccess "T<#x> x <- var.func()",
    checkShortParseSuccess "weak T<#x> x <- var.func()",
    checkShortParseFail "~ T<#x> x <- var.func()",
    checkShortParseSuccess "{ _, weak T<#x> x } <- var.func()",
    checkShortParseFail "{ _, weak T<#x> x } <- T<#x> x",

    checkShortParseSuccess "if (var.func()) { ~ val.call() }",
    checkShortParseSuccess "if (present(var)) { ~ val.call() }",
    checkShortParseFail "if (T<#x> x) { ~ val.call() }",
    checkShortParseSuccess "if (var) { ~ val.call() } else { ~ val.call() }",
    checkShortParseFail "if (var) { ~ val.call() } elif { ~ val.call() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() } else { ~ c() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() } elif (v) { ~ c() }",

    checkShortParseSuccess "while (var.func()) { ~ val.call() }",
    checkShortParseSuccess "while (var.func()) { ~ val.call() } update { ~ call() }",

    checkShortParseSuccess "scoped { T<#x> x <- y } in return _",
    checkShortParseSuccess "scoped { T<#x> x <- y } in return { var, var.T<#x>$func() }",
    checkShortParseSuccess "scoped { T<#x> x <- y } in ~ var.T<#x>$func()",
    checkShortParseSuccess "scoped { T<#x> x <- y } in { _, weak T<#x> x } <- var.func()",

    checkShortParseSuccess "scoped { T<#x> x <- y } in if (var.func()) { ~ val.call() }",
    checkShortParseSuccess "scoped { T<#x> x <- y } in while (var.func()) { ~ val.call() }",

    checkShortParseSuccess "x <- (((var.func())).T$call())",
    checkShortParseSuccess "~ (x <- var).func()",
    checkShortParseFail "x <- (((var.func()))",
    checkShortParseFail "(((x <- var.func())))",
    checkShortParseFail "(x) <- y",
    checkShortParseFail "T (x) <- y",
    checkShortParseFail "~ (T x <- var).func()",
    checkShortParseSuccess "~ call(((var.func())).T$call())",
    checkShortParseSuccess "if (((var.func()).T$call())) { }",

    checkShortParseSuccess "~var.T<#x>$func().func2().func3()",
    checkShortParseSuccess "~T<#x>{val,var.T<#x>$func()}",
    checkShortParseSuccess "x<-var.func()",
    checkShortParseSuccess "T<#x>x<-var.func()",
    checkShortParseSuccess "{_,weak T<#x>x}<-var.func()",
    checkShortParseSuccess "if(v){~c()}elif(v){~c()}",
    checkShortParseSuccess "if(v){~c()}elif(v){~c()}else{~c()}",
    checkShortParseSuccess "if(v){~c()}elif(v){~c()}elif(v){~c()}",
    checkShortParseSuccess "while(var.func()){~val.call()}",
    checkShortParseSuccess "scoped{T<#x>x<-y}in~var.T<#x>$func()",

    checkShortParseSuccess "x <- !y",
    checkShortParseSuccess "x <- !y",
    checkShortParseFail "x <- !!=y",
    checkShortParseSuccess "x <- (!y).func()",
    checkShortParseSuccess "~ !y",
    checkShortParseSuccess "if (!y) { }",

    checkShortParseSuccess "~ !x + !y",
    checkShortParseSuccess "~ !x - !y",
    checkShortParseSuccess "~ !x * !y",
    checkShortParseSuccess "~ !x / !y",
    checkShortParseSuccess "~ !x % !y",
    checkShortParseSuccess "~ !x == !y",
    checkShortParseSuccess "~ !x != !y",
    checkShortParseSuccess "~ !x < !y",
    checkShortParseSuccess "~ !x <= !y",
    checkShortParseSuccess "~ !x > !y",
    checkShortParseSuccess "~ !x >= !y",
    checkShortParseSuccess "~ !x && !y",
    checkShortParseSuccess "~ !x || !y",

    checkShortParseSuccess "x <- y + z",
    checkShortParseSuccess "x <- !y == !z",
    checkShortParseSuccess "x <- (x + y) / z",
    checkShortParseSuccess "~ x <= y",
    checkShortParseFail "~ x < <- y",

    checkShortParseSuccess "x <- 123 + 123",
    checkShortParseSuccess "x <- 123.0 - 123.0",
    checkShortParseFail "x <- 123.",
    checkShortParseSuccess "x <- 0.123 * 0.123",
    checkShortParseFail "x <- .123",
    checkShortParseSuccess "x <- 12.3 / 12.3",
    checkShortParseFail "x <- 12.3.",
    checkShortParseSuccess "x <- 12.3 + -456.7",
    checkShortParseSuccess "x <- 0x123aBc + 0x123aBc",
    checkShortParseFail "x <- 0x123aQc",
    checkShortParseFail "x <- 0x",
    checkShortParseFail "x <- 0x1.2",
    checkShortParseSuccess "x <- \" return \\\"\\\" \" + \"1fds\"",
    checkShortParseFail "x <- \"fsdfd",
    checkShortParseFail "x <- \"\"fsdfd",
    checkShortParseSuccess "x <- 123.0 + z.call()",
    checkShortParseFail "x <- \"123\".call()",
    checkShortParseFail "x <- 123.call()",
    checkShortParseSuccess " x <- 'x'",
    checkShortParseSuccess " x <- '\\xAA'",
    checkShortParseFail " x <- '\\xAAZ'",
    checkShortParseSuccess " x <- '\076'",
    checkShortParseFail " x <- '\\07'",
    checkShortParseSuccess " x <- '\\n'",
    checkShortParseFail " x <- 'x",
    checkShortParseFail " x <- 'xx'",
    checkShortParseSuccess " x <- \"'xx\"",

    checkParsesAs "1 + 2 < 4 && 3 >= 1 * 2 + 1 || true"
                  (\e -> case e of
                              (InfixExpression _
                                (InfixExpression _
                                  (InfixExpression _
                                    (InfixExpression _
                                      (Literal (IntegerLiteral _ "1")) "+"
                                      (Literal (IntegerLiteral _ "2"))) "<"
                                    (Literal (IntegerLiteral _ "4"))) "&&"
                                  (InfixExpression _
                                    (Literal (IntegerLiteral _ "3")) ">="
                                    (InfixExpression _
                                      (InfixExpression _
                                        (Literal (IntegerLiteral _ "1")) "*"
                                        (Literal (IntegerLiteral _ "2"))) "+"
                                      (Literal (IntegerLiteral _ "1"))))) "||"
                                (Literal (BoolLiteral _ True))) -> True
                              _ -> False)
  ]

checkParseSuccess f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check c
      | isCompileError c = compileError $ "Parse " ++ f ++ ":\n" ++ show (getCompileError c)
      | otherwise = return ()

checkParseFail f = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkShortParseSuccess s = do
  let parsed = readSingle "(string)" s :: CompileInfo (Statement SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = compileError $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkShortParseFail s = do
  let parsed = readSingle "(string)" s :: CompileInfo (Statement SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkParsesAs s m = return $ do
  let parsed = readSingle "(string)" s :: CompileInfo (Expression SourcePos)
  check parsed
  e <- parsed
  when (not $ m e) $
    compileError $ "No match in '" ++ s ++ "':\n" ++ show e
  where
    check c
      | isCompileError c = compileError $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()
