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

module Test.Procedure (tests) where

import Control.Monad
import System.FilePath
import Text.Parsec

import Base.CompileError
import Compilation.CompileInfo
import Parser.Procedure ()
import Test.Common
import Types.Positional
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


tests :: [IO (CompileInfo ())]
tests = [
    checkParseSuccess ("testfiles" </> "procedures.0rx"),

    checkShortParseSuccess "return _",
    checkShortParseSuccess "return var",
    checkShortParseFail "return var var",
    checkShortParseFail "return _ var",
    checkShortParseSuccess "return call()",
    checkShortParseSuccess "return var.T<#x>$func()",
    checkShortParseSuccess "return var, var.T<#x>$func()",
    checkShortParseFail "return var  var.T<#x>$func()",
    checkShortParseFail "return var, _",
    checkShortParseFail "return T<#x> var",
    checkShortParseSuccess "return T<#x>{ val }",
    checkShortParseSuccess "\\ T$$func()",
    checkShortParseFail "\\ T<#x>$$func()",
    checkShortParseFail "\\ var.T$$func()",
    checkShortParseFail "\\ T$ $func()",

    checkShortParseSuccess "break",
    checkShortParseFail "break var",
    checkShortParseFail "break _",
    checkShortParseFail "break { }",

    checkShortParseSuccess "\\ var",
    checkShortParseFail "\\ var var",
    checkShortParseSuccess "\\ var.T<#x>$func().func2().func3()",
    checkShortParseSuccess "\\ T<#x>$func().func2().func3()",
    checkShortParseSuccess "\\ #x$func().func2().func3()",
    checkShortParseFail "\\ var.T<#x>.T<#x>$func()",
    checkShortParseFail "\\ var.T<#x>$T<#x>$func()",
    checkShortParseFail "\\ T<#x>$func()$func2()",
    checkShortParseFail "\\ var$func2()",
    checkShortParseFail "\\ var.T<#x>",
    checkShortParseFail "\\ T<#x> var",
    checkShortParseSuccess "\\ T<#x>{ val, var.T<#x>$func() }",
    checkShortParseFail "\\ T<#x>{ val var.T<#x>$func() }",
    checkShortParseFail "\\ T<#x>{}.call()",
    checkShortParseSuccess "\\ (T<#x>{}).call()",

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
    checkShortParseFail "\\ T<#x> x <- var.func()",
    checkShortParseSuccess "_, weak T<#x> x <- var.func()",
    checkShortParseFail "_, weak T<#x> x <- T<#x> x",

    checkShortParseSuccess "if (var.func()) { \\ val.call() }",
    checkShortParseSuccess "if (present(var)) { \\ val.call() }",
    checkShortParseFail "if (T<#x> x) { \\ val.call() }",
    checkShortParseSuccess "if (var) { \\ val.call() } else { \\ val.call() }",
    checkShortParseFail "if (var) { \\ val.call() } elif { \\ val.call() }",
    checkShortParseSuccess "if (v) { \\ c() } elif (v) { \\ c() }",
    checkShortParseSuccess "if (v) { \\ c() } elif (v) { \\ c() } else { \\ c() }",
    checkShortParseSuccess "if (v) { \\ c() } elif (v) { \\ c() } elif (v) { \\ c() }",

    checkShortParseSuccess "while (var.func()) { \\ val.call() }",
    checkShortParseSuccess "while (var.func()) { \\ val.call() } update { \\ call() }",

    checkShortParseSuccess "scoped { T<#x> x <- y } in return _",
    checkShortParseSuccess "scoped { T<#x> x <- y } in return var, var.T<#x>$func()",
    checkShortParseSuccess "scoped { T<#x> x <- y } in \\ var.T<#x>$func()",
    checkShortParseSuccess "scoped { T<#x> x <- y } in _, weak T<#x> x <- var.func()",

    checkShortParseSuccess "scoped { T<#x> x <- y } in if (var.func()) { \\ val.call() }",
    checkShortParseSuccess "scoped { T<#x> x <- y } in while (var.func()) { \\ val.call() }",

    checkShortParseSuccess "x <- (((var.func())).T$call())",
    checkShortParseSuccess "\\ (x <- var).func()",
    checkShortParseFail "x <- (((var.func()))",
    checkShortParseFail "(((x <- var.func())))",
    checkShortParseFail "(x) <- y",
    checkShortParseFail "T (x) <- y",
    checkShortParseFail "\\ (T x <- var).func()",
    checkShortParseSuccess "\\ call(((var.func())).T$call())",
    checkShortParseSuccess "if (((var.func()).T$call())) { }",
    checkShortParseSuccess "fail(\"reason\")",
    checkShortParseFail "\\ fail(\"reason\")",
    checkShortParseSuccess "failed <- 10",

    checkShortParseSuccess "\\var.T<#x>$func().func2().func3()",
    checkShortParseSuccess "\\T<#x>{val,var.T<#x>$func()}",
    checkShortParseSuccess "x<-var.func()",
    checkShortParseSuccess "T<#x>x<-var.func()",
    checkShortParseSuccess "_,weak T<#x>x<-var.func()",
    checkShortParseSuccess "if(v){\\c()}elif(v){\\c()}",
    checkShortParseSuccess "if(v){\\c()}elif(v){\\c()}else{\\c()}",
    checkShortParseSuccess "if(v){\\c()}elif(v){\\c()}elif(v){\\c()}",
    checkShortParseSuccess "while(var.func()){\\val.call()}",
    checkShortParseSuccess "scoped{T<#x>x<-y}in\\var.T<#x>$func()",
    checkShortParseSuccess "scoped{T<#x>x<-y}in{x<-1}",
    checkShortParseSuccess "scoped{T<#x>x<-y}in x<-1",
    checkShortParseFail "scoped{T<#x>x<-y}in{x}",

    checkShortParseSuccess "x <- !y",
    checkShortParseSuccess "x <- !y",
    checkShortParseFail "x <- !!=y",
    checkShortParseSuccess "x <- (!y).func()",
    checkShortParseSuccess "\\ !y",
    checkShortParseSuccess "if (!y) { }",

    checkShortParseSuccess "\\ !x + !y",
    checkShortParseSuccess "\\ !x - !y",
    checkShortParseSuccess "\\ !x * !y",
    checkShortParseSuccess "\\ !x / !y",
    checkShortParseSuccess "\\ !x % !y",
    checkShortParseSuccess "\\ !x == !y",
    checkShortParseSuccess "\\ !x != !y",
    checkShortParseSuccess "\\ !x < !y",
    checkShortParseSuccess "\\ !x <= !y",
    checkShortParseSuccess "\\ !x > !y",
    checkShortParseSuccess "\\ !x >= !y",
    checkShortParseSuccess "\\ !x && !y",
    checkShortParseSuccess "\\ !x || !y",
    checkShortParseSuccess "\\ ~x >> ~y",
    checkShortParseSuccess "\\ ~x << ~y",
    checkShortParseSuccess "\\ ~x & ~y",
    checkShortParseSuccess "\\ ~x | ~y",
    checkShortParseSuccess "\\ ~x ^ ~y",

    checkShortParseSuccess "x <- y + z",
    checkShortParseSuccess "x <- !y == !z",
    checkShortParseSuccess "x <- (x + y) / z",
    checkShortParseSuccess "\\ x <= y",
    checkShortParseFail "\\ x < <- y",

    checkShortParseSuccess "x <- 123 + 123",
    checkShortParseSuccess "x <- 123.0 - 123.0",
    checkShortParseFail "x <- 123.",
    checkShortParseSuccess "x <- 0.123 * 0.123",
    checkShortParseFail "x <- .123",
    checkShortParseSuccess "x <- 12.3 / 12.3",
    checkShortParseFail "x <- 12.3.",
    checkShortParseSuccess "x <- 12.3 + -456.7",
    checkShortParseSuccess "x <- \\x123aBc + \\x123aBc",
    checkShortParseFail "x <- \\x123aQc",
    checkShortParseFail "x <- \\x",
    checkShortParseFail "x <- \\x1.2",
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
                                      (Literal (IntegerLiteral _ False 1)) (NamedOperator "+")
                                      (Literal (IntegerLiteral _ False 2))) (NamedOperator "<")
                                    (Literal (IntegerLiteral _ False 4))) (NamedOperator "&&")
                                  (InfixExpression _
                                    (Literal (IntegerLiteral _ False 3)) (NamedOperator ">=")
                                    (InfixExpression _
                                      (InfixExpression _
                                        (Literal (IntegerLiteral _ False 1)) (NamedOperator "*")
                                        (Literal (IntegerLiteral _ False 2))) (NamedOperator "+")
                                      (Literal (IntegerLiteral _ False 1))))) (NamedOperator "||")
                                (Literal (BoolLiteral _ True))) -> True
                              _ -> False),

    -- This expression isn't really valid, but it ensures that the first ! is
    -- applied only to x and not x*!y.
    checkParsesAs "!x * !y + !z"
                  (\e -> case e of
                              (InfixExpression _
                                (InfixExpression _
                                  (UnaryExpression _ (NamedOperator "!")
                                    (Expression _ (NamedVariable (OutputValue _ (VariableName "x"))) [])) (NamedOperator "*")
                                  (UnaryExpression _ (NamedOperator "!")
                                    (Expression _ (NamedVariable (OutputValue _ (VariableName "y"))) []))) (NamedOperator "+")
                                (UnaryExpression _ (NamedOperator "!")
                                  (Expression _ (NamedVariable (OutputValue _ (VariableName "z"))) []))) -> True
                              _ -> False),

    checkParsesAs "1 `Int$lessThan` 2"
                  (\e -> case e of
                              (InfixExpression _
                                (Literal (IntegerLiteral _ False 1))
                                (FunctionOperator _ (
                                  FunctionSpec _
                                    (TypeFunction _ (JustTypeInstance (TypeInstance BuiltinInt (Positional []))))
                                    (FunctionName "lessThan") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "1 `Something$$foo` 2"
                  (\e -> case e of
                              (InfixExpression _
                                (Literal (IntegerLiteral _ False 1))
                                (FunctionOperator _
                                  (FunctionSpec _
                                    (CategoryFunction _ (CategoryName "Something"))
                                    (FunctionName "foo") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "1 `something.foo` 2"
                  (\e -> case e of
                              (InfixExpression _
                                (Literal (IntegerLiteral _ False 1))
                                (FunctionOperator _
                                  (FunctionSpec _
                                    (ValueFunction _
                                      (Expression _ (NamedVariable (OutputValue _ (VariableName "something"))) []))
                                    (FunctionName "foo") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "1 `require(x).foo` 2"
                  (\e -> case e of
                              InfixExpression _
                                (Literal (IntegerLiteral _ False 1))
                                  (FunctionOperator _
                                    (FunctionSpec _
                                      (ValueFunction _
                                        (Expression _ (BuiltinCall _ (FunctionCall _ BuiltinRequire (Positional [])
                                          (Positional [Expression _ (NamedVariable (OutputValue _ (VariableName "x"))) []]))) []))
                                        (FunctionName "foo") (Positional [])))
                                (Literal (IntegerLiteral _ False 2)) -> True
                              _ -> False),

    checkParsesAs "1 `foo` 2"
                  (\e -> case e of
                              (InfixExpression _
                                (Literal (IntegerLiteral _ False 1))
                                (FunctionOperator _
                                  (FunctionSpec _ UnqualifiedFunction (FunctionName "foo") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "`Bits$not` 2"
                  (\e -> case e of
                              (UnaryExpression _
                                (FunctionOperator _ (
                                  FunctionSpec _
                                    (TypeFunction _ (JustTypeInstance (TypeInstance (CategoryName "Bits") (Positional []))))
                                    (FunctionName "not") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "`Bits$$not` 2"
                  (\e -> case e of
                              (UnaryExpression _
                                (FunctionOperator _
                                  (FunctionSpec _
                                    (CategoryFunction _ (CategoryName "Bits"))
                                    (FunctionName "not") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "`bits.not` 2"
                  (\e -> case e of
                              (UnaryExpression _
                                (FunctionOperator _
                                  (FunctionSpec _
                                    (ValueFunction _
                                      (Expression _ (NamedVariable (OutputValue _ (VariableName "bits"))) []))
                                    (FunctionName "not") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "`require(x).not` 2"
                  (\e -> case e of
                              UnaryExpression _
                                  (FunctionOperator _
                                    (FunctionSpec _
                                      (ValueFunction _
                                        (Expression _ (BuiltinCall _ (FunctionCall _ BuiltinRequire (Positional [])
                                          (Positional [Expression _ (NamedVariable (OutputValue _ (VariableName "x"))) []]))) []))
                                        (FunctionName "not") (Positional [])))
                                (Literal (IntegerLiteral _ False 2)) -> True
                              _ -> False),

    checkParsesAs "`not` 2"
                  (\e -> case e of
                              (UnaryExpression _
                                (FunctionOperator _
                                  (FunctionSpec _ UnqualifiedFunction (FunctionName "not") (Positional [])))
                                (Literal (IntegerLiteral _ False 2))) -> True
                              _ -> False),

    checkParsesAs "\\b10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 2)) -> True
                                      _ -> False),

    checkParsesAs "\\B10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 2)) -> True
                                      _ -> False),

    checkParsesAs "\\o10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 8)) -> True
                                      _ -> False),

    checkParsesAs "\\O10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 8)) -> True
                                      _ -> False),

    checkParsesAs "\\d10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 10)) -> True
                                      _ -> False),

    checkParsesAs "\\D10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 10)) -> True
                                      _ -> False),

    checkParsesAs "\\x10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 16)) -> True
                                      _ -> False),

    checkParsesAs "\\X10" (\e -> case e of
                                      (Literal (IntegerLiteral _ True 16)) -> True
                                      _ -> False),

    checkParsesAs "10" (\e -> case e of
                                   (Literal (IntegerLiteral _ False 10)) -> True
                                   _ -> False),

    checkParsesAs "1.2345" (\e -> case e of
                                       (Literal (DecimalLiteral _ 12345 (-4))) -> True
                                       _ -> False),

    checkParsesAs "1.2345E+4" (\e -> case e of
                                          (Literal (DecimalLiteral _ 12345 0)) -> True
                                          _ -> False),

    checkParsesAs "1.2345E-4" (\e -> case e of
                                          (Literal (DecimalLiteral _ 12345 (-8))) -> True
                                          _ -> False)
  ]

checkParseSuccess :: String -> IO (CompileInfo ())
checkParseSuccess f = do
  contents <- loadFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check c
      | isCompileError c = compileError $ "Parse " ++ f ++ ":\n" ++ show (getCompileError c)
      | otherwise = return ()

checkParseFail :: String -> IO (CompileInfo ())
checkParseFail f = do
  contents <- loadFile f
  let parsed = readMulti f contents :: CompileInfo [ExecutableProcedure SourcePos]
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkShortParseSuccess :: String -> IO (CompileInfo ())
checkShortParseSuccess s = do
  let parsed = readSingle "(string)" s :: CompileInfo (Statement SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = compileError $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkShortParseFail :: String -> IO (CompileInfo ())
checkShortParseFail s = do
  let parsed = readSingle "(string)" s :: CompileInfo (Statement SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkParsesAs :: String -> (Expression SourcePos -> Bool) -> IO (CompileInfo ())
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
