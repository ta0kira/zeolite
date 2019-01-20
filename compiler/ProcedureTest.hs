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

    checkShortParseSuccess "return _",
    checkShortParseSuccess "return var",
    checkShortParseFail "return var var",
    checkShortParseFail "return _ var",
    checkShortParseSuccess "return call()",
    checkShortParseSuccess "return var.T<`x>$func()",
    checkShortParseSuccess "return { var, var.T<`x>$func() }",
    checkShortParseFail "return { var  var.T<`x>$func() }",
    checkShortParseFail "return { var, _ }",
    checkShortParseFail "return T<`x> var",
    checkShortParseSuccess "return T<`x>{ val }",

    checkShortParseSuccess "break",
    checkShortParseFail "break var",
    checkShortParseFail "break _",
    checkShortParseFail "break { }",

    checkShortParseSuccess "~ var",
    checkShortParseFail "~ var var",
    checkShortParseSuccess "~ var.T<`x>$func().func2().func3()",
    checkShortParseSuccess "~ T<`x>$func().func2().func3()",
    checkShortParseSuccess "~ `x$func().func2().func3()",
    checkShortParseFail "~ var.T<`x>.T<`x>$func()",
    checkShortParseFail "~ var.T<`x>$T<`x>$func()",
    checkShortParseFail "~ T<`x>$func()$func2()",
    checkShortParseFail "~ var$func2()",
    checkShortParseFail "~ var.T<`x>",
    checkShortParseFail "~ T<`x> var",
    checkShortParseSuccess "~ T<`x>{ val, var.T<`x>$func() }",
    checkShortParseFail "~ T<`x>{ val var.T<`x>$func() }",
    checkShortParseFail "~ T<`x>{}.call()",
    checkShortParseSuccess "~ (T<`x>{}).call()",

    checkShortParseSuccess "x <- var.func()",
    checkShortParseFail "x <- var.func() var.func()",
    checkShortParseFail "x <- y <- var.func()",
    checkShortParseSuccess "x <- empty",
    checkShortParseSuccess "x <- true",
    checkShortParseSuccess "x <- false",
    checkShortParseSuccess "x <- require(y)",
    checkShortParseSuccess "x <- reduce<`x,`y>(z)",
    checkShortParseSuccess "x <- self",
    checkShortParseSuccess "x <- self.f()",
    checkShortParseFail "empty <- x",
    checkShortParseFail "true <- x",
    checkShortParseFail "false <- x",
    checkShortParseFail "require <- x",
    checkShortParseFail "reduce <- x",
    checkShortParseFail "self <- x",
    checkShortParseFail "T<`x> empty <- x",
    checkShortParseFail "T<`x> true <- x",
    checkShortParseFail "T<`x> false <- x",
    checkShortParseFail "T<`x> require <- x",
    checkShortParseFail "T<`x> reduce <- x",
    checkShortParseFail "T<`x> self <- x",
    checkShortParseSuccess "T<`x> x <- var.func()",
    checkShortParseSuccess "weak T<`x> x <- var.func()",
    checkShortParseFail "~ T<`x> x <- var.func()",
    checkShortParseSuccess "{ _, weak T<`x> x } <- var.func()",
    checkShortParseFail "{ _, weak T<`x> x } <- T<`x> x",

    checkShortParseSuccess "if (var.func()) { ~ val.call() }",
    checkShortParseSuccess "if (present(var)) { ~ val.call() }",
    checkShortParseFail "if (T<`x> x) { ~ val.call() }",
    checkShortParseSuccess "if (var) { ~ val.call() } else { ~ val.call() }",
    checkShortParseFail "if (var) { ~ val.call() } elif { ~ val.call() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() } else { ~ c() }",
    checkShortParseSuccess "if (v) { ~ c() } elif (v) { ~ c() } elif (v) { ~ c() }",

    checkShortParseSuccess "while (var.func()) { ~ val.call() }",

    checkShortParseSuccess "scoped { T<`x> x <- y } in return _",
    checkShortParseSuccess "scoped { T<`x> x <- y } in return { var, var.T<`x>$func() }",
    checkShortParseSuccess "scoped { T<`x> x <- y } in ~ var.T<`x>$func()",
    checkShortParseSuccess "scoped { T<`x> x <- y } in { _, weak T<`x> x } <- var.func()",

    checkShortParseSuccess "scoped { T<`x> x <- y } in if (var.func()) { ~ val.call() }",
    checkShortParseSuccess "scoped { T<`x> x <- y } in while (var.func()) { ~ val.call() }",

    checkShortParseSuccess "x <- (((var.func())).T$call())",
    checkShortParseSuccess "~ (x <- var).func()",
    checkShortParseFail "x <- (((var.func()))",
    checkShortParseFail "(((x <- var.func())))",
    checkShortParseFail "(x) <- y",
    checkShortParseFail "T (x) <- y",
    checkShortParseFail "~ (T x <- var).func()",
    checkShortParseSuccess "~ call(((var.func())).T$call())",
    checkShortParseSuccess "if (((var.func()).T$call())) { }",

    checkShortParseSuccess "~var.T<`x>$func().func2().func3()",
    checkShortParseSuccess "~T<`x>{val,var.T<`x>$func()}",
    checkShortParseSuccess "x<-var.func()",
    checkShortParseSuccess "T<`x>x<-var.func()",
    checkShortParseSuccess "{_,weak T<`x>x}<-var.func()",
    checkShortParseSuccess "if(v){~c()}elif(v){~c()}",
    checkShortParseSuccess "if(v){~c()}elif(v){~c()}else{~c()}",
    checkShortParseSuccess "if(v){~c()}elif(v){~c()}elif(v){~c()}",
    checkShortParseSuccess "while(var.func()){~val.call()}",
    checkShortParseSuccess "scoped{T<`x>x<-y}in~var.T<`x>$func()",

    checkShortParseSuccess "x <- !y",
    checkShortParseSuccess "x <- !!y",
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
    checkShortParseSuccess "x <- !!y == !z",
    checkShortParseSuccess "x <- (x + y) / z",
    checkShortParseSuccess "~ x <= y",
    checkShortParseFail "~ x < <- y"
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
