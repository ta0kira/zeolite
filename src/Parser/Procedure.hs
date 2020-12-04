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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Parser.Procedure (
) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as Set

import Parser.Common
import Parser.Pragma
import Parser.TypeCategory ()
import Parser.TypeInstance ()
import Types.Positional
import Types.Pragma
import Types.Procedure
import Types.TypeCategory


instance ParseFromSource (ExecutableProcedure SourcePos) where
  sourceParser = labeled "executable procedure" $ do
    c <- getPosition
    n <- try sourceParser
    as <- sourceParser
    rs <- sourceParser
    sepAfter (string_ "{")
    pragmas <- parsePragmas [pragmaNoTrace,pragmaTraceCreation]
    pp <- sourceParser
    c2 <- getPosition
    sepAfter (string_ "}")
    return $ ExecutableProcedure [c] pragmas [c2] n as rs pp

instance ParseFromSource (TestProcedure SourcePos) where
  sourceParser = labeled "test procedure" $ do
    c <- getPosition
    kwUnittest
    n <- try sourceParser
    sepAfter (string_ "{")
    pp <- sourceParser
    sepAfter (string_ "}")
    return $ TestProcedure [c] n pp

instance ParseFromSource (ArgValues SourcePos) where
  sourceParser = labeled "procedure arguments" $ do
    c <- getPosition
    as <- between (sepAfter $ string_ "(")
                  (sepAfter $ string_ ")")
                  (sepBy sourceParser (sepAfter $ string_ ","))
    return $ ArgValues [c] (Positional as)

instance ParseFromSource (ReturnValues SourcePos) where
  sourceParser = labeled "procedure returns" $ namedReturns <|> unnamedReturns where
    namedReturns = do
      c <- getPosition
      rs <- between (sepAfter $ string_ "(")
                    (sepAfter $ string_ ")")
                    (sepBy sourceParser (sepAfter $ string_ ","))
      return $ NamedReturns [c] (Positional rs)
    unnamedReturns = do
      c <- getPosition
      notFollowedBy (string_ "(")
      return $ UnnamedReturns [c]

instance ParseFromSource VariableName where
  sourceParser = labeled "variable name" $ do
    noKeywords
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ VariableName (b:e)

instance ParseFromSource (InputValue SourcePos) where
  sourceParser = labeled "input variable" $ variable <|> discard where
    variable = do
      c <- getPosition
      v <- sourceParser
      return $ InputValue [c] v
    discard = do
      c <- getPosition
      sepAfter (string_ "_")
      return $ DiscardInput [c]

instance ParseFromSource (OutputValue SourcePos) where
  sourceParser = labeled "output variable" $ do
    c <- getPosition
    v <- sourceParser
    return $ OutputValue [c] v

instance ParseFromSource (Procedure SourcePos) where
  sourceParser = labeled "procedure" $ do
    c <- getPosition
    rs <- sepBy sourceParser optionalSpace
    return $ Procedure [c] rs

instance ParseFromSource (Statement SourcePos) where
  sourceParser = parseReturn <|>
                 parseBreak <|>
                 parseContinue <|>
                 parseFailCall <|>
                 parseVoid <|>
                 parseAssign <|>
                 parseIgnore where
    parseAssign = labeled "statement" $ do
      c <- getPosition
      as <- sepBy sourceParser (sepAfter $ string_ ",")
      assignOperator
      e <- sourceParser
      statementEnd
      return $ Assignment [c] (Positional as) e
    parseBreak = labeled "break" $ do
      c <- getPosition
      try kwBreak
      return $ LoopBreak [c]
    parseContinue = labeled "continue" $ do
      c <- getPosition
      try kwContinue
      return $ LoopContinue [c]
    parseFailCall = do
      c <- getPosition
      try kwFail
      e <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
      return $ FailCall [c] e
    parseIgnore = do
      c <- getPosition
      statementStart
      e <- sourceParser
      statementEnd
      return $ IgnoreValues [c] e
    parseReturn = labeled "return" $ do
      c <- getPosition
      try kwReturn
      emptyReturn c <|> multiReturn c
    multiReturn :: SourcePos -> Parser (Statement SourcePos)
    multiReturn c = do
      rs <- sepBy sourceParser (sepAfter $ string_ ",")
      statementEnd
      return $ ExplicitReturn [c] (Positional rs)
    emptyReturn :: SourcePos -> Parser (Statement SourcePos)
    emptyReturn c = do
      kwIgnore
      statementEnd
      return $ EmptyReturn [c]
    parseVoid = do
      c <- getPosition
      e <- sourceParser
      return $ NoValueExpression [c] e

instance ParseFromSource (Assignable SourcePos) where
  sourceParser = existing <|> create where
    create = labeled "variable creation" $ do
      t <- sourceParser
      strayFuncCall <|> return ()
      c <- getPosition
      n <- sourceParser
      return $ CreateVariable [c] t n
    existing = labeled "variable name" $ do
      n <- sourceParser
      strayFuncCall <|> return ()
      return $ ExistingVariable n
    strayFuncCall = do
      valueSymbolGet <|> typeSymbolGet <|> categorySymbolGet
      fail "function returns must be explicitly handled"

instance ParseFromSource (VoidExpression SourcePos) where
  sourceParser = conditional <|> loop <|> scoped where
    conditional = do
      e <- sourceParser
      return $ Conditional e
    loop = do
      e <- sourceParser
      return $ Loop e
    scoped = do
      e <- sourceParser
      return $ WithScope e

instance ParseFromSource (IfElifElse SourcePos) where
  sourceParser = labeled "if-elif-else" $ do
    c <- getPosition
    try kwIf >> parseIf c
    where
      parseIf c = do
        i <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
        p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
        next <- parseElif <|> parseElse <|> return TerminateConditional
        return $ IfStatement [c] i p next
      parseElif = do
        c <- getPosition
        try kwElif >> parseIf c
      parseElse = do
        c <- getPosition
        try kwElse
        p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
        return $ ElseStatement [c] p

instance ParseFromSource (WhileLoop SourcePos) where
  sourceParser = labeled "while" $ do
    c <- getPosition
    try kwWhile
    i <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
    p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
    u <- fmap Just parseUpdate <|> return Nothing
    return $ WhileLoop [c] i p u
    where
      parseUpdate = do
        try kwUpdate
        between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser

instance ParseFromSource (ScopedBlock SourcePos) where
  sourceParser = scoped <|> justCleanup where
    scoped = labeled "scoped" $ do
      c <- getPosition
      try kwScoped
      p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
      cl <- fmap Just parseCleanup <|> return Nothing
      kwIn
      -- TODO: If there's a parse error in an otherwise-valid {} then the actual
      -- error might look like a multi-assignment issue.
      s <- unconditional <|> sourceParser
      return $ ScopedBlock [c] p cl s
    justCleanup = do
      c <- getPosition
      cl <- parseCleanup
      kwIn
      s <- sourceParser <|> unconditional
      return $ ScopedBlock [c] (Procedure [] []) (Just cl) s
    parseCleanup = do
      try kwCleanup
      between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
    unconditional = do
      c <- getPosition
      p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
      return $ NoValueExpression [c] (Unconditional p)

unaryOperator :: Parser (Operator c)
unaryOperator = op >>= return . NamedOperator where
  op = labeled "unary operator" $ foldr (<|>) (fail "empty") $ map (try . operator) ops
  ops = logicalUnary ++ arithUnary ++ bitwiseUnary

logicalUnary :: [String]
logicalUnary = ["!"]

arithUnary :: [String]
arithUnary = ["-"]

bitwiseUnary :: [String]
bitwiseUnary = ["~"]

infixOperator :: Parser (Operator c)
infixOperator = op >>= return . NamedOperator where
  op = labeled "binary operator" $ foldr (<|>) (fail "empty") $ map (try . operator) ops
  ops = compareInfix ++ logicalInfix ++ addInfix ++ subInfix ++ multInfix ++ bitwiseInfix ++ bitshiftInfix

compareInfix :: [String]
compareInfix = ["==","!=","<","<=",">",">="]

logicalInfix :: [String]
logicalInfix = ["&&","||"]

addInfix :: [String]
addInfix = ["+"]

subInfix :: [String]
subInfix = ["-"]

multInfix :: [String]
multInfix = ["*","/","%"]

bitwiseInfix :: [String]
bitwiseInfix = ["&","|","^"]

bitshiftInfix :: [String]
bitshiftInfix = [">>","<<"]

infixBefore :: Operator c -> Operator c -> Bool
infixBefore o1 o2 = (infixOrder o1 :: Int) <= (infixOrder o2 :: Int) where
  infixOrder (NamedOperator o)
    -- TODO: Don't hard-code this.
    | o `Set.member` Set.fromList (multInfix ++ bitshiftInfix) = 1
    | o `Set.member` Set.fromList (addInfix ++ subInfix ++ bitwiseInfix) = 2
    | o `Set.member` Set.fromList compareInfix = 4
    | o `Set.member` Set.fromList logicalInfix = 5
  infixOrder _ = 3

functionOperator :: Parser (Operator SourcePos)
functionOperator = do
  c <- getPosition
  infixFuncStart
  q <- sourceParser
  infixFuncEnd
  return $ FunctionOperator [c] q

instance ParseFromSource (Expression SourcePos) where
  sourceParser = do
    e <- notInfix
    asInfix [e] [] <|> return e
    where
      notInfix = literal <|> unary <|> initalize <|> expression
      asInfix es os = do
        c <- getPosition
        o <- infixOperator <|> functionOperator
        e2 <- notInfix
        let es' = es ++ [e2]
        let os' = os ++ [([c],o)]
        asInfix es' os' <|> return (infixToTree [] es' os')
      infixToTree [(e1,c1,o1)] [e2] [] = InfixExpression c1 e1 o1 e2
      infixToTree [] (e1:es) ((c1,o1):os) = infixToTree [(e1,c1,o1)] es os
      infixToTree ((e1,c1,o1):ss) [e2] [] = let e2' = InfixExpression c1 e1 o1 e2 in
                                                infixToTree ss [e2'] []
      infixToTree ((e1,c1,o1):ss) (e2:es) ((c2,o2):os)
        | o1 `infixBefore` o2 = let e1' = InfixExpression c1 e1 o1 e2 in
                                    infixToTree ss (e1':es) ((c2,o2):os)
        | otherwise = infixToTree ((e2,c2,o2):(e1,c1,o1):ss) es os
      infixToTree _ _ _ = undefined
      literal = do
        l <- sourceParser
        return $ Literal l
      unary = do
        c <- getPosition
        o <- unaryOperator <|> functionOperator
        e <- notInfix
        return $ UnaryExpression [c] o e
      expression = labeled "expression" $ do
        c <- getPosition
        s <- sourceParser
        vs <- many sourceParser
        return $ Expression [c] s vs
      initalize = do
        c <- getPosition
        t <- try $ do  -- Avoids consuming the type name if { isn't present.
          t2 <- sourceParser
          sepAfter (labeled "@value initializer" $ string_ "{")
          return t2
        withParams c t <|> withoutParams c t
      withParams c t = do
        try kwTypes
        ps <- between (sepAfter $ string_ "<")
                      (sepAfter $ string_ ">")
                      (sepBy sourceParser (sepAfter $ string_ ","))
        as <- (sepAfter (string_ ",") >> sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
        sepAfter (string_ "}")
        return $ InitializeValue [c] t (Positional ps) (Positional as)
      withoutParams c t = do
        as <- sepBy sourceParser (sepAfter $ string_ ",")
        sepAfter (string_ "}")
        return $ InitializeValue [c] t (Positional []) (Positional as)

instance ParseFromSource (FunctionQualifier SourcePos) where
  -- TODO: This is probably better done iteratively.
  sourceParser = valueFunc <|> categoryFunc <|> typeFunc where
    valueFunc = do
      c <- getPosition
      q <- try sourceParser
      valueSymbolGet
      return $ ValueFunction [c] q
    categoryFunc = do
      c <- getPosition
      q <- try $ do  -- Avoids consuming the type name if : isn't present.
        q2 <- sourceParser
        categorySymbolGet
        return q2
      return $ CategoryFunction [c] q
    typeFunc = do
      c <- getPosition
      q <- try sourceParser
      typeSymbolGet
      return $ TypeFunction [c] q

instance ParseFromSource (FunctionSpec SourcePos) where
  sourceParser = try qualified <|> unqualified where
    qualified = do
      c <- getPosition
      q <- sourceParser
      n <- sourceParser
      ps <- try $ between (sepAfter $ string_ "<")
                          (sepAfter $ string_ ">")
                          (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
      return $ FunctionSpec [c] q n (Positional ps)
    unqualified = do
      c <- getPosition
      n <- sourceParser
      ps <- try $ between (sepAfter $ string_ "<")
                          (sepAfter $ string_ ">")
                          (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
      return $ FunctionSpec [c] UnqualifiedFunction n (Positional ps)

instance ParseFromSource (InstanceOrInferred SourcePos) where
  sourceParser = assigned <|> inferred where
    assigned = do
      c <- getPosition
      t <- sourceParser
      return $ AssignedInstance [c] t
    inferred = do
      c <- getPosition
      sepAfter_ inferredParam
      return $ InferredInstance [c]

parseFunctionCall :: SourcePos -> FunctionName -> Parser (FunctionCall SourcePos)
parseFunctionCall c n = do
  -- NOTE: try is needed here so that < operators work when the left side is
  -- just a variable name, e.g., x < y.
  ps <- try $ between (sepAfter $ string_ "<")
                      (sepAfter $ string_ ">")
                      (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
  es <- between (sepAfter $ string_ "(")
                (sepAfter $ string_ ")")
                (sepBy sourceParser (sepAfter $ string_ ","))
  return $ FunctionCall [c] n (Positional ps) (Positional es)

builtinFunction :: Parser FunctionName
builtinFunction = foldr (<|>) (fail "empty") $ map try [
    kwPresent >> return BuiltinPresent,
    kwReduce >> return BuiltinReduce,
    kwRequire >> return BuiltinRequire,
    kwStrong >> return BuiltinStrong,
    kwTypename >> return BuiltinTypename
  ]

instance ParseFromSource (ExpressionStart SourcePos) where
  sourceParser = labeled "expression start" $
                 parens <|>
                 variableOrUnqualified <|>
                 builtinCall <|>
                 builtinValue <|>
                 sourceContext <|>
                 exprLookup <|>
                 categoryCall <|>
                 typeCall where
    parens = do
      c <- getPosition
      sepAfter (string_ "(")
      e <- try (assign c) <|> expr c
      sepAfter (string_ ")")
      return e
    assign :: SourcePos -> Parser (ExpressionStart SourcePos)
    assign c = do
      n <- sourceParser
      assignOperator
      e <- sourceParser
      return $ InlineAssignment [c] n e
    expr :: SourcePos -> Parser (ExpressionStart SourcePos)
    expr c = do
      e <- sourceParser
      return $ ParensExpression [c] e
    builtinCall = do
      c <- getPosition
      n <- builtinFunction
      f <- parseFunctionCall c n
      return $ BuiltinCall [c] f
    builtinValue = do
      c <- getPosition
      n <- builtinValues
      return $ NamedVariable (OutputValue [c] (VariableName n))
    sourceContext = do
      pragma <- pragmaSourceContext
      case pragma of
           (PragmaSourceContext c) -> return $ ParensExpression [c] $ Literal (StringLiteral [c] (show c))
           _ -> undefined  -- Should be caught above.
    exprLookup = do
      pragma <- pragmaExprLookup
      case pragma of
           (PragmaExprLookup c name) -> return $ NamedMacro c name
           _ -> undefined  -- Should be caught above.
    variableOrUnqualified = do
      c <- getPosition
      n <- sourceParser :: Parser VariableName
      asUnqualifiedCall c n <|> asVariable c n
    asVariable c n = do
      return $ NamedVariable (OutputValue [c] n)
    asUnqualifiedCall c n = do
      f <- parseFunctionCall c (FunctionName (vnName n))
      return $ UnqualifiedCall [c] f
    categoryCall = do
      c <- getPosition
      t <- try $ do  -- Avoids consuming the type name if : isn't present.
        t2 <- sourceParser
        categorySymbolGet
        return t2
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ CategoryCall [c] t f
    typeCall = do
      c <- getPosition
      t <- try sourceParser
      typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ TypeCall [c] t f

instance ParseFromSource (ValueLiteral SourcePos) where
  sourceParser = labeled "literal" $
                 stringLiteral <|>
                 charLiteral <|>
                 escapedInteger <|>
                 integerOrDecimal <|>
                 boolLiteral <|>
                 emptyLiteral where
    stringLiteral = do
      c <- getPosition
      ss <- quotedString
      optionalSpace
      return $ StringLiteral [c] ss
    charLiteral = do
      c <- getPosition
      string_ "'"
      ch <- stringChar <|> char '"'
      string_ "'"
      optionalSpace
      return $ CharLiteral [c] ch
    escapedInteger = do
      c <- getPosition
      escapeStart
      b <- oneOf "bBoOdDxX"
      d <- case b of
               'b' -> parseBin
               'B' -> parseBin
               'o' -> parseOct
               'O' -> parseOct
               'd' -> parseDec
               'D' -> parseDec
               'x' -> parseHex
               'X' -> parseHex
               _ -> undefined
      optionalSpace
      return $ IntegerLiteral [c] True d
    integerOrDecimal = do
      c <- getPosition
      d <- parseDec
      decimal c d <|> integer c d
    decimal c d = do
      char_ '.'
      (n,d2) <- parseSubOne
      e <- decExponent <|> return 0
      optionalSpace
      return $ DecimalLiteral [c] (d*10^n + d2) (e - n)
    decExponent = do
      string_ "e" <|> string_ "E"
      s <- (string_ "+" >> return 1) <|> (string_ "-" >> return (-1)) <|> return 1
      e <- parseDec
      return (s*e)
    integer c d = do
      optionalSpace
      return $ IntegerLiteral [c] False d
    boolLiteral = do
      c <- getPosition
      b <- try $ (kwTrue >> return True) <|> (kwFalse >> return False)
      return $ BoolLiteral [c] b
    emptyLiteral = do
      c <- getPosition
      try kwEmpty
      return $ EmptyLiteral [c]

instance ParseFromSource (ValueOperation SourcePos) where
  sourceParser = try valueCall <|> try conversion where
    valueCall = labeled "function call" $ do
      c <- getPosition
      valueSymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ ValueCall [c] f
    conversion = labeled "type conversion" $ do
      c <- getPosition
      valueSymbolGet
      t <- sourceParser -- NOTE: Should not need try here.
      typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ ConvertedCall [c] t f
