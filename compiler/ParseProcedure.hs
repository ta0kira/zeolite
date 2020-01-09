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

module ParseProcedure (
) where

import Text.Parsec
import Text.Parsec.String

import ParseCategory
import ParseInstance
import ParserBase
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


instance ParseFromSource (ExecutableProcedure SourcePos) where
  sourceParser = labeled "executable procedure" $ do
    c <- getPosition
    n <- try sourceParser
    as <- sourceParser
    rs <- sourceParser
    sepAfter (string "{")
    pp <- sourceParser
    c2 <- getPosition
    sepAfter (string "}")
    return $ ExecutableProcedure [c] [c2] n as rs pp

instance ParseFromSource (ArgValues SourcePos) where
  sourceParser = labeled "procedure arguments" $ do
    c <- getPosition
    as <- between (sepAfter $ string "(")
                  (sepAfter $ string ")")
                  (sepBy sourceParser (sepAfter $ string ","))
    return $ ArgValues [c] (ParamSet as)

instance ParseFromSource (ReturnValues SourcePos) where
  sourceParser = labeled "procedure returns" $ namedReturns <|> unnamedReturns where
    namedReturns = do
      c <- getPosition
      rs <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string ","))
      return $ NamedReturns [c] (ParamSet rs)
    unnamedReturns = do
      c <- getPosition
      notFollowedBy (string "(")
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
      sepAfter $ string "_"
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
  sourceParser = parseIgnore <|>
                 parseReturn <|>
                 parseBreak <|>
                 parseContinue <|>
                 parseVoid <|>
                 parseAssign where
    parseAssign = labeled "statement" $ do
      c <- getPosition
      as <- multiDest <|> try singleDest
      e <- sourceParser
      statementEnd
      return $ Assignment [c] (ParamSet as) e
    parseBreak = labeled "break" $ do
      c <- getPosition
      try kwBreak
      return $ LoopBreak [c]
    parseContinue = labeled "continue" $ do
      c <- getPosition
      try kwContinue
      return $ LoopContinue [c]
    multiDest = do
      as <- between (sepAfter $ string "{")
                    (sepAfter $ string "}")
                    (sepBy sourceParser (sepAfter $ string ","))
      assignOperator
      return as
    singleDest = do
      a <- sourceParser
      assignOperator
      return [a]
    parseIgnore = do
      c <- getPosition
      statementStart
      e <- sourceParser
      statementEnd
      return $ IgnoreValues [c] e
    parseReturn = labeled "return" $ do
      c <- getPosition
      try kwReturn
      multiReturn c <|> singleReturn c <|> emptyReturn c
    multiReturn :: SourcePos -> Parser (Statement SourcePos)
    multiReturn c = do
      rs <- between (sepAfter $ string "{")
                    (sepAfter $ string "}")
                    (sepBy sourceParser (sepAfter $ string ","))
      statementEnd
      return $ ExplicitReturn [c] (ParamSet rs)
    singleReturn :: SourcePos -> Parser (Statement SourcePos)
    singleReturn c = do
      r <- sourceParser
      statementEnd
      return $ ExplicitReturn [c] (ParamSet [r])
    emptyReturn :: SourcePos -> Parser (Statement SourcePos)
    emptyReturn c = do
      kwIgnore
      statementEnd
      return $ EmptyReturn [c]
    parseVoid = do
      e <- sourceParser
      return $ NoValueExpression e

instance ParseFromSource (Assignable SourcePos) where
  sourceParser = existing <|> create where
    create = labeled "variable creation" $ do
      t <- sourceParser
      c <- getPosition
      n <- sourceParser
      return $ CreateVariable [c] t n
    existing = labeled "variable name" $ do
      n <- sourceParser
      return $ ExistingVariable n

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
        i <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
        p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
        next <- parseElif <|> parseElse <|> return TerminateConditional
        return $ IfStatement [c] i p next
      parseElif = do
        c <- getPosition
        try kwElif >> parseIf c
      parseElse = do
        c <- getPosition
        try kwElse
        p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
        return $ ElseStatement [c] p

instance ParseFromSource (WhileLoop SourcePos) where
  sourceParser = labeled "while" $ do
    c <- getPosition
    try kwWhile
    i <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
    p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
    u <- fmap Just parseUpdate <|> return Nothing
    return $ WhileLoop [c] i p u
    where
      parseUpdate = do
        try kwUpdate
        between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser

instance ParseFromSource (ScopedBlock SourcePos) where
  sourceParser = labeled "scoped" $ do
    c <- getPosition
    try kwScoped
    p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
    kwIn
    s <- sourceParser
    return $ ScopedBlock [c] p s

instance ParseFromSource (Expression SourcePos) where
  sourceParser = do
    e <- notInfix
    asInfix [e] [] <|> return e
    where
      notInfix = literal <|> unary <|> expression <|> initalize
      asInfix es os = do
        c <- getPosition
        o <- binaryOperator
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
      literal = do
        l <- sourceParser
        return $ Literal l
      unary = do
        c <- getPosition
        o <- unaryOperator
        e <- sourceParser
        return $ UnaryExpression [c] o e
      expression = labeled "expression" $ do
        c <- getPosition
        s <- try sourceParser
        vs <- many sourceParser
        return $ Expression [c] s vs
      initalize = do
        c <- getPosition
        t <- try sourceParser :: Parser TypeInstance
        sepAfter (string "{")
        withParams c t <|> withoutParams c t
      withParams c t = do
        try kwTypes
        ps <- between (sepAfter $ string "<")
                      (sepAfter $ string ">")
                      (sepBy sourceParser (sepAfter $ string ","))
        as <- (sepAfter (string ",") >> sepBy sourceParser (sepAfter $ string ",")) <|> return []
        sepAfter (string "}")
        return $ InitializeValue [c] t (ParamSet ps) (ParamSet as)
      withoutParams c t = do
        as <- sepBy sourceParser (sepAfter $ string ",")
        sepAfter (string "}")
        return $ InitializeValue [c] t (ParamSet []) (ParamSet as)

parseFunctionCall :: SourcePos -> FunctionName -> Parser (FunctionCall SourcePos)
parseFunctionCall c n = do
  -- NOTE: try is needed here so that < operators work when the left side is
  -- just a variable name, e.g., x < y.
  ps <- try $ between (sepAfter $ string "<")
                      (sepAfter $ string ">")
                      (sepBy sourceParser (sepAfter $ string ",")) <|> return []
  es <- between (sepAfter $ string "(")
                (sepAfter $ string ")")
                (sepBy sourceParser (sepAfter $ string ","))
  return $ FunctionCall [c] n (ParamSet ps) (ParamSet es)

builtinFunction :: Parser FunctionName
builtinFunction = foldr (<|>) (fail "empty") $ map try [
    kwPresent >> return BuiltinPresent,
    kwReduce >> return BuiltinReduce,
    kwRequire >> return BuiltinRequire,
    kwStrong >> return BuiltinStrong,
    kwTypename >> return BuiltinTypename,
    kwFail >> return BuiltinFail
  ]

instance ParseFromSource (ExpressionStart SourcePos) where
  sourceParser = labeled "expression start" $
                 parens <|>
                 variableOrUnqualified <|>
                 builtinCall <|>
                 builtinValue <|>
                 try typeOrCategoryCall <|>
                 typeCall where
    parens = do
      c <- getPosition
      sepAfter (string "(")
      e <- try (assign c) <|> expr c
      sepAfter (string ")")
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
    variableOrUnqualified = do
      c <- getPosition
      n <- sourceParser :: Parser VariableName
      asUnqualifiedCall c n <|> asVariable c n
    asVariable c n = do
      return $ NamedVariable (OutputValue [c] n)
    asUnqualifiedCall c n = do
      f <- parseFunctionCall c (FunctionName (vnName n))
      return $ UnqualifiedCall [c] f
    typeOrCategoryCall = do
      c <- getPosition
      t <- sourceParser :: Parser CategoryName
      asType c t <|> asCategory c t
    asType c t = do
      try typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ TypeCall [c] (JustTypeInstance $ TypeInstance t $ ParamSet []) f
    asCategory c t = do
      categorySymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ CategoryCall [c] t f
    typeCall = do
      c <- getPosition
      t <- try sourceParser
      try typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ TypeCall [c] t f

instance ParseFromSource (ValueLiteral SourcePos) where
  sourceParser = labeled "literal" $
                 stringLiteral <|>
                 hexLiteral <|>
                 numberLiteral <|>
                 boolLiteral <|>
                 emptyLiteral where
    stringLiteral = do
      c <- getPosition
      string "\""
      ss <- manyTill stringChar (string "\"")
      optionalSpace
      return $ StringLiteral [c] $ concat ss
    stringChar = escaped <|> notEscaped where
      escaped = do
        char '\\'
        v <- anyChar
        return ['\\',v]
      notEscaped = fmap (:[]) $ noneOf "\""
    hexLiteral = do
      c <- getPosition
      try (char '0' >> (char 'x' <|> char 'X'))
      ds <- many1 hexDigit
      optionalSpace
      return $ HexLiteral [c] ds
    numberLiteral = do
      c <- getPosition
      ds <- many1 digit
      decimal c ds <|> integer c ds
    decimal c ds = do
      try (char '.')
      ds2 <- many1 digit
      optionalSpace
      return $ DecimalLiteral [c] ds ds2
    integer c ds = do
      optionalSpace
      return $ IntegerLiteral [c] ds
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
