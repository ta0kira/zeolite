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
      kwBreak
      return $ LoopBreak [c]
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
    return $ WhileLoop [c] i p

instance ParseFromSource (ScopedBlock SourcePos) where
  sourceParser = labeled "scoped" $ do
    c <- getPosition
    try kwScoped
    p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
    kwIn
    s <- sourceParser
    return $ ScopedBlock [c] p s

instance ParseFromSource (Expression SourcePos) where
  sourceParser = unary <|> expression <|> initalize where
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
      t <- try sourceParser
      sepAfter (string "{")
      as <- sepBy sourceParser (sepAfter $ string ",")
      sepAfter (string "}")
      return $ InitializeValue [c] t (ParamSet as)

parseFunctionCall :: FunctionName -> Parser (FunctionCall SourcePos)
parseFunctionCall n = do
  c <- getPosition
  -- NOTE: try is needed here so that < operators work when the left side is
  -- just a variable name, e.g., x < y.
  ps <- try $ between (sepAfter $ string "<")
                      (sepAfter $ string ">")
                      (sepBy sourceParser (sepAfter $ string ",")) <|> return []
  es <- between (sepAfter $ string "(")
                (sepAfter $ string ")")
                (sepBy sourceParser (sepAfter $ string ","))
  return $ FunctionCall [c] n (ParamSet ps) (ParamSet es)

instance ParseFromSource (ExpressionStart SourcePos) where
  sourceParser = labeled "expression start" $
                 parens <|>
                 variableOrUnqualified <|>
                 builtinCall <|>
                 builtinValue <|>
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
      n <- builtinFunctions
      f <- parseFunctionCall (FunctionName n)
      return $ UnqualifiedCall [c] f
    builtinValue = do
      c <- getPosition
      n <- builtinValues
      return $ NamedVariable (OutputValue [c] (VariableName n))
    variableOrUnqualified = do
      n <- sourceParser :: Parser VariableName
      asUnqualifiedCall n <|> asVariable n
    asVariable n = do
      c <- getPosition
      return $ NamedVariable (OutputValue [c] n)
    asUnqualifiedCall n = do
      c <- getPosition
      f <- parseFunctionCall (FunctionName (vnName n))
      return $ UnqualifiedCall [c] f
    typeCall = do
      c <- getPosition
      t <- sourceParser -- NOTE: Should not need try here.
      typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall n
      return $ TypeCall [c] t f

instance ParseFromSource (ValueOperation SourcePos) where
  sourceParser = try valueCall <|> try conversion <|> binary where
    valueCall = labeled "function call" $ do
      c <- getPosition
      valueSymbolGet
      n <- sourceParser
      f <- parseFunctionCall n
      return $ ValueCall [c] f
    conversion = labeled "type conversion" $ do
      c <- getPosition
      valueSymbolGet
      t <- sourceParser -- NOTE: Should not need try here.
      typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall n
      return $ ConvertedCall [c] t f
    binary = labeled "binary operator" $ do
      c <- getPosition
      o <- try binaryOperator -- NOTE: Need try for "/", due to "//" and "/*".
      e <- sourceParser
      return $ BinaryOperation [c] o e
