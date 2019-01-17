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
    pp <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
    return $ ExecutableProcedure [c] n as rs pp

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

instance ParseFromSource (VariableName SourcePos) where
  sourceParser = labeled "variable name" $ do
    noKeywords
    c <- getPosition
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ VariableName [c] (b:e)

instance ParseFromSource (InputValue SourcePos) where
  sourceParser = labeled "input variable" $ variable <|> ignore where
    variable = do
      v <- sourceParser
      return $ InputValue v
    ignore = do
      c <- getPosition
      sepAfter $ string "_"
      return $ IgnoreValue [c]

instance ParseFromSource (OutputValue SourcePos) where
  sourceParser = labeled "output variable" $ do
    v <- sourceParser
    return $ OutputValue v

instance ParseFromSource (Procedure SourcePos) where
  sourceParser = labeled "procedure" $ do
    c <- getPosition
    rs <- sepBy sourceParser optionalSpace
    return $ Procedure [c] rs

instance ParseFromSource (Statement SourcePos) where
  sourceParser = parseReturn <|>
                 parseBreak <|>
                 parseVoid <|>
                 parseAssign where
    parseAssign = labeled "statement" $ do
      c <- getPosition
      as <- sideEffectOnly <|> multiDest <|> try singleDest
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
      sepAfter (string "=")
      return as
    singleDest = do
      a <- sourceParser
      sepAfter (string "=")
      return [a]
    sideEffectOnly = do
      statementStart
      return []
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
      c <- getPosition
      t <- sourceParser
      n <- sourceParser
      return $ CreateVariable [c] t n
    existing = labeled "variable name" $ do
      c <- getPosition
      n <- sourceParser
      return $ ExistingVariable [c] n

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

instance ParseFromSource (Expression SourcePos) where
  sourceParser = unary <|> expression <|> initalize where
    unary = do
      c <- getPosition
      o <- unaryOperator
      e <- sourceParser
      return $ UnaryExpression [c] o e
    expression = labeled "expression" $ do
      c <- getPosition
      let ts = [] -- Expression type is unknown at parse time.
      s <- try sourceParser
      vs <- many sourceParser
      return $ Expression [c] (ParamSet ts) s vs
    initalize = do
      c <- getPosition
      t <- try sourceParser
      sepAfter (string "{")
      as <- many (optionalSpace >> singleAssign)
      sepAfter (string "}")
      return $ InitializeValue [c] t (ParamSet as)
    singleAssign = do
      n <- sourceParser
      initSeparator
      e <- sourceParser
      return (n,e)

parseFunctionCall :: FunctionName -> Parser (FunctionCall SourcePos)
parseFunctionCall n = do
  c <- getPosition
  ps <- between (sepAfter $ string "<")
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
      e <- sourceParser
      sepAfter (string ")")
      return $ ParensExpression [c] e
    builtinCall = do
      c <- getPosition
      n <- builtinFunctions
      f <- parseFunctionCall (FunctionName n)
      return $ UnqualifiedCall [c] f
    builtinValue = do
      c <- getPosition
      n <- builtinValues
      return $ VariableValue (OutputValue (VariableName [c] n))
    variableOrUnqualified = do
      n <- sourceParser :: Parser (VariableName SourcePos)
      asUnqualifiedCall n <|> asVariable n
    asVariable n = do
      c <- getPosition
      return $ VariableValue (OutputValue n)
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
