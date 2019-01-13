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
    rs <- between nullParse optionalSpace (sepBy sourceParser optionalSpace)
    return $ Procedure [c] rs

instance ParseFromSource (Statement SourcePos) where
  sourceParser = try parseAssign <|>
                 try parseIgnored <|>
                 try parseReturnVals <|>
                 try parseReturnNone <|>
                 parseVoid where
    parseAssign = labeled "assignment" $ do
      c <- getPosition
      as <- sourceParser
      sepAfter (string "=")
      e <- sourceParser
      return $ Assignment [c] as e
    parseIgnored = labeled "statement" $ do
      c <- getPosition
      i <- sourceParser
      return $ NoValueCall [c] i
    parseReturnVals = labeled "value return" $ do
      c <- getPosition
      kwReturn
      rs <- sepBy1 sourceParser (sepAfter $ string ",")
      statementEnd
      return $ ExplicitReturn [c] (ParamSet rs)
    parseReturnNone = labeled "empty return" $ do
      c <- getPosition
      kwReturn
      statementEnd
      return $ EmptyReturn [c]
    parseVoid = do
      e <- sourceParser
      return $ NoValueExpression e

instance ParseFromSource (Destination SourcePos) where
  sourceParser = labeled "assignment destination" $ do
    c <- getPosition
    as <- sepBy1 sourceParser (sepAfter $ string ",")
    return $ Destination [c] (ParamSet as)

instance ParseFromSource (Assignable SourcePos) where
  sourceParser = try create <|> existing where
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
  sourceParser = try conditional <|> try loop <|> scoped where
    conditional = do
      e <- sourceParser
      return $ Conditional e
    loop = do
      e <- sourceParser
      return $ Loop e
    scoped = do
      s <- sourceParser
      e <- sourceParser
      return $ WithScope s e

instance ParseFromSource (Expression SourcePos) where
  sourceParser = labeled "expression" $ do
    c <- getPosition
    let ts = [] -- Expression type is unknown at parse time.
    a <- sourceParser
    vs <- sepBy sourceParser symbolGet
    return $ Expression [c] (ParamSet ts) a vs

instance ParseFromSource (CategoryOrSingleton SourcePos) where
  sourceParser = labeled "category name" $ do
    noKeywords
    c <- getPosition
    b <- upper
    e <- sepAfter $ many alphaNum
    return $ CategoryOrSingleton [c] (b:e)

instance ParseFromSource (ExpressionStart SourcePos) where
  sourceParser = labeled "expression start" $
                 try variable <|>
                 try categoryCall <|>
                 try typeCall <|>
                 builtinCall where
    variable = do
      v <- sourceParser
      return $ VariableValue v
    categoryCall = do
      c <- getPosition
      t <- sourceParser
      symbolGet
      n <- sourceParser
      es <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string ","))
      return $ TypeCall [c] t n (ParamSet es)
    typeCall = do
      c <- getPosition
      t <- sourceParser
      symbolGet
      n <- sourceParser
      es <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string ","))
      return $ CategoryOrSingletonCall [c] t n (ParamSet es)
    builtinCall = sourceParser

instance ParseFromSource (ValueOperation SourcePos) where
  sourceParser = try conversion <|> valueCall where
    conversion = labeled "type conversion" $ do
      c <- getPosition
      t <- sourceParser
      return $ Conversion [c] t
    valueCall = labeled "function call" $ do
      c <- getPosition
      let ts = [] -- Expression type is unknown at parse time.
      n <- sourceParser
      es <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string ","))
      return $ ValueCall [c] (ParamSet ts) n (ParamSet es)

instance ParseFromSource (IfElifElse SourcePos) where
  sourceParser = labeled "if-elif-else" $ do
    c <- getPosition
    kwIf >> parseIf c
    where
      parseIf c = do
        i <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
        p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
        next <- parseElif <|> parseElse <|> return TerminateConditional
        return $ IfStatement [c] i p next
      parseElif = do
        c <- getPosition
        kwElif >> parseIf c
      parseElse = do
        c <- getPosition
        kwElse
        p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
        return $ ElseStatement [c] p

instance ParseFromSource (WhileLoop SourcePos) where
  sourceParser = labeled "while" $ do
    c <- getPosition
    kwWhile
    i <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
    p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
    return $ WhileLoop [c] i p

instance ParseFromSource (ScopedBlock SourcePos) where
  sourceParser = labeled "scoped" $ do
    c <- getPosition
    kwScoped
    p <- between (sepAfter $ string "{") (sepAfter $ string "}") sourceParser
    s <- sourceParser
    return $ ScopedBlock [c] p s

instance ParseFromSource (Builtin SourcePos) where
  sourceParser = labeled "builtin" $ try reduce <|> try require <|> strong where
    reduce = do
      c <- getPosition
      kwReduce
      sepAfter (string "<")
      from <- sourceParser
      sepAfter (string ",")
      to <- sourceParser
      sepAfter (string ">")
      e <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
      return $ CallReduce [c] from to e
    require = do
      c <- getPosition
      kwRequire
      e <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
      return $ CallRequire [c] e
    strong = do
      c <- getPosition
      kwStrong
      e <- between (sepAfter $ string "(") (sepAfter $ string ")") sourceParser
      return $ CallStrong [c] e
