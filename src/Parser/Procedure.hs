{- -----------------------------------------------------------------------------
Copyright 2019-2023 Kevin P. Barry

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

module Parser.Procedure (
  MarkType(..),
  PragmaExpr(..),
  PragmaStatement(..),
  pragmaExprLookup,
  pragmaHidden,
  pragmaNoTrace,
  pragmaReadOnly,
  pragmaSourceContext,
  pragmaTraceCreation,
) where

import Control.Monad (when)
import qualified Data.Set as Set

import Base.CompilerError
import Base.Positional
import Parser.Common
import Parser.Pragma
import Parser.TextParser
import Parser.TypeCategory ()
import Parser.TypeInstance ()
import Types.Procedure
import Types.TypeCategory


instance ParseFromSource (ExecutableProcedure SourceContext) where
  sourceParser = labeled "executable procedure" $ do
    c <- getSourceContext
    n <- try sourceParser
    as <- sourceParser
    rs <- sourceParser
    sepAfter (string_ "{")
    pragmas <- parsePragmas [pragmaNoTrace,pragmaTraceCreation]
    pp <- sourceParser
    c2 <- getSourceContext
    sepAfter (string_ "}")
    return $ ExecutableProcedure [c] pragmas [c2] n as rs pp

instance ParseFromSource (TestProcedure SourceContext) where
  sourceParser = labeled "test procedure" $ do
    c <- getSourceContext
    kwUnittest
    n <- try sourceParser
    sepAfter (string_ "{")
    cov <- coveragePragma <|> return False
    pp <- sourceParser
    sepAfter (string_ "}")
    return $ TestProcedure [c] n cov pp where
      coveragePragma = autoPragma "DisableCoverage" $ Left (const True)

instance ParseFromSource (ArgValues SourceContext) where
  sourceParser = labeled "procedure arguments" $ do
    c <- getSourceContext
    as <- between (sepAfter $ string_ "(")
                  (sepAfter $ string_ ")")
                  (sepBy sourceParser (sepAfter $ string_ ","))
    return $ ArgValues [c] (Positional as)

instance ParseFromSource (ReturnValues SourceContext) where
  sourceParser = labeled "procedure returns" $ namedReturns <|> unnamedReturns where
    namedReturns = do
      c <- getSourceContext
      rs <- between (sepAfter $ string_ "(")
                    (sepAfter $ string_ ")")
                    (sepBy sourceParser (sepAfter $ string_ ","))
      return $ NamedReturns [c] (Positional rs)
    unnamedReturns = do
      c <- getSourceContext
      notFollowedBy (string_ "(")
      return $ UnnamedReturns [c]

instance ParseFromSource VariableName where
  sourceParser = labeled "variable name" $ do
    noKeywords
    b <- lowerChar
    e <- sepAfter $ many alphaNumChar
    return $ boxVariableName (b:e)

boxVariableName :: String -> VariableName
boxVariableName n
  | n == "self" = VariableSelf
  | otherwise   = VariableName n

instance ParseFromSource (InputValue SourceContext) where
  sourceParser = labeled "input variable" $ variable <|> discard where
    variable = do
      c <- getSourceContext
      v <- sourceParser
      return $ InputValue [c] v
    discard = do
      c <- getSourceContext
      sepAfter_ kwIgnore
      return $ DiscardInput [c]

instance ParseFromSource (OutputValue SourceContext) where
  sourceParser = labeled "output variable" $ do
    c <- getSourceContext
    v <- sourceParser
    return $ OutputValue [c] v

instance ParseFromSource (Procedure SourceContext) where
  sourceParser = labeled "procedure" $ do
    c <- getSourceContext
    rs <- sepBy sourceParser optionalSpace
    return $ Procedure [c] rs

instance ParseFromSource (Statement SourceContext) where
  sourceParser = parseReturn <|>
                 parseBreak <|>
                 parseContinue <|>
                 parseFailCall <|>
                 parseExitCall <|>
                 parseVoid <|>
                 parseSwap <|>
                 parseAssign <|>
                 parsePragma <|>
                 parseIgnore where
    parseAssign = labeled "assignment" $ do
      c <- getSourceContext
      as <- sepBy sourceParser (sepAfter $ string_ ",")
      case as of
           [ExistingVariable (InputValue _ n)] -> parseAssignEmpty c n <|> parseAlwaysAssign c as
           _ -> parseAlwaysAssign c as
    parseAssignEmpty c n = do
      assignEmptyOperator
      e <- sourceParser
      statementEnd
      return $ AssignmentEmpty [c] n e
    parseAlwaysAssign c as = do
      assignOperator
      assignExpr c as <|> assignDefer c as
    parseSwap = labeled "swap" $ do
      c <- getSourceContext
      l <- try $ do
        var <- sourceParser
        swapOperator
        return var
      r <- sourceParser
      return $ VariableSwap [c] l r
    assignExpr c as = do
      e <- sourceParser
      statementEnd
      return $ Assignment [c] (Positional as) e
    assignDefer c as = do
      kwDefer
      statementEnd
      return $ DeferredVariables [c] as
    parseBreak = labeled "break" $ do
      c <- getSourceContext
      kwBreak
      return $ LoopBreak [c]
    parseContinue = labeled "continue" $ do
      c <- getSourceContext
      kwContinue
      return $ LoopContinue [c]
    parseFailCall = do
      c <- getSourceContext
      kwFail
      e <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
      return $ FailCall [c] e
    parseExitCall = do
      c <- getSourceContext
      kwExit
      e <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
      return $ ExitCall [c] e
    parseIgnore = do
      c <- getSourceContext
      statementStart
      e <- sourceParser
      statementEnd
      return $ IgnoreValues [c] e
    parseReturn = labeled "return" $ do
      c <- getSourceContext
      kwReturn
      emptyReturn c <|> multiReturn c
    multiReturn :: SourceContext -> TextParser (Statement SourceContext)
    multiReturn c = do
      rs <- sepBy sourceParser (sepAfter $ string_ ",")
      statementEnd
      return $ ExplicitReturn [c] (Positional rs)
    emptyReturn :: SourceContext -> TextParser (Statement SourceContext)
    emptyReturn c = do
      kwIgnore
      statementEnd
      return $ EmptyReturn [c]
    parseVoid = do
      c <- getSourceContext
      e <- sourceParser
      return $ NoValueExpression [c] e
    parsePragma = ((pragmaReadOnly <|> pragmaHidden) >>= markPragma) <|>
                  pragmaValidateRefs <|> unknownPragma
    markPragma p =
      case p of
           PragmaMarkVars c ReadOnly vs -> return $ MarkReadOnly c vs
           PragmaMarkVars c Hidden   vs -> return $ MarkHidden   c vs

instance ParseFromSource (Assignable SourceContext) where
  sourceParser = existing <|> create where
    create = labeled "variable creation" $ do
      t <- sourceParser
      strayFuncCall <|> return ()
      c <- getSourceContext
      n <- sourceParser
      return $ CreateVariable [c] t n
    existing = labeled "variable name" $ do
      n <- sourceParser
      strayFuncCall <|> return ()
      return $ ExistingVariable n
    strayFuncCall = do
      valueSymbolGet <|> typeSymbolGet <|> categorySymbolGet
      compilerErrorM "function returns must be explicitly handled"

instance ParseFromSource (VoidExpression SourceContext) where
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

instance ParseFromSource (IfElifElse SourceContext) where
  sourceParser = labeled "if-elif-else" $ do
    c <- getSourceContext
    kwIf >> parseIf c
    where
      parseIf c = do
        i <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
        p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
        next <- parseElif <|> parseElse <|> return TerminateConditional
        return $ IfStatement [c] i p next
      parseElif = do
        c <- getSourceContext
        kwElif >> parseIf c
      parseElse = do
        c <- getSourceContext
        kwElse
        p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
        return $ ElseStatement [c] p

instance ParseFromSource (IteratedLoop SourceContext) where
  sourceParser = while <|> trav where
    while = labeled "while" $ do
      c <- getSourceContext
      kwWhile
      i <- between (sepAfter $ string_ "(") (sepAfter $ string_ ")") sourceParser
      p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
      u <- fmap Just parseUpdate <|> return Nothing
      return $ WhileLoop [c] i p u
    trav = labeled "traverse" $ do
      c1 <- getSourceContext
      kwTraverse
      sepAfter_ $ string "("
      e <- sourceParser
      sepAfter_ $ string "->"
      c2 <- getSourceContext
      a <- sourceParser
      sepAfter_ $ string ")"
      p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
      u <- fmap Just parseUpdate <|> return Nothing
      return $ TraverseLoop [c1] e [c2] a p u
    parseUpdate = do
      kwUpdate
      between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser

instance ParseFromSource (ScopedBlock SourceContext) where
  sourceParser = scoped <|> justCleanup where
    scoped = labeled "scoped" $ do
      c <- getSourceContext
      kwScoped
      p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
      cl <- fmap Just parseCleanup <|> return Nothing
      kwIn
      c2 <- getSourceContext
      -- TODO: If there's a parse error in an otherwise-valid {} then the actual
      -- error might look like a multi-assignment issue.
      s <- unconditional <|> sourceParser
      return $ ScopedBlock [c] p cl [c2] s
    justCleanup = do
      c <- getSourceContext
      cl <- parseCleanup
      kwIn
      c2 <- getSourceContext
      s <- sourceParser <|> unconditional
      return $ ScopedBlock [c] (Procedure [] []) (Just cl) [c2] s
    parseCleanup = do
      kwCleanup
      between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
    unconditional = do
      c <- getSourceContext
      p <- between (sepAfter $ string_ "{") (sepAfter $ string_ "}") sourceParser
      return $ NoValueExpression [c] (Unconditional p)

unaryOperator :: TextParser (Operator SourceContext)
unaryOperator = do
  c <- getSourceContext
  o <- op
  return $ NamedOperator [c] o where
    op = labeled "unary operator" $ foldr (<|>) empty $ map (try . operator) ops
    ops = logicalUnary ++ arithUnary ++ bitwiseUnary

logicalUnary :: [String]
logicalUnary = ["!"]

arithUnary :: [String]
arithUnary = ["-"]

bitwiseUnary :: [String]
bitwiseUnary = ["~"]

infixOperator :: TextParser (Operator SourceContext)
infixOperator =  do
  c <- getSourceContext
  o <- op
  return $ NamedOperator [c] o where
    op = labeled "binary operator" $ foldr (<|>) empty $ map (try . operator) ops
    ops = compareInfix ++ logicalInfix ++ addInfix ++ subInfix ++ multInfix ++ divInfix ++ bitwiseInfix ++ bitshiftInfix

compareInfix :: [String]
compareInfix = ["==","!=","<","<=",">",">="]

logicalInfix :: [String]
logicalInfix = ["&&","||"]

addInfix :: [String]
addInfix = ["+"]

subInfix :: [String]
subInfix = ["-"]

multInfix :: [String]
multInfix = ["*"]

divInfix :: [String]
divInfix = ["/","%"]

bitwiseInfix :: [String]
bitwiseInfix = ["&","|","^"]

bitshiftInfix :: [String]
bitshiftInfix = [">>","<<"]

leftAssocInfix :: [String]
leftAssocInfix = addInfix ++ subInfix ++ multInfix ++ divInfix ++ bitwiseInfix ++ bitshiftInfix

rightAssocInfix :: [String]
rightAssocInfix = logicalInfix

nonAssocInfix :: [String]
nonAssocInfix = compareInfix

functionOperator :: TextParser (Operator SourceContext)
functionOperator = do
  c <- getSourceContext
  infixFuncStart
  q <- sourceParser
  infixFuncEnd
  return $ FunctionOperator [c] q

inOperatorSet :: Operator c -> [String] -> Bool
inOperatorSet (NamedOperator _ o) ss = o `Set.member` Set.fromList ss
inOperatorSet _                   _  = False

bothInOperatorSet :: Operator c -> Operator c -> [String] -> Bool
bothInOperatorSet o1 o2 ss = o1 `inOperatorSet` ss && o2 `inOperatorSet` ss

infixPrecedence :: Operator c -> Int
infixPrecedence o
  -- TODO: Don't hard-code this.
  | o `inOperatorSet` (multInfix ++ divInfix ++ bitshiftInfix) = 1
  | o `inOperatorSet` (addInfix ++ subInfix ++ bitwiseInfix) = 2
  | o `inOperatorSet` compareInfix = 4
  | o `inOperatorSet` logicalInfix = 5
infixPrecedence _ = 3

infixBefore :: (Show c, ErrorContextM m) => Operator c -> Operator c -> m Bool
infixBefore o1 o2 = do
  let prec1 = infixPrecedence o1
  let prec2 = infixPrecedence o2
  if prec1 /= prec2
     then return $ prec1 < prec2
     -- NOTE: Ambiguity is checked separately so that the error occurs where the
     -- second operator is parsed, rather than at the end of the expression.
     else if bothInOperatorSet o1 o2 rightAssocInfix
             then return False  -- Logical operators are right-associative.
             else return True   -- Default is left-associative.

checkAmbiguous :: (Show c, ErrorContextM m) => Operator c -> Operator c -> m ()
checkAmbiguous o1 o2 = checked where
  formatOperator o = show (getOperatorName o) ++
                     " (" ++ formatFullContext (getOperatorContext o) ++ ")"
  checked
    | infixPrecedence o1 /= infixPrecedence o2 = return ()
    | bothInOperatorSet o1 o2 leftAssocInfix  = return ()
    | bothInOperatorSet o1 o2 rightAssocInfix = return ()
    | isFunctionOperator o1 && isFunctionOperator o2 = return ()
    | otherwise =
        compilerErrorM $ "the order of operators " ++ formatOperator o1 ++
                         " and " ++ formatOperator o2 ++ " is ambiguous"

instance ParseFromSource (Expression SourceContext) where
  sourceParser = do
    e <- notInfix
    asInfix [e] [] <|> return e
    where
      -- NOTE: InitializeValue is parsed as ExpressionStart.
      notInfix = literal <|> try unaryBuiltin <|> unary <|> delegatedCall <|> expression
      asInfix es os = do
        c <- getSourceContext
        o <- infixOperator <|> functionOperator
        when (not $ null os) $ checkAmbiguous (snd $ last os) o
        e2 <- notInfix
        let es' = es ++ [e2]
        let os' = os ++ [([c],o)]
        asInfix es' os' <|> (infixToTree [] es' os')
      infixToTree [(e1,c1,o1)] [e2] [] = return $ InfixExpression c1 e1 o1 e2
      infixToTree [] (e1:es) ((c1,o1):os) = infixToTree [(e1,c1,o1)] es os
      infixToTree ((e1,c1,o1):ss) [e2] [] = let e2' = InfixExpression c1 e1 o1 e2 in
                                                infixToTree ss [e2'] []
      infixToTree ((e1,c1,o1):ss) (e2:es) ((c2,o2):os) = do
        before <- o1 `infixBefore` o2
        if before
           then let e1' = InfixExpression c1 e1 o1 e2 in
                          infixToTree ss (e1':es) ((c2,o2):os)
           else infixToTree ((e2,c2,o2):(e1,c1,o1):ss) es os
      infixToTree _ _ _ = undefined
      literal = do
        l <- sourceParser
        return $ Literal l
      unaryBuiltin = do
        c <- getSourceContext
        infixFuncStart
        n <- builtinUnary
        ps <- try $ between (sepAfter $ string_ "<")
                            (sepAfter $ string_ ">")
                            (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
        infixFuncEnd
        e <- notInfix
        return $ Expression [c] (BuiltinCall [c] $ FunctionCall [c] n (Positional ps) (Positional [(Nothing,e)])) []
      unary = do
        c <- getSourceContext
        o <- unaryOperator <|> functionOperator
        e <- notInfix
        return $ UnaryExpression [c] o e
      delegatedCall = do
        kwDelegate
        sepAfter_ $ string "->"
        try delegateCall <|> delegateInit
      delegateInit = do
        c <- getSourceContext
        t <- (paramSelf >> return Nothing) <|> fmap Just sourceParser
        return $ DelegatedInitializeValue [c] t
      delegateCall = do
        c <- getSourceContext
        infixFuncStart
        f <- sourceParser
        infixFuncEnd
        return $ DelegatedFunctionCall [c] f
      expression = labeled "expression" $ do
        c <- getSourceContext
        s <- sourceParser
        vs <- many (try sourceParser)
        return $ Expression [c] s vs

instance ParseFromSource (FunctionQualifier SourceContext) where
  -- TODO: This is probably better done iteratively.
  sourceParser = valueFunc <|> categoryFunc <|> typeFunc where
    valueFunc = do
      c <- getSourceContext
      q <- try sourceParser
      valueSymbolGet
      return $ ValueFunction [c] q
    categoryFunc = do
      c <- getSourceContext
      q <- try $ do  -- Avoids consuming the type name if : isn't present.
        q2 <- sourceParser
        categorySymbolGet
        return q2
      return $ CategoryFunction [c] q
    typeFunc = do
      c <- getSourceContext
      q <- try sourceParser
      typeSymbolGet
      return $ TypeFunction [c] q

instance ParseFromSource (FunctionSpec SourceContext) where
  sourceParser = try qualified <|> unqualified where
    qualified = do
      c <- getSourceContext
      q <- sourceParser
      n <- sourceParser
      ps <- try $ between (sepAfter $ string_ "<")
                          (sepAfter $ string_ ">")
                          (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
      return $ FunctionSpec [c] q n (Positional ps)
    unqualified = do
      c <- getSourceContext
      n <- sourceParser
      ps <- try $ between (sepAfter $ string_ "<")
                          (sepAfter $ string_ ">")
                          (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
      return $ FunctionSpec [c] UnqualifiedFunction n (Positional ps)

instance ParseFromSource (InstanceOrInferred SourceContext) where
  sourceParser = assigned <|> inferred where
    assigned = do
      c <- getSourceContext
      t <- sourceParser
      return $ AssignedInstance [c] t
    inferred = do
      c <- getSourceContext
      sepAfter_ inferredParam
      return $ InferredInstance [c]

parseFunctionCall :: SourceContext -> FunctionName -> TextParser (FunctionCall SourceContext)
parseFunctionCall c n = do
  -- NOTE: try is needed here so that < operators work when the left side is
  -- just a variable name, e.g., x < y.
  ps <- try $ between (sepAfter $ string_ "<")
                      (sepAfter $ string_ ">")
                      (sepBy sourceParser (sepAfter $ string_ ",")) <|> return []
  es <- between (sepAfter $ string_ "(")
                (sepAfter $ string_ ")")
                (sepBy parseArg (sepAfter $ string_ ","))
  return $ FunctionCall [c] n (Positional ps) (Positional es) where
    parseArg = do
      l <- try (fmap Just sourceParser) <|> return Nothing
      e <- sourceParser
      return (l,e)

builtinFunction :: TextParser FunctionName
builtinFunction = foldr (<|>) empty $ map try [
    kwPresent  >> return BuiltinPresent,
    kwReduce   >> return BuiltinReduce,
    kwRequire  >> return BuiltinRequire,
    kwStrong   >> return BuiltinStrong,
    kwIdentify >> return BuiltinIdentify,
    kwTypename >> return BuiltinTypename
  ]

builtinUnary :: TextParser FunctionName
builtinUnary = foldr (<|>) empty $ map try [
    kwPresent  >> return BuiltinPresent,
    kwReduce   >> return BuiltinReduce,
    kwRequire  >> return BuiltinRequire,
    kwStrong   >> return BuiltinStrong,
    kwIdentify >> return BuiltinIdentify
  ]

instance ParseFromSource (ExpressionStart SourceContext) where
  sourceParser = labeled "expression start" $
                 parens <|>
                 variableOrUnqualified <|>
                 builtinCall <|>
                 builtinValue <|>
                 sourceContext <|>
                 exprLookup <|>
                 exprMacro <|>
                 categoryCall <|>
                 -- Keep this before typeCall, since it does a look-ahead for {.
                 initalize <|>
                 typeCall <|>
                 stringLiteral <|>
                 charLiteral <|>
                 boolLiteral <|>
                 emptyLiteral where
    parens = do
      c <- getSourceContext
      sepAfter (string_ "(")
      e <- try (assign c) <|> expr c
      sepAfter (string_ ")")
      return e
    assign :: SourceContext -> TextParser (ExpressionStart SourceContext)
    assign c = do
      n <- sourceParser
      o <- (assignEmptyOperator >> return AssignIfEmpty) <|> (assignOperator >> return AlwaysAssign)
      e <- sourceParser
      return $ InlineAssignment [c] n o e
    expr :: SourceContext -> TextParser (ExpressionStart SourceContext)
    expr c = do
      e <- sourceParser
      return $ ParensExpression [c] e
    builtinCall = do
      c <- getSourceContext
      n <- builtinFunction
      f <- parseFunctionCall c n
      return $ BuiltinCall [c] f
    builtinValue = do
      c <- getSourceContext
      n <- builtinValues
      return $ NamedVariable (OutputValue [c] (boxVariableName n))
    sourceContext = do
      pragma <- pragmaSourceContext
      case pragma of
           (PragmaSourceContext c) -> return $ UnambiguousLiteral (StringLiteral [c] (show c))
           _ -> undefined  -- Should be caught above.
    exprLookup = do
      pragma <- pragmaExprLookup
      case pragma of
           (PragmaExprLookup c name) -> return $ NamedMacro c name
           _ -> undefined  -- Should be caught above.
    exprMacro = do
      (c,macro) <- macroExpression
      return $ ExpressionMacro [c] macro
    variableOrUnqualified = do
      c <- getSourceContext
      n <- sourceParser :: TextParser VariableName
      asUnqualifiedCall c n <|> asVariable c n
    asVariable c n = do
      return $ NamedVariable (OutputValue [c] n)
    asUnqualifiedCall c n = do
      f <- parseFunctionCall c (FunctionName (vnName n))
      return $ UnqualifiedCall [c] f
    categoryCall = do
      c <- getSourceContext
      t <- try $ do  -- Avoids consuming the type name if : isn't present.
        t2 <- sourceParser
        categorySymbolGet
        return t2
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ CategoryCall [c] t f
    typeCall = do
      c <- getSourceContext
      t <- try sourceParser
      typeSymbolGet
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ TypeCall [c] t f
    initalize = do
      c <- getSourceContext
      t <- try $ do  -- Avoids consuming the type name if { isn't present.
        t2 <- (paramSelf >> return Nothing) <|> fmap Just sourceParser
        sepAfter (labeled "@value initializer" $ string_ "{")
        return t2
      as <- sepBy sourceParser (sepAfter $ string_ ",")
      sepAfter (string_ "}")
      return $ InitializeValue [c] t (Positional as)
    stringLiteral = do
      c <- getSourceContext
      ss <- quotedString
      optionalSpace
      return $ UnambiguousLiteral $ StringLiteral [c] ss
    charLiteral = do
      c <- getSourceContext
      string_ "'"
      ch <- stringChar <|> char '"'
      string_ "'"
      optionalSpace
      return $ UnambiguousLiteral $ CharLiteral [c] ch
    boolLiteral = do
      c <- getSourceContext
      b <- try $ (kwTrue >> return True) <|> (kwFalse >> return False)
      return $ UnambiguousLiteral $ BoolLiteral [c] b
    emptyLiteral = do
      c <- getSourceContext
      kwEmpty
      return $ UnambiguousLiteral $ EmptyLiteral [c]

instance ParseFromSource (ValueLiteral SourceContext) where
  sourceParser = labeled "literal" $
                 -- NOTE: StringLiteral, CharLiteral, BoolLiteral , and
                 -- EmptyLiteral are parsed as ExpressionStart.
                 escapedIntegerOrDecimal <|>
                 integerOrDecimal where
    escapedIntegerOrDecimal = do
      c <- getSourceContext
      escapeStart
      b <- oneOf "bBoOdDxX"
      let (digitParser,base) = case b of
                                    'b' -> (parseBin,2)
                                    'B' -> (parseBin,2)
                                    'o' -> (parseOct,8)
                                    'O' -> (parseOct,8)
                                    'd' -> (parseDec,10)
                                    'D' -> (parseDec,10)
                                    'x' -> (parseHex,16)
                                    'X' -> (parseHex,16)
                                    _ -> undefined
      d <- fmap snd digitParser
      decimal c d False base digitParser <|> integer c d True
    integerOrDecimal = do
      c <- getSourceContext
      d <- fmap snd parseDec
      decimal c d True 10 parseDec <|> integer c d False
    decimal c d allowExp base digitParser = do
      char_ '.'
      (n,d2) <- digitParser
      e <- if allowExp
              then decExponent <|> return 0
              else return 0
      optionalSpace
      return $ DecimalLiteral [c] (d*base^n + d2) (e - n) base
    decExponent = do
      string_ "e" <|> string_ "E"
      s <- (string_ "+" >> return 1) <|> (string_ "-" >> return (-1)) <|> return 1
      e <- fmap snd parseDec
      return (s*e)
    integer c d unsigned = do
      optionalSpace
      return $ IntegerLiteral [c] unsigned d

instance ParseFromSource (ValueOperation SourceContext) where
  sourceParser = valueCall <|> conversion <|> selectReturn where
    valueCall = labeled "function call" $ do
      c <- getSourceContext
      o <- (valueSymbolGet >> return AlwaysCall) <|> (valueSymbolMaybeGet >> return CallUnlessEmpty)
      n <- sourceParser
      f <- parseFunctionCall c n
      return $ ValueCall [c] o f
    conversion = labeled "type conversion" $ do
      c <- getSourceContext
      inferredParam
      t <- sourceParser -- NOTE: Should not need try here.
      return $ TypeConversion [c] t
    selectReturn = labeled "return selection" $ do
      c <- getSourceContext
      sepAfter_ (string_ "{")
      pos <- labeled "return position" $ fmap snd parseDec
      sepAfter_ (string_ "}")
      return $ SelectReturn [c] (fromIntegral pos)

instance ParseFromSource MacroName where
  sourceParser = labeled "macro name" $ do
    h <- upperChar <|> char '_'
    t <- many (upperChar <|> digitChar <|> char '_')
    optionalSpace
    return $ MacroName (h:t)

pragmaNoTrace :: TextParser (PragmaProcedure SourceContext)
pragmaNoTrace = autoPragma "NoTrace" $ Left parseAt where
  parseAt c = PragmaTracing [c] NoTrace

pragmaTraceCreation :: TextParser (PragmaProcedure SourceContext)
pragmaTraceCreation = autoPragma "TraceCreation" $ Left parseAt where
  parseAt c = PragmaTracing [c] TraceCreation

data PragmaExpr c =
  PragmaExprLookup {
    pelContext :: [c],
    pelName :: MacroName
  } |
  PragmaSourceContext {
    pscContext :: c
  }
  deriving (Show)

pragmaExprLookup :: TextParser (PragmaExpr SourceContext)
pragmaExprLookup = autoPragma "ExprLookup" $ Right parseAt where
  parseAt c = do
    name <- sourceParser
    return $ PragmaExprLookup [c] name

pragmaSourceContext :: TextParser (PragmaExpr SourceContext)
pragmaSourceContext = autoPragma "SourceContext" $ Left parseAt where
  parseAt c = PragmaSourceContext c

macroExpression :: TextParser (SourceContext,MacroExpression)
macroExpression = callTrace where
  callTrace = autoPragma "CallTrace" $ Left (flip (,) MacroCallTrace)

data MarkType = ReadOnly | Hidden deriving (Show)

data PragmaStatement c =
  PragmaMarkVars {
    pmvContext :: [c],
    pmvType :: MarkType,
    pmvVars :: [VariableName]
  }
  deriving (Show)

pragmaReadOnly :: TextParser (PragmaStatement SourceContext)
pragmaReadOnly = autoPragma "ReadOnly" $ Right parseAt where
  parseAt c = do
    vs <- labeled "variable names" $ sepBy sourceParser (sepAfter $ string ",")
    return $ PragmaMarkVars [c] ReadOnly vs

pragmaHidden :: TextParser (PragmaStatement SourceContext)
pragmaHidden = autoPragma "Hidden" $ Right parseAt where
  parseAt c = do
    vs <- labeled "variable names" $ sepBy sourceParser (sepAfter $ string ",")
    return $ PragmaMarkVars [c] Hidden vs

pragmaValidateRefs :: TextParser (Statement SourceContext)
pragmaValidateRefs = autoPragma "ValidateRefs" $ Right parseAt where
  parseAt c = do
    vs <- labeled "variable names" $ sepBy sourceParser (sepAfter $ string ",")
    return $ ValidateRefs [c] vs
