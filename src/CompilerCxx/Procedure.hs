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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerCxx.Procedure (
  categoriesFromTypes,
  categoriesFromDefine,
  categoriesFromRefine,
  compileExecutableProcedure,
  compileMainProcedure,
  compileExpression,
  compileLazyInit,
  compileStatement,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.State (execStateT,get,put,runStateT)
import Control.Monad.Trans (lift)
import Data.List (intercalate)
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Compilation.CompilerState
import Compilation.ProcedureContext (ExprMap)
import Compilation.ScopeContext
import CompilerCxx.CategoryContext
import CompilerCxx.Code
import CompilerCxx.Naming
import Types.Builtin
import Types.DefinedCategory
import Types.Function
import Types.GeneralType
import Types.Positional
import Types.Pragma
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


compileExecutableProcedure :: (Show c, CompileErrorM m, MergeableM m) =>
  ScopeContext c -> ScopedFunction c -> ExecutableProcedure c ->
  m (CompiledData [String],CompiledData [String])
compileExecutableProcedure ctx ff@(ScopedFunction _ _ _ s as1 rs1 ps1 _ _)
                               pp@(ExecutableProcedure c0 pragmas c n as2 rs2 p) = do
  ctx' <- getProcedureContext ctx ff pp
  output <- runDataCompiler compileWithReturn ctx'
  procedureTrace <- setProcedureTrace
  creationTrace  <- setCreationTrace
  return (onlyCode header,wrapProcedure output procedureTrace creationTrace)
  where
    t = scName ctx
    compileWithReturn = do
      ctx0 <- getCleanContext >>= lift . flip ccSetNoTrace (any isNoTrace pragmas)
      compileProcedure ctx0 p >>= put
      unreachable <- csIsUnreachable
      when (not unreachable) $
        doImplicitReturn [] `reviseErrorStateT`
          ("In implicit return from " ++ show n ++ formatFullContextBrace c)
    wrapProcedure output pt ct =
      mergeAll $ [
          onlyCode header2,
          indentCompiled $ onlyCodes pt,
          indentCompiled $ onlyCodes ct,
          indentCompiled $ onlyCodes defineReturns,
          indentCompiled $ onlyCodes nameParams,
          indentCompiled $ onlyCodes nameArgs,
          indentCompiled $ onlyCodes nameReturns,
          indentCompiled output,
          onlyCode close
        ]
    close = "}"
    name = callName n
    header
      | s == ValueScope =
        returnType ++ " " ++ name ++
        "(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);"
      | otherwise =
        returnType ++ " " ++ name ++ "(const ParamTuple& params, const ValueTuple& args);"
    header2
      | s == CategoryScope =
        returnType ++ " " ++ categoryName t ++ "::" ++ name ++ "(const ParamTuple& params, const ValueTuple& args) {"
      | s == TypeScope =
        returnType ++ " " ++ typeName t ++ "::" ++ name ++ "(const ParamTuple& params, const ValueTuple& args) {"
      | s == ValueScope =
        returnType ++ " " ++ valueName t ++ "::" ++ name ++
        "(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {"
      | otherwise = undefined
    returnType = "ReturnTuple"
    setProcedureTrace
      | any isNoTrace pragmas = return []
      | otherwise             = return [startFunctionTracing $ show t ++ "." ++ show n]
    setCreationTrace
      | not $ any isTraceCreation pragmas = return []
      | s /= ValueScope =
          (compileWarningM $ "Creation tracing ignored for " ++ show s ++
             " functions" ++ formatFullContextBrace c0) >> return []
      | otherwise = return [showCreationTrace]
    defineReturns
      | isUnnamedReturns rs2 = []
      | otherwise            = [returnType ++ " returns(" ++ show (length $ pValues rs1) ++ ");"]
    nameParams = flip map (zip ([0..] :: [Int]) $ pValues ps1) $
      (\(i,p2) -> paramType ++ " " ++ paramName (vpParam p2) ++ " = *params.At(" ++ show i ++ ");")
    nameArgs = flip map (zip ([0..] :: [Int]) $ filter (not . isDiscardedInput . snd) $ zip (pValues as1) (pValues $ avNames as2)) $
      (\(i,(t2,n2)) -> "const " ++ variableProxyType (pvType t2) ++ " " ++ variableName (ivName n2) ++
                       " = " ++ writeStoredVariable (pvType t2) (UnwrappedSingle $ "args.At(" ++ show i ++ ")") ++ ";")
    nameReturns
      | isUnnamedReturns rs2 = []
      | otherwise = map (\(i,(t2,n2)) -> nameReturn i (pvType t2) n2) (zip ([0..] :: [Int]) $ zip (pValues rs1) (pValues $ nrNames rs2))
    nameReturn i t2 n2
      | isPrimType t2 = variableProxyType t2 ++ " " ++ variableName (ovName n2) ++ ";"
      | otherwise =
        variableProxyType t2 ++ " " ++ variableName (ovName n2) ++
        " = " ++ writeStoredVariable t2 (UnwrappedSingle $ "returns.At(" ++ show i ++ ")") ++ ";"

compileCondition :: (Show c, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  a -> [c] -> Expression c -> CompilerState a m String
compileCondition ctx c e = do
  (e',ctx') <- lift $ runStateT compile ctx
  lift (ccGetRequired ctx') >>= csRequiresTypes
  noTrace <- csGetNoTrace
  if noTrace
     then return e'
     else return $ predTraceContext c ++ e'
  where
    compile = flip reviseErrorStateT ("In condition at " ++ formatFullContext c) $ do
      (ts,e') <- compileExpression e
      lift $ checkCondition ts
      return $ useAsUnboxed PrimBool e'
      where
        checkCondition (Positional [t]) | t == boolRequiredValue = return ()
        checkCondition (Positional ts) =
          compileErrorM $ "Conditionals must have exactly one Bool return but found {" ++
                         intercalate "," (map show ts) ++ "}"

-- Returns the state so that returns can be properly checked for if/elif/else.
compileProcedure :: (Show c, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  a -> Procedure c -> CompilerState a m a
compileProcedure ctx (Procedure _ ss) = do
  ctx' <- lift $ execStateT (sequence $ map (\s -> warnUnreachable s >> compileStatement s) ss) ctx
  return ctx' where
    warnUnreachable s = do
      unreachable <- csIsUnreachable
      lift $ when unreachable $
                  compileWarningM $ "Statement at " ++
                                    formatFullContext (getStatementContext s) ++
                                    " is unreachable"

maybeSetTrace :: (Show c, CompileErrorM m, MergeableM m,
                  CompilerContext c m [String] a) =>
  [c] -> CompilerState a m ()
maybeSetTrace c = do
  noTrace <- csGetNoTrace
  when (not noTrace) $ csWrite $ setTraceContext c

compileStatement :: (Show c, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  Statement c -> CompilerState a m ()
compileStatement (EmptyReturn c) = do
  maybeSetTrace c
  doImplicitReturn c
compileStatement (ExplicitReturn c es) = do
  es' <- sequence $ map compileExpression $ pValues es
  getReturn $ zip (map getExpressionContext $ pValues es) es'
  where
    -- Single expression, but possibly multi-return.
    getReturn [(_,(Positional ts,e))] = do
      csRegisterReturn c $ Just (Positional ts)
      maybeSetTrace c
      autoPositionalCleanup e
    -- Multi-expression => must all be singles.
    getReturn rs = do
      lift $ mergeAllM (map checkArity $ zip ([0..] :: [Int]) $ map (fst . snd) rs) `reviseErrorM`
        ("In return at " ++ formatFullContext c)
      csRegisterReturn c $ Just $ Positional $ map (head . pValues . fst . snd) rs
      let e = OpaqueMulti $ "ReturnTuple(" ++ intercalate "," (map (useAsUnwrapped . snd . snd) rs) ++ ")"
      maybeSetTrace c
      autoPositionalCleanup e
    checkArity (_,Positional [_]) = return ()
    checkArity (i,Positional ts)  =
      compileErrorM $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
compileStatement (LoopBreak c) = do
  loop <- csGetLoop
  case loop of
       NotInLoop ->
         lift $ compileErrorM $ "Using break outside of while is no allowed" ++ formatFullContextBrace c
       _ -> return ()
  csWrite ["break;"]
compileStatement (LoopContinue c) = do
  loop <- csGetLoop
  case loop of
       NotInLoop ->
         lift $ compileErrorM $ "Using continue outside of while is no allowed" ++ formatFullContextBrace c
       _ -> return ()
  csWrite $ ["{"] ++ lsUpdate loop ++ ["}","continue;"]
compileStatement (FailCall c e) = do
  csRequiresTypes (Set.fromList [BuiltinFormatted,BuiltinString])
  e' <- compileExpression e
  when (length (pValues $ fst e') /= 1) $
    lift $ compileErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e0) = e'
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueTypeMatch r fa t0 formattedRequiredValue) `reviseErrorM`
    ("In fail call at " ++ formatFullContext c)
  csSetNoReturn
  maybeSetTrace c
  csWrite ["BUILTIN_FAIL(" ++ useAsUnwrapped e0 ++ ")"]
compileStatement (IgnoreValues c e) = do
  (_,e') <- compileExpression e
  maybeSetTrace c
  csWrite ["(void) (" ++ useAsWhatever e' ++ ");"]
compileStatement (Assignment c as e) = do
  (ts,e') <- compileExpression e
  r <- csResolver
  fa <- csAllFilters
  -- Check for a count match first, to avoid the default error message.
  _ <- processPairsT alwaysPair (fmap assignableName as) ts `reviseErrorStateT`
    ("In assignment at " ++ formatFullContext c)
  _ <- processPairsT (createVariable r fa) as ts `reviseErrorStateT`
    ("In assignment at " ++ formatFullContext c)
  maybeSetTrace c
  variableTypes <- sequence $ map (uncurry getVariableType) $ zip (pValues as) (pValues ts)
  assignAll (zip3 ([0..] :: [Int]) variableTypes (pValues as)) e'
  where
    assignAll [v] e2 = assignSingle v e2
    assignAll vs e2 = do
      csWrite ["{","const auto r = " ++ useAsReturns e2 ++ ";"]
      sequence_ $ map assignMulti vs
      csWrite ["}"]
    getVariableType (CreateVariable _ t _) _ = return t
    getVariableType (ExistingVariable (InputValue c2 n)) _ = do
      (VariableValue _ _ t _) <- csGetVariable c2 n
      return t
    getVariableType (ExistingVariable (DiscardInput _)) t = return t
    createVariable r fa (CreateVariable c2 t1 n) t2 = do
      -- TODO: Call csRequiresTypes for t1. (Maybe needs a helper function.)
      lift $ mergeAllM [validateGeneralInstance r fa (vtType t1),
                        checkValueTypeMatch r fa t2 t1] `reviseErrorM`
        ("In creation of " ++ show n ++ " at " ++ formatFullContext c2)
      csAddVariable c2 n (VariableValue c2 LocalScope t1 True)
      csWrite [variableStoredType t1 ++ " " ++ variableName n ++ ";"]
    createVariable r fa (ExistingVariable (InputValue c2 n)) t2 = do
      (VariableValue _ _ t1 w) <- csGetVariable c2 n
      when (not w) $ lift $ compileErrorM $ "Cannot assign to read-only variable " ++
                                           show n ++ formatFullContextBrace c2
      -- TODO: Also show original context.
      lift $ (checkValueTypeMatch r fa t2 t1) `reviseErrorM`
        ("In assignment to " ++ show n ++ " at " ++ formatFullContext c2)
      csUpdateAssigned n
    createVariable _ _ _ _ = return ()
    assignSingle (_,t,CreateVariable _ _ n) e2 =
      csWrite [variableName n ++ " = " ++ writeStoredVariable t e2 ++ ";"]
    assignSingle (_,t,ExistingVariable (InputValue c2 n)) e2 = do
      (VariableValue _ s _ _) <- csGetVariable c2 n
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = " ++ writeStoredVariable t e2 ++ ";"]
    assignSingle _ _ = return ()
    assignMulti (i,t,CreateVariable _ _ n) =
      csWrite [variableName n ++ " = " ++
               writeStoredVariable t (UnwrappedSingle $ "r.At(" ++ show i ++ ")") ++ ";"]
    assignMulti (i,t,ExistingVariable (InputValue _ n)) = do
      (VariableValue _ s _ _) <- csGetVariable c n
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = " ++
               writeStoredVariable t (UnwrappedSingle $ "r.At(" ++ show i ++ ")") ++ ";"]
    assignMulti _ = return ()
compileStatement (NoValueExpression _ v) = compileVoidExpression v

compileLazyInit :: (Show c, CompileErrorM m, MergeableM m,
                   CompilerContext c m [String] a) =>
  DefinedMember c -> CompilerState a m ()
compileLazyInit (DefinedMember _ _ _ _ Nothing) = return mergeDefault
compileLazyInit (DefinedMember c _ t1 n (Just e)) = do
  (ts,e') <- compileExpression e
  when (length (pValues ts) /= 1) $
    lift $ compileErrorM $ "Expected single return in initializer" ++ formatFullContextBrace (getExpressionContext e)
  r <- csResolver
  fa <- csAllFilters
  let Positional [t2] = ts
  lift $ (checkValueTypeMatch r fa t2 t1) `reviseErrorM`
    ("In initialization of " ++ show n ++ " at " ++ formatFullContext c)
  csWrite [variableName n ++ "([this]() { return " ++ writeStoredVariable t1 e' ++ "; })"]

compileVoidExpression :: (Show c, CompileErrorM m, MergeableM m,
                         CompilerContext c m [String] a) =>
  VoidExpression c -> CompilerState a m ()
compileVoidExpression (Conditional ie) = compileIfElifElse ie
compileVoidExpression (Loop l) = compileWhileLoop l
compileVoidExpression (WithScope s) = compileScopedBlock s
compileVoidExpression (LineComment s) = csWrite $ map ("// " ++) $ lines s
compileVoidExpression (Unconditional p) = do
  ctx0 <- getCleanContext
  ctx <- compileProcedure ctx0 p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite ["{"]
  (lift $ ccGetOutput ctx) >>= csWrite
  csWrite ["}"]
  csInheritReturns [ctx]

compileIfElifElse :: (Show c, CompileErrorM m, MergeableM m,
                      CompilerContext c m [String] a) =>
  IfElifElse c -> CompilerState a m ()
compileIfElifElse (IfStatement c e p es) = do
  ctx0 <- getCleanContext
  e' <- compileCondition ctx0 c e
  ctx <- compileProcedure ctx0 p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite ["if (" ++ e' ++ ") {"]
  (lift $ ccGetOutput ctx) >>= csWrite
  csWrite ["}"]
  cs <- unwind es
  csInheritReturns (ctx:cs)
  where
    unwind (IfStatement c2 e2 p2 es2) = do
      ctx0 <- getCleanContext
      e2' <- compileCondition ctx0 c2 e2
      ctx <- compileProcedure ctx0 p2
      (lift $ ccGetRequired ctx) >>= csRequiresTypes
      csWrite ["else if (" ++ e2' ++ ") {"]
      (lift $ ccGetOutput ctx) >>= csWrite
      csWrite ["}"]
      cs <- unwind es2
      return $ ctx:cs
    unwind (ElseStatement _ p2) = do
      ctx0 <- getCleanContext
      ctx <- compileProcedure ctx0 p2
      (lift $ ccGetRequired ctx) >>= csRequiresTypes
      csWrite ["else {"]
      (lift $ ccGetOutput ctx) >>= csWrite
      csWrite ["}"]
      return [ctx]
    unwind TerminateConditional = fmap (:[]) get
compileIfElifElse _ = undefined

compileWhileLoop :: (Show c, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  WhileLoop c -> CompilerState a m ()
compileWhileLoop (WhileLoop c e p u) = do
  ctx0 <- getCleanContext
  e' <- compileCondition ctx0 c e
  ctx0' <- case u of
                Just p2 -> do
                  ctx1 <- lift $ ccStartLoop ctx0 (LoopSetup [])
                  ctx2 <- compileProcedure ctx1 p2
                  (lift $ ccGetRequired ctx2) >>= csRequiresTypes
                  p2' <- lift $ ccGetOutput ctx2
                  lift $ ccStartLoop ctx0 (LoopSetup p2')
                _ -> lift $ ccStartLoop ctx0 (LoopSetup [])
  (LoopSetup u') <- lift $ ccGetLoop ctx0'
  ctx <- compileProcedure ctx0' p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite ["while (" ++ e' ++ ") {"]
  (lift $ ccGetOutput ctx) >>= csWrite
  csWrite $ ["{"] ++ u' ++ ["}"]
  csWrite ["}"]

compileScopedBlock :: (Show c, CompileErrorM m, MergeableM m,
                       CompilerContext c m [String] a) =>
  ScopedBlock c -> CompilerState a m ()
compileScopedBlock s = do
  let (vs,p,cl,st) = rewriteScoped s
  -- Capture context so we can discard scoped variable names.
  ctx0 <- getCleanContext
  r <- csResolver
  fa <- csAllFilters
  sequence_ $ map (createVariable r fa) vs
  ctxP <- compileProcedure ctx0 p
  (ctxP',cl',ctxCl) <-
    case cl of
         Just p2 -> do
           ctx0' <- lift $ ccClearOutput ctxP
           ctxCl <- compileProcedure ctx0' p2
           p2' <- lift $ ccGetOutput ctxCl
           noTrace <- csGetNoTrace
           -- TODO: It might be helpful to add a new trace-context line for this
           -- so that the line that triggered the cleanup is still in the trace.
           let p2'' = if noTrace
                      then []
                      else ["{",startCleanupTracing] ++ p2' ++ ["}"]
           ctxP' <- lift $ ccPushCleanup ctxP (CleanupSetup [ctxCl] p2'')
           return (ctxP',p2'',ctxCl)
         Nothing -> return (ctxP,[],ctxP)
  -- Make variables to be created visible *after* p has been compiled so that p
  -- can't refer to them.
  ctxP'' <- lift $ execStateT (sequence $ map showVariable vs) ctxP'
  ctxS <- compileProcedure ctxP'' (Procedure [] [st])
  csWrite ["{"]
  (lift $ ccGetOutput ctxS) >>= csWrite
  csWrite cl'
  csWrite ["}"]
  sequence_ $ map showVariable vs
  (lift $ ccGetRequired ctxS)  >>= csRequiresTypes
  (lift $ ccGetRequired ctxCl) >>= csRequiresTypes
  csInheritReturns [ctxS]
  csInheritReturns [ctxCl]
  where
    createVariable r fa (c,t,n) = do
      lift $ validateGeneralInstance r fa (vtType t) `reviseErrorM`
        ("In creation of " ++ show n ++ " at " ++ formatFullContext c)
      csWrite [variableStoredType t ++ " " ++ variableName n ++ ";"]
    showVariable (c,t,n) = do
      -- TODO: Call csRequiresTypes for t. (Maybe needs a helper function.)
      csAddVariable c n (VariableValue c LocalScope t True)
    -- Don't merge if the second scope has cleanup, so that the latter can't
    -- refer to variables defined in the first scope.
    rewriteScoped (ScopedBlock _ p cl@(Just _)
                               s2@(NoValueExpression _ (WithScope
                                   (ScopedBlock _ _ (Just _) _)))) =
      ([],p,cl,s2)
    -- Merge chained scoped sections into a single section.
    rewriteScoped (ScopedBlock c (Procedure c2 ss1) cl1
                               (NoValueExpression _ (WithScope
                                (ScopedBlock _ (Procedure _ ss2) cl2 s2)))) =
      rewriteScoped $ ScopedBlock c (Procedure c2 $ ss1 ++ ss2) (cl1 <|> cl2) s2
    -- Gather to-be-created variables.
    rewriteScoped (ScopedBlock _ p cl (Assignment c2 vs e)) =
      (created,p,cl,Assignment c2 (Positional existing) e) where
        (created,existing) = foldr update ([],[]) (pValues vs)
        update (CreateVariable c t n) (cs,es) = ((c,t,n):cs,(ExistingVariable $ InputValue c n):es)
        update e2 (cs,es) = (cs,e2:es)
    -- Merge the statement into the scoped block.
    rewriteScoped (ScopedBlock _ p cl s2) =
      ([],p,cl,s2)

compileExpression :: (Show c, CompileErrorM m, MergeableM m,
                      CompilerContext c m [String] a) =>
  Expression c -> CompilerState a m (ExpressionType,ExprValue)
compileExpression = compile where
  compile (Literal (StringLiteral _ l)) = do
    csRequiresTypes (Set.fromList [BuiltinString])
    return (Positional [stringRequiredValue],UnboxedPrimitive PrimString $ "PrimString_FromLiteral(" ++ escapeChars l ++ ")")
  compile (Literal (CharLiteral _ l)) = do
    csRequiresTypes (Set.fromList [BuiltinChar])
    return (Positional [charRequiredValue],UnboxedPrimitive PrimChar $ "PrimChar('" ++ escapeChar l ++ "')")
  compile (Literal (IntegerLiteral c True l)) = do
    csRequiresTypes (Set.fromList [BuiltinInt])
    when (l > 2^(64 :: Integer) - 1) $ lift $ compileErrorM $
      "Literal " ++ show l ++ formatFullContextBrace c ++ " is greater than the max value for 64-bit unsigned"
    let l' = if l > 2^(63 :: Integer) - 1 then l - 2^(64 :: Integer) else l
    return (Positional [intRequiredValue],UnboxedPrimitive PrimInt $ "PrimInt(" ++ show l' ++ ")")
  compile (Literal (IntegerLiteral c False l)) = do
    csRequiresTypes (Set.fromList [BuiltinInt])
    when (l > 2^(63 :: Integer) - 1) $ lift $ compileErrorM $
      "Literal " ++ show l ++ formatFullContextBrace c ++ " is greater than the max value for 64-bit signed"
    when ((-l) > (2^(63 :: Integer) - 2)) $ lift $ compileErrorM $
      "Literal " ++ show l ++ formatFullContextBrace c ++ " is less than the min value for 64-bit signed"
    return (Positional [intRequiredValue],UnboxedPrimitive PrimInt $ "PrimInt(" ++ show l ++ ")")
  compile (Literal (DecimalLiteral _ l e)) = do
    csRequiresTypes (Set.fromList [BuiltinFloat])
    -- TODO: Check bounds.
    return (Positional [floatRequiredValue],UnboxedPrimitive PrimFloat $ "PrimFloat(" ++ show l ++ "E" ++ show e ++ ")")
  compile (Literal (BoolLiteral _ True)) = do
    csRequiresTypes (Set.fromList [BuiltinBool])
    return (Positional [boolRequiredValue],UnboxedPrimitive PrimBool "true")
  compile (Literal (BoolLiteral _ False)) = do
    csRequiresTypes (Set.fromList [BuiltinBool])
    return (Positional [boolRequiredValue],UnboxedPrimitive PrimBool "false")
  compile (Literal (EmptyLiteral _)) = do
    return (Positional [emptyValue],UnwrappedSingle "Var_empty")
  compile (Expression _ s os) = do
    foldl transform (compileExpressionStart s) os
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec _ (CategoryFunction c2 cn) fn ps)) e) =
    compile (Expression c (CategoryCall c2 cn (FunctionCall c fn ps (Positional [e]))) [])
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec _ (TypeFunction c2 tn) fn ps)) e) =
    compile (Expression c (TypeCall c2 tn (FunctionCall c fn ps (Positional [e]))) [])
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec _ (ValueFunction c2 e0) fn ps)) e) =
    compile (Expression c (ParensExpression c2 e0) [ValueCall c (FunctionCall c fn ps (Positional [e]))])
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec c2 UnqualifiedFunction fn ps)) e) =
    compile (Expression c (UnqualifiedCall c2 (FunctionCall c fn ps (Positional [e]))) [])
  compile (UnaryExpression c (NamedOperator "-") (Literal (IntegerLiteral _ _ l))) =
    compile (Literal (IntegerLiteral c False (-l)))
  compile (UnaryExpression c (NamedOperator "-") (Literal (DecimalLiteral _ l e))) =
    compile (Literal (DecimalLiteral c (-l) e))
  compile (UnaryExpression c (NamedOperator o) e) = do
    (Positional ts,e') <- compileExpression e
    t' <- requireSingle c ts
    doUnary t' e'
    where
      doUnary t e2
        | o == "!" = doNot t e2
        | o == "-" = doNeg t e2
        | o == "~" = doComp t e2
        | otherwise = lift $ compileErrorM $ "Unknown unary operator \"" ++ o ++ "\" " ++
                                            formatFullContextBrace c
      doNot t e2 = do
        when (t /= boolRequiredValue) $
          lift $ compileErrorM $ "Cannot use " ++ show t ++ " with unary ! operator" ++
                                formatFullContextBrace c
        return $ (Positional [boolRequiredValue],UnboxedPrimitive PrimBool $ "!" ++ useAsUnboxed PrimBool e2)
      doNeg t e2
        | t == intRequiredValue = return $ (Positional [intRequiredValue],
                                            UnboxedPrimitive PrimInt $ "-" ++ useAsUnboxed PrimInt e2)
        | t == floatRequiredValue = return $ (Positional [floatRequiredValue],
                                             UnboxedPrimitive PrimFloat $ "-" ++ useAsUnboxed PrimFloat e2)
        | otherwise = lift $ compileErrorM $ "Cannot use " ++ show t ++ " with unary - operator" ++
                                            formatFullContextBrace c
      doComp t e2
        | t == intRequiredValue = return $ (Positional [intRequiredValue],
                                            UnboxedPrimitive PrimInt $ "~" ++ useAsUnboxed PrimInt e2)
        | otherwise = lift $ compileErrorM $ "Cannot use " ++ show t ++ " with unary ~ operator" ++
                                            formatFullContextBrace c
  compile (InitializeValue c t ps es) = do
    es' <- sequence $ map compileExpression $ pValues es
    (ts,es'') <- getValues es'
    csCheckValueInit c t (Positional ts) ps
    params <- expandParams $ tiParams t
    params2 <- expandParams2 $ ps
    sameType <- csSameType t
    s <- csCurrentScope
    let typeInstance = getType sameType s params
    -- TODO: This is unsafe if used in a type or category constructor.
    return (Positional [ValueType RequiredValue $ SingleType $ JustTypeInstance t],
            UnwrappedSingle $ valueCreator (tiName t) ++ "(" ++ typeInstance ++ ", " ++ params2 ++ ", " ++ es'' ++ ")")
    where
      getType True TypeScope  _ = "*this"
      getType True ValueScope _ = "parent"
      getType _    _ params = typeCreator (tiName t) ++ "(" ++ params ++ ")"
      -- Single expression, but possibly multi-return.
      getValues [(Positional ts,e)] = return (ts,useAsArgs e)
      -- Multi-expression => must all be singles.
      getValues rs = do
        lift $ mergeAllM (map checkArity $ zip ([0..] :: [Int]) $ map fst rs) `reviseErrorM`
          ("In return at " ++ formatFullContext c)
        return (map (head . pValues . fst) rs,
                "ArgTuple(" ++ intercalate ", " (map (useAsUnwrapped . snd) rs) ++ ")")
      checkArity (_,Positional [_]) = return ()
      checkArity (i,Positional ts)  =
        compileErrorM $ "Initializer position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (CategoryFunction c2 cn) fn ps)) e2) =
    compile (Expression c (CategoryCall c2 cn (FunctionCall c fn ps (Positional [e1,e2]))) [])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (TypeFunction c2 tn) fn ps)) e2) =
    compile (Expression c (TypeCall c2 tn (FunctionCall c fn ps (Positional [e1,e2]))) [])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (ValueFunction c2 e0) fn ps)) e2) =
    compile (Expression c (ParensExpression c2 e0) [ValueCall c (FunctionCall c fn ps (Positional [e1,e2]))])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec c2 UnqualifiedFunction fn ps)) e2) =
    compile (Expression c (UnqualifiedCall c2 (FunctionCall c fn ps (Positional [e1,e2]))) [])
  compile (InfixExpression c e1 (NamedOperator o) e2) = do
    e1' <- compileExpression e1
    e2' <- if o `Set.member` logical
              then isolateExpression e2 -- Ignore named-return assignments.
              else compileExpression e2
    bindInfix c e1' o e2'
  isolateExpression e = do
    ctx <- getCleanContext
    (e',ctx') <- lift $ runStateT (compileExpression e) ctx
    (lift $ ccGetRequired ctx') >>= csRequiresTypes
    return e'
  arithmetic1 = Set.fromList ["*","/"]
  arithmetic2 = Set.fromList ["%"]
  arithmetic3 = Set.fromList ["+","-"]
  equals = Set.fromList ["==","!="]
  comparison = Set.fromList ["==","!=","<","<=",">",">="]
  logical = Set.fromList ["&&","||"]
  bitwise = Set.fromList ["&","|","^",">>","<<"]
  bindInfix c (Positional ts1,e1) o (Positional ts2,e2) = do
    -- TODO: Needs better error messages.
    t1' <- requireSingle c ts1
    t2' <- requireSingle c ts2
    bind t1' t2'
    where
      bind t1 t2
        | t1 /= t2 =
          lift $ compileErrorM $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
                                show t2 ++ formatFullContextBrace c
        | o `Set.member` comparison && t1 == intRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimInt PrimBool e1 o e2)
        | o `Set.member` comparison && t1 == floatRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimFloat PrimBool e1 o e2)
        | o `Set.member` comparison && t1 == stringRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimString PrimBool e1 o e2)
        | o `Set.member` comparison && t1 == charRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimChar PrimBool e1 o e2)
        | o `Set.member` arithmetic1 && t1 == intRequiredValue = do
          return (Positional [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` bitwise && t1 == intRequiredValue = do
          return (Positional [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` arithmetic2 && t1 == intRequiredValue = do
          return (Positional [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` arithmetic3 && t1 == intRequiredValue = do
          return (Positional [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` arithmetic1 && t1 == floatRequiredValue = do
          return (Positional [floatRequiredValue],glueInfix PrimFloat PrimFloat e1 o e2)
        | o `Set.member` arithmetic3 && t1 == floatRequiredValue = do
          return (Positional [floatRequiredValue],glueInfix PrimFloat PrimFloat e1 o e2)
        | o == "+" && t1 == stringRequiredValue = do
          return (Positional [stringRequiredValue],glueInfix PrimString PrimString e1 o e2)
        | o `Set.member` logical && t1 == boolRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimBool PrimBool e1 o e2)
        | o == "-" && t1 == charRequiredValue = do
          return (Positional [intRequiredValue],glueInfix PrimChar PrimInt e1 o e2)
        | o `Set.member` equals && t1 == boolRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimBool PrimBool e1 o e2)
        | otherwise =
          lift $ compileErrorM $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
                                show t2 ++ formatFullContextBrace c
      glueInfix t1 t2 e3 o2 e4 =
        UnboxedPrimitive t2 $ useAsUnboxed t1 e3 ++ o2 ++ useAsUnboxed t1 e4
  transform e (ConvertedCall c t f) = do
    (Positional ts,e') <- e
    t' <- requireSingle c ts
    r <- csResolver
    fa <- csAllFilters
    let vt = ValueType RequiredValue $ SingleType $ JustTypeInstance t
    lift $ (checkValueTypeMatch r fa t' vt) `reviseErrorM`
      ("In converted call at " ++ formatFullContext c)
    f' <- lookupValueFunction vt f
    compileFunctionCall (Just $ useAsUnwrapped e') f' f
  transform e (ValueCall c f) = do
    (Positional ts,e') <- e
    t' <- requireSingle c ts
    f' <- lookupValueFunction t' f
    compileFunctionCall (Just $ useAsUnwrapped e') f' f
  requireSingle _ [t] = return t
  requireSingle c2 ts =
    lift $ compileErrorM $ "Function call requires 1 return but found but found {" ++
                          intercalate "," (map show ts) ++ "}" ++ formatFullContextBrace c2

lookupValueFunction :: (Show c, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  ValueType -> FunctionCall c -> CompilerState a m (ScopedFunction c)
lookupValueFunction (ValueType WeakValue t) (FunctionCall c _ _ _) =
  lift $ compileErrorM $ "Use strong to convert " ++ show t ++
                        " to optional first" ++ formatFullContextBrace c
lookupValueFunction (ValueType OptionalValue t) (FunctionCall c _ _ _) =
  lift $ compileErrorM $ "Use require to convert " ++ show t ++
                        " to required first" ++ formatFullContextBrace c
lookupValueFunction (ValueType RequiredValue t) (FunctionCall c n _ _) =
  csGetTypeFunction c (Just t) n

compileExpressionStart :: (Show c, CompileErrorM m, MergeableM m,
                           CompilerContext c m [String] a) =>
  ExpressionStart c -> CompilerState a m (ExpressionType,ExprValue)
compileExpressionStart (NamedVariable (OutputValue c n)) = do
  (VariableValue _ s t _) <- csGetVariable c n
  csCheckVariableInit c n
  scoped <- autoScope s
  let lazy = s == CategoryScope
  return (Positional [t],readStoredVariable lazy t (scoped ++ variableName n))
compileExpressionStart (NamedMacro c n) = do
  e <- csExprLookup c n
  compileExpression e `reviseErrorStateT` ("In env lookup at " ++ formatFullContext c)
compileExpressionStart (CategoryCall c t f@(FunctionCall _ n _ _)) = do
  f' <- csGetCategoryFunction c (Just t) n
  csRequiresTypes $ Set.fromList [t,sfType f']
  t' <- expandCategory t
  compileFunctionCall (Just t') f' f
compileExpressionStart (TypeCall c t f@(FunctionCall _ n _ _)) = do
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r fa (SingleType t) `reviseErrorM` ("In function call at " ++ formatFullContext c)
  f' <- csGetTypeFunction c (Just $ SingleType t) n
  when (sfScope f' /= TypeScope) $ lift $ compileErrorM $ "Function " ++ show n ++
                                          " cannot be used as a type function" ++
                                          formatFullContextBrace c
  csRequiresTypes $ Set.unions $ map categoriesFromTypes [SingleType t]
  csRequiresTypes $ Set.fromList [sfType f']
  t' <- expandGeneralInstance $ SingleType t
  compileFunctionCall (Just t') f' f
compileExpressionStart (UnqualifiedCall c f@(FunctionCall _ n _ _)) = do
  ctx <- get
  f' <- lift $ collectOneOrErrorM [tryCategory ctx,tryNonCategory ctx]
  csRequiresTypes $ Set.fromList [sfType f']
  compileFunctionCall Nothing f' f
  where
    tryCategory ctx = ccGetCategoryFunction ctx c Nothing n
    tryNonCategory ctx = do
      f' <- ccGetTypeFunction ctx c Nothing n
      s <- ccCurrentScope ctx
      when (sfScope f' > s) $ compileErrorM $
        "Function " ++ show n ++ " is not in scope here" ++ formatFullContextBrace c
      return f'
-- TODO: Compile BuiltinCall like regular functions, for consistent validation.
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinPresent ps es)) = do
  csRequiresTypes (Set.fromList [BuiltinBool])
  when (length (pValues ps) /= 0) $
    lift $ compileErrorM $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    lift $ compileErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    lift $ compileErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  when (isWeakValue t0) $
    lift $ compileErrorM $ "Weak values not allowed here" ++ formatFullContextBrace c
  return $ (Positional [boolRequiredValue],
            UnboxedPrimitive PrimBool $ valueBase ++ "::Present(" ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinReduce ps es)) = do
  when (length (pValues ps) /= 2) $
    lift $ compileErrorM $ "Expected 2 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    lift $ compileErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    lift $ compileErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  let (Positional [t1,t2]) = ps
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r fa t1
  lift $ validateGeneralInstance r fa t2
  lift $ (checkValueTypeMatch r fa t0 (ValueType OptionalValue t1)) `reviseErrorM`
    ("In argument to reduce call at " ++ formatFullContext c)
  -- TODO: If t1 -> t2 then just return e without a Reduce call.
  t1' <- expandGeneralInstance t1
  t2' <- expandGeneralInstance t2
  csRequiresTypes $ categoriesFromTypes t1
  csRequiresTypes $ categoriesFromTypes t2
  return $ (Positional [ValueType OptionalValue t2],
            UnwrappedSingle $ typeBase ++ "::Reduce(" ++ t1' ++ ", " ++ t2' ++ ", " ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinRequire ps es)) = do
  when (length (pValues ps) /= 0) $
    lift $ compileErrorM $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    lift $ compileErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    lift $ compileErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  when (isWeakValue t0) $
    lift $ compileErrorM $ "Weak values not allowed here" ++ formatFullContextBrace c
  return $ (Positional [ValueType RequiredValue (vtType t0)],
            UnwrappedSingle $ valueBase ++ "::Require(" ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinStrong ps es)) = do
  when (length (pValues ps) /= 0) $
    lift $ compileErrorM $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    lift $ compileErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    lift $ compileErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  let t1 = Positional [ValueType OptionalValue (vtType t0)]
  if isWeakValue t0
     -- Weak values are already unboxed.
     then return (t1,UnwrappedSingle $ valueBase ++ "::Strong(" ++ useAsUnwrapped e ++ ")")
     else return (t1,e)
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinTypename ps es)) = do
  when (length (pValues ps) /= 1) $
    lift $ compileErrorM $ "Expected 1 type parameter" ++ formatFullContextBrace c
  when (length (pValues es) /= 0) $
    lift $ compileErrorM $ "Expected 0 arguments" ++ formatFullContextBrace c
  let t = head $ pValues ps
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r fa t
  t' <- expandGeneralInstance t
  csRequiresTypes $ Set.unions $ map categoriesFromTypes $ pValues ps
  return $ (Positional [formattedRequiredValue],
            valueAsWrapped $ UnboxedPrimitive PrimString $ typeBase ++ "::TypeName(" ++ t' ++ ")")
compileExpressionStart (BuiltinCall _ _) = undefined
compileExpressionStart (ParensExpression _ e) = compileExpression e
compileExpressionStart (InlineAssignment c n e) = do
  (VariableValue _ s t0 w) <- csGetVariable c n
  when (not w) $ lift $ compileErrorM $ "Cannot assign to read-only variable " ++
                                        show n ++ formatFullContextBrace c
  (Positional [t],e') <- compileExpression e -- TODO: Get rid of the Positional matching here.
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueTypeMatch r fa t t0) `reviseErrorM`
    ("In assignment at " ++ formatFullContext c)
  csUpdateAssigned n
  scoped <- autoScope s
  let lazy = s == CategoryScope
  return (Positional [t0],readStoredVariable lazy t0 $ "(" ++ scoped ++ variableName n ++
                                                     " = " ++ writeStoredVariable t0 e' ++ ")")

compileFunctionCall :: (Show c, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  Maybe String -> ScopedFunction c -> FunctionCall c ->
  CompilerState a m (ExpressionType,ExprValue)
compileFunctionCall e f (FunctionCall c _ ps es) = do
  r <- csResolver
  fa <- csAllFilters
  f' <- lift $ parsedToFunctionType f `reviseErrorM`
          ("In function call at " ++ formatFullContext c)
  f'' <- lift $ assignFunctionParams r fa ps f' `reviseErrorM`
          ("In function call at " ++ formatFullContext c)
  es' <- sequence $ map compileExpression $ pValues es
  (ts,es'') <- getValues es'
  -- Called an extra time so arg count mismatches have reasonable errors.
  lift $ processPairs_ (\_ _ -> return ()) (ftArgs f'') (Positional ts) `reviseErrorM`
    ("In function call at " ++ formatFullContext c)
  lift $ processPairs_ (checkArg r fa) (ftArgs f'') (Positional $ zip ([0..] :: [Int]) ts) `reviseErrorM`
    ("In function call at " ++ formatFullContext c)
  csRequiresTypes $ Set.unions $ map categoriesFromTypes $ pValues ps
  csRequiresTypes (Set.fromList [sfType f])
  params <- expandParams2 ps
  scoped <- autoScope (sfScope f)
  call <- assemble e scoped (sfScope f) params es''
  return $ (ftReturns f'',OpaqueMulti call)
  where
    assemble Nothing _ ValueScope ps2 es2 =
      return $ callName (sfName f) ++ "(Var_self, " ++ ps2 ++ ", " ++ es2 ++ ")"
    assemble Nothing scoped _ ps2 es2 =
      return $ scoped ++ callName (sfName f) ++ "(" ++ ps2 ++ ", " ++ es2 ++ ")"
    assemble (Just e2) _ ValueScope ps2 es2 =
      return $ valueBase ++ "::Call(" ++ e2 ++ ", " ++ functionName f ++ ", " ++ ps2 ++ ", " ++ es2 ++ ")"
    assemble (Just e2) _ _ ps2 es2 =
      return $ e2 ++ ".Call(" ++ functionName f ++ ", " ++ ps2 ++ ", " ++ es2 ++ ")"
    -- TODO: Lots of duplication with assignments and initialization.
    -- Single expression, but possibly multi-return.
    getValues [(Positional ts,e2)] = return (ts,useAsArgs e2)
    -- Multi-expression => must all be singles.
    getValues rs = do
      lift $ mergeAllM (map checkArity $ zip ([0..] :: [Int]) $ map fst rs) `reviseErrorM`
        ("In return at " ++ formatFullContext c)
      return (map (head . pValues . fst) rs, "ArgTuple(" ++ intercalate ", " (map (useAsUnwrapped . snd) rs) ++ ")")
    checkArity (_,Positional [_]) = return ()
    checkArity (i,Positional ts)  =
      compileErrorM $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    checkArg r fa t0 (i,t1) = do
      checkValueTypeMatch r fa t1 t0 `reviseErrorM` ("In argument " ++ show i ++ " to " ++ show (sfName f))

compileMainProcedure :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> ExprMap c -> Expression c -> m (CompiledData [String])
compileMainProcedure tm em e = do
  ctx <- getMainContext tm em
  runDataCompiler compiler ctx where
    procedure = Procedure [] [IgnoreValues [] e]
    compiler = do
      ctx0 <- getCleanContext
      compileProcedure ctx0 procedure >>= put

autoScope :: (CompilerContext c m s a) =>
  SymbolScope -> CompilerState a m String
autoScope s = do
  s1 <- csCurrentScope
  return $ scoped s1 s
  where
    scoped ValueScope TypeScope     = "parent."
    scoped ValueScope CategoryScope = "parent.parent."
    scoped TypeScope  CategoryScope = "parent."
    -- NOTE: Don't use this->; otherwise, self won't work properly.
    scoped _ _ = ""

categoriesFromTypes :: GeneralInstance -> Set.Set CategoryName
categoriesFromTypes = Set.fromList . getAll where
  getAll (TypeMerge _ ps) = concat $ map getAll ps
  getAll (SingleType (JustTypeInstance (TypeInstance t ps))) = t:(concat $ map getAll $ pValues ps)
  getAll _ = []

categoriesFromRefine :: TypeInstance -> Set.Set CategoryName
categoriesFromRefine (TypeInstance t ps) = t `Set.insert` (Set.unions $ map categoriesFromTypes $ pValues ps)

categoriesFromDefine :: DefinesInstance -> Set.Set CategoryName
categoriesFromDefine (DefinesInstance t ps) = t `Set.insert` (Set.unions $ map categoriesFromTypes $ pValues ps)

expandParams :: (CompilerContext c m s a) =>
  Positional GeneralInstance -> CompilerState a m String
expandParams ps = do
  ps' <- sequence $ map expandGeneralInstance $ pValues ps
  return $ "T_get(" ++ intercalate "," (map ("&" ++) ps') ++ ")"

expandParams2 :: (CompilerContext c m s a) =>
  Positional GeneralInstance -> CompilerState a m String
expandParams2 ps = do
  ps' <- sequence $ map expandGeneralInstance $ pValues ps
  return $ "ParamTuple(" ++ intercalate "," (map ("&" ++) ps') ++ ")"

expandCategory :: (CompilerContext c m s a) =>
  CategoryName -> CompilerState a m String
expandCategory t = return $ categoryGetter t ++ "()"

expandGeneralInstance :: (CompilerContext c m s a) =>
  GeneralInstance -> CompilerState a m String
expandGeneralInstance (TypeMerge MergeUnion     []) = return $ allGetter ++ "()"
expandGeneralInstance (TypeMerge MergeIntersect []) = return $ anyGetter ++ "()"
expandGeneralInstance (TypeMerge m ps) = do
  ps' <- sequence $ map expandGeneralInstance ps
  return $ getter m ++ "(L_get<" ++ typeBase ++ "*>(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
  where
    getter MergeUnion     = unionGetter
    getter MergeIntersect = intersectGetter
expandGeneralInstance (SingleType (JustTypeInstance (TypeInstance t ps))) = do
  ps' <- sequence $ map expandGeneralInstance $ pValues ps
  return $ typeGetter t ++ "(T_get(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
expandGeneralInstance (SingleType (JustParamName p)) = do
  s <- csGetParamScope p
  scoped <- autoScope s
  return $ scoped ++ paramName p

doImplicitReturn :: (Show c,CompilerContext c m [String] a) => [c] -> CompilerState a m ()
doImplicitReturn c = do
  named <- csIsNamedReturns
  (CleanupSetup cs ss) <- csGetCleanup
  when (not $ null ss) $ do
    sequence_ $ map (csInheritReturns . (:[])) cs
    csWrite ss
  csRegisterReturn c Nothing
  if not named
     then csWrite ["return ReturnTuple(0);"]
     else do
       vars <- csPrimNamedReturns
       sequence_ $ map (csWrite . (:[]) . assign) vars
       csWrite ["return returns;"]
  where
    assign (ReturnVariable i n t) =
      "returns.At(" ++ show i ++ ") = " ++ useAsUnwrapped (readStoredVariable False t $ variableName n) ++ ";"

autoPositionalCleanup :: (CompilerContext c m [String] a) => ExprValue -> CompilerState a m ()
autoPositionalCleanup e = do
  (CleanupSetup cs ss) <- csGetCleanup
  if null ss
     then csWrite ["return " ++ useAsReturns e ++ ";"]
     else do
       csWrite ["{","ReturnTuple returns = " ++ useAsReturns e ++ ";"]
       sequence_ $ map (csInheritReturns . (:[])) cs
       csWrite ss
       csWrite ["return returns;","}"]
