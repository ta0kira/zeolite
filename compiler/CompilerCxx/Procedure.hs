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
import qualified Data.Map as Map
import qualified Data.Set as Set

import Builtin
import CategoryCompiler
import CompilerState
import DefinedCategory
import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase
import CompilerCxx.Code
import CompilerCxx.Naming


compileExecutableProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> CategoryName -> ParamSet (ValueParam c) -> ParamSet (ValueParam c) ->
  [DefinedMember c] -> [ParamFilter c] -> [ParamFilter c] -> Map.Map FunctionName (ScopedFunction c) ->
  Map.Map VariableName (VariableValue c) ->
  (ScopedFunction c,ExecutableProcedure c) -> m (CompiledData [String],CompiledData [String])
compileExecutableProcedure tm t ps pi ms pa fi fa va
                 (ff@(ScopedFunction _ _ _ s as1 rs1 ps1 fs _),
                  (ExecutableProcedure _ c n as2 rs2 p)) = do
  rs' <- if isUnnamedReturns rs2
            then return $ ValidatePositions rs1
            else fmap (ValidateNames rs1 . Map.fromList) $ processParamPairs pairOutput rs1 (nrNames rs2)
  va' <- updateArgVariables va as1 as2
  va'' <- updateReturnVariables va' rs1 rs2
  let pa' = if s == CategoryScope
               then fs
               else pa ++ fs
  let localScopes = Map.fromList $ zip (map vpParam $ psParams ps1) (repeat LocalScope)
  let typeScopes = Map.fromList $ zip (map vpParam $ psParams ps) (repeat TypeScope)
  let valueScopes = Map.fromList $ zip (map vpParam $ psParams pi) (repeat ValueScope)
  let sa = case s of
                CategoryScope -> localScopes
                TypeScope -> Map.union typeScopes localScopes
                ValueScope -> Map.unions [localScopes,typeScopes,valueScopes]
  let localFilters = getFunctionFilterMap ff
  let typeFilters = getFilterMap (psParams ps) pa
  let valueFilters = getFilterMap (psParams pi) fi
  let allFilters = case s of
                   CategoryScope -> localFilters
                   TypeScope -> Map.union localFilters typeFilters
                   ValueScope -> Map.unions [localFilters,typeFilters,valueFilters]
  let ns0 = if isUnnamedReturns rs2
               then []
               else zipWith3 ReturnVariable [0..] (map ovName $ psParams $ nrNames rs2) (map pvType $ psParams rs1)
  let ns = filter (isPrimType . rvType) ns0
  let ctx = ProcedureContext {
      pcScope = s,
      pcType = t,
      pcExtParams = ps,
      pcIntParams = pi,
      pcMembers = ms,
      pcCategories = tm,
      pcAllFilters = allFilters,
      pcExtFilters = pa',
      -- fs is duplicated so value initialization checks work properly.
      pcIntFilters = fi ++ fs,
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = va'',
      pcReturns = rs',
      pcPrimNamed = ns,
      pcRequiredTypes = Set.empty,
      pcOutput = [],
      pcDisallowInit = False,
      pcLoopSetup = NotInLoop,
      pcCleanupSetup = CleanupSetup [] []
    }
  output <- runDataCompiler compileWithReturn ctx
  return (onlyCode header,wrapProcedure output)
  where
    compileWithReturn = do
      ctx0 <- getCleanContext
      compileProcedure ctx0 p >>= put
      csRegisterReturn c Nothing `reviseErrorStateT`
        ("In implicit return from " ++ show n ++ formatFullContextBrace c)
      doNamedReturn
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)
    wrapProcedure output =
      mergeAll $ [
          onlyCode header2,
          indentCompiled $ onlyCode setProcedureTrace,
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
    returnType = "ReturnTuple"
    setProcedureTrace = startFunctionTracing $ show t ++ "." ++ show n
    defineReturns = [returnType ++ " returns(" ++ show (length $ psParams rs1) ++ ");"]
    nameParams = flip map (zip [0..] $ psParams ps1) $
      (\(i,p) -> paramType ++ " " ++ paramName (vpParam p) ++ " = *params.At(" ++ show i ++ ");")
    nameArgs = flip map (zip [0..] $ filter (not . isDiscardedInput . snd) $ zip (psParams as1) (psParams $ avNames as2)) $
      (\(i,(t,n)) -> "const " ++ variableProxyType (pvType t) ++ " " ++ variableName (ivName n) ++
                     " = " ++ writeStoredVariable (pvType t) (UnwrappedSingle $ "args.At(" ++ show i ++ ")") ++ ";")
    nameReturns
      | isUnnamedReturns rs2 = []
      | otherwise = map (\(i,(t,n)) -> nameReturn i (pvType t) n) (zip [0..] $ zip (psParams rs1) (psParams $ nrNames rs2))
    nameReturn i t n
      | isPrimType t = variableProxyType t ++ " " ++ variableName (ovName n) ++ ";"
      | otherwise =
        variableProxyType t ++ " " ++ variableName (ovName n) ++
        " = " ++ writeStoredVariable t (UnwrappedSingle $ "returns.At(" ++ show i ++ ")") ++ ";"

compileCondition :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  a -> [c] -> Expression c -> CompilerState a m String
compileCondition ctx c e = do
  (e',ctx') <- lift $ runStateT compile ctx
  lift (ccGetRequired ctx') >>= csRequiresTypes
  return $ predTraceContext c ++ e'
  where
    compile = flip reviseErrorStateT ("In condition at " ++ formatFullContext c) $ do
      (ts,e') <- compileExpression e
      lift $ checkCondition ts
      return $ useAsUnboxed PrimBool e'
      where
        checkCondition (ParamSet [t]) | t == boolRequiredValue = return ()
        checkCondition (ParamSet ts) =
          compileError $ "Conditionals must have exactly one Bool return but found {" ++
                         intercalate "," (map show ts) ++ "}"

-- Returns the state so that returns can be properly checked for if/elif/else.
compileProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  a -> Procedure c -> CompilerState a m a
compileProcedure ctx (Procedure c ss) = do
  ctx' <- lift $ execStateT (sequence $ map (\s -> warnUnreachable s >> compileStatement s) ss) ctx
  return ctx' where
    warnUnreachable s = do
      unreachable <- csIsUnreachable
      lift $ when unreachable $
                  compileWarningM $ "Statement at " ++
                                    formatFullContext (getStatementContext s) ++
                                    " is unreachable"

compileStatement :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  Statement c -> CompilerState a m ()
compileStatement (EmptyReturn c) = do
  csRegisterReturn c Nothing
  csWrite $ setTraceContext c
  doNamedReturn
compileStatement (ExplicitReturn c es) = do
  es' <- sequence $ map compileExpression $ psParams es
  getReturn $ zip (map getExpressionContext $ psParams es) es'
  where
    -- Single expression, but possibly multi-return.
    getReturn [(_,(ParamSet ts,e))] = do
      csRegisterReturn c $ Just (ParamSet ts)
      csWrite $ setTraceContext c
      csWrite ["returns = " ++ useAsReturns e ++ ";"]
      doReturnCleanup
      csWrite ["return returns;"]
    -- Multi-expression => must all be singles.
    getReturn rs = do
      lift $ mergeAllM (map checkArity $ zip [1..] $ map (fst . snd) rs) `reviseError`
        ("In return at " ++ formatFullContext c)
      csRegisterReturn c $ Just $ ParamSet $ map (head . psParams . fst . snd) rs
      csWrite $ concat $ map bindReturn $ zip [0..] rs
      doReturnCleanup
      csWrite ["return returns;"]
    checkArity (_,ParamSet [_]) = return ()
    checkArity (i,ParamSet ts)  =
      compileError $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    bindReturn (i,(c0,(_,e))) = setTraceContext c0 ++ [
        "returns.At(" ++ show i ++ ") = " ++ useAsUnwrapped e ++ ";"
      ]
compileStatement (LoopBreak c) = do
  loop <- csGetLoop
  case loop of
       NotInLoop ->
         lift $ compileError $ "Using break outside of while is no allowed" ++ formatFullContextBrace c
       _ -> return ()
  csWrite ["break;"]
compileStatement (LoopContinue c) = do
  loop <- csGetLoop
  case loop of
       NotInLoop ->
         lift $ compileError $ "Using continue outside of while is no allowed" ++ formatFullContextBrace c
       _ -> return ()
  csWrite $ ["{"] ++ lsUpdate loop ++ ["}","continue;"]
compileStatement (FailCall c e) = do
  e' <- compileExpression e
  when (length (psParams $ fst e') /= 1) $
    lift $ compileError $ "Expected single return in argument" ++ formatFullContextBrace c
  let (ParamSet [t0],e) = e'
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueTypeMatch r fa t0 formattedRequiredValue) `reviseError`
    ("In fail call at " ++ formatFullContext c)
  csSetNoReturn
  csWrite $ setTraceContext c
  csWrite ["BuiltinFail(" ++ useAsUnwrapped e ++ ");"]
compileStatement (IgnoreValues c e) = do
  (_,e') <- compileExpression e
  csWrite $ setTraceContext c
  csWrite ["(void) (" ++ useAsWhatever e' ++ ");"]
compileStatement (Assignment c as e) = do
  (ts,e') <- compileExpression e
  r <- csResolver
  fa <- csAllFilters
  processParamPairsT (createVariable r fa) as ts `reviseErrorStateT`
    ("In assignment at " ++ formatFullContext c)
  csWrite $ setTraceContext c
  variableTypes <- sequence $ map (uncurry getVariableType) $ zip (psParams as) (psParams ts)
  assignAll (zip3 [0..] variableTypes (psParams as)) e'
  where
    assignAll [v] e = assignSingle v e
    assignAll vs e = do
      csWrite ["{","const auto r = " ++ useAsReturns e ++ ";"]
      sequence $ map assignMulti vs
      csWrite ["}"]
    getVariableType (CreateVariable _ t _) _ = return t
    getVariableType (ExistingVariable (InputValue c n)) _ = do
      (VariableValue _ _ t _) <- csGetVariable c n
      return t
    getVariableType (ExistingVariable (DiscardInput _)) t = return t
    createVariable r fa (CreateVariable c t1 n) t2 = do
      -- TODO: Call csRequiresTypes for t1. (Maybe needs a helper function.)
      lift $ mergeAllM [validateGeneralInstance r fa (vtType t1),
                        checkValueTypeMatch r fa t2 t1] `reviseError`
        ("In creation of " ++ show n ++ " at " ++ formatFullContext c)
      csAddVariable c n (VariableValue c LocalScope t1 True)
      csWrite [variableStoredType t1 ++ " " ++ variableName n ++ ";"]
    createVariable r fa (ExistingVariable (InputValue c n)) t2 = do
      (VariableValue _ s1 t1 w) <- csGetVariable c n
      when (not w) $ lift $ compileError $ "Cannot assign to read-only variable " ++
                                           show n ++ formatFullContextBrace c
      -- TODO: Also show original context.
      lift $ (checkValueTypeMatch r fa t2 t1) `reviseError`
        ("In assignment to " ++ show n ++ " at " ++ formatFullContext c)
      csUpdateAssigned n
    createVariable _ _ _ _ = return ()
    assignSingle (i,t,CreateVariable _ _ n) e =
      csWrite [variableName n ++ " = " ++ writeStoredVariable t e ++ ";"]
    assignSingle (i,t,ExistingVariable (InputValue _ n)) e = do
      (VariableValue _ s _ _) <- csGetVariable c n
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = " ++ writeStoredVariable t e ++ ";"]
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

compileLazyInit :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                   CompilerContext c m [String] a) =>
  DefinedMember c -> CompilerState a m ()
compileLazyInit (DefinedMember _ _ _ _ Nothing) = return mergeDefault
compileLazyInit (DefinedMember c _ t1 n (Just e)) = do
  (ts,e') <- compileExpression e
  when (length (psParams ts) /= 1) $
    lift $ compileError $ "Expected single return in initializer" ++ formatFullContextBrace (getExpressionContext e)
  r <- csResolver
  fa <- csAllFilters
  let ParamSet [t2] = ts
  lift $ (checkValueTypeMatch r fa t2 t1) `reviseError`
    ("In initialization of " ++ show n ++ " at " ++ formatFullContext c)
  csWrite [variableName n ++ "([]() { return " ++ writeStoredVariable t1 e' ++ "; })"]

compileVoidExpression :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                         CompilerContext c m [String] a) =>
  VoidExpression c -> CompilerState a m ()
compileVoidExpression (Conditional ie) = compileIfElifElse ie
compileVoidExpression (Loop l) = compileWhileLoop l
compileVoidExpression (WithScope s) = compileScopedBlock s
compileVoidExpression (LineComment s) = csWrite $ map ("// " ++) $ lines s

compileIfElifElse :: (Show c, Monad m, CompileErrorM m, MergeableM m,
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
    unwind (IfStatement c e p es) = do
      ctx0 <- getCleanContext
      e' <- compileCondition ctx0 c e
      ctx <- compileProcedure ctx0 p
      (lift $ ccGetRequired ctx) >>= csRequiresTypes
      csWrite ["else if (" ++ e' ++ ") {"]
      (lift $ ccGetOutput ctx) >>= csWrite
      csWrite ["}"]
      cs <- unwind es
      return $ ctx:cs
    unwind (ElseStatement c p) = do
      ctx0 <- getCleanContext
      ctx <- compileProcedure ctx0 p
      (lift $ ccGetRequired ctx) >>= csRequiresTypes
      csWrite ["else {"]
      (lift $ ccGetOutput ctx) >>= csWrite
      csWrite ["}"]
      return [ctx]
    unwind TerminateConditional = fmap (:[]) get

compileWhileLoop :: (Show c, Monad m, CompileErrorM m, MergeableM m,
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

compileScopedBlock :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                       CompilerContext c m [String] a) =>
  ScopedBlock c -> CompilerState a m ()
compileScopedBlock s = do
  let (vs,p,cl,st) = rewriteScoped s
  -- Capture context so we can discard scoped variable names.
  ctx0 <- getCleanContext
  r <- csResolver
  fa <- csAllFilters
  let cc = case cl of
                Just (Procedure _ ss2) -> ss2
                _ -> []
  sequence $ map (createVariable r fa) vs
  ctxP <- compileProcedure ctx0 p
  (ctxP',cl',ctxCl) <-
    case cl of
         Just p2 -> do
           ctx0' <- lift $ ccClearOutput ctxP
           ctxCl <- compileProcedure ctx0' p2
           p2' <- lift $ ccGetOutput ctxCl
           -- TODO: It might be helpful to add a new trace-context line for this
           -- so that the line that triggered the cleanup is still in the trace.
           let p2'' = ["{",startCleanupTracing] ++ p2' ++ ["}"]
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
  sequence $ map showVariable vs
  (lift $ ccGetRequired ctxS)  >>= csRequiresTypes
  (lift $ ccGetRequired ctxCl) >>= csRequiresTypes
  csInheritReturns [ctxS]
  csInheritReturns [ctxCl]
  where
    createVariable r fa (c,t,n) = do
      lift $ validateGeneralInstance r fa (vtType t) `reviseError`
        ("In creation of " ++ show n ++ " at " ++ formatFullContext c)
      csWrite [variableStoredType t ++ " " ++ variableName n ++ ";"]
    showVariable (c,t,n) = do
      -- TODO: Call csRequiresTypes for t. (Maybe needs a helper function.)
      csAddVariable c n (VariableValue c LocalScope t True)
    -- Don't merge if the second scope has cleanup, so that the latter can't
    -- refer to variables defined in the first scope.
    rewriteScoped (ScopedBlock c p cl@(Just _)
                               s@(NoValueExpression _ (WithScope
                                  (ScopedBlock _ _ (Just _) _)))) =
      ([],p,cl,s)
    -- Merge chained scoped sections into a single section.
    rewriteScoped (ScopedBlock c (Procedure c2 ss1) cl1
                               (NoValueExpression _ (WithScope
                                (ScopedBlock _ (Procedure _ ss2) cl2 s)))) =
      rewriteScoped $ ScopedBlock c (Procedure c2 $ ss1 ++ ss2) (cl1 <|> cl2) s
    -- Gather to-be-created variables.
    rewriteScoped (ScopedBlock _ p cl (Assignment c2 vs e)) =
      (created,p,cl,Assignment c2 (ParamSet existing) e) where
        (created,existing) = foldr update ([],[]) (psParams vs)
        update (CreateVariable c t n) (cs,es) = ((c,t,n):cs,(ExistingVariable $ InputValue c n):es)
        update e (cs,es) = (cs,e:es)
    -- Merge the statement into the scoped block.
    rewriteScoped (ScopedBlock _ p cl s) =
      ([],p,cl,s)

compileExpression :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                      CompilerContext c m [String] a) =>
  Expression c -> CompilerState a m (ExpressionType,ExprValue)
compileExpression = compile where
  compile (Literal (StringLiteral c l)) = do
    return (ParamSet [stringRequiredValue],UnboxedPrimitive PrimString $ "PrimString_FromLiteral(" ++ escapeChars l ++ ")")
  compile (Literal (CharLiteral c l)) = do
    return (ParamSet [charRequiredValue],UnboxedPrimitive PrimChar $ "PrimChar('" ++ escapeChar l ++ "')")
  compile (Literal (IntegerLiteral c True l)) = do
    when (l > 2^64 - 1) $ lift $ compileError $
      "Literal " ++ show l ++ formatFullContextBrace c ++ " is greater than the max value for 64-bit unsigned"
    let l' = if l > 2^63 - 1 then l - 2^64 else l
    return (ParamSet [intRequiredValue],UnboxedPrimitive PrimInt $ "PrimInt(" ++ show l' ++ ")")
  compile (Literal (IntegerLiteral c False l)) = do
    when (l > 2^63 - 1) $ lift $ compileError $
      "Literal " ++ show l ++ formatFullContextBrace c ++ " is greater than the max value for 64-bit signed"
    when ((-l) > 2^63 - 2) $ lift $ compileError $
      "Literal " ++ show l ++ formatFullContextBrace c ++ " is less than the min value for 64-bit signed"
    return (ParamSet [intRequiredValue],UnboxedPrimitive PrimInt $ "PrimInt(" ++ show l ++ ")")
  compile (Literal (DecimalLiteral c l e)) = do
    -- TODO: Check bounds.
    return (ParamSet [floatRequiredValue],UnboxedPrimitive PrimFloat $ "PrimFloat(" ++ show l ++ "E" ++ show e ++ ")")
  compile (Literal (BoolLiteral c True)) = do
    return (ParamSet [boolRequiredValue],UnboxedPrimitive PrimBool "true")
  compile (Literal (BoolLiteral c False)) = do
    return (ParamSet [boolRequiredValue],UnboxedPrimitive PrimBool "false")
  compile (Literal (EmptyLiteral c)) = do
    return (ParamSet [emptyValue],UnwrappedSingle "Var_empty")
  compile (Expression c s os) = do
    foldl transform (compileExpressionStart s) os
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec _ (CategoryFunction c2 cn) fn ps)) e) =
    compile (Expression c (CategoryCall c2 cn (FunctionCall c fn ps (ParamSet [e]))) [])
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec _ (TypeFunction c2 tn) fn ps)) e) =
    compile (Expression c (TypeCall c2 tn (FunctionCall c fn ps (ParamSet [e]))) [])
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec _ (ValueFunction c2 e0) fn ps)) e) =
    compile (Expression c (ParensExpression c2 e0) [ValueCall c (FunctionCall c fn ps (ParamSet [e]))])
  compile (UnaryExpression c (FunctionOperator _ (FunctionSpec c2 UnqualifiedFunction fn ps)) e) =
    compile (Expression c (UnqualifiedCall c2 (FunctionCall c fn ps (ParamSet [e]))) [])
  compile (UnaryExpression c (NamedOperator "-") (Literal (IntegerLiteral _ _ l))) =
    compile (Literal (IntegerLiteral c False (-l)))
  compile (UnaryExpression c (NamedOperator "-") (Literal (DecimalLiteral _ l e))) =
    compile (Literal (DecimalLiteral c (-l) e))
  compile (UnaryExpression c (NamedOperator o) e) = do
    (ParamSet ts,e') <- compileExpression e
    t' <- requireSingle c ts
    doUnary t' e'
    where
      doUnary t e
        | o == "!" = doNot t e
        | o == "-" = doNeg t e
        | otherwise = lift $ compileError $ "Unknown unary operator \"" ++ o ++ "\" " ++
                                            formatFullContextBrace c
      doNot t e = do
        when (t /= boolRequiredValue) $
          lift $ compileError $ "Cannot use " ++ show t ++ " with unary ! operator" ++
                                formatFullContextBrace c
        return $ (ParamSet [boolRequiredValue],UnboxedPrimitive PrimBool $ "!" ++ useAsUnboxed PrimBool e)
      doNeg t e
        | t == intRequiredValue = return $ (ParamSet [intRequiredValue],
                                            UnboxedPrimitive PrimInt $ "-" ++ useAsUnboxed PrimInt e)
        | t == floatRequiredValue = return $ (ParamSet [floatRequiredValue],
                                             UnboxedPrimitive PrimFloat $ "-" ++ useAsUnboxed PrimFloat e)
        | otherwise = lift $ compileError $ "Cannot use " ++ show t ++ " with unary - operator" ++
                                            formatFullContextBrace c
  compile (InitializeValue c t ps es) = do
    es' <- sequence $ map compileExpression $ psParams es
    (ts,es'') <- getValues es'
    csCheckValueInit c t (ParamSet ts) ps
    params <- expandParams $ tiParams t
    params2 <- expandParams2 $ ps
    sameType <- csSameType t
    s <- csCurrentScope
    let typeInstance = getType sameType s params
    -- TODO: This is unsafe if used in a type or category constructor.
    return (ParamSet [ValueType RequiredValue $ SingleType $ JustTypeInstance t],
            UnwrappedSingle $ valueCreator ++ "(" ++ typeInstance ++ ", " ++ params2 ++ ", " ++ es'' ++ ")")
    where
      getType True TypeScope  _ = "*this"
      getType True ValueScope _ = "parent"
      getType _    _ params = typeCreator ++ "(" ++ params ++ ")"
      -- Single expression, but possibly multi-return.
      getValues [(ParamSet ts,e)] = return (ts,useAsArgs e)
      -- Multi-expression => must all be singles.
      getValues rs = do
        lift $ mergeAllM (map checkArity $ zip [1..] $ map fst rs) `reviseError`
          ("In return at " ++ formatFullContext c)
        return (map (head . psParams . fst) rs,
                "ArgTuple(" ++ intercalate ", " (map (useAsUnwrapped . snd) rs) ++ ")")
      checkArity (_,ParamSet [_]) = return ()
      checkArity (i,ParamSet ts)  =
        compileError $ "Initializer position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (CategoryFunction c2 cn) fn ps)) e2) =
    compile (Expression c (CategoryCall c2 cn (FunctionCall c fn ps (ParamSet [e1,e2]))) [])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (TypeFunction c2 tn) fn ps)) e2) =
    compile (Expression c (TypeCall c2 tn (FunctionCall c fn ps (ParamSet [e1,e2]))) [])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (ValueFunction c2 e0) fn ps)) e2) =
    compile (Expression c (ParensExpression c2 e0) [ValueCall c (FunctionCall c fn ps (ParamSet [e1,e2]))])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec c2 UnqualifiedFunction fn ps)) e2) =
    compile (Expression c (UnqualifiedCall c2 (FunctionCall c fn ps (ParamSet [e1,e2]))) [])
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
  arithmetic = Set.union arithmetic1 arithmetic2
  equals = Set.fromList ["==","!="]
  comparison = Set.fromList ["==","!=","<","<=",">",">="]
  logical = Set.fromList ["&&","||"]
  bindInfix c (ParamSet ts1,e1) o (ParamSet ts2,e2) = do
    -- TODO: Needs better error messages.
    t1' <- requireSingle c ts1
    t2' <- requireSingle c ts2
    bind t1' t2'
    where
      bind t1 t2
        | t1 /= t2 =
          lift $ compileError $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
                                show t2 ++ formatFullContextBrace c
        | o `Set.member` comparison && t1 == intRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix PrimInt PrimBool e1 o e2)
        | o `Set.member` comparison && t1 == floatRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix PrimFloat PrimBool e1 o e2)
        | o `Set.member` comparison && t1 == stringRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix PrimString PrimBool e1 o e2)
        | o `Set.member` comparison && t1 == charRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix PrimChar PrimBool e1 o e2)
        | o `Set.member` arithmetic1 && t1 == intRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` arithmetic2 && t1 == intRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` arithmetic3 && t1 == intRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix PrimInt PrimInt e1 o e2)
        | o `Set.member` arithmetic1 && t1 == floatRequiredValue = do
          return (ParamSet [floatRequiredValue],glueInfix PrimFloat PrimFloat e1 o e2)
        | o `Set.member` arithmetic3 && t1 == floatRequiredValue = do
          return (ParamSet [floatRequiredValue],glueInfix PrimFloat PrimFloat e1 o e2)
        | o == "+" && t1 == stringRequiredValue = do
          return (ParamSet [stringRequiredValue],glueInfix PrimString PrimString e1 o e2)
        | o `Set.member` logical && t1 == boolRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix PrimBool PrimBool e1 o e2)
        | o == "-" && t1 == charRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix PrimChar PrimInt e1 o e2)
        | o `Set.member` equals && t1 == boolRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix PrimBool PrimBool e1 o e2)
        | otherwise =
          lift $ compileError $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
                                show t2 ++ formatFullContextBrace c
      glueInfix t1 t2 e1 o e2 =
        UnboxedPrimitive t2 $ useAsUnboxed t1 e1 ++ o ++ useAsUnboxed t1 e2
  transform e (ConvertedCall c t f) = do
    (ParamSet ts,e') <- e
    t' <- requireSingle c ts
    r <- csResolver
    fa <- csAllFilters
    let vt = ValueType RequiredValue $ SingleType $ JustTypeInstance t
    lift $ (checkValueTypeMatch r fa t' vt) `reviseError`
      ("In conversion at " ++ formatFullContext c)
    f' <- lookupValueFunction vt f
    compileFunctionCall (Just $ useAsUnwrapped e') f' f
  transform e (ValueCall c f) = do
    (ParamSet ts,e') <- e
    t' <- requireSingle c ts
    f' <- lookupValueFunction t' f
    compileFunctionCall (Just $ useAsUnwrapped e') f' f
  requireSingle c [t] = return t
  requireSingle c ts =
    lift $ compileError $ "Function call requires 1 return but found but found {" ++
                          intercalate "," (map show ts) ++ "}" ++ formatFullContextBrace c

lookupValueFunction :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  ValueType -> FunctionCall c -> CompilerState a m (ScopedFunction c)
lookupValueFunction (ValueType WeakValue t) (FunctionCall c _ _ _) =
  lift $ compileError $ "Use strong to convert " ++ show t ++
                        " to optional first" ++ formatFullContextBrace c
lookupValueFunction (ValueType OptionalValue t) (FunctionCall c _ _ _) =
  lift $ compileError $ "Use require to convert " ++ show t ++
                        " to required first" ++ formatFullContextBrace c
lookupValueFunction (ValueType RequiredValue t) (FunctionCall c n _ _) =
  csGetTypeFunction c (Just t) n

compileExpressionStart :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                           CompilerContext c m [String] a) =>
  ExpressionStart c -> CompilerState a m (ExpressionType,ExprValue)
compileExpressionStart (NamedVariable (OutputValue c n)) = do
  (VariableValue _ s t _) <- csGetVariable c n
  csCheckVariableInit c n
  scoped <- autoScope s
  let lazy = s == CategoryScope
  return (ParamSet [t],readStoredVariable lazy t (scoped ++ variableName n))
compileExpressionStart (CategoryCall c t f@(FunctionCall _ n _ _)) = do
  f' <- csGetCategoryFunction c (Just t) n
  csRequiresTypes $ Set.fromList [t,sfType f']
  t' <- expandCategory t
  compileFunctionCall (Just t') f' f
compileExpressionStart (TypeCall c t f@(FunctionCall _ n _ _)) = do
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r fa (SingleType t)
  f' <- csGetTypeFunction c (Just $ SingleType t) n
  when (sfScope f' /= TypeScope) $ lift $ compileError $ "Function " ++ show n ++
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
      when (sfScope f' > s) $ compileError $
        "Function " ++ show n ++ " is not in scope here" ++ formatFullContextBrace c
      return f'
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinPresent ps es)) = do
  when (length (psParams ps) /= 0) $
    lift $ compileError $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument" ++ formatFullContextBrace c
  let (ParamSet [t0],e) = head es'
  when (isWeakValue t0) $
    lift $ compileError $ "Weak values not allowed here" ++ formatFullContextBrace c
  return $ (ParamSet [boolRequiredValue],
            UnboxedPrimitive PrimBool $ valueBase ++ "::Present(" ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinReduce ps es)) = do
  when (length (psParams ps) /= 2) $
    lift $ compileError $ "Expected 2 type parameters" ++ formatFullContextBrace c
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument" ++ formatFullContextBrace c
  let (ParamSet [t0],e) = head es'
  let (ParamSet [t1,t2]) = ps
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r fa t1
  lift $ validateGeneralInstance r fa t2
  lift $ (checkValueTypeMatch r fa t0 (ValueType OptionalValue t1)) `reviseError`
    ("In argument to reduce call at " ++ formatFullContext c)
  -- TODO: If t1 -> t2 then just return e without a Reduce call.
  t1' <- expandGeneralInstance t1
  t2' <- expandGeneralInstance t2
  csRequiresTypes $ categoriesFromTypes t1
  csRequiresTypes $ categoriesFromTypes t2
  return $ (ParamSet [ValueType OptionalValue t2],
            UnwrappedSingle $ typeBase ++ "::Reduce(" ++ t1' ++ ", " ++ t2' ++ ", " ++ useAsUnwrapped e ++ ")")
-- TODO: Compile BuiltinCall like regular functions, for consistent validation.
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinRequire ps es)) = do
  when (length (psParams ps) /= 0) $
    lift $ compileError $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument" ++ formatFullContextBrace c
  let (ParamSet [t0],e) = head es'
  when (isWeakValue t0) $
    lift $ compileError $ "Weak values not allowed here" ++ formatFullContextBrace c
  return $ (ParamSet [ValueType RequiredValue (vtType t0)],
            UnwrappedSingle $ valueBase ++ "::Require(" ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinStrong ps es)) = do
  when (length (psParams ps) /= 0) $
    lift $ compileError $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument" ++ formatFullContextBrace c
  let (ParamSet [t0],e) = head es'
  let t1 = ParamSet [ValueType OptionalValue (vtType t0)]
  if isWeakValue t0
     -- Weak values are already unboxed.
     then return (t1,UnwrappedSingle $ valueBase ++ "::Strong(" ++ useAsUnwrapped e ++ ")")
     else return (t1,e)
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinTypename ps es)) = do
  when (length (psParams ps) /= 1) $
    lift $ compileError $ "Expected 1 type parameter" ++ formatFullContextBrace c
  when (length (psParams es) /= 0) $
    lift $ compileError $ "Expected 0 arguments" ++ formatFullContextBrace c
  let t = head $ psParams ps
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r fa t
  t' <- expandGeneralInstance t
  csRequiresTypes $ Set.unions $ map categoriesFromTypes $ psParams ps
  return $ (ParamSet [formattedRequiredValue],
            valueAsWrapped $ UnboxedPrimitive PrimString $ typeBase ++ "::TypeName(" ++ t' ++ ")")
compileExpressionStart (ParensExpression c e) = compileExpression e
compileExpressionStart (InlineAssignment c n e) = do
  (VariableValue c2 s t0 w) <- csGetVariable c n
  when (not w) $ lift $ compileError $ "Cannot assign to read-only variable " ++
                                        show n ++ formatFullContextBrace c
  (ParamSet [t],e') <- compileExpression e -- TODO: Get rid of the ParamSet matching here.
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueTypeMatch r fa t t0) `reviseError`
    ("In assignment at " ++ formatFullContext c)
  csUpdateAssigned n
  scoped <- autoScope s
  let lazy = s == CategoryScope
  return (ParamSet [t0],readStoredVariable lazy t0 $ "(" ++ scoped ++ variableName n ++
                                                     " = " ++ writeStoredVariable t0 e' ++ ")")

compileFunctionCall :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  Maybe String -> ScopedFunction c -> FunctionCall c ->
  CompilerState a m (ExpressionType,ExprValue)
compileFunctionCall e f (FunctionCall c _ ps es) = do
  r <- csResolver
  fa <- csAllFilters
  f' <- lift $ parsedToFunctionType f `reviseError`
          ("In function call at " ++ formatFullContext c)
  f'' <- lift $ assignFunctionParams r fa ps f' `reviseError`
          ("In function call at " ++ formatFullContext c)
  es' <- sequence $ map compileExpression $ psParams es
  (ts,es'') <- getValues es'
  -- Called an extra time so arg count mismatches have reasonable errors.
  lift $ processParamPairs (\_ _ -> return ()) (ftArgs f'') (ParamSet ts) `reviseError`
    ("In function call at " ++ formatFullContext c)
  lift $ processParamPairs (checkArg r fa) (ftArgs f'') (ParamSet $ zip [1..] ts) `reviseError`
    ("In function call at " ++ formatFullContext c)
  csRequiresTypes $ Set.unions $ map categoriesFromTypes $ psParams ps
  csRequiresTypes (Set.fromList [sfType f])
  params <- expandParams2 ps
  scoped <- autoScope (sfScope f)
  call <- assemble e scoped (sfScope f) (functionName f) params es''
  return $ (ftReturns f'',OpaqueMulti call)
  where
    assemble Nothing _ ValueScope n ps es =
      return $ callName (sfName f) ++ "(Var_self, " ++ ps ++ ", " ++ es ++ ")"
    assemble Nothing scoped _ n ps es =
      return $ scoped ++ callName (sfName f) ++ "(" ++ ps ++ ", " ++ es ++ ")"
    assemble (Just e) _ ValueScope n ps es =
      return $ valueBase ++ "::Call(" ++ e ++ ", " ++ functionName f ++ ", " ++ ps ++ ", " ++ es ++ ")"
    assemble (Just e) _ _ n ps es =
      return $ e ++ ".Call(" ++ functionName f ++ ", " ++ ps ++ ", " ++ es ++ ")"
    -- TODO: Lots of duplication with assignments and initialization.
    -- Single expression, but possibly multi-return.
    getValues [(ParamSet ts,e)] = return (ts,useAsArgs e)
    -- Multi-expression => must all be singles.
    getValues rs = do
      lift $ mergeAllM (map checkArity $ zip [1..] $ map fst rs) `reviseError`
        ("In return at " ++ formatFullContext c)
      return (map (head . psParams . fst) rs, "ArgTuple(" ++ intercalate ", " (map (useAsUnwrapped . snd) rs) ++ ")")
    checkArity (_,ParamSet [_]) = return ()
    checkArity (i,ParamSet ts)  =
      compileError $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    checkArg r fa t0 (i,t1) = do
      checkValueTypeMatch r fa t1 t0 `reviseError` ("In argument " ++ show i)

compileMainProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> Expression c -> m (CompiledData [String])
compileMainProcedure tm e = do
  let ctx = ProcedureContext {
      pcScope = LocalScope,
      pcType = CategoryNone,
      pcExtParams = ParamSet [],
      pcIntParams = ParamSet [],
      pcMembers = [],
      pcCategories = tm,
      pcAllFilters = Map.empty,
      pcExtFilters = [],
      pcIntFilters = [],
      pcParamScopes = Map.empty,
      pcFunctions = Map.empty,
      pcVariables = Map.empty,
      pcReturns = ValidatePositions (ParamSet []),
      pcPrimNamed = [],
      pcRequiredTypes = Set.empty,
      pcOutput = [],
      pcDisallowInit = False,
      pcLoopSetup = NotInLoop,
      pcCleanupSetup = CleanupSetup [] []
    }
  runDataCompiler compiler ctx where
    procedure = Procedure [] [IgnoreValues [] e]
    compiler = do
      ctx0 <- getCleanContext
      compileProcedure ctx0 procedure >>= put

autoScope :: (Monad m, CompilerContext c m s a) =>
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
  getAll (SingleType (JustTypeInstance (TypeInstance t ps))) = t:(concat $ map getAll $ psParams ps)
  getAll _ = []

expandParams :: (Monad m, CompilerContext c m s a) =>
  ParamSet GeneralInstance -> CompilerState a m String
expandParams ps = do
  ps' <- sequence $ map expandGeneralInstance $ psParams ps
  return $ "T_get(" ++ intercalate "," (map ("&" ++) ps') ++ ")"

expandParams2 :: (Monad m, CompilerContext c m s a) =>
  ParamSet GeneralInstance -> CompilerState a m String
expandParams2 ps = do
  ps' <- sequence $ map expandGeneralInstance $ psParams ps
  return $ "ParamTuple(" ++ intercalate "," (map ("&" ++) ps') ++ ")"

expandCategory :: (Monad m, CompilerContext c m s a) =>
  CategoryName -> CompilerState a m String
expandCategory t = return $ categoryGetter t ++ "()"

expandGeneralInstance :: (Monad m, CompilerContext c m s a) =>
  GeneralInstance -> CompilerState a m String
expandGeneralInstance (TypeMerge MergeUnion     []) = return $ allGetter ++ "()"
expandGeneralInstance (TypeMerge MergeIntersect []) = return $ anyGetter ++ "()"
expandGeneralInstance (TypeMerge m ps) = do
  ps' <- sequence $ map expandGeneralInstance ps
  return $ getter ++ "(L_get<" ++ typeBase ++ "*>(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
  where
    getter
      | m == MergeUnion     = unionGetter
      | m == MergeIntersect = intersectGetter
expandGeneralInstance (SingleType (JustTypeInstance (TypeInstance t ps))) = do
  ps' <- sequence $ map expandGeneralInstance $ psParams ps
  return $ typeGetter t ++ "(T_get(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
expandGeneralInstance (SingleType (JustParamName p)) = do
  s <- csGetParamScope p
  scoped <- autoScope s
  return $ scoped ++ paramName p

doNamedReturn :: (Monad m, CompilerContext c m [String] a) => CompilerState a m ()
doNamedReturn = do
  vars <- csPrimNamedReturns
  sequence $ map (csWrite . (:[]) . assign) vars
  doReturnCleanup
  csWrite ["return returns;"]
  where
    assign (ReturnVariable i n t) =
      "returns.At(" ++ show i ++ ") = " ++ useAsUnwrapped (readStoredVariable False t $ variableName n) ++ ";"

doReturnCleanup :: (Monad m, CompilerContext c m [String] a) => CompilerState a m ()
doReturnCleanup = do
  (CleanupSetup cs ss) <- csGetCleanup
  if null ss
     then return ()
     else do
       sequence $ map (csInheritReturns . (:[])) cs
       csWrite ss
