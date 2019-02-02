{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerCxx.Procedure (
  categoriesFromTypes,
  compileExecutableProcedure,
  compileExpression,
  compileStatement,
) where

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
  (ScopedFunction c,ExecutableProcedure c) -> m (CompiledData [String])
compileExecutableProcedure tm t ps pi ms pa fi fa va
                 (ff@(ScopedFunction _ _ _ s as1 rs1 ps1 fs _),
                  (ExecutableProcedure _ c n as2 rs2 p)) = do
  rs' <- if isUnnamedReturns rs2
            then return $ ValidatePositions rs1
            else fmap (ValidateNames . Map.fromList) $ processParamPairs pairOutput rs1 (nrNames rs2)
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
      pcRequiredTypes = Set.empty,
      pcOutput = [],
      pcDisallowInit = False
    }
  output <- runDataCompiler compileWithReturn ctx
  return $ wrapProcedure output
  where
    compileWithReturn = do
      ctx0 <- getCleanContext
      compileProcedure ctx0 p >>= put
      csRegisterReturn c (ParamSet []) `reviseErrorStateT`
        ("In implicit return from " ++ show n ++ " [" ++ formatFullContext c ++ "]")
      csWrite ["return returns;"]
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)
    wrapProcedure output =
      mergeAll $ [
          onlyCode header,
          indentCompiled $ onlyCode setProcedureTrace,
          indentCompiled $ onlyCode defineReturns,
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
        returnType ++ " " ++ name ++ "(const S<TypeValue>& Var_self, " ++
        "Params<" ++ show (length $ psParams ps1) ++ ">::Type params, " ++
        "Args<" ++ show (length $ psParams as1) ++ ">::Type args) {"
      | otherwise =
        returnType ++ " " ++ name ++ "(" ++
        "Params<" ++ show (length $ psParams ps1) ++ ">::Type params, " ++
        "Args<" ++ show (length $ psParams as1) ++ ">::Type args) {"
    returnType = "Returns<" ++ show (length $ psParams rs1) ++ ">::Type"
    setProcedureTrace = "TRACE_FUNCTION(\"" ++ show t ++ "." ++ show n ++ "\")"
    defineReturns = returnType ++ " returns;"
    nameParams = flip map (zip [0..] $ psParams ps1) $
      (\(i,p) -> paramType ++ " " ++ paramName (vpParam p) ++ " = *std::get<" ++ show i ++ ">(params);")
    nameArgs = flip map (zip [0..] $ filter (not . isDiscardedInput) $ psParams $ avNames as2) $
      (\(i,n) -> proxyType ++ " " ++ variableName (ivName n) ++ " = SafeGet<" ++ show i ++ ">(args);")
    nameReturns
      | isUnnamedReturns rs2 = []
      -- NOTE: SafeGet is not safe for named returns.
      | otherwise = flip map (zip [0..] $ psParams $ nrNames rs2) $
      (\(i,n) -> proxyType ++ " " ++ variableName (ovName n) ++ " = std::get<" ++ show i ++ ">(returns);")

compileCondition :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  a -> [c] -> Expression c -> CompilerState a m String
compileCondition ctx c e = do
  (e',ctx') <- lift $ runStateT compile ctx
  lift (ccGetRequired ctx') >>= csRequiresTypes
  return e'
  where
    compile = flip reviseErrorStateT ("In condition at " ++ formatFullContext c) $ do
      (ts,e') <- compileExpression e
      lift $ checkCondition ts
      return $ "std::get<0>(" ++ e' ++ ")->AsBool()"
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
  ctx' <- lift $ execStateT (sequence $ map compileStatement ss) ctx
  return ctx'

compileStatement :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  Statement c -> CompilerState a m ()
compileStatement (EmptyReturn c) = do
  csRegisterReturn c (ParamSet [])
  csWrite ["return returns;"]
compileStatement (ExplicitReturn c es) = do
  es' <- sequence $ map compileExpression $ psParams es
  getReturn $ zip (map getExpressionContext $ psParams es) es'
  where
    -- Single expression, but possibly multi-return.
    getReturn [(_,(ParamSet ts,e))] = do
      csRegisterReturn c (ParamSet ts)
      csWrite [setTraceContext c]
      csWrite ["return " ++ e ++ ";"]
    -- Multi-expression => must all be singles.
    getReturn rs = do
      lift $ mergeAllM (map checkArity $ zip [1..] $ map (fst . snd) rs) `reviseError`
        ("In return at " ++ formatFullContext c)
      csRegisterReturn c $ ParamSet $ map (head . psParams . fst . snd) rs
      csWrite $ concat $ map bindReturn $ zip [0..] rs
      csWrite ["return returns;"]
    checkArity (_,ParamSet [_]) = return ()
    checkArity (i,ParamSet ts)  =
      compileError $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    bindReturn (i,(c0,(_,e))) = [
        setTraceContext c0,
        "std::get<" ++ show i ++ ">(returns) = " ++ "std::get<0>(" ++ e ++ ");"
      ]
compileStatement (LoopBreak c) = do
  -- TODO: This can only be used inside of a loop.
  csWrite ["break;"]
compileStatement (IgnoreValues c e) = do
  (_,e') <- compileExpression e
  csWrite [setTraceContext c]
  csWrite [e' ++ ";"]
compileStatement (Assignment c as e) = do
  (ts,e') <- compileExpression e
  r <- csResolver
  fa <- csAllFilters
  processParamPairsT (createVariable r fa) as ts `reviseErrorStateT`
    ("In assignment at " ++ formatFullContext c)
  csWrite [setTraceContext c]
  csWrite ["{","const auto r = " ++ e' ++ ";"]
  sequence $ map assignVariable $ zip [0..] $ psParams as
  csWrite ["}"]
  where
    createVariable r fa (CreateVariable c t1 n) t2 = do
      -- TODO: Call csRequiresTypes for t1. (Maybe needs a helper function.)
      lift $ mergeAllM [validateGeneralInstance r fa (vtType t1),
                        checkValueTypeMatch r fa t2 t1] `reviseError`
        ("In creation of " ++ show n ++ " at " ++ formatFullContext c)
      csAddVariable c n (VariableValue c LocalScope t1)
      csWrite [variableType (vtRequired t1) ++ " " ++ variableName n ++ ";"]
    createVariable r fa (ExistingVariable (InputValue c n)) t2 = do
      (VariableValue _ s1 t1) <- csGetVariable c n
      -- TODO: Also show original context.
      lift $ (checkValueTypeMatch r fa t2 t1) `reviseError`
        ("In assignment to " ++ show n ++ " at " ++ formatFullContext c)
      csUpdateAssigned n
    createVariable _ _ _ _ = return ()
    assignVariable (i,CreateVariable _ _ n) =
      csWrite [variableName n ++ " = SafeGet<" ++ show i ++ ">(r);"]
    assignVariable (i,ExistingVariable (InputValue _ n)) = do
      (VariableValue _ s _) <- csGetVariable c n
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = SafeGet<" ++ show i ++ ">(r);"]
    assignVariable _ = return ()
compileStatement (NoValueExpression v) = compileVoidExpression v

compileVoidExpression :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                         CompilerContext c m [String] a) =>
  VoidExpression c -> CompilerState a m ()
compileVoidExpression (Conditional ie) = compileIfElifElse ie
compileVoidExpression (Loop l) = compileWhileLoop l
compileVoidExpression (WithScope s) = compileScopedBlock s

compileIfElifElse :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                      CompilerContext c m [String] a) =>
  IfElifElse c -> CompilerState a m ()
compileIfElifElse (IfStatement c e p es) = do
  ctx0 <- getCleanContext
  e' <- compileCondition ctx0 c e
  ctx <- compileProcedure ctx0 p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite [setTraceContext c]
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
      -- TODO: Figure out how to set the trace context for this expression.
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
compileWhileLoop (WhileLoop c e p) = do
  ctx0 <- getCleanContext
  e' <- compileCondition ctx0 c e
  ctx <- compileProcedure ctx0 p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite [setTraceContext c]
  csWrite ["while (" ++ e' ++ ") {"]
  (lift $ ccGetOutput ctx) >>= csWrite
  csWrite [setTraceContext c] -- Set it again for the conditional.
  csWrite ["}"]

compileScopedBlock :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                       CompilerContext c m [String] a) =>
  ScopedBlock c -> CompilerState a m ()
compileScopedBlock s = do
  let (vs,p,st) = rewriteScoped s
  -- Capture context so we can discard scoped variable names.
  ctx0 <- getCleanContext
  r <- csResolver
  fa <- csAllFilters
  sequence $ map (createVariable r fa) vs
  ctx <- compileProcedure ctx0 p
  -- This needs to come at the end so that statements in the scoped block cannot
  -- refer variables created in the final statement. Both of the following need
  -- to compile in the same context as p.
  ctx' <- lift $ execStateT (sequence $ map showVariable vs) ctx
  ctx'' <- compileProcedure ctx' (Procedure [] [st])
  csWrite ["{"]
  (lift $ ccGetOutput ctx'') >>= csWrite
  csWrite ["}"]
  sequence $ map showVariable vs
  (lift $ ccGetRequired ctx'') >>= csRequiresTypes
  csInheritReturns [ctx'']
  where
    createVariable r fa (c,t,n) = do
      lift $ validateGeneralInstance r fa (vtType t) `reviseError`
        ("In creation of " ++ show n ++ " at " ++ formatFullContext c)
      csWrite [variableType (vtRequired t) ++ " " ++ variableName n ++ ";"]
    showVariable (c,t,n) = do
      -- TODO: Call csRequiresTypes for t. (Maybe needs a helper function.)
      csAddVariable c n (VariableValue c LocalScope t)
    -- Merge chained scoped sections into a single section.
    rewriteScoped w@(ScopedBlock c (Procedure c2 ss1)
                                 (NoValueExpression (WithScope
                                 (ScopedBlock _ (Procedure _ ss2) s)))) =
      rewriteScoped $ ScopedBlock c (Procedure c2 $ ss1 ++ ss2) s
    -- Gather to-be-created variables.
    rewriteScoped (ScopedBlock c p (Assignment c2 vs e)) =
      (created,p,Assignment c2 (ParamSet existing) e) where
        (created,existing) = foldr update ([],[]) (psParams vs)
        update (CreateVariable c t n) (cs,es) = ((c,t,n):cs,(ExistingVariable $ InputValue c n):es)
        update e (cs,es) = (cs,e:es)
    -- Merge the statement into the scoped block.
    rewriteScoped (ScopedBlock c p s) =
      ([],p,s)

compileExpression :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                      CompilerContext c m [String] a) =>
  Expression c -> CompilerState a m (ExpressionType,String)
compileExpression = compile where
  compile (Literal (StringLiteral c l)) = do
    return (ParamSet [stringRequiredValue],"T_get(Box_String(\"" ++ l ++ "\"))")
  compile (Literal (IntegerLiteral c l)) = do
    -- TODO: Check bounds.
    return (ParamSet [intRequiredValue],"T_get(Box_Int(" ++ l ++ "))")
  compile (Literal (HexLiteral c l)) = do
    -- TODO: Check bounds.
    return (ParamSet [intRequiredValue],"T_get(Box_Int(0x" ++ l ++ "))")
  compile (Literal (DecimalLiteral c l1 l2)) = do
    -- TODO: Check bounds.
    return (ParamSet [floatRequiredValue],"T_get(Box_Float(" ++ l1 ++ "." ++ l2 ++ "))")
  compile (Literal (BoolLiteral c True)) = do
    return (ParamSet [boolRequiredValue],"T_get(Var_true)")
  compile (Literal (BoolLiteral c False)) = do
    return (ParamSet [boolRequiredValue],"T_get(Var_false)")
  compile (Literal (EmptyLiteral c)) = do
    return (ParamSet [emptyValue],"T_get(Var_empty)")
  compile (Expression c s os) = do
    foldl transform (compileExpressionStart s) os
  compile (UnaryExpression c o e) = do
    (ParamSet ts,e') <- compileExpression e
    t' <- requireSingle c ts
    doUnary t' e'
    where
      doUnary t e
        | o == "!" = doNot t e
        | o == "-" = doNeg t e
        | otherwise = lift $ compileError $ "Unknown unary operator \"" ++ o ++ "\" "++
                                            " [" ++ formatFullContext c ++ "]"
      doNot t e = do
        when (t /= boolRequiredValue) $
          lift $ compileError $ "Operator ! requires a Bool value "++
                                " [" ++ formatFullContext c ++ "]"
        return $ (ParamSet [boolRequiredValue],"T_get(Box_Bool(!std::get<0>(" ++ e ++ ")->AsBool()))")
      doNeg t e
        | t == intRequiredValue = return $ (ParamSet [intRequiredValue],
                                            "T_get(Box_Int(-std::get<0>(" ++ e ++ ")->AsInt()))")
        | t == floatRequiredValue = return $ (ParamSet [floatRequiredValue],
                                             "T_get(Box_Float(-std::get<0>(" ++ e ++ ")->AsFloat()))")
        | otherwise = lift $ compileError $ "Operator - requires an Int or Float value "++
                                            " [" ++ formatFullContext c ++ "]"
  compile (InitializeValue c t ps es) = do
    es' <- sequence $ map compileExpression $ psParams es
    (ts,es'') <- getValues es'
    csCheckValueInit c t (ParamSet ts) ps
    params <- expandParams $ tiParams t
    params2 <- expandParams $ ps
    -- TODO: This is unsafe if used in a type or category constructor.
    return (ParamSet [ValueType RequiredValue $ SingleType $ JustTypeInstance t],
            "T_get(" ++ valueCreator ++ "(" ++ typeCreator ++ "(" ++ params ++ "), " ++
                                          params2 ++ ", " ++ es'' ++ "))")
    where
      -- Single expression, but possibly multi-return.
      getValues [(ParamSet ts,e)] = return (ts,e)
      -- Multi-expression => must all be singles.
      getValues rs = do
        lift $ mergeAllM (map checkArity $ zip [1..] $ map fst rs) `reviseError`
          ("In return at " ++ formatFullContext c)
        return (map (head . psParams . fst) rs,
                "T_get(" ++ intercalate ", " (map ((\e -> "std::get<0>(" ++ e ++ ")") . snd) rs) ++ ")")
      checkArity (_,ParamSet [_]) = return ()
      checkArity (i,ParamSet ts)  =
        compileError $ "Initializer position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
  compile (InfixExpression c e1 o e2) = do
    e1' <- compileExpression e1
    e2' <- compileExpression e2
    bindInfix c e1' o e2'
  arithmetic1 = Set.fromList ["*","/"]
  arithmetic2 = Set.fromList ["%"]
  arithmetic3 = Set.fromList ["+","-"]
  arithmetic = Set.union arithmetic1 arithmetic2
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
                                show t2 ++ " [" ++ formatFullContext c ++ "]"
        | o `Set.member` comparison && t1 == intRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix "Int" "Bool" e1 o e2)
        | o `Set.member` comparison && t1 == floatRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix "Float" "Bool" e1 o e2)
        | o `Set.member` comparison && t1 == stringRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix "String" "Bool" e1 o e2)
        | o `Set.member` arithmetic1 && t1 == intRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix "Int" "Int" e1 o e2)
        | o `Set.member` arithmetic2 && t1 == intRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix "Int" "Int" e1 o e2)
        | o `Set.member` arithmetic3 && t1 == intRequiredValue = do
          return (ParamSet [intRequiredValue],glueInfix "Int" "Int" e1 o e2)
        | o `Set.member` arithmetic1 && t1 == floatRequiredValue = do
          return (ParamSet [floatRequiredValue],glueInfix "Float" "Float" e1 o e2)
        | o `Set.member` arithmetic3 && t1 == floatRequiredValue = do
          return (ParamSet [floatRequiredValue],glueInfix "Float" "Float" e1 o e2)
        | o == "+" && t1 == stringRequiredValue = do
          return (ParamSet [stringRequiredValue],glueInfix "String" "String" e1 o e2)
        | o `Set.member` logical && t1 == boolRequiredValue = do
          return (ParamSet [boolRequiredValue],glueInfix "Bool" "Bool" e1 o e2)
        | otherwise =
          lift $ compileError $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
                                show t2 ++ " [" ++ formatFullContext c ++ "]"
      glueInfix t1 t2 e1 o e2 =
        "T_get(Box_" ++ t2 ++ "(std::get<0>(" ++ e1 ++ ")->As" ++ t1 ++ "()" ++ o ++
                               "std::get<0>(" ++ e2 ++ ")->As" ++ t1 ++ "()" ++ "))"
  transform e (ConvertedCall c t f) = do
    (ParamSet ts,e') <- e
    t' <- requireSingle c ts
    r <- csResolver
    fa <- csAllFilters
    let vt = ValueType RequiredValue $ SingleType $ JustTypeInstance t
    lift $ (checkValueTypeMatch r fa t' vt) `reviseError`
      ("In conversion at " ++ formatFullContext c)
    f' <- lookupValueFunction vt f
    compileFunctionCall (Just $ "std::get<0>(" ++ e' ++ ")") f' f
  transform e (ValueCall c f) = do
    (ParamSet ts,e') <- e
    t' <- requireSingle c ts
    f' <- lookupValueFunction t' f
    compileFunctionCall (Just $ "std::get<0>(" ++ e' ++ ")") f' f
  requireSingle c [t] = return t
  requireSingle c ts =
    lift $ compileError $ "Function call requires 1 return but found but found {" ++
                          intercalate "," (map show ts) ++ "} [" ++ formatFullContext c ++ "]"

lookupValueFunction :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  ValueType -> FunctionCall c -> CompilerState a m (ScopedFunction c)
lookupValueFunction (ValueType WeakValue t) _ =
  lift $ compileError $ "Use strong to convert " ++ show t ++ " to optional first"
lookupValueFunction (ValueType OptionalValue t) _ =
  lift $ compileError $ "Use require to convert " ++ show t ++ " to required first"
lookupValueFunction (ValueType RequiredValue t) (FunctionCall c n _ _) =
  csGetTypeFunction c (Just t) n

compileExpressionStart :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                           CompilerContext c m [String] a) =>
  ExpressionStart c -> CompilerState a m (ExpressionType,String)
compileExpressionStart (NamedVariable (OutputValue c n)) = do
  (VariableValue _ s t) <- csGetVariable c n
  scoped <- autoScope s
  return (ParamSet [t],"T_get(" ++ scoped ++ variableName n ++ ")")
compileExpressionStart (CategoryCall c t f@(FunctionCall _ n _ _)) = do
  f' <- csGetCategoryFunction c (Just t) n
  csRequiresTypes $ Set.fromList [t,sfType f']
  t' <- expandCategory t
  compileFunctionCall (Just t') f' f
compileExpressionStart (TypeCall c t f@(FunctionCall _ n _ _)) = do
  f' <- csGetTypeFunction c (Just $ SingleType t) n
  when (sfScope f' /= TypeScope) $ lift $ compileError $ "Function " ++ show n ++
                                          " cannot be used as a type function [" ++
                                          formatFullContext c ++ "]"
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
        "Function " ++ show n ++ " is not in scope here [" ++ formatFullContext c ++ "]"
      return f'
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinPresent ps es)) = do
  when (length (psParams ps) /= 0) $
    lift $ compileError $ "Expected 0 type parameters [" ++ formatFullContext c ++ "]"
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument [" ++ formatFullContext c ++ "]"
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument [" ++ formatFullContext c ++ "]"
  let (ParamSet [t0],e) = head es'
  when (isWeakValue t0) $
    lift $ compileError $ "Weak values not allowed here [" ++ formatFullContext c ++ "]"
  return $ (ParamSet [boolRequiredValue],
            valueBase ++ "::Present(std::get<0>(" ++ e ++ "))")
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinReduce ps es)) = do
  when (length (psParams ps) /= 2) $
    lift $ compileError $ "Expected 2 type parameters [" ++ formatFullContext c ++ "]"
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument [" ++ formatFullContext c ++ "]"
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument [" ++ formatFullContext c ++ "]"
  let (ParamSet [t0],e) = head es'
  let (ParamSet [t1,t2]) = ps
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueTypeMatch r fa t0 (ValueType OptionalValue t1)) `reviseError`
    ("In argument to reduce call at " ++ formatFullContext c)
  -- TODO: If t1 -> t2 then just return e without a Reduce call.
  t1' <- expandGeneralInstance t1
  t2' <- expandGeneralInstance t2
  csRequiresTypes $ categoriesFromTypes t1
  csRequiresTypes $ categoriesFromTypes t2
  return $ (ParamSet [ValueType OptionalValue t2],
            typeBase ++ "::Reduce(" ++ t1' ++ ", " ++ t2' ++ ", std::get<0>(" ++ e ++ "))")
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinRequire ps es)) = do
  when (length (psParams ps) /= 0) $
    lift $ compileError $ "Expected 0 type parameters [" ++ formatFullContext c ++ "]"
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument [" ++ formatFullContext c ++ "]"
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument [" ++ formatFullContext c ++ "]"
  let (ParamSet [t0],e) = head es'
  when (isWeakValue t0) $
    lift $ compileError $ "Weak values not allowed here [" ++ formatFullContext c ++ "]"
  return $ (ParamSet [ValueType RequiredValue (vtType t0)],
            valueBase ++ "::Require(std::get<0>(" ++ e ++ "))")
compileExpressionStart (BuiltinCall c f@(FunctionCall _ BuiltinStrong ps es)) = do
  when (length (psParams ps) /= 0) $
    lift $ compileError $ "Expected 0 type parameters [" ++ formatFullContext c ++ "]"
  when (length (psParams es) /= 1) $
    lift $ compileError $ "Expected 1 argument [" ++ formatFullContext c ++ "]"
  es' <- sequence $ map compileExpression $ psParams es
  when (length (psParams $ fst $ head es') /= 1) $
    lift $ compileError $ "Expected single return in argument [" ++ formatFullContext c ++ "]"
  let (ParamSet [t0],e) = head es'
  let t1 = ParamSet [ValueType OptionalValue (vtType t0)]
  if isWeakValue t0
     then return (t1,valueBase ++ "::Strong(std::get<0>(" ++ e ++ "))")
     else return (t1,e)
compileExpressionStart (ParensExpression c e) = do
  (t,e') <- compileExpression e
  return (t,"(" ++ e' ++ ")")
compileExpressionStart (InlineAssignment c n e) = do
  (VariableValue c2 s t0) <- csGetVariable c n
  (ParamSet [t],e') <- compileExpression e -- TODO: Get rid of the ParamSet matching here.
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueTypeMatch r fa t t0) `reviseError`
    ("In assignment at " ++ formatFullContext c)
  -- TODO: This might not be safe when operators are used, since the assignment
  -- might get short-circuited.
  csUpdateAssigned n
  scoped <- autoScope s
  return (ParamSet [t0],"T_get(" ++ scoped ++ variableName n ++ " = std::get<0>(" ++ e' ++ "))")

compileFunctionCall :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  Maybe String -> ScopedFunction c -> FunctionCall c ->
  CompilerState a m (ExpressionType,String)
compileFunctionCall e f (FunctionCall c _ ps es) = do
  r <- csResolver
  fa <- csAllFilters
  f' <- lift $ parsedToFunctionType f `reviseError`
          ("In function call at " ++ formatFullContext c)
  f'' <- lift $ assignFunctionParams r fa ps f' `reviseError`
          ("In function call at " ++ formatFullContext c)
  es' <- sequence $ map compileExpression $ psParams es
  (ts,es'') <- getValues es'
  lift $ processParamPairs (checkArg r fa) (ftArgs f'') (ParamSet $ zip [1..] ts) `reviseError`
    ("In function call at " ++ formatFullContext c)
  csRequiresTypes $ Set.unions $ map categoriesFromTypes $ psParams ps
  csRequiresTypes (Set.fromList [sfType f])
  params <- expandParams ps
  call <- assemble e (sfScope f) (functionName f) params es''
  return $ (ftReturns f'',call)
  where
    assemble (Just e) ValueScope n ps es =
      return $ valueBase ++ "::Call(" ++ e ++ ", " ++ functionName f ++ ", " ++ ps ++ ", " ++ es ++ ")"
    assemble Nothing ValueScope n ps es =
      return $ valueBase ++ "::Call(Var_self, " ++ functionName f ++ ", " ++ ps ++ ", " ++ es ++ ")"
    assemble (Just e) _ n ps es =
      return $ e ++ ".Call(" ++ functionName f ++ ", " ++ ps ++ ", " ++ es ++ ")"
    assemble _ _ n ps es = do
      scoped <- autoScope $ sfScope f
      return $ scoped ++ "Call(" ++ functionName f ++ ", " ++ ps ++ ", " ++ es ++ ")"
    -- TODO: Lots of duplication with assignments and initialization.
    -- Single expression, but possibly multi-return.
    getValues [(ParamSet ts,e)] = return (ts,e)
    -- Multi-expression => must all be singles.
    getValues rs = do
      lift $ mergeAllM (map checkArity $ zip [1..] $ map fst rs) `reviseError`
        ("In return at " ++ formatFullContext c)
      return (map (head . psParams . fst) rs,
              "T_get(" ++ intercalate ", " (map ((\e -> "std::get<0>(" ++ e ++ ")") . snd) rs) ++ ")")
    checkArity (_,ParamSet [_]) = return ()
    checkArity (i,ParamSet ts)  =
      compileError $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    checkArg r fa t0 (i,t1) = do
      checkValueTypeMatch r fa t1 t0 `reviseError` ("In argument " ++ show i)

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

expandCategory :: (Monad m, CompilerContext c m s a) =>
  CategoryName -> CompilerState a m String
expandCategory t = return $ categoryGetter t ++ "()"

expandGeneralInstance :: (Monad m, CompilerContext c m s a) =>
  GeneralInstance -> CompilerState a m String
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
