{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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
  CxxFunctionType(..),
  categoriesFromTypes,
  categoriesFromDefine,
  categoriesFromRefine,
  compileExecutableProcedure,
  compileMainProcedure,
  compileLazyInit,
  compileRegularInit,
  compileTestProcedure,
  procedureDeclaration,
  selectTestFromArgv1,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.State (execStateT,get,put,runStateT)
import Control.Monad.Trans (lift)
import Data.List (intercalate,nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.MergeTree
import Base.Positional
import Compilation.CompilerState
import Compilation.ProcedureContext (ExprMap)
import Compilation.ScopeContext
import CompilerCxx.CategoryContext
import CompilerCxx.Code
import CompilerCxx.Naming
import Types.Builtin
import Types.DefinedCategory
import Types.Function
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


procedureDeclaration :: Monad m => Bool -> Bool -> ScopedFunction c -> m (CompiledData [String])
procedureDeclaration immutable abstract f = return $ onlyCode func where
  func
    | abstract = "virtual " ++ proto ++ " = 0;"
    | otherwise = proto ++ ";"
  name = callName (sfName f)
  suffix
    | immutable = " const"
    | otherwise = ""
  proto
    | sfScope f == CategoryScope =
      "ReturnTuple " ++ name ++ "(const ParamsArgs& params_args)"
    | sfScope f == TypeScope =
      "ReturnTuple " ++ name ++ "(const ParamsArgs& params_args) const"
    | sfScope f == ValueScope =
      "ReturnTuple " ++ name ++ "(const ParamsArgs& params_args)" ++ suffix
    | otherwise = undefined

data CxxFunctionType =
  InlineFunction |
  OutOfLineFunction String |
  FinalInlineFunction
  deriving Show

compileExecutableProcedure :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> CxxFunctionType -> ScopeContext c -> ScopedFunction c ->
  ExecutableProcedure c -> m (CompiledData [String])
compileExecutableProcedure immutable cxxType ctx
  ff@(ScopedFunction _ _ _ s as1 rs1 ps1 _ _)
  pp@(ExecutableProcedure c pragmas c2 n as2 rs2 p) = do
  ctx' <- getProcedureContext ctx ff pp
  output <- runDataCompiler compileWithReturn ctx'
  procedureTrace <- setProcedureTrace
  creationTrace  <- setCreationTrace
  return $ wrapProcedure output procedureTrace creationTrace
  where
    compileWithReturn = do
      ctx0 <- getCleanContext >>= lift . flip ccSetNoTrace (any isNoTrace pragmas)
      compileProcedure ctx0 p >>= put
      unreachable <- csIsUnreachable
      when (not unreachable) $
        doImplicitReturn c2 <??
          "In implicit return from " ++ show n ++ formatFullContextBrace c
    funcMergeDeps f = mconcat $ (onlyDeps (Set.fromList [sfType f])):(map funcMergeDeps $ sfMerges f)
    wrapProcedure output pt ct =
      mconcat [
          funcMergeDeps ff,
          onlyCode proto,
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
    prefix = case cxxType of
                  OutOfLineFunction cn -> cn ++ "::"
                  _ -> ""
    final = case cxxType of
                 FinalInlineFunction -> " final"
                 _ -> ""
    suffix
      | immutable = " const"
      | otherwise = ""
    proto
      | s == CategoryScope =
        "ReturnTuple " ++ prefix ++ name ++ "(const ParamsArgs& params_args)" ++ final ++ " {"
      | s == TypeScope =
        "ReturnTuple " ++ prefix ++ name ++ "(const ParamsArgs& params_args) const" ++ final ++ " {"
      | s == ValueScope =
        "ReturnTuple " ++ prefix ++ name ++ "(const ParamsArgs& params_args)" ++ suffix ++ final ++ " {"
      | otherwise = undefined
    setProcedureTrace
      | any isNoTrace pragmas = return []
      | otherwise             = return [startFunctionTracing (scName ctx) ff]
    setCreationTrace
      | not $ any isTraceCreation pragmas = return []
      | s /= ValueScope =
          (compilerWarningM $ "Creation tracing ignored for " ++ show s ++
            " functions" ++ formatFullContextBrace c) >> return []
      | otherwise = return [showCreationTrace]
    defineReturns
      | isUnnamedReturns rs2 = []
      | otherwise            = ["ReturnTuple returns(" ++ show (length $ pValues rs1) ++ ");"]
    nameParams = flip map (zip ([0..] :: [Int]) $ pValues ps1) $
      (\(i,p2) -> paramType ++ " " ++ paramName (vpParam p2) ++ " = params_args.GetParam(" ++ show i ++ ");")
    nameArgs = map nameSingleArg (zip ([0..] :: [Int]) $ zip (pValues as1) (pValues $ avNames as2))
    nameSingleArg (i,(t2,n2))
      | isDiscardedInput n2 = "// Arg " ++ show i ++ " (" ++ show (pvType t2) ++ ") is discarded"
      | otherwise = "const " ++ variableProxyType (pvType t2) ++ " " ++ variableName (ivName n2) ++
                    " = " ++ writeStoredVariable (pvType t2) (UnwrappedSingle $ "params_args.GetArg(" ++ show i ++ ")") ++ ";"
    nameReturns
      | isUnnamedReturns rs2 = []
      | otherwise = map (\(i,(t2,n2)) -> nameReturn i (pvType t2) n2) (zip ([0..] :: [Int]) $ zip (pValues rs1) (pValues $ nrNames rs2))
    nameReturn i t2 n2
      | isStoredUnboxed t2 = variableProxyType t2 ++ " " ++ variableName (ovName n2) ++ ";"
      | otherwise =
        variableProxyType t2 ++ " " ++ variableName (ovName n2) ++
        " = " ++ writeStoredVariable t2 (UnwrappedSingle $ "returns.At(" ++ show i ++ ")") ++ ";"

compileCondition :: (Ord c, Show c, CollectErrorsM m,
                     CompilerContext c m [String] a) =>
  a -> [c] -> Expression c -> CompilerState a m (String,a)
compileCondition ctx c e = do
  (e',ctx') <- resetBackgroundM $ lift $ runStateT compile ctx
  noTrace <- csGetNoTrace
  if noTrace
     then return (e',ctx')
     else do
       let c2 = getExpressionContext e
       csAddTrace $ formatFullContext c2
       return (predTraceContext c2 ++ "(" ++ e' ++ ")",ctx')
  where
    compile = "In condition at " ++ formatFullContext c ??> do
      (ts,e') <- compileExpression e
      checkCondition ts
      return $ useAsUnboxed PrimBool e'
      where
        checkCondition (Positional [t]) | t == boolRequiredValue = return ()
        checkCondition (Positional ts) =
          compilerErrorM $ "Expected exactly one Bool value but got " ++
                           intercalate ", " (map show ts)

-- Returns the state so that returns can be properly checked for if/elif/else.
compileProcedure :: (Ord c, Show c, CollectErrorsM m,
                     CompilerContext c m [String] a) =>
  a -> Procedure c -> CompilerState a m a
compileProcedure ctx (Procedure _ ss) = do
  ctx' <- lift $ execStateT (sequence $ map compile ss) ctx
  return ctx' where
    compile s = do
      unreachable <- csIsUnreachable
      if unreachable && not (isRawCodeLine s)
         then compilerWarningM $ "Statement at " ++
                                 formatFullContext (getStatementContext s) ++
                                 " is unreachable (skipping compilation)"
         else do
           s' <- resetBackgroundM $ compileStatement s
           return s'

maybeSetTrace :: (Ord c, Show c, CollectErrorsM m,
                  CompilerContext c m [String] a) =>
  [c] -> CompilerState a m ()
maybeSetTrace c = do
  noTrace <- csGetNoTrace
  when (not noTrace) $ do
    csWrite $ setTraceContext c
    csAddTrace $ formatFullContext c

compileStatement :: (Ord c, Show c, CollectErrorsM m,
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
      autoPositionalCleanup c e
    -- Multi-expression => must all be singles.
    getReturn rs = do
      lift (mapCompilerM_ checkArity $ zip ([0..] :: [Int]) $ map (fst . snd) rs) <??
        ("In return at " ++ formatFullContext c)
      csRegisterReturn c $ Just $ Positional $ map (head . pValues . fst . snd) rs
      let e = OpaqueMulti $ "ReturnTuple(" ++ intercalate "," (map (useAsUnwrapped . snd . snd) rs) ++ ")"
      maybeSetTrace c
      autoPositionalCleanup c e
    checkArity (_,Positional [_]) = return ()
    checkArity (i,Positional ts)  =
      compilerErrorM $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
compileStatement (LoopBreak c) = do
  loop <- csGetLoop
  case loop of
       NotInLoop ->
         compilerErrorM $ "Using break outside of while is no allowed" ++ formatFullContextBrace c
       _ -> return ()
  csSetJumpType c JumpBreak
  get >>= autoInsertCleanup c JumpBreak
  csWrite ["break;"]
compileStatement (LoopContinue c) = do
  loop <- csGetLoop
  case loop of
       NotInLoop ->
         compilerErrorM $ "Using continue outside of while is no allowed" ++ formatFullContextBrace c
       _ -> return ()
  csSetJumpType c JumpContinue
  get >>= autoInsertCleanup c JumpContinue
  csWrite $ ["{"] ++ lsUpdate loop ++ ["}","continue;"]
compileStatement (FailCall c e) = do
  csAddRequired (Set.fromList [BuiltinFormatted,BuiltinString])
  e' <- compileExpression e
  when (length (pValues $ fst e') /= 1) $
    compilerErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e0) = e'
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueAssignment r fa t0 formattedRequiredValue) <??
    "In fail call at " ++ formatFullContext c
  csSetJumpType c JumpFailCall
  maybeSetTrace c
  csWrite ["BUILTIN_FAIL(" ++ useAsUnwrapped e0 ++ ")"]
compileStatement (RawFailCall s) = do
  csSetJumpType [] JumpFailCall
  csWrite ["RAW_FAIL(" ++ show s ++ ")"]
compileStatement (IgnoreValues c e) = do
  (_,e') <- compileExpression e
  maybeSetTrace c
  csWrite ["(void) (" ++ useAsWhatever e' ++ ");"]
compileStatement (DeferredVariables c as) = message ??> mapM_ createVariable as
  where
    message = "Deferred initialization at " ++ formatFullContext c
    createVariable (CreateVariable c2 t1 n) =
      "In creation of " ++ show n ++ " at " ++ formatFullContext c2 ??> do
        self <- autoSelfType
        t1' <- lift $ replaceSelfValueType self t1
        csAddVariable (UsedVariable c2 n) (VariableValue c2 LocalScope t1' VariableDefault)
        csWrite [variableStoredType t1' ++ " " ++ variableName n ++ ";"]
        csSetDeferred (UsedVariable c2 n)
    createVariable (ExistingVariable (InputValue c2 n)) =
      "In deferring of " ++ show n ++ " at " ++ formatFullContext c2 ??>
      csSetDeferred (UsedVariable c2 n)
    createVariable (ExistingVariable (DiscardInput c2)) =
      compilerErrorM $ "Cannot defer discarded value" ++ formatFullContextBrace c2
compileStatement (Assignment c as e) = message ??> do
  (ts,e') <- compileExpression e
  r <- csResolver
  fa <- csAllFilters
  -- Check for a count match first, to avoid the default error message.
  _ <- processPairsT alwaysPair (fmap assignableName as) ts
  _ <- processPairsT (createVariable r fa) as ts
  maybeSetTrace c
  variableTypes <- sequence $ map getVariableType (pValues as)
  assignAll (zip3 ([0..] :: [Int]) variableTypes (pValues as)) e'
  where
    message = "In assignment at " ++ formatFullContext c
    assignAll [v] e2 = assignSingle v e2
    assignAll vs e2 = do
      csWrite ["{","const auto r = " ++ useAsReturns e2 ++ ";"]
      sequence_ $ map assignMulti vs
      csWrite ["}"]
    getVariableType (CreateVariable _ t _) = return t
    getVariableType (ExistingVariable (InputValue c2 n)) = do
      (VariableValue _ _ t _) <- csGetVariable (UsedVariable c2 n)
      return t
    getVariableType _ = return undefined
    createVariable r fa (CreateVariable c2 t1 n) t2 =
      "In creation of " ++ show n ++ " at " ++ formatFullContext c2 ??> do
        self <- autoSelfType
        t1' <- lift $ replaceSelfValueType self t1
        -- TODO: Call csAddRequired for t1'. (Maybe needs a helper function.)
        lift $ collectAllM_ [validateGeneralInstance r (Map.keysSet fa) (vtType t1'),
                             checkValueAssignment r fa t2 t1']
        csAddVariable (UsedVariable c2 n) (VariableValue c2 LocalScope t1' VariableDefault)
        csWrite [variableStoredType t1' ++ " " ++ variableName n ++ ";"]
    createVariable r fa (ExistingVariable (InputValue c2 n)) t2 =
      "In assignment to " ++ show n ++ " at " ++ formatFullContext c2 ??> do
        (VariableValue _ _ t1 _) <- getWritableVariable c2 n
        -- TODO: Also show original context.
        lift $ (checkValueAssignment r fa t2 t1)
        csUpdateAssigned n
    createVariable _ _ _ _ = return ()
    assignSingle (_,t,CreateVariable _ _ n) e2 =
      csWrite [variableName n ++ " = " ++ writeStoredVariable t e2 ++ ";"]
    assignSingle (_,t,ExistingVariable (InputValue c2 n)) e2 = do
      (VariableValue _ s _ _) <- csGetVariable (UsedVariable c2 n)
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = " ++ writeStoredVariable t e2 ++ ";"]
    assignSingle (_,_,ExistingVariable (DiscardInput _)) e2 = do
      csWrite ["(void) (" ++ useAsWhatever e2 ++ ");"]
    assignMulti (i,t,CreateVariable _ _ n) =
      csWrite [variableName n ++ " = " ++
               writeStoredVariable t (UnwrappedSingle $ "r.At(" ++ show i ++ ")") ++ ";"]
    assignMulti (i,t,ExistingVariable (InputValue _ n)) = do
      (VariableValue _ s _ _) <- csGetVariable (UsedVariable c n)
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = " ++
               writeStoredVariable t (UnwrappedSingle $ "r.At(" ++ show i ++ ")") ++ ";"]
    assignMulti _ = return ()
compileStatement (NoValueExpression _ v) = compileVoidExpression v
compileStatement (MarkReadOnly c vs) = mapM_ (\v -> csSetReadOnly (UsedVariable c v)) vs
compileStatement (MarkHidden   c vs) = mapM_ (\v -> csSetHidden   (UsedVariable c v)) vs
compileStatement (ValidateRefs c vs) = mapM_ validate vs where
  validate n = do
    (VariableValue _ _ t _) <- csGetVariable (UsedVariable c n)
    let e = readStoredVariable False t (variableName n)
    maybeSetTrace c
    csWrite [useAsUnwrapped e ++ ".Validate(\"" ++ show n ++ "\");"]
compileStatement (ShowVariable c t n) = csAddVariable (UsedVariable c n) (VariableValue c LocalScope t VariableDefault)
compileStatement (RawCodeLine s) = csWrite [s]

compileRegularInit :: (Ord c, Show c, CollectErrorsM m,
                       CompilerContext c m [String] a) =>
  DefinedMember c -> CompilerState a m ()
compileRegularInit (DefinedMember _ _ _ _ Nothing) = return ()
compileRegularInit (DefinedMember c2 s t n2 (Just e)) = resetBackgroundM $ do
  csAddVariable (UsedVariable c2 n2) (VariableValue c2 s t VariableDefault)
  let assign = Assignment c2 (Positional [ExistingVariable (InputValue c2 n2)]) e
  compileStatement assign

getWritableVariable :: (Show c, CollectErrorsM m, CompilerContext c m [String] a) =>
  [c] -> VariableName -> CompilerState a m (VariableValue c)
getWritableVariable c n = do
  v@(VariableValue _ _ _ ro) <- csGetVariable (UsedVariable c n)
  case ro of
       VariableReadOnly [] -> compilerErrorM $ "Variable " ++ show n ++
                              formatFullContextBrace c ++ " is read-only"
       VariableReadOnly c2 -> compilerErrorM $ "Variable " ++ show n ++
                              formatFullContextBrace c ++ " is marked read-only at " ++ formatFullContext c2
       _ -> return v

compileLazyInit :: (Ord c, Show c, CollectErrorsM m,
                   CompilerContext c m [String] a) =>
  DefinedMember c -> CompilerState a m ()
compileLazyInit (DefinedMember _ _ _ _ Nothing) = return ()
compileLazyInit (DefinedMember c _ t1 n (Just e)) = resetBackgroundM $ do
  (ts,e') <- compileExpression e
  when (length (pValues ts) /= 1) $
    compilerErrorM $ "Expected single return in initializer" ++ formatFullContextBrace (getExpressionContext e)
  r <- csResolver
  fa <- csAllFilters
  let Positional [t2] = ts
  lift $ (checkValueAssignment r fa t2 t1) <??
    "In initialization of " ++ show n ++ " at " ++ formatFullContext c
  csWrite [variableName n ++ "([this]() { return " ++ writeStoredVariable t1 e' ++ "; })"]

compileVoidExpression :: (Ord c, Show c, CollectErrorsM m,
                         CompilerContext c m [String] a) =>
  VoidExpression c -> CompilerState a m ()
compileVoidExpression (Conditional ie) = compileIfElifElse ie
compileVoidExpression (Loop l) = compileIteratedLoop l
compileVoidExpression (WithScope s) = compileScopedBlock s
compileVoidExpression (LineComment s) = csWrite $ map ("// " ++) $ lines s
compileVoidExpression (Unconditional p) = do
  ctx0 <- getCleanContext
  ctx <- compileProcedure ctx0 p
  csWrite ["{"]
  autoInlineOutput ctx
  csWrite ["}"]

compileIfElifElse :: (Ord c, Show c, CollectErrorsM m,
                      CompilerContext c m [String] a) =>
  IfElifElse c -> CompilerState a m ()
compileIfElifElse (IfStatement c e p es) = do
  ctx0 <- getCleanContext
  cs <- commonIf ctx0 "if" c e p es
  csInheritStatic cs
  where
    unwind ctx0 (IfStatement c2 e2 p2 es2) = commonIf ctx0 "else if" c2 e2 p2 es2
    unwind ctx0 (ElseStatement _ p2) = do
      ctx <- compileProcedure ctx0 p2
      inheritRequired ctx
      csWrite ["else {"]
      getAndIndentOutput ctx >>= csWrite
      csWrite ["}"]
      return [ctx]
    unwind ctx0 TerminateConditional = return [ctx0]
    commonIf ctx0 s c2 e2 p2 es2 = do
      (e2',ctx1) <- compileCondition ctx0 c2 e2
      ctx <- compileProcedure ctx1 p2
      inheritRequired ctx
      csWrite [s ++ " (" ++ e2' ++ ") {"]
      getAndIndentOutput ctx >>= csWrite
      csWrite ["}"]
      cs <- unwind ctx1 es2
      return $ ctx:cs
compileIfElifElse _ = undefined

compileIteratedLoop :: (Ord c, Show c, CollectErrorsM m, CompilerContext c m [String] a) =>
  IteratedLoop c -> CompilerState a m ()
compileIteratedLoop (WhileLoop c e p u) = do
  ctx0 <- getCleanContext
  (e',ctx1) <- compileCondition ctx0 c e
  csInheritStatic [ctx1]
  ctx0' <- case u of
                Just p2 -> do
                  ctx2 <- lift $ ccStartLoop ctx1 (LoopSetup [])
                  ctx3 <- compileProcedure ctx2 p2
                  inheritRequired ctx3
                  p2' <- getAndIndentOutput ctx3
                  lift $ ccStartLoop ctx1 (LoopSetup p2')
                _ -> lift $ ccStartLoop ctx1 (LoopSetup [])
  (LoopSetup u') <- lift $ ccGetLoop ctx0'
  ctx <- compileProcedure ctx0' p
  inheritRequired ctx
  csWrite ["while (" ++ e' ++ ") {"]
  getAndIndentOutput ctx >>= csWrite
  csWrite $ ["{"] ++ u' ++ ["}"]
  csWrite ["}"]
compileIteratedLoop (TraverseLoop c1 e c2 a (Procedure c3 ss) u) = "In compilation of traverse at " ++ formatFullContext c1 ??> do
  (Positional ts,e') <- compileExpression e
  checkContainer ts
  r <- csResolver
  fa <- csAllFilters
  let [t] = ts
  let autoParam = ParamName "#auto"
  let autoType  = singleType $ JustParamName False autoParam
  (Positional [t2]) <- lift $ guessParams r fa (Positional [orderOptionalValue autoType])
                                               (Positional [autoParam])
                                               (Positional [InferredInstance c1])
                                               (Positional [t])
  let currVar = hiddenVariableName $ VariableName "traverse"
  let currType = orderOptionalValue $ fixTypeParams t2
  let currExpr    = BuiltinCall [] $ FunctionCall [] BuiltinRequire (Positional []) (Positional [RawExpression (Positional [currType]) (UnwrappedSingle currVar)])
  let currPresent = BuiltinCall [] $ FunctionCall [] BuiltinPresent (Positional []) (Positional [RawExpression (Positional [currType]) (UnwrappedSingle currVar)])
  let callNext = Expression c1 currExpr [ValueCall c1 $ FunctionCall c1 (FunctionName "next") (Positional []) (Positional [])]
  let callGet  = Expression c2 currExpr [ValueCall c2 $ FunctionCall c2 (FunctionName "get")  (Positional []) (Positional [])]
  (Positional [typeGet],exprNext) <- compileExpression callNext
  when (typeGet /= currType) $ compilerErrorM $ "Unexpected return type from next(): " ++ show typeGet ++ " (expected) " ++ show currType ++ " (actual)"
  let assnGet = if isAssignableDiscard a then [] else [Assignment c2 (Positional [a]) callGet]
  let showVar = case a of
                     CreateVariable c4 t3 n -> [ShowVariable c4 t3 n]
                     _ -> []
  let next = [RawCodeLine $ currVar ++ " = " ++ writeStoredVariable currType exprNext ++ ";"]
  csAddRequired $ categoriesFromTypes $ vtType currType
  compileStatement $ NoValueExpression [] $ WithScope $ ScopedBlock []
    (Procedure [] [RawCodeLine $ variableStoredType currType ++ " " ++ currVar ++ " = " ++ writeStoredVariable currType e' ++ ";"]) Nothing []
    (NoValueExpression [] $ Loop $ WhileLoop [] (Expression [] currPresent [])
      (Procedure c3 (assnGet ++ ss))
      (Just $ Procedure [] (next ++ showVar ++ update)))
    where
      update = case u of
                    Just (Procedure _ ss2) -> ss2
                    _                      -> []
      checkContainer [_] = return ()
      checkContainer ts =
        compilerErrorM $ "Expected exactly one Order<?> value but got " ++
                         intercalate ", " (map show ts)

compileScopedBlock :: (Ord c, Show c, CollectErrorsM m,
                       CompilerContext c m [String] a) =>
  ScopedBlock c -> CompilerState a m ()
compileScopedBlock s@(ScopedBlock _ _ _ c2 _) = do
  let (vs,p,cl,st) = rewriteScoped s
  case st of
       DeferredVariables c3 _ ->
         compilerErrorM $ "Cannot defer variable initialization at the top level of scoped/cleanup in statements" ++ formatFullContextBrace c3
       _ -> return ()
  self <- autoSelfType
  vs' <- lift $ mapCompilerM (replaceSelfVariable self) vs
  -- Capture context so we can discard scoped variable names.
  ctx0 <- getCleanContext
  r <- csResolver
  fa <- csAllFilters
  sequence_ $ map (createVariable r fa) vs'
  ctxP0 <- compileProcedure ctx0 p
  -- Make variables to be created visible *after* p has been compiled so that p
  -- can't refer to them.
  ctxP <- lift $ execStateT (sequence $ map showVariable vs') ctxP0
  ctxCl0 <- lift $ ccClearOutput ctxP >>= flip ccStartCleanup c2
  ctxP' <-
    case cl of
         -- Insert cleanup into the context for the in block.
         Just (Procedure c ss) -> do
           noTrace <- csGetNoTrace
           let trace = if noTrace then [] else [RawCodeLine startCleanupTracing]
           let p2' = Procedure c $ [RawCodeLine "{"] ++ trace ++ ss ++ [RawCodeLine "}"]
           ctxCl <- compileProcedure ctxCl0 p2' <?? "In cleanup starting at " ++ formatFullContext c
           ctxP' <- lift $ ccPushCleanup ctxP ctxCl
           return ctxP'
         -- Insert an empty cleanup so that it can be used below.
         Nothing -> lift $ ccPushCleanup ctxP ctxCl0
  ctxS <- compileProcedure ctxP' (Procedure [] [st])
  case st of
       -- Make sure that top-level assignments removed deferred status.
       Assignment _ (Positional existing) _ -> mapM_ setAssigned existing
       _ -> return ()
  csWrite ["{"]
  autoInlineOutput ctxS
  -- NOTE: Keep this after inlining the in block in case the in block contains a
  -- jump. (If it does, the cleanup will already be inlined.)
  unreachable <- csIsUnreachable
  when (not unreachable) $ autoInsertCleanup c2 NextStatement ctxP'
  csWrite ["}"]
  sequence_ $ map showVariable vs'
  where
    setAssigned (ExistingVariable (InputValue _ n)) = csUpdateAssigned n
    setAssigned _ = return ()
    replaceSelfVariable self (c,t,n) = do
      t' <- replaceSelfValueType self t
      return (c,t',n)
    createVariable r fa (c,t,n) = do
      lift $ validateGeneralInstance r (Map.keysSet fa) (vtType t) <??
        "In creation of " ++ show n ++ " at " ++ formatFullContext c
      csWrite [variableStoredType t ++ " " ++ variableName n ++ ";"]
    showVariable (c,t,n) = do
      -- TODO: Call csAddRequired for t. (Maybe needs a helper function.)
      csAddVariable (UsedVariable c n) (VariableValue c LocalScope t VariableDefault)
    -- Don't merge if the second scope has cleanup, so that the latter can't
    -- refer to variables defined in the first scope.
    rewriteScoped (ScopedBlock _ p cl@(Just _) _
                               s2@(NoValueExpression _ (WithScope
                                   (ScopedBlock _ _ (Just _) _ _)))) =
      ([],p,cl,s2)
    -- Merge chained scoped sections into a single section.
    rewriteScoped (ScopedBlock c (Procedure c3 ss1) cl1 c4
                               (NoValueExpression _ (WithScope
                                (ScopedBlock _ (Procedure _ ss2) cl2 _ s2)))) =
      rewriteScoped $ ScopedBlock c (Procedure c3 $ ss1 ++ ss2) (cl1 <|> cl2) c4 s2
    -- Gather to-be-created variables.
    rewriteScoped (ScopedBlock _ p cl _ (Assignment c3 vs e)) =
      (created,p,cl,Assignment c3 (Positional existing) e) where
        (created,existing) = foldr update ([],[]) (pValues vs)
        update (CreateVariable c t n) (cs,es) = ((c,t,n):cs,(ExistingVariable $ InputValue c n):es)
        update e2 (cs,es) = (cs,e2:es)
    rewriteScoped (ScopedBlock _ p cl _ (DeferredVariables c3 vs)) =
      (created,p,cl,DeferredVariables c3 existing) where
        (created,existing) = foldr update ([],[]) vs
        update (CreateVariable c t n) (cs,es) = ((c,t,n):cs,(ExistingVariable $ InputValue c n):es)
        update e2 (cs,es) = (cs,e2:es)
    -- Merge the statement into the scoped block.
    rewriteScoped (ScopedBlock _ p cl _ s2) =
      ([],p,cl,s2)

compileExpression :: (Ord c, Show c, CollectErrorsM m,
                      CompilerContext c m [String] a) =>
  Expression c -> CompilerState a m (ExpressionType,ExpressionValue)
compileExpression = compile where
  compile (Literal l) = compileValueLiteral l
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
  compile (UnaryExpression _ (NamedOperator c "-") (Literal (IntegerLiteral _ _ l))) =
    compile (Literal (IntegerLiteral c False (-l)))
  compile (UnaryExpression _ (NamedOperator c "-") (Literal (DecimalLiteral _ l e))) =
    compile (Literal (DecimalLiteral c (-l) e))
  compile (UnaryExpression _ (NamedOperator c o) e) = do
    (Positional ts,e') <- compileExpression e
    t' <- requireSingle c ts
    doUnary t' e'
    where
      doUnary t e2
        | o == "!" = doNot t e2
        | o == "-" = doNeg t e2
        | o == "~" = doComp t e2
        | otherwise = compilerErrorM $ "Unknown unary operator \"" ++ o ++ "\" " ++
                                       formatFullContextBrace c
      doNot t e2 = do
        when (t /= boolRequiredValue) $
          compilerErrorM $ "Cannot use " ++ show t ++ " with unary ! operator" ++
                            formatFullContextBrace c
        return $ (Positional [boolRequiredValue],UnboxedPrimitive PrimBool $ "!(" ++ useAsUnboxed PrimBool e2 ++ ")")
      doNeg t e2
        | t == intRequiredValue = return $ (Positional [intRequiredValue],
                                            UnboxedPrimitive PrimInt $ "-" ++ useAsUnboxed PrimInt e2)
        | t == floatRequiredValue = return $ (Positional [floatRequiredValue],
                                             UnboxedPrimitive PrimFloat $ "-(" ++ useAsUnboxed PrimFloat e2 ++ ")")
        | otherwise = compilerErrorM $ "Cannot use " ++ show t ++ " with unary - operator" ++
                                       formatFullContextBrace c
      doComp t e2
        | t == intRequiredValue = return $ (Positional [intRequiredValue],
                                            UnboxedPrimitive PrimInt $ "~(" ++ useAsUnboxed PrimInt e2 ++ ")")
        | otherwise = compilerErrorM $ "Cannot use " ++ show t ++ " with unary ~ operator" ++
                                       formatFullContextBrace c
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (CategoryFunction c2 cn) fn ps)) e2) =
    compile (Expression c (CategoryCall c2 cn (FunctionCall c fn ps (Positional [e1,e2]))) [])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (TypeFunction c2 tn) fn ps)) e2) =
    compile (Expression c (TypeCall c2 tn (FunctionCall c fn ps (Positional [e1,e2]))) [])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec _ (ValueFunction c2 e0) fn ps)) e2) =
    compile (Expression c (ParensExpression c2 e0) [ValueCall c (FunctionCall c fn ps (Positional [e1,e2]))])
  compile (InfixExpression c e1 (FunctionOperator _ (FunctionSpec c2 UnqualifiedFunction fn ps)) e2) =
    compile (Expression c (UnqualifiedCall c2 (FunctionCall c fn ps (Positional [e1,e2]))) [])
  compile (InfixExpression _ e1 (NamedOperator c o) e2) = do
    e1' <- compileExpression e1
    e2' <- if o `Set.member` logical
              then isolateExpression e2 -- Ignore named-return assignments.
              else compileExpression e2
    bindInfix c e1' o e2'
  compile (RawExpression ts e) = return (ts,e)
  isolateExpression e = do
    ctx <- getCleanContext
    (e',ctx') <- lift $ runStateT (compileExpression e) ctx
    inheritRequired ctx'
    csInheritUsed ctx'
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
          compilerErrorM $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
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
        | o == "^" && t1 == boolRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimBool PrimBool e1 o e2)
        | o == "-" && t1 == charRequiredValue = do
          return (Positional [intRequiredValue],glueInfix PrimChar PrimInt e1 o e2)
        | o `Set.member` equals && t1 == boolRequiredValue = do
          return (Positional [boolRequiredValue],glueInfix PrimBool PrimBool e1 o e2)
        | otherwise =
          compilerErrorM $ "Cannot " ++ show o ++ " " ++ show t1 ++ " and " ++
                                 show t2 ++ formatFullContextBrace c
      glueInfix t1 t2 e3 o2 e4 =
        UnboxedPrimitive t2 $ "(" ++ useAsUnboxed t1 e3 ++ ")" ++ o2 ++ "(" ++ useAsUnboxed t1 e4 ++ ")"
  transform e (TypeConversion c t) = do
    (Positional ts,e') <- e
    t' <- requireSingle c ts
    r <- csResolver
    fa <- csAllFilters
    let vt = ValueType (vtRequired t') t
    (lift $ checkValueAssignment r fa t' vt) <??
      "In explicit type conversion at " ++ formatFullContext c
    return (Positional [vt],e')
  transform e (ValueCall c f) = do
    (Positional ts,e') <- e
    t' <- requireSingle c ts
    f' <- lookupValueFunction t' f
    compileFunctionCall (Just $ useAsUnwrapped e') f' f
  transform e (SelectReturn c pos) = do
    (Positional ts,e') <- e
    when (not $ isOpaqueMulti e') $
      compilerErrorM $ "Return selection can only be used with function returns" ++ formatFullContextBrace c
    when (pos >= length ts) $
      compilerErrorM $ "Position " ++ show pos ++ " exceeds return count " ++ show (length ts) ++ formatFullContext c
    return (Positional [ts !! pos],WrappedSingle $ useAsReturns e' ++ ".At(" ++ show pos ++ ")")
  requireSingle _ [t] = return t
  requireSingle c2 ts =
    compilerErrorM $ "Function call requires 1 return but found but found {" ++
                            intercalate "," (map show ts) ++ "}" ++ formatFullContextBrace c2

lookupValueFunction :: (Ord c, Show c, CollectErrorsM m,
                        CompilerContext c m [String] a) =>
  ValueType -> FunctionCall c -> CompilerState a m (ScopedFunction c)
lookupValueFunction (ValueType WeakValue t) (FunctionCall c _ _ _) =
  compilerErrorM $ "Use strong to convert weak " ++ show t ++
                        " to optional first" ++ formatFullContextBrace c
lookupValueFunction (ValueType OptionalValue t) (FunctionCall c _ _ _) =
  compilerErrorM $ "Use require to convert optional " ++ show t ++
                        " to required first" ++ formatFullContextBrace c
lookupValueFunction (ValueType RequiredValue t) (FunctionCall c n _ _) =
  csGetTypeFunction c (Just t) n

compileExpressionStart :: (Ord c, Show c, CollectErrorsM m,
                           CompilerContext c m [String] a) =>
  ExpressionStart c -> CompilerState a m (ExpressionType,ExpressionValue)
compileExpressionStart (NamedVariable (OutputValue c n)) = do
  let var = UsedVariable c n
  (VariableValue _ s t _) <- csGetVariable var
  csCheckVariableInit [var]
  csAddUsed var
  scoped <- autoScope s
  let lazy = s == CategoryScope
  return (Positional [t],readStoredVariable lazy t (scoped ++ variableName n))
compileExpressionStart (NamedMacro c n) = do
  e <- csExprLookup c n
  csReserveExprMacro c n
  e' <- compileExpression e <?? "In expansion of " ++ show n ++ " at " ++ formatFullContext c
  -- NOTE: This will be skipped if expression compilation fails.
  csReleaseExprMacro c n
  return e'
compileExpressionStart (CategoryCall c t f@(FunctionCall _ n _ _)) = do
  f' <- csGetCategoryFunction c (Just t) n
  csAddRequired $ Set.fromList [t,sfType f']
  t' <- expandCategory t
  compileFunctionCall (Just t') f' f
compileExpressionStart (TypeCall c t f@(FunctionCall _ n _ _)) = do
  self <- autoSelfType
  t' <- lift $ replaceSelfInstance self (singleType t)
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstanceForCall r fa t' <?? "In function call at " ++ formatFullContext c
  f' <- csGetTypeFunction c (Just t') n
  when (sfScope f' /= TypeScope) $ compilerErrorM $ "Function " ++ show n ++
                                                    " cannot be used as a type function" ++
                                                    formatFullContextBrace c
  csAddRequired $ Set.unions $ map categoriesFromTypes [t']
  csAddRequired $ Set.fromList [sfType f']
  same <- maybeSingleType t' >>= checkSame
  t2 <- if same
           then return Nothing
           else fmap Just $ expandGeneralInstance t'
  compileFunctionCall t2 f' f
  where
    maybeSingleType = lift . tryCompilerM . matchOnlyLeaf
    checkSame (Just (JustTypeInstance t2)) = csSameType t2
    checkSame _ = return False
compileExpressionStart (UnqualifiedCall c f@(FunctionCall _ n _ _)) = do
  ctx <- get
  f' <- lift $ collectFirstM [tryCategory ctx,tryNonCategory ctx] <?? "In function call at " ++ formatFullContext c
  csAddRequired $ Set.fromList [sfType f']
  compileFunctionCall Nothing f' f
  where
    tryCategory ctx = ccGetCategoryFunction ctx c Nothing n
    tryNonCategory ctx = do
      f' <- ccGetTypeFunction ctx c Nothing n
      s <- ccCurrentScope ctx
      when (sfScope f' > s) $ compilerErrorM $
        "Function " ++ show n ++ " is not in scope here" ++ formatFullContextBrace c
      return f'
-- TODO: Compile BuiltinCall like regular functions, for consistent validation.
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinPresent ps es)) = do
  csAddRequired (Set.fromList [BuiltinBool])
  when (length (pValues ps) /= 0) $
    compilerErrorM $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    compilerErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    compilerErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  when (isWeakValue t0) $
    compilerErrorM $ "Weak values not allowed here" ++ formatFullContextBrace c
  return $ (Positional [boolRequiredValue],
            UnboxedPrimitive PrimBool $ "BoxedValue::Present(" ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinReduce ps es)) = do
  when (length (pValues ps) /= 2) $
    compilerErrorM $ "Expected 2 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    compilerErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    compilerErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  self <- autoSelfType
  ps' <- lift $ disallowInferred ps
  [t1,t2] <- lift $ mapCompilerM (replaceSelfInstance self) ps'
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r (Map.keysSet fa) t1
  lift $ validateGeneralInstance r (Map.keysSet fa) t2
  lift $ (checkValueAssignment r fa t0 (ValueType OptionalValue t1)) <??
    "In argument to reduce call at " ++ formatFullContext c
  -- TODO: If t1 -> t2 then just return e without a Reduce call.
  t1' <- expandGeneralInstance t1
  t2' <- expandGeneralInstance t2
  csAddRequired $ categoriesFromTypes t1
  csAddRequired $ categoriesFromTypes t2
  return $ (Positional [ValueType OptionalValue t2],
            UnwrappedSingle $ typeBase ++ "::Reduce(" ++ t1' ++ ", " ++ t2' ++ ", " ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinRequire ps es)) = do
  when (length (pValues ps) /= 0) $
    compilerErrorM $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    compilerErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    compilerErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  when (isWeakValue t0) $
    compilerErrorM $ "Weak values not allowed here" ++ formatFullContextBrace c
  return $ (Positional [ValueType RequiredValue (vtType t0)],
            UnwrappedSingle $ "BoxedValue::Require(" ++ useAsUnwrapped e ++ ")")
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinStrong ps es)) = do
  when (length (pValues ps) /= 0) $
    compilerErrorM $ "Expected 0 type parameters" ++ formatFullContextBrace c
  when (length (pValues es) /= 1) $
    compilerErrorM $ "Expected 1 argument" ++ formatFullContextBrace c
  es' <- sequence $ map compileExpression $ pValues es
  when (length (pValues $ fst $ head es') /= 1) $
    compilerErrorM $ "Expected single return in argument" ++ formatFullContextBrace c
  let (Positional [t0],e) = head es'
  let t1 = Positional [ValueType OptionalValue (vtType t0)]
  if isWeakValue t0
     -- Weak values are already unboxed.
     then return (t1,UnwrappedSingle $ "BoxedValue::Strong(" ++ useAsUnwrapped e ++ ")")
     else return (t1,e)
compileExpressionStart (BuiltinCall c (FunctionCall _ BuiltinTypename ps es)) = do
  when (length (pValues ps) /= 1) $
    compilerErrorM $ "Expected 1 type parameter" ++ formatFullContextBrace c
  when (length (pValues es) /= 0) $
    compilerErrorM $ "Expected 0 arguments" ++ formatFullContextBrace c
  self <- autoSelfType
  ps' <- lift $ disallowInferred ps
  [t] <- lift $ mapCompilerM (replaceSelfInstance self) ps'
  r <- csResolver
  fa <- csAllFilters
  lift $ validateGeneralInstance r (Map.keysSet fa) t
  t' <- expandGeneralInstance t
  csAddRequired $ Set.unions $ map categoriesFromTypes [t]
  return $ (Positional [formattedRequiredValue],
            valueAsWrapped $ UnboxedPrimitive PrimString $ typeBase ++ "::TypeName(" ++ t' ++ ")")
compileExpressionStart (BuiltinCall _ _) = undefined
compileExpressionStart (ParensExpression _ e) = compileExpression e
compileExpressionStart (InlineAssignment c n e) = do
  (VariableValue _ s t0 _) <- getWritableVariable c n
  (Positional [t],e') <- compileExpression e -- TODO: Get rid of the Positional matching here.
  r <- csResolver
  fa <- csAllFilters
  lift $ (checkValueAssignment r fa t t0) <??
    "In assignment at " ++ formatFullContext c
  csUpdateAssigned n
  scoped <- autoScope s
  let lazy = s == CategoryScope
  return (Positional [t0],readStoredVariable lazy t0 $ "(" ++ scoped ++ variableName n ++
                                                     " = " ++ writeStoredVariable t0 e' ++ ")")
compileExpressionStart (InitializeValue c t es) = do
  scope <- csCurrentScope
  t' <- case scope of
              CategoryScope -> case t of
                                    Nothing -> compilerErrorM $ "Param " ++ show ParamSelf ++ " not found"
                                    Just t0 -> return t0
              _ -> do
                self <- csSelfType
                case t of
                    Just t0 -> lift $ replaceSelfSingle (singleType $ JustTypeInstance self) t0
                    Nothing -> return self
  es' <- sequence $ map compileExpression $ pValues es
  (ts,es'') <- lift $ getValues es'
  csCheckValueInit c t' (Positional ts)
  params <- expandParams $ tiParams t'
  sameType <- csSameType t'
  s <- csCurrentScope
  let typeInstance = getType t' sameType s params
  args <- getArgs es''
  -- TODO: This is unsafe if used in a type or category constructor.
  return (Positional [ValueType RequiredValue $ singleType $ JustTypeInstance t'],
          UnwrappedSingle $ valueCreator (tiName t') ++ "(" ++ typeInstance ++ ", " ++ args ++ ")")
  where
    getType _  True ValueScope _      = "PARAM_SELF"
    getType _  True TypeScope  _      = "PARAM_SELF"
    getType t2 _    _          params = typeCreator (tiName t2) ++ "(" ++ params ++ ")"
    -- Single expression, but possibly multi-return.
    getValues [(Positional ts,e)] = return (ts,useAsArgs e)
    -- Multi-expression => must all be singles.
    getValues rs = do
      (mapCompilerM_ checkArity $ zip ([0..] :: [Int]) $ map fst rs) <??
        "In return at " ++ formatFullContext c
      return (map (head . pValues . fst) rs, intercalate ", " (map (useAsUnwrapped . snd) rs))
    checkArity (_,Positional [_]) = return ()
    checkArity (i,Positional ts)  =
      compilerErrorM $ "Initializer position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    getArgs argEs = do
      asNames <- lift $ collectArgNames $ pValues es
      canForward <- case asNames of
                         Just an -> csCanForward [] an
                         _       -> return False
      if canForward
         then return "params_args"
         else return $ "PassParamsArgs(" ++ argEs ++ ")"
compileExpressionStart (UnambiguousLiteral l) = compileValueLiteral l

compileValueLiteral :: (Ord c, Show c, CollectErrorsM m,
                           CompilerContext c m [String] a) =>
  ValueLiteral c -> CompilerState a m (ExpressionType,ExpressionValue)
compileValueLiteral (StringLiteral _ l) = do
  csAddRequired (Set.fromList [BuiltinString])
  return $ expressionFromLiteral PrimString (escapeChars l)
compileValueLiteral (CharLiteral _ l) = do
  csAddRequired (Set.fromList [BuiltinChar])
  return $ expressionFromLiteral PrimChar ("'" ++ escapeChar l ++ "'")
compileValueLiteral (IntegerLiteral c True l) = do
  csAddRequired (Set.fromList [BuiltinInt])
  when (l > 2^(64 :: Integer) - 1) $ compilerErrorM $
    "Literal " ++ show l ++ formatFullContextBrace c ++ " is greater than the max value for 64-bit unsigned"
  return $ expressionFromLiteral PrimInt (show l ++ "ULL")
compileValueLiteral (IntegerLiteral c False l) = do
  csAddRequired (Set.fromList [BuiltinInt])
  when (l > 2^(63 :: Integer) - 1) $ compilerErrorM $
    "Literal " ++ show l ++ formatFullContextBrace c ++ " is greater than the max value for 64-bit signed"
  when ((-l) > 2^(63 :: Integer)) $ compilerErrorM $
    "Literal " ++ show l ++ formatFullContextBrace c ++ " is less than the min value for 64-bit signed"
  -- NOTE: clang++ processes -abcLL as -(abcLL), which means that -(2^63)
  -- written out as a literal looks like an unsigned overflow. Using ULL here
  -- silences that warning.
  return $ expressionFromLiteral PrimInt (show l ++ "ULL")
compileValueLiteral (DecimalLiteral _ l e) = do
  csAddRequired (Set.fromList [BuiltinFloat])
  -- TODO: Check bounds.
  return $ expressionFromLiteral PrimFloat (show l ++ "E" ++ show e)
compileValueLiteral (BoolLiteral _ True) = do
  csAddRequired (Set.fromList [BuiltinBool])
  return $ expressionFromLiteral PrimBool "true"
compileValueLiteral (BoolLiteral _ False) = do
  csAddRequired (Set.fromList [BuiltinBool])
  return $ expressionFromLiteral PrimBool "false"
compileValueLiteral (EmptyLiteral _) = do
  return (Positional [emptyType],UnwrappedSingle "Var_empty")

disallowInferred :: (Ord c, Show c, CollectErrorsM m) => Positional (InstanceOrInferred c) -> m [GeneralInstance]
disallowInferred = mapCompilerM disallow . pValues where
  disallow (AssignedInstance _ t) = return t
  disallow (InferredInstance c) =
    compilerErrorM $ "Type inference is not allowed in reduce calls" ++ formatFullContextBrace c

compileFunctionCall :: (Ord c, Show c, CollectErrorsM m,
                        CompilerContext c m [String] a) =>
  Maybe String -> ScopedFunction c -> FunctionCall c ->
  CompilerState a m (ExpressionType,ExpressionValue)
compileFunctionCall e f (FunctionCall c _ ps es) = message ??> do
  r <- csResolver
  fa <- csAllFilters
  es' <- sequence $ map compileExpression $ pValues es
  (ts,es'') <- lift $ getValues es'
  f' <- lift $ parsedToFunctionType f
  let psActual = case ps of
                      (Positional []) -> Positional $ take (length $ pValues $ ftParams f') $ repeat (InferredInstance c)
                      _ -> ps
  self <- autoSelfType
  ps' <- lift $ fmap Positional $ mapCompilerM (replaceSelfParam self) $ pValues psActual
  ps2 <- lift $ guessParamsFromArgs r fa f ps' (Positional ts)
  f'' <- lift $ assignFunctionParams r fa Map.empty ps2 f'
  lift $ mapCompilerM_ backgroundMessage $ zip3 (map vpParam $ pValues $ sfParams f) (pValues ps') (pValues ps2)
  -- Called an extra time so arg count mismatches have reasonable errors.
  lift $ processPairs_ (\_ _ -> return ()) (ftArgs f'') (Positional ts)
  lift $ processPairs_ (checkArg r fa) (ftArgs f'') (Positional $ zip ([0..] :: [Int]) ts)
  csAddRequired $ Set.unions $ map categoriesFromTypes $ pValues ps2
  csAddRequired (Set.fromList [sfType f])
  params <- expandParams2 ps2
  scope <- csCurrentScope
  scoped <- autoScope (sfScope f)
  paramsArgs <- getParamsArgs (pValues ps2) params es''
  call <- assemble e scoped scope (sfScope f) paramsArgs
  return $ (ftReturns f'',OpaqueMulti call)
  where
    replaceSelfParam self (AssignedInstance c2 t) = do
      t' <- replaceSelfInstance self t
      return $ AssignedInstance c2 t'
    replaceSelfParam _ t = return t
    message = "In call to " ++ show (sfName f) ++ " at " ++ formatFullContext c
    backgroundMessage (n,(InferredInstance c2),t) =
      compilerBackgroundM $ "Parameter " ++ show n ++ " (from " ++ show (sfType f) ++ "." ++
        show (sfName f) ++ ") inferred as " ++ show t ++ " at " ++ formatFullContext c2
    backgroundMessage _ = return ()
    getParamsArgs ps2 paramEs argEs = do
      psNames <- lift $ collectParamNames ps2
      asNames <- lift $ collectArgNames $ pValues es
      canForward <- case (psNames,asNames) of
                         (Just pn,Just an) -> csCanForward pn an
                         _                 -> return False
      if canForward
         then return "params_args"
         else return $ "PassParamsArgs(" ++ intercalate ", " (paramEs ++ argEs) ++ ")"
    assemble Nothing _ ValueScope ValueScope paramsArgs =
      return $ callName (sfName f) ++ "(" ++ paramsArgs ++ ")"
    assemble Nothing _ TypeScope TypeScope paramsArgs =
      return $ callName (sfName f) ++ "(" ++ paramsArgs ++ ")"
    assemble Nothing scoped ValueScope TypeScope paramsArgs =
      return $ scoped ++ callName (sfName f) ++ "(" ++ paramsArgs ++ ")"
    assemble Nothing scoped _ _ paramsArgs =
      return $ scoped ++ callName (sfName f) ++ "(" ++ paramsArgs ++ ")"
    assemble (Just e2) _ _ ValueScope paramsArgs =
      return $ valueBase ++ "::Call(" ++ e2 ++ ", " ++ functionName f ++ ", " ++ paramsArgs ++ ")"
    assemble (Just e2) _ _ TypeScope paramsArgs =
      return $ typeBase ++ "::Call(" ++ e2 ++ ", " ++ functionName f ++ ", " ++ paramsArgs ++ ")"
    assemble (Just e2) _ _ _ paramsArgs =
      return $ e2 ++ ".Call(" ++ functionName f ++ ", " ++ paramsArgs ++ ")"
    -- TODO: Lots of duplication with assignments and initialization.
    -- Single expression, but possibly multi-return.
    getValues [(Positional ts,e2)] = return (ts,[useAsArgs e2])
    -- Multi-expression => must all be singles.
    getValues rs = do
      (mapCompilerM_ checkArity $ zip ([0..] :: [Int]) $ map fst rs) <??
        "In return at " ++ formatFullContext c
      return (map (head . pValues . fst) rs,map (useAsUnwrapped . snd) rs)
    checkArity (_,Positional [_]) = return ()
    checkArity (i,Positional ts)  =
      compilerErrorM $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    checkArg r fa t0 (i,t1) = do
      checkValueAssignment r fa t1 t0 <?? "In argument " ++ show i ++ " to " ++ show (sfName f)
    collectParamNames = fmap sequence . mapCompilerM collectParamName
    collectParamName = fmap getParamName . tryCompilerM . matchOnlyLeaf
    getParamName (Just (JustParamName _ n)) = Just n
    getParamName _ = Nothing

collectArgNames :: CollectErrorsM m => [Expression c] -> m (Maybe [VariableName])
collectArgNames = fmap sequence . mapCompilerM collectArgName where
  collectArgName (Expression _ (NamedVariable (OutputValue _ n)) []) = return $ Just n
  collectArgName _ = return Nothing

guessParamsFromArgs :: (Ord c, Show c, CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ScopedFunction c -> Positional (InstanceOrInferred c) ->
  Positional ValueType -> m (Positional GeneralInstance)
guessParamsFromArgs r fa f ps ts = do
  fm <- getFunctionFilterMap f
  args <- processPairs (\t1 t2 -> return $ TypePattern Covariant t1 t2) ts (fmap pvType $ sfArgs f)
  filts <- fmap concat $ processPairs (guessesFromFilters fm) ts (fmap pvType $ sfArgs f)
  pa <- fmap Map.fromList $ processPairs toInstance (fmap vpParam $ sfParams f) ps
  gs <- inferParamTypes r fa pa (args ++ filts)
  gs' <- mergeInferredTypes r fa fm pa gs
  let pa3 = gs' `Map.union` pa
  fmap Positional $ mapCompilerM (subPosition pa3) (pValues $ sfParams f) where
    subPosition pa2 p =
      case (vpParam p) `Map.lookup` pa2 of
           Just t  -> return t
           Nothing -> compilerErrorM $ "Something went wrong inferring " ++
                      show (vpParam p) ++ formatFullContextBrace (vpContext p)
    toInstance p1 (AssignedInstance _ t) = return (p1,t)
    toInstance p1 (InferredInstance _)   = return (p1,singleType $ JustInferredType p1)

guessParams :: (Ord c, Show c, CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> Positional ValueType -> Positional ParamName ->
  Positional (InstanceOrInferred c) -> Positional ValueType -> m (Positional GeneralInstance)
guessParams r fa args params ps ts = do
  args' <- processPairs (\t1 t2 -> return $ TypePattern Covariant t1 t2) ts args
  pa <- fmap Map.fromList $ processPairs toInstance params ps
  gs <- inferParamTypes r fa pa args'
  gs' <- mergeInferredTypes r fa (Map.fromList $ zip (pValues params) (repeat [])) pa gs
  let pa3 = gs' `Map.union` pa
  fmap Positional $ mapCompilerM (subPosition pa3) (pValues params) where
    subPosition pa2 p =
      case p `Map.lookup` pa2 of
           Just t  -> return t
           Nothing -> compilerErrorM $ "Something went wrong inferring " ++ show p
    toInstance p1 (AssignedInstance _ t) = return (p1,t)
    toInstance p1 (InferredInstance _)   = return (p1,singleType $ JustInferredType p1)

compileMainProcedure :: (Ord c, Show c, CollectErrorsM m) =>
  CategoryMap c -> ExprMap c -> Expression c -> m (CompiledData [String])
compileMainProcedure tm em e = do
  ctx <- getMainContext tm em
  runDataCompiler compiler ctx where
    procedure = Procedure [] [IgnoreValues [] e]
    compiler = do
      ctx0 <- getCleanContext
      compileProcedure ctx0 procedure >>= put

compileTestProcedure :: (Ord c, Show c, CollectErrorsM m) =>
  CategoryMap c -> ExprMap c -> TestProcedure c -> m (CompiledData [String])
compileTestProcedure tm em (TestProcedure c n cov p) = do
  ctx <- getMainContext tm em
  p' <- runDataCompiler compiler ctx <??
    "In unittest " ++ show n ++ formatFullContextBrace c
  return $ mconcat [
      onlyCode $ "ReturnTuple " ++ testFunctionName n ++ "() {",
      indentCompiled handleCoverage,
      indentCompiled $ onlyCode $ startTestTracing n,
      indentCompiled p',
      indentCompiled $ onlyCode $ "return ReturnTuple();",
      onlyCode "}"
    ] where
    compiler = do
      ctx0 <- getCleanContext
      compileProcedure ctx0 p >>= put
    handleCoverage
      | cov       = onlyCode "LogCalls::DisableCallLogging();"
      | otherwise = emptyCode

selectTestFromArgv1 :: CollectErrorsM m => [FunctionName] -> m ([String],CompiledData [String])
selectTestFromArgv1 fs = return (includes,allCode) where
  allCode = mconcat [
      initMap,
      selectFromMap
    ]
  initMap = onlyCodes $ [
      "const std::unordered_map<std::string, ReturnTuple(*)()> tests{"
    ] ++ map (("  " ++) . testEntry) fs ++ [
      "};"
    ]
  selectFromMap = onlyCodes [
      "if (argc < 2) FAIL() << argv[0] << \" [unittest name]\";",
      "const auto name = argv[1];",
      "const auto test = tests.find(name);",
      "if (test != tests.end()) {",
      "  (void) (*test->second)();",
      " } else {",
      "  FAIL() << argv[0] << \": unittest \" << name << \" does not exist\";",
      "}"
    ]
  testEntry f = "{ \"" ++ show f ++ "\", &" ++ testFunctionName f ++ " },"
  includes = [
      "#include <string>",
      "#include <unordered_map>"
    ]

autoScope :: CompilerContext c m s a =>
  SymbolScope -> CompilerState a m String
autoScope s = do
  s1 <- csCurrentScope
  return $ scoped s1 s
  where
    scoped ValueScope TypeScope     = "parent->"
    scoped ValueScope CategoryScope = "parent->parent."
    scoped TypeScope  CategoryScope = "parent."
    -- NOTE: Don't use this->; otherwise, self won't work properly.
    scoped _ _ = ""

categoriesFromTypes :: GeneralInstance -> Set.Set CategoryName
categoriesFromTypes = reduceMergeTree Set.unions Set.unions getAll where
  getAll (JustTypeInstance (TypeInstance t ps)) =
    t `Set.insert` (Set.unions $ map categoriesFromTypes $ pValues ps)
  getAll _ = Set.empty

categoriesFromRefine :: TypeInstance -> Set.Set CategoryName
categoriesFromRefine (TypeInstance t ps) = t `Set.insert` (Set.unions $ map categoriesFromTypes $ pValues ps)

categoriesFromDefine :: DefinesInstance -> Set.Set CategoryName
categoriesFromDefine (DefinesInstance t ps) = t `Set.insert` (Set.unions $ map categoriesFromTypes $ pValues ps)

expandParams :: (CollectErrorsM m, CompilerContext c m s a) =>
  Positional GeneralInstance -> CompilerState a m String
expandParams ps = do
  ps' <- sequence $ map expandGeneralInstance $ pValues ps
  return $ "T_get(" ++ intercalate ", " ps' ++ ")"

expandParams2 :: (CollectErrorsM m, CompilerContext c m s a) =>
  Positional GeneralInstance -> CompilerState a m [String]
expandParams2 ps = sequence $ map expandGeneralInstance $ pValues ps

expandCategory :: CompilerContext c m s a =>
  CategoryName -> CompilerState a m String
expandCategory t = return $ categoryGetter t ++ "()"

expandGeneralInstance :: (CollectErrorsM m, CompilerContext c m s a) =>
  GeneralInstance -> CompilerState a m String
expandGeneralInstance t = do
  r <- csResolver
  f <- csAllFilters
  scope <- csCurrentScope
  t' <- lift $ dedupGeneralInstance r f t
  t'' <-  case scope of
               CategoryScope -> return t'
               _ -> do
                 self <- csSelfType
                 lift $ reverseSelfInstance self t'
  expand t'' where
    expand t2
      | t2 == minBound = return $ allGetter ++ "()"
      | t2 == maxBound = return $ anyGetter ++ "()"
      | otherwise = reduceMergeTree getAny getAll getSingle t2
    getAny ts = combine ts >>= return . (unionGetter ++)
    getAll ts = combine ts >>= return . (intersectGetter ++)
    getSingle (JustTypeInstance (TypeInstance t2 ps)) = do
      ps' <- sequence $ map expand $ pValues ps
      let count = length ps'
      return $ typeGetter t2 ++ "(Params<" ++ show count ++ ">::Type(" ++ intercalate "," ps' ++ "))"
    getSingle (JustParamName _ ParamSelf) = return "S<const TypeInstance>(PARAM_SELF)"
    getSingle (JustParamName _ p)  = do
      s <- csGetParamScope p
      scoped <- autoScope s
      return $ scoped ++ paramName p
    getSingle (JustInferredType p) = getSingle (JustParamName False p)
    combine ps = do
      ps' <- sequence ps
      return $ "(L_get<S<const " ++ typeBase ++ ">>(" ++ intercalate "," ps' ++ "))"

doImplicitReturn :: (CollectErrorsM m, Ord c, Show c, CompilerContext c m [String] a) =>
  [c] -> CompilerState a m ()
doImplicitReturn c = do
  named <- csIsNamedReturns
  csRegisterReturn c Nothing
  get >>= autoInsertCleanup c JumpReturn
  if not named
     then csWrite ["return ReturnTuple();"]
     else do
       getPrimNamedReturns
       csWrite ["return returns;"]
  where

autoPositionalCleanup :: (Ord c,Eq c,Show c,CollectErrorsM m, CompilerContext c m [String] a) =>
  [c] -> ExpressionValue -> CompilerState a m ()
autoPositionalCleanup c e = do
  named <- csIsNamedReturns
  (CleanupBlock ss _ _ _ _) <- csGetCleanup JumpReturn
  if null ss
     then do
       csSetJumpType c JumpReturn
       csWrite ["return " ++ useAsReturns e ++ ";"]
     else do
       if named
          then do
            csWrite ["returns.TransposeFrom(" ++ useAsReturns e ++ ");"]
            setPrimNamedReturns
            get >>= autoInsertCleanup c JumpReturn
            csWrite ["return returns;"]
          else do
            csWrite ["{","ReturnTuple returns = " ++ useAsReturns e ++ ";"]
            get >>= autoInsertCleanup c JumpReturn
            csWrite ["return returns;","}"]

setPrimNamedReturns ::  (CollectErrorsM m, CompilerContext c m [String] a) =>
  CompilerState a m ()
setPrimNamedReturns = do
  vars <- csPrimNamedReturns
  sequence_ $ map (csWrite . (:[]) . assign) vars where
    assign (ReturnVariable i n t) =
      variableName n ++ " = " ++ writeStoredVariable t (position i) ++ ";"
    position i = WrappedSingle $ "returns.At(" ++ show i ++ ")"

getPrimNamedReturns ::  (CollectErrorsM m, CompilerContext c m [String] a) =>
  CompilerState a m ()
getPrimNamedReturns = do
  vars <- csPrimNamedReturns
  sequence_ $ map (csWrite . (:[]) . assign) vars where
    assign (ReturnVariable i n t) =
      "returns.At(" ++ show i ++ ") = " ++ useAsUnwrapped (readStoredVariable False t $ variableName n) ++ ";"

autoInsertCleanup :: (Ord c, Show c, CollectErrorsM m, CompilerContext c m [String] a) =>
  [c] -> JumpType -> a -> CompilerState a m ()
autoInsertCleanup c j ctx = do
  (CleanupBlock ss ds vs jump req) <- lift $ ccGetCleanup ctx j
  csCheckVariableInit (nub vs) <??
    "In inlining of cleanup block after statement at " ++ formatFullContext c
  let vs2 = map (\(UsedVariable c0 v) -> UsedVariable (c ++ c0) v) vs
  csInheritDeferred ds
  -- This is needed in case a cleanup is inlined within another cleanup, e.g.,
  -- e.g., if the latter has a break statement.
  mapM_ csAddUsed vs2
  csWrite ss
  csAddRequired req
  csSetJumpType c jump

inheritRequired :: (CollectErrorsM m, CompilerContext c m [String] a) =>
  a -> CompilerState a m ()
inheritRequired ctx = do
  lift (ccGetRequired ctx) >>= csAddRequired
  lift (ccGetTraces ctx) >>= sequence . map csAddTrace >> return ()

autoInlineOutput :: (Ord c, Show c, CollectErrorsM m, CompilerContext c m [String] a) =>
  a -> CompilerState a m ()
autoInlineOutput ctx = do
  inheritRequired ctx
  getAndIndentOutput ctx >>= csWrite
  csInheritStatic [ctx]

getAndIndentOutput :: (Ord c, Show c, CollectErrorsM m, CompilerContext c m [String] a) =>
  a -> CompilerState a m [String]
getAndIndentOutput ctx = fmap indentCode (lift $ ccGetOutput ctx)

indentCode :: [String] -> [String]
indentCode = map ("  " ++)
