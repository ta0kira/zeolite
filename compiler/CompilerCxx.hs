{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerCxx (
  CxxOutput(..),
  compileCategoryDeclaration,
  compileCategoryDefinition,
  compileInterfaceDefinition,
) where

import Control.Monad (when)
import Control.Monad.Trans.State (execStateT,get,put)
import Control.Monad.Trans (lift)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CategoryCompiler
import CompilerState
import DefinedCategory
import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


data CxxOutput =
  CxxOutput {
    coFilename :: String,
    coOutput :: [String]
  }
  deriving (Show) -- TODO: Remove this.

headerFilename :: TypeName -> String
headerFilename n = "Category_" ++ show n ++ ".hxx"

sourceFilename :: TypeName -> String
sourceFilename n = "Category_" ++ show n ++ ".cxx"

baseHeaderIncludes :: [String]
baseHeaderIncludes = ["#include \"category-header.hxx\""]

baseSourceIncludes :: [String]
baseSourceIncludes = ["#include \"category-source.hxx\""]

categoryBase :: String
categoryBase = "TypeCategory"

typeBase :: String
typeBase = "TypeInstance"

valueBase :: String
valueBase = "TypeValue"

paramType :: String
paramType = typeBase ++ "&"

variableType :: String
variableType = "S<" ++ valueBase ++ ">"

proxyType :: String
proxyType = variableType ++ "&"

paramName :: ParamName -> String
paramName p = "Param_" ++ tail (pnName p) -- Remove leading '#'.

variableName :: VariableName -> String
variableName v = "Var_" ++ show v

initializerName :: VariableName -> String
initializerName v = "Init_" ++ show v

categoryName :: TypeName -> String
categoryName n = "Category_" ++ show n

typeName :: TypeName -> String
typeName n = "Type_" ++ show n

valueName :: TypeName -> String
valueName n = "Value_" ++ show n

callName :: ScopedFunction c -> String
callName f = "Call_" ++ show (sfType f) ++ "_" ++ show (sfName f)

functionName :: ScopedFunction c -> String
functionName f = "Function_" ++ show (sfType f) ++ "_" ++ show (sfName f)

functionLabelType :: ScopedFunction c -> String
functionLabelType f =
  "Function<" ++ scope ++ "," ++ show pn ++ "," ++ show an ++ "," ++ show rn ++ ">" where
    pn = length $ psParams $ sfParams f
    an = length $ psParams $ sfArgs f
    rn = length $ psParams $ sfReturns f
    scope
      | sfScope f == CategoryScope = "CategoryScope"
      | sfScope f == TypeScope     = "TypeScope"
      | sfScope f == ValueScope    = "ValueScope"

createLabelForFunction :: ScopedFunction c -> String
createLabelForFunction f = "const " ++ functionLabelType f ++ "& " ++ functionName f ++
                           " = *new " ++ functionLabelType f ++ "(\"" ++
                           show (sfType f) ++ "\", \"" ++ show (sfName f) ++ "\");"

indentCompiled :: CompiledData [String] -> CompiledData [String]
indentCompiled (CompiledData r o) = CompiledData r $ map ("  " ++) o

onlyCode :: String -> CompiledData [String]
onlyCode = onlyCodes . (:[])

onlyCodes :: [String] -> CompiledData [String]
onlyCodes = CompiledData Set.empty

compileCategoryDeclaration :: Monad m => CategoryMap c -> AnyCategory c -> m CxxOutput
compileCategoryDeclaration _ t =
  return $ CxxOutput (headerFilename name) $ guardTop ++ content ++ guardBottom where
    name = getCategoryName t
    guardTop = ["#ifndef " ++ guardName,"#define " ++ guardName]
    guardBottom = ["#endif"]
    guardName = "HEADER_" ++ show name
    content = baseHeaderIncludes ++ labels
    labels = map label $ filter ((== name) . sfType) $ getCategoryFunctions t
    label f = "extern const " ++ functionLabelType f ++ "& " ++ functionName f ++ ";"

compileInterfaceDefinition :: Monad m => CategoryMap c -> AnyCategory c -> m CxxOutput
compileInterfaceDefinition _ t =
  return $ CxxOutput (sourceFilename name) $ content where
    name = getCategoryName t
    content = baseSourceIncludes ++ headers ++ labels
    headers = ["#include \"" ++ headerFilename name ++ "\""]
    labels = map createLabelForFunction $ filter ((== name) . sfType) $ getCategoryFunctions t

compileCategoryDefinition :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> DefinedCategory c -> m CxxOutput
compileCategoryDefinition tm dd@(DefinedCategory c n _ ps fs) = do
  let filename = sourceFilename n
  (_,t) <- getConcreteCategory tm (c,n)
  let filters = getCategoryFilterMap t
  let r = categoriesToTypeResolver tm
  fa <- setInternalFunctions r t fs
  pa <- pairProceduresToFunctions fa ps
  let (cp,tp,vp) = partitionByScope (sfScope . fst) pa
  (CompiledData req out) <- mergeAllM [
      return $ CompiledData (Set.fromList [n]) [],
      createLabels n (Map.elems fa),
      namespaceStart,
      declareDispatchInit,
      compileCategory tm t dd filters fa cp,
      compileType     tm t dd filters fa tp,
      compileValue    tm t dd filters fa vp,
      defineDispatchInit n (Map.elems fa),
      namespaceEnd
    ]
  let includes = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $ Set.toList req
  return $ CxxOutput filename (baseSourceIncludes ++ includes ++ out)
  where
    -- NOTE: This is always ValueScope for initializer checks.
    mv = filter ((== ValueScope) . dmScope) $ dcMembers dd
    namespaceStart = return $ onlyCode $ "namespace {"
    namespaceEnd   = return $ onlyCode $ "}"
    typeInstance ps =
      TypeInstance n $ ParamSet $ map (SingleType . JustParamName . vpParam) ps
    createLabels n = return . onlyCodes . map createLabelForFunction . filter ((== n) . sfType)
    createLabelForFunction f = "const " ++ functionLabelType f ++ "& " ++ functionName f ++
                               " = *new " ++ functionLabelType f ++ "(\"" ++
                               show (sfType f) ++ "\", \"" ++ show (sfName f) ++ "\");" where
    compileCategory tm t d filters fa ps = do
      let t' = typeInstance $ getCategoryParams t
      let ms = filter ((== CategoryScope) . dmScope) $ dcMembers d
      let pv = ParamSet $ map vpParam $ getCategoryParams t
      ma <- mapMembers ms
      let ma0 = Map.filter ((\s -> s == LocalScope || s == CategoryScope) . vvScope) $ builtinVariables t'
      let ma' = Map.union ma0 ma
      mergeAllM [
          return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ " : public " ++ categoryBase ++ " {",
          fmap indentCompiled $ categoryConstructor tm t d ms,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure tm pv mv filters fa ma') ps,
          fmap indentCompiled $ mergeAllM $ map createMember ms,
          return $ indentCompiled $ onlyCode $ "Dispatcher dispatcher;",
          return $ onlyCode "}"
        ]
    categoryConstructor tm t d ms = do
      let dispatcher = "dispatcher(" ++ dispatchInitName ++ "())"
      ctx <- getContextForInit tm t d CategoryScope
      mergeAllM [
          return $ onlyCode $ categoryName (getCategoryName t) ++ "() : " ++ dispatcher ++ " {",
          fmap indentCompiled $ mergeAllM $ map (initMember ctx) ms,
          return $ onlyCode "}"
        ]
    compileType tm t d filters fa ps = do
      let t' = typeInstance $ getCategoryParams t
      let ms = filter ((== TypeScope) . dmScope) $ dcMembers d
      let pv = ParamSet $ map vpParam $ getCategoryParams t
      ma <- mapMembers ms
      let ma0 = Map.filter ((\s -> s == LocalScope || s == TypeScope) . vvScope) $ builtinVariables t'
      let ma' = Map.union ma0 ma
      mergeAllM [
          return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
          fmap indentCompiled $ typeConstructor tm t d ms,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure tm pv mv filters fa ma') ps,
          return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
          fmap indentCompiled $ mergeAllM $ map createParam $ map (jpnName . stType) $ psParams $ tiParams t',
          fmap indentCompiled $ mergeAllM $ map createMember ms,
          return $ onlyCode "}"
        ]
    typeConstructor tm t d ms = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName (getCategoryName t) ++ "& p"
      let argsPassed = "Params<" ++ show (length ps) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(*std::get<" ++ show i ++ ">(params))") $ zip [0..] ps
      let allInit = intercalate ", " $ initParent:initPassed
      ctx <- getContextForInit tm t d TypeScope
      mergeAllM [
          return $ onlyCode $ typeName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {",
          fmap indentCompiled $ mergeAllM $ map (initMember ctx) ms,
          return $ onlyCode "}"
        ]
    compileValue tm t d filters fa ps = do
      let t' = typeInstance $ getCategoryParams t
      let ms = filter ((== ValueScope) . dmScope) $ dcMembers d
      let pv = ParamSet $ map vpParam $ getCategoryParams t
      ma <- mapMembers ms
      let ma0 = Map.filter ((\s -> s == LocalScope || s == ValueScope) . vvScope) $ builtinVariables t'
      let ma' = Map.union ma0 ma
      mergeAllM [
          return $ onlyCode $ "struct " ++ valueName (getCategoryName t) ++ " : public " ++ valueBase ++ " {",
          fmap indentCompiled $ valueConstructor tm t d ms,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure tm pv mv filters fa ma') ps,
          return $ indentCompiled $ onlyCode $ typeName (getCategoryName t) ++ "& parent;",
          fmap indentCompiled $ mergeAllM $ map createMember ms,
          return $ onlyCode "}"
        ]
    valueConstructor tm t d ms = do
      let argParent = typeName (getCategoryName t) ++ "& p"
      let argsPassed = "Args<" ++ show (length ms) ++ ">::Type args"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,m) -> variableName (dmName m) ++ "(std::get<" ++ show i ++ ">(args))") $ zip [0..] ms
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ valueName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    createMember m = return $ onlyCode $ variableType ++ " " ++ variableName (dmName m) ++ ";"
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName p ++ ";"
    initMember _   (DefinedMember _ _ _ _ Nothing) = return mergeDefault
    initMember ctx (DefinedMember c _ _ n (Just e)) = do
        let assign = Assignment c (ParamSet [ExistingVariable (InputValue c n)]) e
        runDataCompiler (compileStatement assign) ctx
    dispatchInitName = "init_dispatcher"
    dispatchInit = "Dispatcher " ++ dispatchInitName ++ "()"
    declareDispatchInit = return $ onlyCode $ dispatchInit ++ ";"
    defineDispatchInit n fs = return $ mergeAll [
        onlyCode $ dispatchInit ++ " {",
        indentCompiled $ onlyCode "Dispatcher d;",
        -- TODO: This might contain duplicates.
        indentCompiled $ mergeAll $ map dispatch $ expand fs,
        indentCompiled $ onlyCode "return d;",
        onlyCode "}"
      ] where
        expand = concat . map (\f -> f:(expand $ sfMerges f))
        -- NOTE: The first argument can come from another type. The second must
        -- be from this type.
        dispatch f = CompiledData (Set.fromList [sfType f])
                                  ["d.register(" ++ functionName f ++ ", &" ++ function f ++ ");"]
        function f
          | sfScope f == CategoryScope = categoryName n ++ "::" ++ callName f
          | sfScope f == TypeScope     = typeName n     ++ "::" ++ callName f
          | sfScope f == ValueScope    = valueName n    ++ "::" ++ callName f

getContextForInit :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> AnyCategory c -> DefinedCategory c -> SymbolScope -> m (ProcedureContext c)
getContextForInit tm t d s = do
  let ps = ParamSet $ map vpParam $ getCategoryParams t
  -- NOTE: This is always ValueScope for initializer checks.
  let ms = filter ((== ValueScope) . dmScope) $ dcMembers d
  let pa = if s == CategoryScope
              then Map.empty
              else getCategoryFilterMap t
  let sa = Map.map (const TypeScope) pa
  let r = categoriesToTypeResolver tm
  fa <- setInternalFunctions r t (dcFunctions d)
  return $ ProcedureContext {
      pcScope = s,
      pcType = getCategoryName t,
      pcParams = ps,
      pcMembers = ms,
      pcCategories = tm,
      pcFilters = pa,
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = Map.empty,
      pcReturns = NoValidation,
      pcRequiredTypes = Set.empty,
      pcOutput = []
    }

compileExecutableProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> ParamSet ParamName -> [DefinedMember c] -> ParamFilters ->
  Map.Map FunctionName (ScopedFunction c) ->
  Map.Map VariableName (VariableValue c) ->
  (ScopedFunction c,ExecutableProcedure c) -> m (CompiledData [String])
compileExecutableProcedure tm ps ms pa fa va
                 (ff@(ScopedFunction _ _ t s as1 rs1 _ _ _),
                  (ExecutableProcedure _ c n as2 rs2 p)) = do
  rs' <- if isUnnamedReturns rs2
            then return $ ValidatePositions rs1
            else fmap (ValidateNames . Map.fromList) $ processParamPairs pairOutput rs1 (nrNames rs2)
  va' <- updateArgVariables va as1 as2
  va'' <- updateReturnVariables va' rs1 rs2
  let localFilters = getFunctionFilterMap ff
  let pa' = if s == CategoryScope
               then localFilters
               else Map.union pa localFilters
  let typeScopes = Map.map (const TypeScope) pa
  let localScopes = Map.map (const LocalScope) localFilters
  let sa = if s == CategoryScope
              then localScopes
              else Map.union typeScopes localScopes
  let ctx = ProcedureContext {
      pcScope = s,
      pcType = t,
      pcParams = ps,
      pcMembers = ms,
      pcCategories = tm,
      pcFilters = pa',
      pcParamScopes = sa,
      pcFunctions = fa, -- TODO: Add builtin functions.
      pcVariables = va'',
      pcReturns = rs',
      pcRequiredTypes = Set.empty,
      pcOutput = []
    }
  output <- runDataCompiler compileWithReturn ctx
  return $ wrapProcedure t ff as2 rs2 output
  where
    compileWithReturn = do
      ctx0 <- get
      compileProcedure ctx0 p >>= put
      csRegisterReturn c (ParamSet []) `reviseErrorStateT`
        ("In implicit return from " ++ show n ++ " [" ++ formatFullContext c ++ "]")
      csWrite ["return returns;"]
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)
    wrapProcedure n f as rs output =
      mergeAll $ [
          onlyCode header,
          indentCompiled $ defineReturns,
          indentCompiled $ onlyCodes nameParams,
          indentCompiled $ onlyCodes nameArgs,
          indentCompiled $ onlyCodes nameReturns,
          indentCompiled output,
          onlyCode close
        ] where
      close = "}"
      name = callName f
      header
        | sfScope f == ValueScope =
          returnType ++ " " ++ name ++ "(Value self, " ++
          "Params<" ++ show (length $ psParams $ sfParams f) ++ ">::Type params, " ++
          "Args<" ++ show (length $ psParams $ sfArgs f) ++ ">::Type args) {"
        | otherwise =
          returnType ++ " " ++ name ++ "(" ++
          "Params<" ++ show (length $ psParams $ sfParams f) ++ ">::Type params, " ++
          "Args<" ++ show (length $ psParams $ sfArgs f) ++ ">::Type args) {"
      returnType = "Returns<" ++ show (length $ psParams $ sfReturns f) ++ ">::Type"
      defineReturns = onlyCode $ returnType ++ " returns;"
      nameParams = flip map (zip [0..] $ psParams $ sfParams f) $
        (\(i,p) -> paramType ++ " " ++ paramName (vpParam p) ++ " = *std::get<" ++ show i ++ ">(params);")
      nameArgs = flip map (zip [0..] $ filter (not . isDiscardedInput) $ psParams $ avNames as) $
        (\(i,n) -> proxyType ++ " " ++ variableName (ivName n) ++ " = std::get<" ++ show i ++ ">(args);")
      nameReturns
        | isUnnamedReturns rs = []
        | otherwise = flip map (zip [0..] $ psParams $ nrNames rs) $
        (\(i,n) -> proxyType ++ " " ++ variableName (ovName n) ++ " = std::get<" ++ show i ++ ">(returns);")

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
  getReturn es'
  where
    -- Single expression, but possibly multi-return.
    getReturn [(ParamSet ts,e)] = do
      csRegisterReturn c (ParamSet ts)
      csWrite ["return " ++ e ++ ";"]
    -- Multi-expression => must all be singles.
    getReturn rs = do
      lift $ mergeAllM (map checkArity $ zip [1..] $ map fst rs) `reviseError`
        ("In return at " ++ formatFullContext c)
      csRegisterReturn c $ ParamSet $ map (head . psParams . fst) rs
      csWrite $ map bindReturn $ zip [0..] $ map snd rs
      csWrite ["return returns;"]
    checkArity (_,ParamSet [_]) = return ()
    checkArity (i,ParamSet ts)  =
      compileError $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    bindReturn (i,e) = "std::get<" ++ show i ++ ">(returns) = " ++ "std::get<0>(" ++ e ++ ");"
compileStatement (LoopBreak c) = do
  -- TODO: This can only be used inside of a loop.
  csWrite ["break;"]
compileStatement (IgnoreValues c e) = do
  (_,e') <- compileExpression e
  csWrite [e' ++ ";"]
compileStatement (Assignment c as e) = do
  (ts,e') <- compileExpression e
  r <- csResolver
  fa <- csAllFilters
  processParamPairsT (createVariable r fa) as ts `reviseErrorStateT`
    ("In assignment at " ++ formatFullContext c)
  csWrite ["{","auto r = " ++ e' ++ ";"]
  sequence $ map assignVariable $ zip [0..] $ psParams as
  csWrite ["}"]
  where
    createVariable r fa (CreateVariable c t1 n) t2 = do
      -- TODO: Call csRequiresTypes for t1. (Maybe needs a helper function.)
      lift $ mergeAllM [validateGeneralInstance r fa (vtType t1),
                        checkValueTypeMatch r fa t2 t1] `reviseError`
        ("In variable assignment at " ++ formatFullContext c)
      csAddVariable c n (VariableValue c LocalScope t1)
      csWrite [variableType ++ " " ++ variableName n ++ ";"]
    createVariable r fa (ExistingVariable (InputValue c n)) t2 = do
      (VariableValue _ s1 t1) <- csGetVariable c n
      -- TODO: Also show original context.
      lift $ (checkValueTypeMatch r fa t2 t1) `reviseError`
        ("In variable assignment at " ++ formatFullContext c)
      csUpdateAssigned n
    createVariable _ _ _ _ = return ()
    assignVariable (i,CreateVariable _ _ n) =
      csWrite [variableName n ++ " = std::get<" ++ show i ++ ">(r);"]
    assignVariable (i,ExistingVariable (InputValue _ n)) = do
      (VariableValue _ s _) <- csGetVariable c n
      scoped <- autoScope s
      csWrite [scoped ++ variableName n ++ " = std::get<" ++ show i ++ ">(r);"]
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
  (ts,e') <- compileExpression e
  lift $ checkCondition ts `reviseError` ("In condition at " ++ formatFullContext c)
  ctx0 <- get
  ctx <- compileProcedure ctx0 p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite ["if (" ++ e' ++ ") {"]
  (lift $ ccGetOutput ctx) >>= csWrite
  csWrite ["}"]
  cs <- unwind es
  csInheritReturns (ctx:cs)
  where
    unwind (IfStatement c e p es) = do
      (ts,e') <- compileExpression e
      lift $ checkCondition ts `reviseError` ("In condition at " ++ formatFullContext c)
      ctx0 <- get
      ctx <- compileProcedure ctx0 p
      (lift $ ccGetRequired ctx) >>= csRequiresTypes
      csWrite ["else if (" ++ e' ++ ") {"]
      (lift $ ccGetOutput ctx) >>= csWrite
      csWrite ["}"]
      cs <- unwind es
      return $ ctx:cs
    unwind (ElseStatement c p) = do
      ctx0 <- get
      ctx <- compileProcedure ctx0 p
      (lift $ ccGetRequired ctx) >>= csRequiresTypes
      csWrite ["else {"]
      (lift $ ccGetOutput ctx) >>= csWrite
      csWrite ["}"]
      return [ctx]
    unwind TerminateConditional = fmap (:[]) get
    checkCondition (ParamSet [t]) = return () -- TODO: Make sure ts is [Bool].
    checkCondition _ = compileError "Conditionals must have exactly one Bool return"

compileWhileLoop :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  WhileLoop c -> CompilerState a m ()
compileWhileLoop (WhileLoop c e p) = do
  (ts,e') <- compileExpression e
  lift $ checkCondition ts `reviseError` ("In condition at " ++ formatFullContext c)
  ctx0 <- get
  ctx <- compileProcedure ctx0 p
  (lift $ ccGetRequired ctx) >>= csRequiresTypes
  csWrite ["while (" ++ e' ++ ") {"]
  (lift $ ccGetOutput ctx) >>= csWrite
  csWrite ["}"]
  where
    -- TODO: Maybe make this a helper, or use a special type of Expression.
    checkCondition (ParamSet [t]) = return () -- TODO: Make sure ts is [Bool].
    checkCondition _ = compileError "Conditionals must have exactly one Bool return"

compileScopedBlock :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                       CompilerContext c m [String] a) =>
  ScopedBlock c -> CompilerState a m ()
compileScopedBlock s = do
  let (vs,p,st) = rewriteScoped s
  -- Capture context so we can discard scoped variable names.
  ctx0 <- get
  sequence $ map createVariable vs
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
    createVariable (c,t,n) = csWrite [variableType ++ " " ++ variableName n ++ ";"]
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
compileExpression = compile where -- TODO: Rewrite for operator precedence?
  compile (Expression c s os) = do
    foldr transform (compileExpressionStart s) os
  compile (UnaryExpression c o e) = do
    lift $ compileError $ "UnaryExpression " ++ formatFullContext c
  compile (InitializeValue c t es) = do
    es' <- sequence $ map compileExpression $ psParams es
    (ts,es'') <- getValues es'
    ms <- csGetValueInit c t
    r <- csResolver
    fa <- csAllFilters
    lift $ processParamPairs (checkInit r fa) ms (ParamSet $ zip [1..] ts) `reviseError`
      ("In initialization at " ++ formatFullContext c)
    params <- expandParams $ tiParams t
    return (ParamSet [ValueType RequiredValue $ SingleType $ JustTypeInstance t],
            -- TODO: This needs a constant.
            "internal_instance(" ++ params ++ ").create(" ++ es'' ++ ")")
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
      checkInit r fa (MemberValue c n t0) (i,t1) = do
        checkValueTypeMatch r fa t1 t0 `reviseError`
          ("In initializer " ++ show i ++ " for " ++ show n ++ " [" ++ formatFullContext c ++ "]")
  transform (ConvertedCall c t f) e = do
    (ParamSet [t'],e') <- e -- TODO: Get rid of the ParamSet matching here.
    r <- csResolver
    fa <- csAllFilters
    let vt = ValueType RequiredValue $ SingleType $ JustTypeInstance t
    lift $ (checkValueTypeMatch r fa vt t') `reviseError`
      ("In conversion at " ++ formatFullContext c)
    f' <- lookupFunction (Just t') f
    (t2,e2) <- compileFunctionCall f' f
    return (t2,"std::get<0>(" ++ e' ++ ")->" ++ e2)
  transform (ValueCall c f) e = do
    (ParamSet [t'],e') <- e -- TODO: Get rid of the ParamSet matching here.
    f' <- lookupFunction (Just t') f
    (t2,e2) <- compileFunctionCall f' f
    return (t2,"std::get<0>(" ++ e' ++ ")->" ++ e2)
  transform (BinaryOperation c s e2) e = do
    lift $ compileError $ "BinaryOperation " ++ formatFullContext c

compileExpressionStart :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                           CompilerContext c m [String] a) =>
  ExpressionStart c -> CompilerState a m (ExpressionType,String)
compileExpressionStart (NamedVariable (OutputValue c n)) = do
  (VariableValue _ s t) <- csGetVariable c n
  scoped <- autoScope s
  return (ParamSet [t],"T_get(" ++ scoped ++ variableName n ++ ")")
compileExpressionStart (TypeCall c t f) = do
  f' <- lookupFunction (Just $ ValueType RequiredValue $ SingleType t) f
  compileFunctionCall f' f
compileExpressionStart (UnqualifiedCall c f) = do
  f' <- lookupFunction Nothing f
  compileFunctionCall f' f
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
  return (ParamSet [t0],"T_get(" ++ scoped ++ variableName n ++ " = " ++ e' ++ ")")

lookupFunction :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  Maybe ValueType -> FunctionCall c -> CompilerState a m (ScopedFunction c)
lookupFunction Nothing (FunctionCall c n ps as) = csGetFunction c n Nothing
lookupFunction (Just t) (FunctionCall c n ps as)
  | vtRequired t /= RequiredValue =
    lift $ compileError $ "Cannot call function " ++ show n ++ " on value of type " ++ show t
  | otherwise = do
    let t' = vtType t
    csGetFunction c n (Just t')

compileFunctionCall :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                        CompilerContext c m [String] a) =>
  ScopedFunction c -> FunctionCall c -> CompilerState a m (ExpressionType,String)
compileFunctionCall f (FunctionCall c _ ps es) = do
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
  -- TODO: Also include param values.
  csRequiresTypes (Set.fromList [sfType f])
  scoped <- autoScope $ sfScope f
  params <- expandParams ps
  return $ (ftReturns f'',scoped ++ "call(" ++ functionName f ++ ", " ++ params ++ ", " ++ es'' ++ ")")
  where
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
    scoped s1 s2
      | s1 == s2 = "this->"
      | otherwise = ""

expandParams :: (Monad m, CompilerContext c m s a) =>
  ParamSet GeneralInstance -> CompilerState a m String
expandParams ps = do
  ps' <- sequence $ map expandType $ psParams ps
  return $ "T_get(" ++ intercalate "," (map ("&" ++) ps') ++ ")"
  where

expandType :: (Monad m, CompilerContext c m s a) =>
  GeneralInstance -> CompilerState a m String
expandType (TypeMerge MergeUnion ps) = do
  ps' <- sequence $ map expandType ps
  -- TODO: This needs a helper for reuse.
  return $ "Merge_Union(L_get(" ++ intercalate "," ps' ++ "))"
expandType (TypeMerge MergeIntersect ps) = do
  ps' <- sequence $ map expandType ps
  -- TODO: This needs a helper for reuse.
  return $ "Merge_Intersect(L_get(" ++ intercalate "," ps' ++ "))"
expandType (SingleType (JustTypeInstance (TypeInstance t ps))) = do
  ps' <- sequence $ map expandType $ psParams ps
  -- TODO: This needs a helper for reuse.
  return $ "Instance_" ++ show t ++ "(" ++ intercalate "," ps' ++ ")"
expandType (SingleType (JustParamName p)) = do
  s <- csGetParamScope p
  scoped <- autoScope s
  return $ scoped ++ paramName p

builtinVariables :: TypeInstance -> Map.Map VariableName (VariableValue c)
builtinVariables t = Map.fromList [
    (VariableName "self",VariableValue [] ValueScope (ValueType RequiredValue $ SingleType $ JustTypeInstance t)),
    (VariableName "empty",VariableValue [] LocalScope (ValueType OptionalValue $ TypeMerge MergeUnion []))
  ]
