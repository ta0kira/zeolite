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
import Control.Monad.State (get)
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
paramName p = "Param_" ++ tail (pnName p) -- Remove leading '`'.

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
      ma <- mapMembers ms
      mergeAllM [
          return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ " : public " ++ categoryBase ++ " {",
          fmap indentCompiled $ categoryConstructor tm t d ms,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure t' tm filters fa ma) ps,
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
      ma <- mapMembers ms
      mergeAllM [
          return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
          fmap indentCompiled $ typeConstructor tm t d ms,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure t' tm filters fa ma) ps,
          return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
          fmap indentCompiled $ mergeAllM $ map createParam $ map (jpnName . stType) $ psParams $ tiParams t',
          fmap indentCompiled $ mergeAllM $ map createMember ms,
          return $ onlyCode "}"
        ]
    typeConstructor tm t d ms = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName (getCategoryName t) ++ "& p"
      let argsPassed = map (\i -> paramType ++ " a" ++ show i) [0..(length ps)-1]
      let allArgs = intercalate ", " $ argParent:argsPassed
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(a" ++ show i ++ ")") $ zip [0..] ps
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
      ma <- mapMembers ms
      mergeAllM [
          return $ onlyCode $ "struct " ++ valueName (getCategoryName t) ++ " : public " ++ valueBase ++ " {",
          fmap indentCompiled $ valueConstructor tm t d ms,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure t' tm filters fa ma) ps,
          return $ indentCompiled $ onlyCode $ typeName (getCategoryName t) ++ "& parent;",
          fmap indentCompiled $ mergeAllM $ map createMember ms,
          return $ onlyCode "}"
        ]
    valueConstructor tm t d ms = do
      let argParent = typeName (getCategoryName t) ++ "& p"
      let argsPassed = map (\i -> "const " ++ variableType ++ "& a" ++ show i) [0..(length ms)-1]
      let allArgs = intercalate ", " $ argParent:argsPassed
      let initParent = "parent(p)"
      let initPassed = map (\(i,m) -> variableName (dmName m) ++ "(a" ++ show i ++ ")") $ zip [0..] ms
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ valueName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    createMember m = return $ onlyCode $ variableType ++ " " ++ variableName (dmName m) ++ ";"
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName p ++ ";"
    initMember _   (DefinedMember _ _ _ _ Nothing) = return mergeDefault
    initMember ctx (DefinedMember c _ _ n (Just e)) = do
        let assign = Assignment c (ParamSet [ExistingVariable (InputValue c n)]) e
        runDataCompiler (compileStatement assign) ctx
    dispatchInitName = "initDispatcher"
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
  let t' = TypeInstance (getCategoryName t)
                        (ParamSet $ map (SingleType . JustParamName . vpParam) $ getCategoryParams t)
  let pa = if s == CategoryScope
              then Map.empty
              else getCategoryFilterMap t
  let sa = Map.map (const TypeScope) pa
  let r = categoriesToTypeResolver tm
  fa <- setInternalFunctions r t (dcFunctions d)
  return $ ProcedureContext {
      pcScope = s,
      pcType = t',
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
  TypeInstance -> CategoryMap c -> ParamFilters ->
  Map.Map FunctionName (ScopedFunction c) ->
  Map.Map VariableName (VariableValue c) ->
  (ScopedFunction c,ExecutableProcedure c) -> m (CompiledData [String])
compileExecutableProcedure t tm pa fa va
                 (ff@(ScopedFunction _ _ _ s as1 rs1 _ _ _),
                  (ExecutableProcedure _ _ as2 rs2 p)) = do
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
      pcCategories = tm,
      pcFilters = pa',
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = va'',
      pcReturns = rs',
      pcRequiredTypes = Set.empty,
      pcOutput = []
    }
  output <- runDataCompiler (compileProcedure p) ctx
  return $ wrapProcedure (tiName t) ff as2 rs2 output
  where
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
        (\(i,p) -> paramType ++ " " ++ paramName (vpParam p) ++ " = std::get<" ++ show i ++ ">(params);")
      nameArgs = flip map (zip [0..] $ filter (not . isDiscardedInput) $ psParams $ avNames as) $
        (\(i,n) -> proxyType ++ " " ++ variableName (ivName n) ++ " = std::get<" ++ show i ++ ">(args);")
      nameReturns
        | isUnnamedReturns rs = []
        | otherwise = flip map (zip [0..] $ psParams $ nrNames rs) $
        (\(i,n) -> proxyType ++ " " ++ variableName (ovName n) ++ " = std::get<" ++ show i ++ ">(returns);")

-- Returns the state so that returns can be properly checked for if/elif/else.
compileProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  Procedure c -> CompilerState a m a
compileProcedure (Procedure c ss) = do
  sequence $ map compileStatement ss
  get

compileStatement :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                     CompilerContext c m [String] a) =>
  Statement c -> CompilerState a m ()
compileStatement (EmptyReturn c) = do
  csCheckReturn c (ParamSet [])
  csWrite ["return returns;"]
compileStatement (ExplicitReturn c es) = do
  es' <- sequence $ map compileExpression $ psParams es
  getReturn es'
  where
    -- Single expression, but possibly multi-return.
    getReturn [(ParamSet ts,e)] = do
      csCheckReturn c (ParamSet ts)
      csWrite ["return " ++ e ++ ";"]
    -- Multi-expression => must all be singles.
    getReturn rs = do
      lift $ mergeAllM $ map checkArity $ zip [1..] $ map fst rs
      csCheckReturn c $ ParamSet $ map (head . psParams . fst) rs
      csWrite $ map bindReturn $ zip [0..] $ map snd rs
      csWrite ["return returns;"]
    checkArity (_,ParamSet [_]) = return ()
    checkArity (i,ParamSet ts)  =
      compileError $ "Return position " ++ show i ++ " has " ++ show (length ts) ++ " values but should have 1"
    bindReturn (i,e) = "std::get<" ++ show i ++ ">(returns) = " ++ "std::get<0>(" ++ e ++ ");"
compileStatement (LoopBreak c) = do
  -- TODO: This can only be used inside of a loop.
  csWrite ["break;"]
compileStatement (Assignment c as e) = do
  (ts,e') <- compileExpression e
  r <- csResolver
  fa <- csAllFilters
  processParamPairsT (createVariable r fa) as ts
  csWrite ["{","auto r = " ++ e' ++ ";"]
  sequence $ map assignVariable $ zip [0..] $ psParams as
  csWrite ["}"]
  where
    createVariable r fa (CreateVariable c t1 n) t2 = do
      lift $ checkValueTypeMatch r fa t2 t1
      csAddVariable c n (VariableValue c LocalScope t1)
      csWrite [variableType ++ " " ++ show n ++ ";"]
    createVariable r fa (ExistingVariable (InputValue c n)) t2 = do
      (VariableValue _ s1 t1) <- csGetVariable c n
      -- TODO: Also show original context.
      lift $ checkValueTypeMatch r fa t2 t1
    createVariable _ _ _ _ = return ()
    assignVariable (i,CreateVariable _ _ n) =
      csWrite [variableName n ++ " = std::get<" ++ show i ++ ">(r);"]
    assignVariable (i,ExistingVariable (InputValue _ n)) =
      csWrite [variableName n ++ " = std::get<" ++ show i ++ ">(r);"]
    assignVariable _ = return ()
compileStatement (NoValueExpression v) = compileVoidExpression v

compileExpression :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                      CompilerContext c m [String] a) =>
  Expression c -> CompilerState a m (ExpressionType,String)
compileExpression = compile . rewrite where
  compile (Expression c s os) = lift $ do
    compileErrorM "undefined"
  compile (UnaryExpression c o e) = lift $ do
    compileErrorM "undefined"
  compile (InitializeValue c t vs) = lift $ do
    compileErrorM "undefined"
  rewrite s = s

compileVoidExpression :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                         CompilerContext c m [String] a) =>
  VoidExpression c -> CompilerState a m ()
compileVoidExpression (Conditional ie) = lift $ do
  compileErrorM "undefined"
compileVoidExpression (Loop l) = lift $ do
  compileErrorM "undefined"
compileVoidExpression (WithScope s) = compileScopedBlock s

compileScopedBlock :: (Show c, Monad m, CompileErrorM m, MergeableM m,
                       CompilerContext c m [String] a) =>
  ScopedBlock c -> CompilerState a m ()
compileScopedBlock s = do
  let (vs,p) = rewriteScoped s
  sequence $ map createVariable vs
  csWrite ["{"]
  compileProcedure p
  csWrite ["}"]
  where
    createVariable (c,t,n) = do
      -- TODO: Call csRequiresTypes for t. (Maybe needs a helper function.)
      csAddVariable c n (VariableValue c LocalScope t)
      csWrite [variableType ++ " " ++ show n ++ ";"]
    -- Merge chained scoped sections into a single section.
    rewriteScoped w@(ScopedBlock c (Procedure c2 ss1)
                                 (NoValueExpression (WithScope
                                 (ScopedBlock _ (Procedure _ ss2) s)))) =
      rewriteScoped $ ScopedBlock c (Procedure c2 $ ss1 ++ ss2) s
    -- Gather to-be-created variables.
    rewriteScoped (ScopedBlock c (Procedure c2 ss) (Assignment c3 vs e)) =
      (created,Procedure c2 $ ss ++ [Assignment c3 (ParamSet existing) e]) where
        (created,existing) = foldr update ([],[]) (psParams vs)
        update (CreateVariable c t n) (cs,es) = ((c,t,n):cs,(ExistingVariable $ InputValue c n):es)
        update e (cs,es) = (cs,e:es)
    -- Merge the statement into the scoped block.
    rewriteScoped (ScopedBlock c (Procedure c2 ss) s) =
      ([],Procedure c2 $ ss ++ [s])
