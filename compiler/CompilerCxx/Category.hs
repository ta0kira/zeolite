{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module CompilerCxx.Category (
  CxxOutput(..),
  compileCategoryDeclaration,
  compileCategoryDefinition,
  compileInterfaceDefinition,
) where

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
import CompilerCxx.Procedure


data CxxOutput =
  CxxOutput {
    coFilename :: String,
    coOutput :: [String]
  }
  deriving (Show) -- TODO: Remove this.


createLabelForFunction :: ScopedFunction c -> String
createLabelForFunction f = "const " ++ functionLabelType f ++ "& " ++ functionName f ++
                           " = *new " ++ functionLabelType f ++ "(\"" ++
                           show (sfType f) ++ "\", \"" ++ show (sfName f) ++ "\");"

compileCategoryDeclaration :: Monad m => CategoryMap c -> AnyCategory c -> m CxxOutput
compileCategoryDeclaration _ t =
  return $ CxxOutput (headerFilename name) $ guardTop ++ content ++ guardBottom where
    name = getCategoryName t
    guardTop = ["#ifndef " ++ guardName,"#define " ++ guardName]
    guardBottom = ["#endif"]
    guardName = "HEADER_" ++ show name
    content = baseHeaderIncludes ++ labels ++ getCategory ++ getType
    labels = map label $ filter ((== name) . sfType) $ getCategoryFunctions t
    label f = "extern const " ++ functionLabelType f ++ "& " ++ functionName f ++ ";"
    getCategory = [categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "();"]
    getType = [typeBase ++ "& " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
               show (length $getCategoryParams t) ++ ">::Type params);"]

compileInterfaceDefinition :: Monad m => CategoryMap c -> AnyCategory c -> m CxxOutput
compileInterfaceDefinition _ t =
  return $ CxxOutput (sourceFilename name) $ content where
    name = getCategoryName t
    content = baseSourceIncludes ++ headers ++ labels
    headers = ["#include \"" ++ headerFilename name ++ "\""]
    labels = map createLabelForFunction $ filter ((== name) . sfType) $ getCategoryFunctions t

defineCategoryGetter :: AnyCategory c -> [String]
defineCategoryGetter t = [
    categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "() {",
    "  return " ++ internalCategory (getCategoryName t) ++ "();",
    "}"
  ]

defineInternalGetter :: AnyCategory c -> [String]
defineInternalGetter t = [
    internal ++ "& " ++ internalCategory (getCategoryName t) ++ "() {",
    "  static " ++ internal ++ "& category = *new " ++ internal ++ "();",
    "  return category;",
    "}"
  ]
  where
    internal = categoryName (getCategoryName t)

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
  let np = length $ getCategoryParams t
  let nv = length $ filter ((== ValueScope) . dmScope) $ dcMembers dd
  (CompiledData req out) <- mergeAllM [
      return $ CompiledData (Set.fromList [n]) [],
      createLabels n (Map.elems fa),
      namespaceStart,
      declareTypes,
      declareDispatchInit,
      declareCreateType np,
      declareCreateValue nv,
      compileCategory tm t dd Map.empty fa cp,
      compileType     tm t dd filters   fa tp,
      compileValue    tm t dd filters   fa vp,
      defineDispatchInit n (Map.elems fa),
      defineCreateType np,
      defineCreateValue nv,
      namespaceEnd,
      return $ onlyCodes $ defineCategoryGetter t
    ]
  let includes = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $ Set.toList req
  return $ CxxOutput filename (baseSourceIncludes ++ includes ++ out)
  -- TODO: Split all of this up.
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
                               show (sfType f) ++ "\", \"" ++ show (sfName f) ++ "\");"
    declareTypes =
      return $ onlyCodes $ map (\f -> "class " ++ f n ++ ";") [categoryName,typeName,valueName]
    getName = return $ onlyCode $ "std::string CategoryName() const { return \"" ++ show n ++ "\"; }"
    compileCategory tm t d filters fa ps = do
      let t' = typeInstance $ getCategoryParams t
      let ms = filter ((== CategoryScope) . dmScope) $ dcMembers d
      let pv = ParamSet $ map vpParam $ getCategoryParams t
      ma <- mapMembers ms
      let ma0 = Map.filter ((\s -> s == LocalScope || s == CategoryScope) . vvScope) $ builtinVariables t'
      let ma' = Map.union ma0 ma
      let r = categoriesToTypeResolver tm
      mergeAllM [
          return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ " : public " ++ categoryBase ++ " {",
          fmap indentCompiled $ categoryConstructor tm t d ms,
          fmap indentCompiled $ getName,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure tm pv mv filters fa ma') ps,
          fmap indentCompiled $ mergeAllM $ map (createMember r filters) ms,
          return $ indentCompiled $ onlyCode $ dispatcherType ++ " " ++ dispatcherName ++ ";",
          return $ onlyCode "};",
          return $ onlyCodes $ defineInternalGetter t
        ]
    categoryConstructor tm t d ms = do
      let dispatcher = dispatcherName ++ "(" ++ dispatchInitName ++ "())"
      ctx <- getContextForInit tm t d CategoryScope
      mergeAllM [
          return $ onlyCode $ categoryName (getCategoryName t) ++ "() : " ++ dispatcher ++ " {",
          fmap indentCompiled $ mergeAllM $ map (initMember ctx) ms,
          return $ onlyCode "}"
        ]
    compileType tm t d filters fa ps = do
      let t' = typeInstance $ getCategoryParams t
      let ms = filter ((== TypeScope) . dmScope) $ dcMembers d
      let nv = length $ filter ((== ValueScope) . dmScope) $ dcMembers d
      let pv = ParamSet $ map vpParam $ getCategoryParams t
      ma <- mapMembers ms
      let ma0 = Map.filter ((\s -> s == LocalScope || s == TypeScope) . vvScope) $ builtinVariables t'
      let ma' = Map.union ma0 ma
      let r = categoriesToTypeResolver tm
      mergeAllM [
          return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
          fmap indentCompiled $ typeConstructor tm t d ms,
          fmap indentCompiled $ getName,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure tm pv mv filters fa ma') ps,
          return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
          fmap indentCompiled $ mergeAllM $ map createParam $ map (jpnName . stType) $ psParams $ tiParams t',
          fmap indentCompiled $ mergeAllM $ map (createMember r filters) ms,
          return $ onlyCode "};"
        ]
    declareCreateType np =
      return $ onlyCode $ typeName n ++ "& " ++ typeCreator ++
                          "(Params<" ++ show np ++ ">::Type params);"
    defineCreateType np =
      return $ onlyCode $ typeName n ++ "& " ++ typeCreator ++
                          "(Params<" ++ show np ++ ">::Type params) { /*???*/ }"
    declareCreateValue nv =
      return $ onlyCode $ "S<TypeValue> " ++ valueCreator ++
                          "(" ++ typeName n ++ "& parent, Args<" ++ show nv ++ ">::Type args);"
    defineCreateValue nv =
      mergeAllM [
          return $ onlyCode $ "S<TypeValue> " ++ valueCreator ++
                              "(" ++ typeName n ++ "& parent, Args<" ++ show nv ++ ">::Type args) {",
          return $ onlyCode $ "  return S_get(new " ++ valueName n ++ "(parent, args));",
          return $ onlyCode $ "}"
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
      let r = categoriesToTypeResolver tm
      mergeAllM [
          return $ onlyCode $ "struct " ++ valueName (getCategoryName t) ++ " : public " ++ valueBase ++ " {",
          fmap indentCompiled $ valueConstructor tm t d ms,
          fmap indentCompiled $ getName,
          fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure tm pv mv filters fa ma') ps,
          return $ indentCompiled $ onlyCode $ typeName (getCategoryName t) ++ "& parent;",
          fmap indentCompiled $ mergeAllM $ map (createMember r filters) ms,
          return $ onlyCode "};"
        ]
    valueConstructor tm t d ms = do
      let argParent = typeName (getCategoryName t) ++ "& p"
      let argsPassed = "Args<" ++ show (length ms) ++ ">::Type args"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,m) -> variableName (dmName m) ++ "(std::get<" ++ show i ++ ">(args))") $ zip [0..] ms
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ valueName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    createMember r fa m = do
      validateGeneralInstance r fa (vtType $ dmType m) `reviseError`
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
      return $ onlyCode $ variableType (vtRequired $ dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName p ++ ";"
    initMember _   (DefinedMember _ _ _ _ Nothing) = return mergeDefault
    initMember ctx (DefinedMember c _ _ n (Just e)) = do
        let assign = Assignment c (ParamSet [ExistingVariable (InputValue c n)]) e
        runDataCompiler (compileStatement assign) ctx
    dispatchInitName = "InitDispatcher"
    dispatcherName = "dispatcher_"
    dispatcherType = "Dispatcher<" ++ categoryName n ++ "," ++ typeName n ++ "," ++ valueName n ++ ">"
    dispatchInit = dispatcherType ++ " " ++ dispatchInitName ++ "()"
    declareDispatchInit = return $ onlyCode $ dispatchInit ++ ";"
    defineDispatchInit n fs = return $ mergeAll [
        onlyCode $ dispatchInit ++ " {",
        indentCompiled $ onlyCode $ dispatcherType ++ " d;",
        -- TODO: This might contain duplicates.
        indentCompiled $ mergeAll $ map dispatch $ expand fs,
        indentCompiled $ onlyCode "return d;",
        onlyCode "}"
      ] where
        expand = concat . map (\f -> f:(expand $ sfMerges f))
        -- NOTE: The first argument can come from another type. The second must
        -- be from this type.
        dispatch f = CompiledData (Set.fromList [sfType f])
                                  ["d.Register(" ++ functionName f ++ ", &" ++ function f ++ ");"]
        function f
          | sfScope f == CategoryScope = categoryName n ++ "::" ++ callName n (sfName f)
          | sfScope f == TypeScope     = typeName n     ++ "::" ++ callName n (sfName f)
          | sfScope f == ValueScope    = valueName n    ++ "::" ++ callName n (sfName f)

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

builtinVariables :: TypeInstance -> Map.Map VariableName (VariableValue c)
builtinVariables t = Map.fromList [
    (VariableName "self",VariableValue [] ValueScope (ValueType RequiredValue $ SingleType $ JustTypeInstance t)),
    (VariableName "empty",VariableValue [] LocalScope (ValueType OptionalValue $ TypeMerge MergeUnion [])),
    (VariableName "true",VariableValue [] LocalScope boolRequiredValue),
    (VariableName "false",VariableValue [] LocalScope boolRequiredValue)
  ]
