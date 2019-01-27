{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module CompilerCxx.Category (
  CxxOutput(..),
  compileCategoryDeclaration,
  compileConcreteDefinition,
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
    getCategory = declareGetCategory t
    getType = declareGetType t

compileInterfaceDefinition :: (MergeableM m, Monad m) => AnyCategory c -> m CxxOutput
compileInterfaceDefinition t = do
  top <- return emptyCode
  bottom <- return emptyCode
  ce <- return emptyCode
  te <- typeConstructor
  commonDefineAll t emptyCode emptyCode emptyCode te
  where
    typeConstructor = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName (getCategoryName t) ++ "& p"
      let argsPassed = "Params<" ++ show (length ps) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(*std::get<" ++ show i ++ ">(params))") $ zip [0..] ps
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ typeName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"

compileConcreteDefinition :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> DefinedCategory c -> m CxxOutput
compileConcreteDefinition ta dd@(DefinedCategory c n ms ps fs) = do
  (_,t) <- getConcreteCategory ta (c,n)
  let params = ParamSet $ map vpParam $ getCategoryParams t
  let typeInstance = TypeInstance n $ fmap (SingleType . JustParamName) params
  let filters = getCategoryFilterMap t
  let r = categoriesToTypeResolver ta
  fa <- setInternalFunctions r t fs
  -- Functions explicitly declared externally.
  let externalFuncs = Set.fromList $ map sfName $ filter ((== n) . sfType) $ getCategoryFunctions t
  -- Functions explicitly declared internally.
  let overrideFuncs = Map.fromList $ map (\f -> (sfName f,f)) fs
  -- Functions only declared internally.
  let internalFuncs = Map.filter (not . (`Set.member` externalFuncs) . sfName) overrideFuncs
  pa <- pairProceduresToFunctions fa ps
  let (cp,tp,vp) = partitionByScope (sfScope . fst) pa
  let (cm,tm,vm) = partitionByScope dmScope ms
  let cm0 = builtins typeInstance CategoryScope
  let tm0 = builtins typeInstance TypeScope
  let vm0 = builtins typeInstance ValueScope
  cm' <- mapMembers cm
  tm' <- mapMembers tm
  vm' <- mapMembers vm
  let cv = Map.union cm0 cm'
  let tv = Map.union tm0 tm'
  let vv = Map.union vm0 vm'
  let memberCount = length vm
  top <- mergeAllM [
      return $ onlyCodes $ map createLabelForFunction (Map.elems internalFuncs),
      return $ onlyCode $ "class " ++ valueName n ++ ";",
      declareDispatchInit,
      declareInternalValue n memberCount
    ]
  defineValue <- mergeAllM [
      return $ onlyCode $ "struct " ++ valueName n ++ " : public " ++ valueBase ++ " {",
      fmap indentCompiled $ valueConstructor ta t vm,
      fmap indentCompiled $ valueDispatch,
      return $ indentCompiled $ defineCategoryName n,
      fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure ta params vm filters fa vv) vp,
      fmap indentCompiled $ mergeAllM $ map (createMember r filters) vm,
      return $ indentCompiled $ onlyCode $ typeName n ++ "& parent;",
      return $ onlyCode "};"
    ]
  bottom <- mergeAllM [
      return $ defineValue,
      defineDispatchInit n (Map.elems fa),
      defineInternalValue n memberCount
    ]
  ce <- mergeAllM [
      categoryConstructor ta t cm,
      categoryDispatch,
      mergeAllM $ map (compileExecutableProcedure ta params vm filters fa cv) cp,
      mergeAllM $ map (createMember r filters) cm,
      return $ onlyCode $ dispatcherType ++ " " ++ dispatcherName ++ ";"
    ]
  te <- mergeAllM [
      typeConstructor ta t tm,
      typeDispatch,
      mergeAllM $ map (compileExecutableProcedure ta params vm filters fa tv) tp,
      mergeAllM $ map (createMember r filters) tm
    ]
  commonDefineAll t top bottom ce te
  where
    builtins t s0 = Map.filter ((<= s0) . vvScope) $ builtinVariables t
    categoryConstructor tm t ms = do
      let dispatcher = dispatcherName ++ "(" ++ dispatchInitName ++ "())"
      ctx <- getContextForInit tm t dd CategoryScope
      mergeAllM [
          return $ onlyCode $ categoryName n ++ "() : " ++ dispatcher ++ " {",
          return $ indentCompiled $ onlyCode $ "TRACE_FUNCTION(\"" ++ show n ++ " (init @category)\")",
          fmap indentCompiled $ mergeAllM $ map (initMember ctx) ms,
          return $ onlyCode "}"
        ]
    typeConstructor tm t ms = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName n ++ "& p"
      let argsPassed = "Params<" ++ show (length ps) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(*std::get<" ++ show i ++ ">(params))") $ zip [0..] ps
      let allInit = intercalate ", " $ initParent:initPassed
      ctx <- getContextForInit tm t dd TypeScope
      mergeAllM [
          return $ onlyCode $ typeName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {",
          return $ indentCompiled $ onlyCode $ "TRACE_FUNCTION(\"" ++ show n ++ " (init @type)\")",
          fmap indentCompiled $ mergeAllM $ map (initMember ctx) ms,
          return $ onlyCode "}"
        ]
    valueConstructor tm t ms = do
      let argParent = typeName n ++ "& p"
      let argsPassed = "Args<" ++ show (length ms) ++ ">::Type args"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,m) -> variableName (dmName m) ++ "(std::get<" ++ show i ++ ">(args))") $ zip [0..] ms
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ valueName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    createMember r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) `reviseError`
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
      return $ onlyCode $ variableType (vtRequired $ dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName p ++ ";"
    initMember _   (DefinedMember _ _ _ _ Nothing) = return mergeDefault
    initMember ctx (DefinedMember c _ _ n (Just e)) = do
        let assign = Assignment c (ParamSet [ExistingVariable (InputValue c n)]) e
        runDataCompiler (compileStatement assign) ctx
    categoryDispatch =
      return $ onlyCodes $ [
          "DReturns Dispatch(" ++
          "const DFunction<SymbolScope::CategoryScope>& label, " ++
          "DParams params, " ++
          "DArgs args) final {",
          "  return dispatcher_.Dispatch(*this, label, params, args);",
          "}"
        ]
    typeDispatch =
      return $ onlyCodes $ [
          "DReturns Dispatch(" ++
          "const DFunction<SymbolScope::TypeScope>& label, " ++
          "DParams params, " ++
          "DArgs args) final {",
          "  return parent.dispatcher_.Dispatch(*this, label, params, args);",
          "}"
        ]
    valueDispatch =
      return $ onlyCodes $ [
          "DReturns Dispatch(" ++
          "const S<TypeValue>& self, " ++
          "const DFunction<SymbolScope::ValueScope>& label, " ++
          "DParams params," ++
          "DArgs args) final {",
          "  return parent.parent.dispatcher_.Dispatch(*this, self, label, params, args);",
          "}"
        ]
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

commonDefineAll :: (MergeableM m, Monad m) =>
  AnyCategory c -> CompiledData [String] -> CompiledData [String] ->
  CompiledData [String] -> CompiledData [String] -> m CxxOutput
commonDefineAll t top bottom ce te = do
  let filename = sourceFilename name
  (CompiledData req out) <- mergeAllM [
      return $ CompiledData (Set.fromList [name]) [],
      createLabels (getCategoryFunctions t),
      return $ onlyCode $ "namespace {",
      declareTypes,
      declareInternalType name paramCount,
      return top,
      commonDefineCategory t ce,
      return $ onlyCodes $ defineInternalCategory t,
      commonDefineType t te,
      defineInternalType name paramCount,
      return bottom,
      return $ onlyCode $ "}",
      return $ onlyCodes $ defineGetCatetory t,
      return $ onlyCodes $ defineGetType t
    ]
  let includes = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $ Set.toList req
  return $ CxxOutput filename (baseSourceIncludes ++ includes ++ out)
  where
    declareTypes =
      return $ onlyCodes $ map (\f -> "class " ++ f name ++ ";") [categoryName,typeName]
    paramCount = length $ getCategoryParams t
    name = getCategoryName t
    createLabels = return . onlyCodes . map createLabelForFunction . filter ((== name) . sfType)

createLabelForFunction :: ScopedFunction c -> String
createLabelForFunction f = "const " ++ functionLabelType f ++ "& " ++ functionName f ++
                           " = *new " ++ functionLabelType f ++ "(\"" ++
                           show (sfType f) ++ "\", \"" ++ show (sfName f) ++ "\");"

commonDefineCategory :: (MergeableM m, Monad m) =>
  AnyCategory c -> CompiledData [String] -> m (CompiledData [String])
commonDefineCategory t extra = do
  mergeAllM $ [
      return $ onlyCode $ "struct " ++ categoryName name ++ " : public " ++ categoryBase ++ " {",
      return $ indentCompiled $ defineCategoryName name,
      return $ indentCompiled extra,
      return $ onlyCode "};"
    ]
  where
    name = getCategoryName t

commonDefineType :: (MergeableM m, Monad m) =>
  AnyCategory c -> CompiledData [String] -> m (CompiledData [String])
commonDefineType t extra = do
  mergeAllM [
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
      return $ indentCompiled $ defineCategoryName name,
      return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
      fmap indentCompiled $ createParams,
      return $ indentCompiled extra,
      return $ onlyCode "};"
    ]
  where
    name = getCategoryName t
    createParams = mergeAllM $ map createParam $ getCategoryParams t
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"

defineCategoryName :: CategoryName -> CompiledData [String]
defineCategoryName t = onlyCode $ "std::string CategoryName() const { return \"" ++ show t ++ "\"; }"

declareGetCategory :: AnyCategory c -> [String]
declareGetCategory t = [categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "();"]

defineGetCatetory :: AnyCategory c -> [String]
defineGetCatetory t = [
    categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "() {",
    "  return " ++ categoryCreator ++ "();",
    "}"
  ]

declareGetType :: AnyCategory c -> [String]
declareGetType t = [typeBase ++ "& " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $getCategoryParams t) ++ ">::Type params);"]

defineGetType :: AnyCategory c -> [String]
defineGetType t = [
    typeBase ++ "& " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $getCategoryParams t) ++ ">::Type params) {",
    "  return " ++ typeCreator ++ "(params);",
    "}"
  ]

defineInternalCategory :: AnyCategory c -> [String]
defineInternalCategory t = [
    internal ++ "& " ++ categoryCreator ++ "() {",
    "  static auto& category = *new " ++ internal ++ "();",
    "  return category;",
    "}"
  ]
  where
    internal = categoryName (getCategoryName t)

declareInternalType :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
declareInternalType t n =
  return $ onlyCode $ typeName t ++ "& " ++ typeCreator ++
                      "(Params<" ++ show n ++ ">::Type params);"

defineInternalType :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
defineInternalType t n = return $ onlyCodes [
    typeName t ++ "& " ++ typeCreator ++ "(Params<" ++ show n ++ ">::Type params) {",
    "  static auto& cache = *new InstanceMap<" ++ show n ++ "," ++ typeName t ++ ">();",
    "  auto& cached = cache[params];",
    "  if (!cached) { cached = R_get(new " ++ typeName t ++ "(" ++ categoryCreator ++ "(), params)); }",
    "  return *cached;",
    "}"
  ]

declareInternalValue :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
declareInternalValue t n =
  return $ onlyCode $ "S<TypeValue> " ++ valueCreator ++
                      "(" ++ typeName t ++ "& parent, Args<" ++ show n ++ ">::Type args);"

defineInternalValue :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
defineInternalValue t n =
  return $ onlyCodes [
      "S<TypeValue> " ++ valueCreator ++ "(" ++ typeName t ++ "& parent, Args<" ++ show n ++ ">::Type args) {",
      "  return S_get(new " ++ valueName t ++ "(parent, args));",
      "}"
    ]

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
  let typeInstance = TypeInstance (getCategoryName t) $ fmap (SingleType . JustParamName) ps
  let builtin = Map.filter ((== LocalScope) . vvScope) $ builtinVariables typeInstance
  members <- mapMembers $ filter ((<= s) . dmScope) (dcMembers d)
  return $ ProcedureContext {
      pcScope = s,
      pcType = getCategoryName t,
      pcParams = ps,
      pcMembers = ms,
      pcCategories = tm,
      pcFilters = pa,
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = Map.union builtin members,
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
