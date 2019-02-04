{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module CompilerCxx.Category (
  CxxOutput(..),
  compileCategoryDeclaration,
  compileConcreteDefinition,
  compileInterfaceDefinition,
) where

import Control.Monad (when)
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
    getCategory
      | isInstanceInterface t = []
      | otherwise             = declareGetCategory t
    getType
      | isInstanceInterface t = []
      | otherwise             = declareGetType t

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
compileConcreteDefinition ta dd@(DefinedCategory c n pi fi ms ps fs) = do
  (_,t) <- getConcreteCategory ta (c,n)
  let params = ParamSet $ getCategoryParams t
  -- TODO: Check these for duplicates with params.
  -- TODO: Check type instances.
  let params2 = ParamSet pi
  let typeInstance = TypeInstance n $ fmap (SingleType . JustParamName . vpParam) params
  let filters = getCategoryFilters t
  let filters2 = fi
  let allFilters = getFilterMap (getCategoryParams t ++ pi) $ filters ++ filters2
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
  disallowTypeMembers tm
  let cm0 = builtins typeInstance CategoryScope
  let tm0 = builtins typeInstance TypeScope
  let vm0 = builtins typeInstance ValueScope
  cm' <- mapMembers cm
  tm' <- mapMembers $ cm ++ tm
  vm' <- mapMembers $ cm ++ tm ++ vm
  let cv = Map.union cm0 cm'
  let tv = Map.union tm0 tm'
  let vv = Map.union vm0 vm'
  let internalCount = length pi
  let memberCount = length vm
  top <- mergeAllM [
      return $ onlyCodes $ map createLabelForFunction (Map.elems internalFuncs),
      return $ onlyCode $ "class " ++ valueName n ++ ";",
      declareDispatchInit,
      declareInternalValue n internalCount memberCount
    ]
  defineValue <- mergeAllM [
      return $ onlyCode $ "struct " ++ valueName n ++ " : public " ++ valueBase ++ " {",
      fmap indentCompiled $ valueConstructor ta t vm,
      fmap indentCompiled $ valueDispatch,
      return $ indentCompiled $ defineCategoryName n,
      fmap indentCompiled $ mergeAllM $ map (compileExecutableProcedure ta n params params2 vm filters filters2 fa vv) vp,
      fmap indentCompiled $ mergeAllM $ map (createMember r allFilters) vm,
      fmap indentCompiled $ createParams,
      return $ indentCompiled $ onlyCode $ typeName n ++ "& parent;",
      return $ onlyCode "};"
    ]
  bottom <- mergeAllM [
      return $ defineValue,
      defineDispatchInit n (Map.elems fa),
      defineInternalValue n internalCount memberCount
    ]
  ce <- mergeAllM [
      categoryConstructor ta t cm,
      categoryDispatch,
      mergeAllM $ map (compileExecutableProcedure ta n params params2 vm filters filters2 fa cv) cp,
      mergeAllM $ map (createMember r allFilters) cm,
      return $ onlyCode $ dispatcherType ++ " " ++ dispatcherName ++ ";"
    ]
  te <- mergeAllM [
      typeConstructor ta t tm,
      typeDispatch,
      mergeAllM $ map (compileExecutableProcedure ta n params params2 vm filters filters2 fa tv) tp,
      mergeAllM $ map (createMember r allFilters) tm
    ]
  commonDefineAll t top bottom ce te
  where
    disallowTypeMembers :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
      [DefinedMember c] -> m ()
    disallowTypeMembers tm =
      mergeAllM $ flip map tm
        (\m -> compileError $ "Member " ++ show (dmName m) ++
                              " is not allowed to be @type-scoped [" ++
                              formatFullContext (dmContext m) ++ "]")
    createParams = mergeAllM $ map createParam pi
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"
    builtins t s0 = Map.filter ((<= s0) . vvScope) $ builtinVariables t
    -- TODO: Can probably remove this if @type members are disallowed. Or, just
    -- skip it if there are no @type members.
    getCycleCheck n = [
        "CycleCheck<" ++ n ++ ">::Check();",
        "CycleCheck<" ++ n ++ "> marker(*this);"
      ]
    categoryConstructor tm t ms = do
      let dispatcher = dispatcherName ++ "(" ++ dispatchInitName ++ "())"
      ctx <- getContextForInit tm t dd CategoryScope
      initMembers <- runDataCompiler (sequence $ map initMember ms) ctx
      mergeAllM [
          return $ onlyCode $ categoryName n ++ "() : " ++ dispatcher ++ " {",
          return $ indentCompiled $ onlyCodes $ getCycleCheck (categoryName n),
          return $ indentCompiled $ onlyCode $ "TRACE_FUNCTION(\"" ++ show n ++ " (init @category)\")",
          return $ indentCompiled $ initMembers,
          return $ onlyCode "}"
        ]
    typeConstructor tm t ms = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName n ++ "& p"
      let paramsPassed = "Params<" ++ show (length ps) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,paramsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(*std::get<" ++ show i ++ ">(params))") $ zip [0..] ps
      let allInit = intercalate ", " $ initParent:initPassed
      ctx <- getContextForInit tm t dd TypeScope
      initMembers <- runDataCompiler (sequence $ map initMember ms) ctx
      mergeAllM [
          return $ onlyCode $ typeName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {",
          return $ indentCompiled $ onlyCodes $ getCycleCheck (typeName n),
          return $ indentCompiled $ onlyCode $ "TRACE_FUNCTION(\"" ++ show n ++ " (init @type)\")",
          return $ indentCompiled $ initMembers,
          return $ onlyCode "}"
        ]
    valueConstructor tm t ms = do
      let argParent = typeName n ++ "& p"
      let paramsPassed = "Params<" ++ show (length pi) ++ ">::Type params"
      let argsPassed = "Args<" ++ show (length ms) ++ ">::Type args"
      let allArgs = intercalate ", " [argParent,paramsPassed,argsPassed]
      let initParent = "parent(p)"
      let initParams = map (\(i,p) -> paramName (vpParam p) ++ "(*std::get<" ++ show i ++ ">(params))") $ zip [0..] pi
      let initArgs = map (\(i,m) -> variableName (dmName m) ++ "(std::get<" ++ show i ++ ">(args))") $ zip [0..] ms
      let allInit = intercalate ", " $ initParent:(initParams ++ initArgs)
      return $ onlyCode $ valueName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    createMember r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) `reviseError`
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
      return $ onlyCode $ variableType (vtRequired $ dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    initMember (DefinedMember _ _ _ _ Nothing) = return mergeDefault
    initMember (DefinedMember c s t n (Just e)) = do
      csAddVariable c n (VariableValue c s t)
      let assign = Assignment c (ParamSet [ExistingVariable (InputValue c n)]) e
      compileStatement assign
    categoryDispatch =
      return $ onlyCodes $ [
          "DReturns Dispatch(" ++
          "const DFunction<SymbolScope::CATEGORY>& label, " ++
          "DParams params, " ++
          "DArgs args) final {",
          "  return dispatcher_.Dispatch(*this, label, params, args);",
          "}"
        ]
    typeDispatch =
      return $ onlyCodes $ [
          "DReturns Dispatch(" ++
          "const DFunction<SymbolScope::TYPE>& label, " ++
          "DParams params, " ++
          "DArgs args) final {",
          "  return parent.dispatcher_.Dispatch(*this, label, params, args);",
          "}"
        ]
    valueDispatch =
      return $ onlyCodes $ [
          "DReturns Dispatch(" ++
          "const S<TypeValue>& self, " ++
          "const DFunction<SymbolScope::VALUE>& label, " ++
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
          | sfScope f == CategoryScope = categoryName n ++ "::" ++ callName (sfName f)
          | sfScope f == TypeScope     = typeName n     ++ "::" ++ callName (sfName f)
          | sfScope f == ValueScope    = valueName n    ++ "::" ++ callName (sfName f)

commonDefineAll :: (MergeableM m, Monad m) =>
  AnyCategory c -> CompiledData [String] -> CompiledData [String] ->
  CompiledData [String] -> CompiledData [String] -> m CxxOutput
commonDefineAll t top bottom ce te = do
  let filename = sourceFilename name
  (CompiledData req out) <- mergeAllM $ [
      return $ CompiledData (Set.fromList [name]) [],
      createLabels (getCategoryFunctions t)
    ] ++ conditionalContent
  let includes = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $
                   filter (not . isBuiltinCategory) $ Set.toList req
  return $ CxxOutput filename (baseSourceIncludes ++ includes ++ out)
  where
    conditionalContent
      | isInstanceInterface t = []
      | otherwise = [
        return $ onlyCode $ "namespace {",
        declareTypes,
        declareInternalType name paramCount,
        return top,
        commonDefineCategory t ce,
        return $ onlyCodes getInternal,
        commonDefineType t te,
        defineInternalType name paramCount,
        return bottom,
        return $ onlyCode $ "}",
        return $ onlyCodes getCategory,
        return $ onlyCodes getType
      ]
    declareTypes =
      return $ onlyCodes $ map (\f -> "class " ++ f name ++ ";") [categoryName,typeName]
    paramCount = length $ getCategoryParams t
    name = getCategoryName t
    createLabels = return . onlyCodes . map createLabelForFunction . filter ((== name) . sfType)
    getInternal = defineInternalCategory t
    getCategory = defineGetCatetory t
    getType = defineGetType t

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
      return $ CompiledData depends [],
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
      return $ indentCompiled $ defineCategoryName name,
      return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
      return $ indentCompiled createParams,
      return $ indentCompiled canConvertFrom,
      return $ indentCompiled typeArgsForParent,
      return $ indentCompiled extra,
      return $ onlyCode "};"
    ]
  where
    name = getCategoryName t
    depends = Set.unions $ map (categoriesFromTypes . SingleType . JustTypeInstance . vrType) $ getCategoryRefines t
    createParams = mergeAll $ map createParam $ getCategoryParams t
    createParam p = onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"
    canConvertFrom
      | isInstanceInterface t = emptyCode
      | otherwise = onlyCodes $ [
          "bool CanConvertFrom(const TypeInstance& from) const final {",
          -- TODO: This should be a typedef.
          "  std::vector<const TypeInstance*> args;",
          "  if (!from.TypeArgsForParent(parent, args)) return false;",
          -- TODO: Create a helper function for this error.
          "  FAIL_IF(args.size() != " ++ show (length params) ++ ") << " ++
          "\"Wrong number of args (\" << args.size() << \") " ++
          "for \" << CategoryName();"
        ] ++ checks ++ ["  return true;","}"]
    params = map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
    checks = concat $ map singleCheck $ zip [0..] params
    singleCheck (i,(p,Covariant)) = [
        "  if (!TypeInstance::CanConvert(*args[" ++ show i ++ "], " ++ paramName p ++ ")) return false;"
      ]
    singleCheck (i,(p,Contravariant)) = [
        "  if (!TypeInstance::CanConvert(" ++ paramName p ++ ", *args[" ++ show i ++ "])) return false;"
      ]
    singleCheck (i,(p,Invariant)) = [
        "  if (!TypeInstance::CanConvert(*args[" ++ show i ++ "], " ++ paramName p ++ ")) return false;",
        "  if (!TypeInstance::CanConvert(" ++ paramName p ++ ", *args[" ++ show i ++ "])) return false;"
      ]
    typeArgsForParent
      | isInstanceInterface t = emptyCode
      | otherwise = onlyCodes $ [
          "bool TypeArgsForParent(" ++
          "const TypeCategory& category, " ++
          "std::vector<const TypeInstance*>& args) const final {"
        ] ++ allCats ++ ["  return false;","}"]
    myType = (getCategoryName t,map (SingleType . JustParamName . fst) params)
    refines = map (\r -> (tiName r,psParams $ tiParams r)) $ map vrType $ getCategoryRefines t
    allCats = concat $ map singleCat (myType:refines)
    singleCat (t,ps) = [
        "  if (&category == &" ++ categoryGetter t ++ "()) {",
        "    args = std::vector<const TypeInstance*>{" ++ expanded ++ "};",
        "    return true;",
        "  }"
      ]
      where
        expanded = intercalate "," $ map ('&':) $ map expandLocalType ps

-- Similar to Procedure.expandGeneralInstance but doesn't account for scope.
expandLocalType :: GeneralInstance -> String
expandLocalType (TypeMerge m ps) =
  getter ++ "(L_get<" ++ typeBase ++ "*>(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
  where
    ps' = map expandLocalType ps
    getter
      | m == MergeUnion     = unionGetter
      | m == MergeIntersect = intersectGetter
expandLocalType (SingleType (JustTypeInstance (TypeInstance t ps))) =
  typeGetter t ++ "(T_get(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
  where
    ps' = map expandLocalType $ psParams ps
expandLocalType (SingleType (JustParamName p)) = paramName p

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
            show (length $ getCategoryParams t) ++ ">::Type params) {",
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
  CategoryName -> Int -> Int -> m (CompiledData [String])
declareInternalValue t p n =
  return $ onlyCode $ "S<TypeValue> " ++ valueCreator ++
                      "(" ++ typeName t ++ "& parent, Params<" ++ show p ++
                      ">::Type params, Args<" ++ show n ++ ">::Type args);"

defineInternalValue :: Monad m =>
  CategoryName -> Int -> Int -> m (CompiledData [String])
defineInternalValue t p n =
  return $ onlyCodes [
      "S<TypeValue> " ++ valueCreator ++ "(" ++ typeName t ++ "& parent, Params<" ++
      show p ++ ">::Type params, Args<" ++ show n ++ ">::Type args) {",
      "  return S_get(new " ++ valueName t ++ "(parent, params, args));",
      "}"
    ]

getContextForInit :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> AnyCategory c -> DefinedCategory c -> SymbolScope -> m (ProcedureContext c)
getContextForInit tm t d s = do
  let ps = ParamSet $ getCategoryParams t
  -- NOTE: This is always ValueScope for initializer checks.
  let ms = filter ((== ValueScope) . dmScope) $ dcMembers d
  let pa = if s == CategoryScope
              then []
              else getCategoryFilters t
  let sa = Map.fromList $ zip (map vpParam $ getCategoryParams t) (repeat TypeScope)
  let r = categoriesToTypeResolver tm
  fa <- setInternalFunctions r t (dcFunctions d)
  let typeInstance = TypeInstance (getCategoryName t) $ fmap (SingleType . JustParamName . vpParam) ps
  let builtin = Map.filter ((== LocalScope) . vvScope) $ builtinVariables typeInstance
  -- Using < ensures that variables can only be referenced after initialization.
  -- TODO: This doesn't really help if access is done via a function.
  members <- mapMembers $ filter ((< s) . dmScope) (dcMembers d)
  return $ ProcedureContext {
      pcScope = s,
      pcType = getCategoryName t,
      pcExtParams = ps,
      pcIntParams = ParamSet [],
      pcMembers = ms,
      pcCategories = tm,
      pcAllFilters = getFilterMap (psParams ps) pa,
      pcExtFilters = pa,
      pcIntFilters = [],
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = Map.union builtin members,
      pcReturns = NoValidation,
      pcRequiredTypes = Set.empty,
      pcOutput = [],
      pcDisallowInit = True
    }

builtinVariables :: TypeInstance -> Map.Map VariableName (VariableValue c)
builtinVariables t = Map.fromList [
    (VariableName "self",VariableValue [] ValueScope (ValueType RequiredValue $ SingleType $ JustTypeInstance t))
  ]
