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
{-# LANGUAGE Safe #-}

module CompilerCxx.Category (
  CategoryModule(..),
  CxxOutput(..),
  PrivateSource(..),
  createMainFile,
  createTestFile,
  compileCategoryDeclaration,
  compileCategoryModule,
  compileConcreteDefinition,
  compileConcreteTemplate,
  compileInterfaceDefinition,
  compileModuleMain,
) where

import Data.List (intercalate,sortBy)
import Prelude hiding (pi)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Compilation.CompilerState
import Compilation.ScopeContext
import CompilerCxx.CategoryContext
import CompilerCxx.Code
import CompilerCxx.Naming
import CompilerCxx.Procedure
import Types.DefinedCategory
import Types.GeneralType
import Types.Positional
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


data CxxOutput =
  CxxOutput {
    coCategory :: Maybe CategoryName,
    coFilename :: String,
    coNamespace :: Namespace,
    coUsesNamespace :: [Namespace],
    coUsesCategory :: [CategoryName],
    coOutput :: [String]
  }

data CategoryModule c =
  CategoryModule {
    cnBase :: CategoryMap c,
    cnNamespaces :: [Namespace],
    cnPublic :: [AnyCategory c],
    cnPrivate :: [PrivateSource c],
    cnExternal :: [String]
  }

data PrivateSource c =
  PrivateSource {
    psNamespace :: Namespace,
    psCategory :: [AnyCategory c],
    psDefine :: [DefinedCategory c]
  }

compileCategoryModule :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryModule c -> m [CxxOutput]
compileCategoryModule (CategoryModule tm ns cs xa ex) = do
  checkSupefluous $ Set.toList $ (Set.fromList ex) `Set.difference` ca
  tm' <- includeNewTypes tm cs
  hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm' ns) cs
  let interfaces = filter (not . isValueConcrete) cs
  cxx <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
  xa2 <- collectAllOrErrorM $ map (compileInternal ns) xa
  let xx = concat $ map snd xa2
  let dm = mapByName $ concat $ map fst xa2
  checkDefined dm ex $ filter isValueConcrete cs
  return $ hxx ++ cxx ++ xx where
    compileInternal ns0 (PrivateSource ns1 cs2 ds) = do
      let cs' = cs++cs2
      tm' <- includeNewTypes tm cs'
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm' ns0) cs2
      let interfaces = filter (not . isValueConcrete) cs2
      cxx1 <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      cxx2 <- collectAllOrErrorM $ map (compileDefinition tm' (ns1:ns)) ds
      let dm = mapByName ds
      checkDefined dm [] $ filter isValueConcrete cs2
      return (ds,hxx ++ cxx1 ++ cxx2)
    compileDefinition tm2 ns2 d = do
      tm2' <- mergeInternalInheritance tm2 d
      compileConcreteDefinition tm2' ns2 d
    mapByName = Map.fromListWith (++) . map (\d -> (dcName d,[d]))
    ca = Set.fromList $ map (show . getCategoryName) $ filter isValueConcrete cs
    checkDefined dm ex2 = mergeAllM . map (checkSingle dm (Set.fromList ex2))
    checkSingle dm es t =
      case ((show $ getCategoryName t) `Set.member` es, getCategoryName t `Map.lookup` dm) of
           (False,Just [_]) -> return ()
           (True,Nothing)   -> return ()
           (True,Just [d]) ->
             compileError ("Category " ++ show (getCategoryName t) ++
                           formatFullContextBrace (getCategoryContext t) ++
                           " was declared external but is also defined at " ++ formatFullContext (dcContext d))
           (False,Nothing) ->
             compileError ("Category " ++ show (getCategoryName t) ++
                           formatFullContextBrace (getCategoryContext t) ++
                           " has not been defined or declared external")
           (_,Just ds) ->
             flip reviseError ("Category " ++ show (getCategoryName t) ++
                               formatFullContextBrace (getCategoryContext t) ++
                               " is defined " ++ show (length ds) ++ " times") $
               mergeAllM $ map (\d -> compileError $ "Defined at " ++ formatFullContext (dcContext d)) ds
    checkSupefluous es2
      | null es2 = return ()
      | otherwise = compileError $ "External categories either not concrete or not present: " ++
                                   intercalate ", " es2

compileModuleMain :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryModule c -> CategoryName -> FunctionName -> m CxxOutput
compileModuleMain (CategoryModule tm _ cs xa _) n f = do
  xx <- fmap concat $ collectAllOrErrorM $ filter (not . isCompileError) $ map maybeCompileMain xa
  reconcile xx where
    maybeCompileMain (PrivateSource _ cs2 ds) = do
      let cs' = cs++cs2
      tm' <- includeNewTypes tm cs'
      let dm = Set.fromList $ map dcName ds
      if n `Set.member` dm
         then do
           (ns2,main) <- createMainFile tm' n f
           return [CxxOutput Nothing mainFilename NoNamespace [ns2] [n] main]
         else return []
    reconcile [x] = return x
    reconcile []  = compileErrorM $ "No matches for main category " ++ show n
    reconcile _   = compileErrorM $ "Multiple matches for main category " ++ show n

compileCategoryDeclaration :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> [Namespace] -> AnyCategory c -> m CxxOutput
compileCategoryDeclaration _ ns t =
  return $ CxxOutput (Just $ getCategoryName t)
                     (headerFilename name)
                     (getCategoryNamespace t)
                     (ns ++ [getCategoryNamespace t])
                     (Set.toList $ cdRequired file)
                     (cdOutput file) where
    file = mergeAll $ [
        CompiledData depends [],
        onlyCodes guardTop,
        onlyCodes baseHeaderIncludes,
        addNamespace t content,
        onlyCodes guardBottom
      ]
    depends = getCategoryDeps t
    content = onlyCodes $ collection ++ labels ++ getCategory2 ++ getType
    name = getCategoryName t
    guardTop = ["#ifndef " ++ guardName,"#define " ++ guardName]
    guardBottom = ["#endif  // " ++ guardName]
    guardName = "HEADER_" ++ guardNamespace ++ show name
    guardNamespace
      | isStaticNamespace $ getCategoryNamespace t = show (getCategoryNamespace t) ++ "_"
      | otherwise = ""
    labels = map label $ filter ((== name) . sfType) $ getCategoryFunctions t
    label f = "extern " ++ functionLabelType f ++ " " ++ functionName f ++ ";"
    collection
      | isValueConcrete t = []
      | otherwise         = ["extern const void* const " ++ collectionName name ++ ";"]
    getCategory2
      | isInstanceInterface t = []
      | otherwise             = declareGetCategory t
    getType
      | isInstanceInterface t = []
      | otherwise             = declareGetType t

compileInterfaceDefinition :: MergeableM m => AnyCategory c -> m CxxOutput
compileInterfaceDefinition t = do
  te <- typeConstructor
  commonDefineAll t [] emptyCode emptyCode emptyCode te []
  where
    typeConstructor = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName (getCategoryName t) ++ "& p"
      let argsPassed = "Params<" ++ show (length ps) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(*std::get<" ++ show i ++ ">(params))") $ zip ([0..] :: [Int]) ps
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ typeName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"

compileConcreteTemplate :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> CategoryName -> m CxxOutput
compileConcreteTemplate ta n = do
  (_,t) <- getConcreteCategory ta ([],n)
  compileConcreteDefinition ta [] (defined t) `reviseError` ("In generated template for " ++ show n) where
    defined t = DefinedCategory {
        dcContext = [],
        dcName = getCategoryName t,
        dcParams = [],
        dcRefines = [],
        dcDefines = [],
        dcParamFilter = [],
        dcMembers = [],
        dcProcedures = map defaultFail (getCategoryFunctions t),
        dcFunctions = []
      }
    defaultFail f = ExecutableProcedure {
        epContext = [],
        epEnd = [],
        epName = sfName f,
        epArgs = ArgValues [] $ Positional $ map createArg [1..(length $ pValues $ sfArgs f)],
        epReturns = UnnamedReturns [],
        epProcedure = failProcedure f
      }
    createArg = InputValue [] . VariableName . ("arg" ++) . show
    failProcedure f = Procedure [] [
        NoValueExpression [] $ LineComment $ "// TODO: Implement " ++ funcName f ++ ".",
        FailCall [] (Literal (StringLiteral [] $ funcName f ++ " is not implemented"))
      ]
    funcName f = show (sfType f) ++ "." ++ show (sfName f)

compileConcreteDefinition :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> [Namespace] -> DefinedCategory c -> m CxxOutput
compileConcreteDefinition ta ns dd@(DefinedCategory c n pi _ _ fi ms _ fs) = do
  (_,t) <- getConcreteCategory ta (c,n)
  let r = CategoryResolver ta
  [cp,tp,vp] <- getProcedureScopes ta dd
  cf <- collectAllOrErrorM $ applyProcedureScope compileExecutableProcedure cp
  tf <- collectAllOrErrorM $ applyProcedureScope compileExecutableProcedure tp
  vf <- collectAllOrErrorM $ applyProcedureScope compileExecutableProcedure vp
  -- Functions explicitly declared externally.
  let externalFuncs = Set.fromList $ map sfName $ filter ((== n) . sfType) $ getCategoryFunctions t
  -- Functions explicitly declared internally.
  let overrideFuncs = Map.fromList $ map (\f -> (sfName f,f)) fs
  -- Functions only declared internally.
  let internalFuncs = Map.filter (not . (`Set.member` externalFuncs) . sfName) overrideFuncs
  let (cm,tm,vm) = partitionByScope dmScope ms
  disallowTypeMembers tm
  let internalCount = length pi
  let memberCount = length vm
  let fe = Map.elems internalFuncs
  let allFuncs = getCategoryFunctions t ++ fe
  let filters = getCategoryFilters t
  let filters2 = fi
  let allFilters = getFilterMap (getCategoryParams t ++ pi) $ filters ++ filters2
  top <- mergeAllM [
      return $ onlyCode $ "class " ++ valueName n ++ ";",
      declareInternalValue n internalCount memberCount
    ]
  defineValue <- mergeAllM [
      return $ onlyCode $ "struct " ++ valueName n ++ " : public " ++ valueBase ++ " {",
      fmap indentCompiled $ valueConstructor vm,
      fmap indentCompiled $ valueDispatch allFuncs,
      return $ indentCompiled $ defineCategoryName2 n,
      return $ indentCompiled $ mergeAll $ map fst vf,
      fmap indentCompiled $ mergeAllM $ map (createMember r allFilters) vm,
      fmap indentCompiled $ createParams,
      return $ indentCompiled $ onlyCode $ typeName n ++ "& parent;",
      return $ onlyCode "};"
    ]
  bottom <- mergeAllM $ [
      return $ defineValue,
      defineInternalValue n internalCount memberCount
    ] ++ map (return . snd) (cf ++ tf ++ vf)
  ce <- mergeAllM [
      categoryConstructor t cm,
      categoryDispatch allFuncs,
      return $ mergeAll $ map fst cf,
      mergeAllM $ map (createMemberLazy r allFilters) cm
    ]
  te <- mergeAllM [
      typeConstructor t tm,
      typeDispatch allFuncs,
      return $ mergeAll $ map fst tf,
      mergeAllM $ map (createMember r allFilters) tm
    ]
  commonDefineAll t ns top bottom ce te fe
  where
    disallowTypeMembers :: (Show c, CompileErrorM m, MergeableM m) =>
      [DefinedMember c] -> m ()
    disallowTypeMembers tm =
      mergeAllM $ flip map tm
        (\m -> compileError $ "Member " ++ show (dmName m) ++
                              " is not allowed to be @type-scoped" ++
                              formatFullContextBrace (dmContext m))
    createParams = mergeAllM $ map createParam pi
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"
    -- TODO: Can probably remove this if @type members are disallowed. Or, just
    -- skip it if there are no @type members.
    getCycleCheck n2 = [
        "CycleCheck<" ++ n2 ++ ">::Check();",
        "CycleCheck<" ++ n2 ++ "> marker(*this);"
      ]
    categoryConstructor t ms2 = do
      ctx <- getContextForInit ta t dd CategoryScope
      initMembers <- runDataCompiler (sequence $ map compileLazyInit ms2) ctx
      let initMembersStr = intercalate ", " $ cdOutput initMembers
      let initColon = if null initMembersStr then "" else " : "
      mergeAllM [
          return $ onlyCode $ categoryName n ++ "()" ++ initColon ++ initMembersStr ++ " {",
          return $ indentCompiled $ onlyCodes $ getCycleCheck (categoryName n),
          return $ indentCompiled $ onlyCode $ startFunctionTracing $ show n ++ " (init @category)",
          return $ onlyCode "}",
          return $ clearCompiled initMembers -- Inherit required types.
        ]
    typeConstructor t ms2 = do
      let ps2 = map vpParam $ getCategoryParams t
      let argParent = categoryName n ++ "& p"
      let paramsPassed = "Params<" ++ show (length ps2) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,paramsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(*std::get<" ++ show i ++ ">(params))") $ zip ([0..] :: [Int]) ps2
      let allInit = intercalate ", " $ initParent:initPassed
      ctx <- getContextForInit ta t dd TypeScope
      initMembers <- runDataCompiler (sequence $ map initMember ms2) ctx
      mergeAllM [
          return $ onlyCode $ typeName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {",
          return $ indentCompiled $ onlyCodes $ getCycleCheck (typeName n),
          return $ indentCompiled $ onlyCode $ startFunctionTracing $ show n ++ " (init @type)",
          return $ indentCompiled $ initMembers,
          return $ onlyCode "}"
        ]
    valueConstructor ms2 = do
      let argParent = typeName n ++ "& p"
      let paramsPassed = "const ParamTuple& params"
      let argsPassed = "const ValueTuple& args"
      let allArgs = intercalate ", " [argParent,paramsPassed,argsPassed]
      let initParent = "parent(p)"
      let initParams = map (\(i,p) -> paramName (vpParam p) ++ "(*params.At(" ++ show i ++ "))") $ zip ([0..] :: [Int]) pi
      let initArgs = map (\(i,m) -> variableName (dmName m) ++ "(" ++ unwrappedArg i m ++ ")") $ zip ([0..] :: [Int]) ms2
      let allInit = intercalate ", " $ initParent:(initParams ++ initArgs)
      return $ onlyCode $ valueName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    unwrappedArg i m = writeStoredVariable (dmType m) (UnwrappedSingle $ "args.At(" ++ show i ++ ")")
    createMember r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) `reviseError`
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
      return $ onlyCode $ variableStoredType (dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    createMemberLazy r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) `reviseError`
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
      return $ onlyCode $ variableLazyType (dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    initMember (DefinedMember _ _ _ _ Nothing) = return mergeDefault
    initMember (DefinedMember c2 s t n2 (Just e)) = do
      csAddVariable c2 n2 (VariableValue c2 s t True)
      let assign = Assignment c2 (Positional [ExistingVariable (InputValue c2 n2)]) e
      compileStatement assign
    categoryDispatch fs2 =
      return $ onlyCodes $ [
          "ReturnTuple Dispatch(" ++
          "const CategoryFunction& label, " ++
          "const ParamTuple& params, " ++
          "const ValueTuple& args) final {"
        ] ++ createFunctionDispatch n CategoryScope fs2 ++ ["}"]
    typeDispatch fs2 =
      return $ onlyCodes $ [
          "ReturnTuple Dispatch(" ++
          "const TypeFunction& label, " ++
          "const ParamTuple& params, " ++
          "const ValueTuple& args) final {"
        ] ++ createFunctionDispatch n TypeScope fs2 ++ ["}"]
    valueDispatch fs2 =
      return $ onlyCodes $ [
          "ReturnTuple Dispatch(" ++
          "const S<TypeValue>& self, " ++
          "const ValueFunction& label, " ++
          "const ParamTuple& params," ++
          "const ValueTuple& args) final {"
        ] ++ createFunctionDispatch n ValueScope fs2 ++ ["}"]

commonDefineAll :: MergeableM m =>
  AnyCategory c -> [Namespace] -> CompiledData [String] -> CompiledData [String] ->
  CompiledData [String] -> CompiledData [String] ->
  [ScopedFunction c] -> m CxxOutput
commonDefineAll t ns top bottom ce te fe = do
  let filename = sourceFilename name
  (CompiledData req out) <- fmap (addNamespace t) $ mergeAllM $ [
      return $ CompiledData (Set.fromList [name]) [],
      return $ mergeAll [createCollection,createAllLabels]
    ] ++ conditionalContent
  let inherited = Set.fromList $ (map (tiName . vrType) $ getCategoryRefines t) ++
                                 (map (diName . vdType) $ getCategoryDefines t)
  let includes = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $
                   Set.toList $ Set.union req inherited
  return $ CxxOutput (Just $ getCategoryName t)
                     filename
                     (getCategoryNamespace t)
                     ((getCategoryNamespace t):ns)
                     (Set.toList req)
                     (baseSourceIncludes ++ includes ++ out)
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
        return $ onlyCode $ "}  // namespace",
        return $ onlyCodes $ getCategory2 ++ getType
      ]
    declareTypes =
      return $ onlyCodes $ map (\f -> "class " ++ f name ++ ";") [categoryName,typeName]
    paramCount = length $ getCategoryParams t
    name = getCategoryName t
    createCollection = onlyCodes [
        "namespace {",
        "const int " ++ collectionValName ++ " = 0;",
        "}  // namespace",
        "const void* const " ++ collectionName name ++ " = &" ++ collectionValName ++ ";"
      ]
    collectionValName = "collection_" ++ show name
    (fc,ft,fv) = partitionByScope sfScope $ getCategoryFunctions t ++ fe
    createAllLabels = onlyCodes $ concat $ map createLabels [fc,ft,fv]
    createLabels = map (uncurry createLabelForFunction) . zip [0..] . sortBy compareName . filter ((== name) . sfType)
    getInternal = defineInternalCategory t
    getCategory2 = defineGetCatetory t
    getType = defineGetType t
    compareName x y = sfName x `compare` sfName y

addNamespace :: AnyCategory c -> CompiledData [String] -> CompiledData [String]
addNamespace t cs
  | isStaticNamespace $ getCategoryNamespace t = mergeAll [
      onlyCode $ "namespace " ++ show (getCategoryNamespace t) ++ " {",
      cs,
      onlyCode $ "}  // namespace " ++ show (getCategoryNamespace t),
      onlyCode $ "using namespace " ++ show (getCategoryNamespace t) ++ ";"
    ]
  | isDynamicNamespace $ getCategoryNamespace t = mergeAll [
      onlyCode $ "#ifdef " ++ dynamicNamespaceName,
      onlyCode $ "namespace " ++ dynamicNamespaceName ++ " {",
      onlyCode $ "#endif  // " ++ dynamicNamespaceName,
      cs,
      onlyCode $ "#ifdef " ++ dynamicNamespaceName,
      onlyCode $ "}  // namespace " ++ dynamicNamespaceName,
      onlyCode $ "using namespace " ++ dynamicNamespaceName ++ ";",
      onlyCode $ "#endif  // " ++ dynamicNamespaceName
    ]
  | otherwise = cs

createLabelForFunction :: Int -> ScopedFunction c -> String
createLabelForFunction i f = functionLabelType f ++ " " ++ functionName f ++
                              " = " ++ newFunctionLabel i f ++ ";"

createFunctionDispatch :: CategoryName -> SymbolScope -> [ScopedFunction c] -> [String]
createFunctionDispatch n s fs = [typedef] ++ concat (map table $ byCategory) ++
                                             concat (map dispatch $ byCategory) ++ [fallback] where
  filtered = filter ((== s) . sfScope) fs
  flatten f = f:(concat $ map flatten $ sfMerges f)
  flattened = concat $ map flatten filtered
  byCategory = Map.toList $ Map.fromListWith (++) $ map (\f -> (sfType f,[f])) flattened
  typedef
    | s == CategoryScope = "  using CallType = ReturnTuple(" ++ categoryName n ++
                           "::*)(const ParamTuple&, const ValueTuple&);"
    | s == TypeScope     = "  using CallType = ReturnTuple(" ++ typeName n ++
                           "::*)(const ParamTuple&, const ValueTuple&);"
    | s == ValueScope    = "  using CallType = ReturnTuple(" ++ valueName n ++
                           "::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);"
    | otherwise = undefined
  name f
    | s == CategoryScope = categoryName n ++ "::" ++ callName f
    | s == TypeScope     = typeName n     ++ "::" ++ callName f
    | s == ValueScope    = valueName n    ++ "::" ++ callName f
    | otherwise = undefined
  table (n2,fs2) =
    ["  static const CallType " ++ tableName n2 ++ "[] = {"] ++
    map (\f -> "    &" ++ name f ++ ",") (Set.toList $ Set.fromList $ map sfName fs2) ++
    ["  };"]
  dispatch (n2,fs2) = [
      "  if (label.collection == " ++ collectionName n2 ++ ") {",
      "    if (label.function_num < 0 || label.function_num >= " ++ show (length fs2) ++ ") {",
      "      FAIL() << \"Bad function call \" << label;",
      "    }",
      "    return (this->*" ++ tableName n2 ++ "[label.function_num])(" ++ args ++ ");",
      "  }"
    ]
  args
    | s == CategoryScope = "params, args"
    | s == TypeScope     = "params, args"
    | s == ValueScope    = "self, params, args"
    | otherwise = undefined
  fallback
    | s == CategoryScope = "  return TypeCategory::Dispatch(label, params, args);"
    | s == TypeScope     = "  return TypeInstance::Dispatch(label, params, args);"
    | s == ValueScope    = "  return TypeValue::Dispatch(self, label, params, args);"
    | otherwise = undefined

commonDefineCategory :: MergeableM m =>
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

commonDefineType :: MergeableM m =>
  AnyCategory c -> CompiledData [String] -> m (CompiledData [String])
commonDefineType t extra = do
  mergeAllM [
      return $ CompiledData depends [],
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
      return $ indentCompiled $ defineCategoryName2 name,
      return $ indentCompiled $ defineTypeName name (map vpParam $ getCategoryParams t),
      return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
      return $ indentCompiled createParams,
      return $ indentCompiled canConvertFrom,
      return $ indentCompiled typeArgsForParent,
      return $ indentCompiled extra,
      return $ onlyCode "};"
    ]
  where
    name = getCategoryName t
    depends = getCategoryDeps t
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
          "  if(args.size() != " ++ show (length params) ++ ") {",
          "    FAIL() << \"Wrong number of args (\" << args.size() << \")  for \" << CategoryName();",
          "  }"
        ] ++ checks ++ ["  return true;","}"]
    params = map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
    checks = concat $ map singleCheck $ zip ([0..] :: [Int]) params
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
    refines = map (\r -> (tiName r,pValues $ tiParams r)) $ map vrType $ getCategoryRefines t
    allCats = concat $ map singleCat (myType:refines)
    singleCat (t2,ps) = [
        "  if (&category == &" ++ categoryGetter t2 ++ "()) {",
        "    args = std::vector<const TypeInstance*>{" ++ expanded ++ "};",
        "    return true;",
        "  }"
      ]
      where
        expanded = intercalate "," $ map ('&':) $ map expandLocalType ps

-- Similar to Procedure.expandGeneralInstance but doesn't account for scope.
expandLocalType :: GeneralInstance -> String
expandLocalType (TypeMerge MergeUnion     []) = allGetter ++ "()"
expandLocalType (TypeMerge MergeIntersect []) = anyGetter ++ "()"
expandLocalType (TypeMerge m ps) =
  getter m  ++ "(L_get<" ++ typeBase ++ "*>(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
  where
    ps' = map expandLocalType ps
    getter MergeUnion     = unionGetter
    getter MergeIntersect = intersectGetter
expandLocalType (SingleType (JustTypeInstance (TypeInstance t ps))) =
  typeGetter t ++ "(T_get(" ++ intercalate "," (map ("&" ++) ps') ++ "))"
  where
    ps' = map expandLocalType $ pValues ps
expandLocalType (SingleType (JustParamName p)) = paramName p

defineCategoryName :: CategoryName -> CompiledData [String]
defineCategoryName t = onlyCode $ "std::string CategoryName() const final { return \"" ++ show t ++ "\"; }"

defineCategoryName2 :: CategoryName -> CompiledData [String]
defineCategoryName2 _ = onlyCode $ "std::string CategoryName() const final { return parent.CategoryName(); }"

defineTypeName :: CategoryName -> [ParamName] -> CompiledData [String]
defineTypeName _ ps =
  onlyCodes [
      "void BuildTypeName(std::ostream& output) const final {",
      "  return TypeInstance::TypeNameFrom(output, parent" ++ concat (map ((", " ++) . paramName) ps) ++ ");",
      "}"
    ]

declareGetCategory :: AnyCategory c -> [String]
declareGetCategory t = [categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "();"]

defineGetCatetory :: AnyCategory c -> [String]
defineGetCatetory t = [
    categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "() {",
    "  return " ++ categoryCreator (getCategoryName t) ++ "();",
    "}"
  ]

declareGetType :: AnyCategory c -> [String]
declareGetType t = [typeBase ++ "& " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $getCategoryParams t) ++ ">::Type params);"]

defineGetType :: AnyCategory c -> [String]
defineGetType t = [
    typeBase ++ "& " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $ getCategoryParams t) ++ ">::Type params) {",
    "  return " ++ typeCreator (getCategoryName t) ++ "(params);",
    "}"
  ]

defineInternalCategory :: AnyCategory c -> [String]
defineInternalCategory t = [
    internal ++ "& " ++ categoryCreator (getCategoryName t) ++ "() {",
    "  static auto& category = *new " ++ internal ++ "();",
    "  return category;",
    "}"
  ]
  where
    internal = categoryName (getCategoryName t)

declareInternalType :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
declareInternalType t n =
  return $ onlyCode $ typeName t ++ "& " ++ typeCreator t ++
                      "(Params<" ++ show n ++ ">::Type params);"

defineInternalType :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
defineInternalType t n = return $ onlyCodes [
    typeName t ++ "& " ++ typeCreator t ++ "(Params<" ++ show n ++ ">::Type params) {",
    "  static auto& cache = *new InstanceMap<" ++ show n ++ "," ++ typeName t ++ ">();",
    "  auto& cached = cache[params];",
    "  if (!cached) { cached = R_get(new " ++ typeName t ++ "(" ++ categoryCreator t ++ "(), params)); }",
    "  return *cached;",
    "}"
  ]

declareInternalValue :: Monad m =>
  CategoryName -> Int -> Int -> m (CompiledData [String])
declareInternalValue t _ _ =
  return $ onlyCodes [
      "S<TypeValue> " ++ valueCreator t ++
      "(" ++ typeName t ++ "& parent, " ++
      "const ParamTuple& params, const ValueTuple& args);"
    ]

defineInternalValue :: Monad m =>
  CategoryName -> Int -> Int -> m (CompiledData [String])
defineInternalValue t _ _ =
  return $ onlyCodes [
      "S<TypeValue> " ++ valueCreator t ++ "(" ++ typeName t ++ "& parent, " ++
      "const ParamTuple& params, const ValueTuple& args) {",
      "  return S_get(new " ++ valueName t ++ "(parent, params, args));",
      "}"
    ]

createMainCommon :: String -> CompiledData [String] -> [String]
createMainCommon n (CompiledData req out) =
  baseSourceIncludes ++ mainSourceIncludes ++ depIncludes req ++ [
      "int main(int argc, const char** argv) {",
      "  SetSignalHandler();",
      "  ProgramArgv program_argv(argc, argv);",
      "  " ++ startFunctionTracing n
    ] ++ out ++ ["}"] where
      depIncludes req2 = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $
                           Set.toList req2

createMainFile :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> CategoryName -> FunctionName -> m (Namespace,[String])
createMainFile tm n f = flip reviseError ("In the creation of the main binary procedure") $ do
  ca <- fmap indentCompiled (compileMainProcedure tm expr)
  let file = createMainCommon "main" ca
  (_,t) <- getConcreteCategory tm ([],n)
  return (getCategoryNamespace t,file) where
    funcCall = FunctionCall [] f (Positional []) (Positional [])
    mainType = JustTypeInstance $ TypeInstance n (Positional [])
    expr = Expression [] (TypeCall [] mainType funcCall) []

createTestFile :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> Expression c -> m ([CategoryName],[String])
createTestFile tm e = flip reviseError ("In the creation of the test binary procedure") $ do
  ca@(CompiledData req _) <- fmap indentCompiled (compileMainProcedure tm e)
  let file = createMainCommon "main" ca
  return (Set.toList req,file)
