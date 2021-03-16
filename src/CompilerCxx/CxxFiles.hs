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

{-# LANGUAGE Safe #-}

module CompilerCxx.CxxFiles (
  CxxOutput(..),
  FileContext(..),
  generateExtensionTemplate,
  generateMainFile,
  generateNativeConcrete,
  generateNativeInterface,
  generateStreamlinedExtension,
  generateTestFile,
  generateVerboseExtension,
) where

import Data.List (intercalate,sortBy)
import Prelude hiding (pi)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.Positional
import Compilation.CompilerState
import Compilation.ProcedureContext (ExprMap)
import Compilation.ScopeContext
import CompilerCxx.Category
import CompilerCxx.CategoryContext
import CompilerCxx.Code
import CompilerCxx.Naming
import CompilerCxx.Procedure
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


data CxxOutput =
  CxxOutput {
    coCategory :: Maybe CategoryName,
    coFilename :: String,
    coNamespace :: Namespace,
    coUsesNamespace :: Set.Set Namespace,
    coUsesCategory :: Set.Set CategoryName,
    coOutput :: [String]
  }
  deriving (Show)

data FileContext c =
  FileContext {
    fcTesting :: Bool,
    fcCategories :: CategoryMap c,
    fcNamespaces :: Set.Set Namespace,
    fcExprMap :: ExprMap c
  }

generateNativeConcrete :: (Ord c, Show c, CollectErrorsM m) =>
  FileContext c -> (AnyCategory c,DefinedCategory c) -> m [CxxOutput]
generateNativeConcrete (FileContext testing tm ns em) (t,d) = do
  dec <- compileCategoryDeclaration testing ns t
  def <- generateCategoryDefinition testing (NativeConcrete t d tm ns em)
  return (dec:def)

generateNativeInterface :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> AnyCategory c -> m [CxxOutput]
generateNativeInterface testing t = do
  dec <- compileCategoryDeclaration testing Set.empty t
  def <- generateCategoryDefinition testing (NativeInterface t)
  return (dec:def)

generateStreamlinedExtension :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> AnyCategory c -> m [CxxOutput]
generateStreamlinedExtension testing t = do
  dec <- compileCategoryDeclaration testing Set.empty t
  def <- generateCategoryDefinition testing (StreamlinedExtension t)
  return (dec:def)

generateVerboseExtension :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> AnyCategory c -> m [CxxOutput]
generateVerboseExtension testing t =
  fmap (:[]) $ compileCategoryDeclaration testing Set.empty t

data CategoryDefinition c =
  NativeInterface {
    niCategory :: AnyCategory c
  } |
  NativeConcrete {
    ncCategory :: AnyCategory c,
    ncDefined :: DefinedCategory c,
    ncCategories :: CategoryMap c,
    ncNamespaces :: Set.Set Namespace,
    ncExprMap :: ExprMap c
  } |
  StreamlinedExtension {
    seCategory :: AnyCategory c
  } |
  StreamlinedTemplate {
    stCategory :: AnyCategory c
  }

generateCategoryDefinition :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> CategoryDefinition c -> m [CxxOutput]
generateCategoryDefinition testing = common where
  common (NativeInterface t) = fmap (:[]) $ compileInterfaceDefinition testing t
  common (StreamlinedTemplate _) = undefined
  common (NativeConcrete t d tm ns em) = do
    tm' <- mergeInternalInheritance tm d
    fmap (:[]) $ compileConcreteDefinition testing tm' em ns (Just $ getCategoryRefines t) d
  common (StreamlinedExtension t) = compileConcreteStreamlined testing t

compileCategoryDeclaration :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> Set.Set Namespace -> AnyCategory c -> m CxxOutput
compileCategoryDeclaration testing ns t =
  return $ CxxOutput (Just $ getCategoryName t)
                     (headerFilename name)
                     (getCategoryNamespace t)
                     (getCategoryNamespace t `Set.insert` ns)
                     (cdRequired file)
                     (cdOutput file) where
    file = mconcat $ [
        CompiledData depends [],
        onlyCodes guardTop,
        onlyCodes $ (if testing then testsOnlyCategoryGuard (getCategoryName t) else []),
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

compileInterfaceDefinition :: CollectErrorsM m => Bool -> AnyCategory c -> m CxxOutput
compileInterfaceDefinition testing t = do
  te <- typeConstructor
  commonDefineAll testing t Set.empty Nothing emptyCode emptyCode emptyCode te []
  where
    typeConstructor = do
      let ps = map vpParam $ getCategoryParams t
      let argParent = categoryName (getCategoryName t) ++ "& p"
      let argsPassed = "Params<" ++ show (length ps) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,argsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(std::get<" ++ show i ++ ">(params))") $ zip ([0..] :: [Int]) ps
      let allInit = intercalate ", " $ initParent:initPassed
      return $ onlyCode $ typeName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"

generateExtensionTemplate :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> CategoryMap c -> CategoryName -> m CxxOutput
generateExtensionTemplate testing ta n = do
  (_,t) <- getConcreteCategory ta ([],n)
  compileConcreteDefinition testing ta Map.empty Set.empty Nothing (defined t) <?? "In generated template for " ++ show n where
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
        epPragmas = [],
        epEnd = [],
        epName = sfName f,
        epArgs = ArgValues [] $ Positional $ map createArg [1..(length $ pValues $ sfArgs f)],
        epReturns = UnnamedReturns [],
        epProcedure = failProcedure f
      }
    createArg = InputValue [] . VariableName . ("arg" ++) . show
    failProcedure f = Procedure [] [
        NoValueExpression [] $ LineComment $ "TODO: Implement " ++ funcName f ++ ".",
        FailCall [] (Literal (StringLiteral [] $ funcName f ++ " is not implemented"))
      ]
    funcName f = show (sfType f) ++ "." ++ show (sfName f)

compileConcreteStreamlined :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> AnyCategory c -> m [CxxOutput]
compileConcreteStreamlined testing t =  "In streamlined compilation of " ++ show (getCategoryName t) ??> do
  let guard = if testing
                 then testsOnlySourceGuard
                 else noTestsOnlySourceGuard
  -- TODO: Implement this.
  let hxx = CxxOutput (Just $ getCategoryName t)
                      (headerStreamlined $ getCategoryName t)
                      (getCategoryNamespace t)
                      (Set.fromList [getCategoryNamespace t])
                      (Set.fromList $ getCategoryMentions t)
                      guard
  let cxx = CxxOutput (Just $ getCategoryName t)
                      (sourceStreamlined $ getCategoryName t)
                      (getCategoryNamespace t)
                      (Set.fromList [getCategoryNamespace t])
                      (Set.fromList $ getCategoryMentions t)
                      []
  return [hxx,cxx]

compileConcreteDefinition :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> CategoryMap c -> ExprMap c -> Set.Set Namespace -> Maybe [ValueRefine c] ->
  DefinedCategory c -> m CxxOutput
compileConcreteDefinition testing ta em ns rs dd@(DefinedCategory c n pi _ _ fi ms _ fs) = do
  (_,t) <- getConcreteCategory ta (c,n)
  let r = CategoryResolver ta
  [cp,tp,vp] <- getProcedureScopes ta em dd
  let (cm,tm,vm) = partitionByScope dmScope ms
  let filters = getCategoryFilters t
  let filters2 = fi
  allFilters <- getFilterMap (getCategoryParams t ++ pi) $ filters ++ filters2
  -- Functions explicitly declared externally.
  let externalFuncs = Set.fromList $ map sfName $ filter ((== n) . sfType) $ getCategoryFunctions t
  -- Functions explicitly declared internally.
  let overrideFuncs = Map.fromList $ map (\f -> (sfName f,f)) fs
  -- Functions only declared internally.
  let internalFuncs = Map.filter (not . (`Set.member` externalFuncs) . sfName) overrideFuncs
  let fe = Map.elems internalFuncs
  let allFuncs = getCategoryFunctions t ++ fe
  ce <- concatM [
      categoryConstructor t cm,
      categoryDispatch allFuncs,
      fmap indentCompiled $ concatM $ map (procedureDeclaration False . fst) (psProcedures cp),
      concatM $ map (createMemberLazy r allFilters) cm
    ]
  disallowTypeMembers tm
  te <- concatM [
      typeConstructor t tm,
      typeDispatch allFuncs,
      fmap indentCompiled $ concatM $ map (procedureDeclaration False . fst) (psProcedures tp),
      concatM $ map (createMember r allFilters) tm
    ]
  let internalCount = length pi
  let memberCount = length vm
  top <- concatM [
      return $ onlyCode $ "class " ++ valueName n ++ ";",
      declareInternalValue n internalCount memberCount
    ]
  defineValue <- concatM [
      return $ onlyCode $ "struct " ++ valueName n ++ " : public " ++ valueBase ++ " {",
      fmap indentCompiled $ valueConstructor vm,
      fmap indentCompiled $ valueDispatch allFuncs,
      return $ indentCompiled $ defineCategoryName ValueScope n,
      fmap indentCompiled $ concatM $ map (procedureDeclaration False . fst) (psProcedures vp),
      fmap indentCompiled $ concatM $ map (createMember r allFilters) vm,
      fmap indentCompiled $ createParams,
      return $ indentCompiled $ onlyCode $ "const S<" ++ typeName n ++ "> parent;",
      return $ indentCompiled $ onlyCodes $ traceCreation (psProcedures vp),
      return $ onlyCode "};"
    ]
  bottom <- concatM $ [
      return $ defineValue,
      defineInternalValue n internalCount memberCount
    ] ++
      applyProcedureScope compileExecutableProcedure cp ++
      applyProcedureScope compileExecutableProcedure tp ++
      applyProcedureScope compileExecutableProcedure vp
  commonDefineAll testing t ns rs top bottom ce te fe
  where
    disallowTypeMembers :: (Ord c, Show c, CollectErrorsM m) =>
      [DefinedMember c] -> m ()
    disallowTypeMembers tm =
      collectAllM_ $ flip map tm
        (\m -> compilerErrorM $ "Member " ++ show (dmName m) ++
                               " is not allowed to be @type-scoped" ++
                               formatFullContextBrace (dmContext m))
    createParams = concatM $ map createParam pi
    createParam p = return $ onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"
    -- TODO: Can probably remove this if @type members are disallowed. Or, just
    -- skip it if there are no @type members.
    getCycleCheck n2 = [
        "CycleCheck<" ++ n2 ++ ">::Check();",
        "CycleCheck<" ++ n2 ++ "> marker(*this);"
      ]
    categoryConstructor t ms2 = do
      ctx <- getContextForInit ta em t dd CategoryScope
      initMembers <- runDataCompiler (sequence $ map compileLazyInit ms2) ctx
      let initMembersStr = intercalate ", " $ cdOutput initMembers
      let initColon = if null initMembersStr then "" else " : "
      concatM [
          return $ onlyCode $ categoryName n ++ "()" ++ initColon ++ initMembersStr ++ " {",
          return $ indentCompiled $ onlyCodes $ getCycleCheck (categoryName n),
          return $ indentCompiled $ onlyCode $ startInitTracing n CategoryScope,
          return $ onlyCode "}",
          return $ clearCompiled initMembers -- Inherit required types.
        ]
    typeConstructor t ms2 = do
      let ps2 = map vpParam $ getCategoryParams t
      let argParent = categoryName n ++ "& p"
      let paramsPassed = "Params<" ++ show (length ps2) ++ ">::Type params"
      let allArgs = intercalate ", " [argParent,paramsPassed]
      let initParent = "parent(p)"
      let initPassed = map (\(i,p) -> paramName p ++ "(std::get<" ++ show i ++ ">(params))") $ zip ([0..] :: [Int]) ps2
      let allInit = intercalate ", " $ initParent:initPassed
      ctx <- getContextForInit ta em t dd TypeScope
      initMembers <- runDataCompiler (sequence $ map compileRegularInit ms2) ctx
      concatM [
          return $ onlyCode $ typeName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {",
          return $ indentCompiled $ onlyCodes $ getCycleCheck (typeName n),
          return $ indentCompiled $ onlyCode $ startInitTracing n TypeScope,
          return $ indentCompiled $ initMembers,
          return $ onlyCode "}"
        ]
    valueConstructor ms2 = do
      let argParent = "S<" ++ typeName n ++ "> p"
      let paramsPassed = "const ParamTuple& params"
      let argsPassed = "const ValueTuple& args"
      let allArgs = intercalate ", " [argParent,paramsPassed,argsPassed]
      let initParent = "parent(p)"
      let initParams = map (\(i,p) -> paramName (vpParam p) ++ "(params.At(" ++ show i ++ "))") $ zip ([0..] :: [Int]) pi
      let initArgs = map (\(i,m) -> variableName (dmName m) ++ "(" ++ unwrappedArg i m ++ ")") $ zip ([0..] :: [Int]) ms2
      let allInit = intercalate ", " $ initParent:(initParams ++ initArgs)
      return $ onlyCode $ valueName n ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"
    unwrappedArg i m = writeStoredVariable (dmType m) (UnwrappedSingle $ "args.At(" ++ show i ++ ")")
    createMember r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) <??
        "In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m)
      return $ onlyCode $ variableStoredType (dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    createMemberLazy r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) <??
        "In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m)
      return $ onlyCode $ variableLazyType (dmType m) ++ " " ++ variableName (dmName m) ++ ";"
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
          "const S<TypeInstance>& self, " ++
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
    traceCreation vp
      | any isTraceCreation $ concat $ map (epPragmas . snd) vp = [captureCreationTrace]
      | otherwise = []

commonDefineAll :: CollectErrorsM m =>
  Bool -> AnyCategory c -> Set.Set Namespace -> Maybe [ValueRefine c] ->
  CompiledData [String] -> CompiledData [String] -> CompiledData [String] ->
  CompiledData [String] -> [ScopedFunction c] -> m CxxOutput
commonDefineAll testing t ns rs top bottom ce te fe = do
  let filename = sourceFilename name
  (CompiledData req out) <- fmap (addNamespace t) $ concatM $ [
      return $ CompiledData (Set.fromList (name:getCategoryMentions t)) [],
      return $ mconcat [createCollection,createAllLabels]
    ] ++ conditionalContent
  let rs' = case rs of
                 Nothing -> []
                 Just rs2 -> rs2
  let guard = if testing
                 then testsOnlySourceGuard
                 else noTestsOnlySourceGuard
  let inherited = Set.unions $ (map (categoriesFromRefine . vrType) (getCategoryRefines t ++ rs')) ++
                               (map (categoriesFromDefine . vdType) $ getCategoryDefines t)
  let includes = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $
                   Set.toList $ Set.union req inherited
  return $ CxxOutput (Just $ getCategoryName t)
                     filename
                     (getCategoryNamespace t)
                     (getCategoryNamespace t `Set.insert` ns)
                     req
                     (guard ++ baseSourceIncludes ++ includes ++ out)
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
        commonDefineType t rs te,
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

createMainCommon :: String -> CompiledData [String] -> CompiledData [String] -> [String]
createMainCommon n (CompiledData req0 out0) (CompiledData req1 out1) =
  baseSourceIncludes ++ mainSourceIncludes ++ depIncludes (req0 `Set.union` req1) ++ out0 ++ [
      "int main(int argc, const char** argv) {",
      "  SetSignalHandler();",
      "  " ++ startFunctionTracing CategoryNone (FunctionName n)
    ] ++ map ("  " ++) out1 ++ ["}"] where
      depIncludes req2 = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $
                           Set.toList req2

generateMainFile :: (Ord c, Show c, CollectErrorsM m) =>
  CategoryMap c -> ExprMap c -> CategoryName -> FunctionName -> m (Namespace,[String])
generateMainFile tm em n f = "In the creation of the main binary procedure" ??> do
  ca <- compileMainProcedure tm em expr
  let file = noTestsOnlySourceGuard ++ createMainCommon "main" emptyCode (argv <> ca)
  (_,t) <- getConcreteCategory tm ([],n)
  return (getCategoryNamespace t,file) where
    funcCall = FunctionCall [] f (Positional []) (Positional [])
    mainType = JustTypeInstance $ TypeInstance n (Positional [])
    expr = Expression [] (TypeCall [] mainType funcCall) []
    argv = onlyCode "ProgramArgv program_argv(argc, argv);"

generateTestFile :: (Ord c, Show c, CollectErrorsM m) =>
  CategoryMap c -> ExprMap c  -> [String] -> [TestProcedure c] -> m (CompiledData [String])
generateTestFile tm em args ts = "In the creation of the test binary procedure" ??> do
  ts' <- fmap mconcat $ mapErrorsM (compileTestProcedure tm em) ts
  (include,sel) <- selectTestFromArgv1 $ map tpName ts
  let (CompiledData req _) = ts' <> sel
  let file = testsOnlySourceGuard ++ createMainCommon "testcase" (onlyCodes include <> ts') (argv <> sel)
  return $ CompiledData req file where
    args' = map escapeChars args
    argv = onlyCodes [
        "const char* argv2[] = { \"testcase\" " ++ concat (map (", " ++) args') ++ " };",
        "ProgramArgv program_argv(sizeof argv2 / sizeof(char*), argv2);"
      ]
