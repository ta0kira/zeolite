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
  CxxOutput(..),
  LanguageModule(..),
  PrivateSource(..),
  compileCategoryDeclaration,
  compileLanguageModule,
  compileConcreteDefinition,
  compileConcreteTemplate,
  compileInterfaceDefinition,
  compileModuleMain,
  compileTestsModule,
) where

import Control.Monad (foldM,when)
import Data.List (intercalate,sortBy)
import Prelude hiding (pi)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.MergeTree
import Compilation.CompilerState
import Compilation.ProcedureContext (ExprMap)
import Compilation.ScopeContext
import CompilerCxx.CategoryContext
import CompilerCxx.Code
import CompilerCxx.Naming
import CompilerCxx.Procedure
import Types.Builtin
import Types.DefinedCategory
import Types.GeneralType
import Types.Positional
import Types.Pragma
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


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

data LanguageModule c =
  LanguageModule {
    lmPublicNamespaces :: Set.Set Namespace,
    lmPrivateNamespaces :: Set.Set Namespace,
    lmLocalNamespaces :: Set.Set Namespace,
    lmPublicDeps :: [AnyCategory c],
    lmPrivateDeps :: [AnyCategory c],
    lmTestingDeps :: [AnyCategory c],
    lmPublicLocal :: [AnyCategory c],
    lmPrivateLocal :: [AnyCategory c],
    lmTestingLocal :: [AnyCategory c],
    lmExternal :: [CategoryName],
    lmStreamlined :: [CategoryName],
    lmExprMap :: ExprMap c
  }

data PrivateSource c =
  PrivateSource {
    psNamespace :: Namespace,
    psTesting :: Bool,
    psCategory :: [AnyCategory c],
    psDefine :: [DefinedCategory c]
  }

compileLanguageModule :: (Show c, CompileErrorM m) =>
  LanguageModule c -> [PrivateSource c] -> m [CxxOutput]
compileLanguageModule (LanguageModule ns0 ns1 ns2 cs0 ps0 ts0 cs1 ps1 ts1 ex ss em) xa = do
  checkSupefluous $ Set.toList $ (Set.fromList ex) `Set.difference` ca
  -- Check public sources up front so that error messages aren't duplicated for
  -- every source file.
  ta <- tmTesting
  xx1 <- fmap concat $ mapErrorsM (compileSourceP False tmPublic  nsPublic)  cs1
  xx2 <- fmap concat $ mapErrorsM (compileSourceP False tmPrivate nsPrivate) ps1
  xx3 <- fmap concat $ mapErrorsM (compileSourceP True  tmTesting nsTesting) ts1
  (ds,xx4) <- fmap mergeGeneratedX $ mapErrorsM compileSourceX xa
  xx5 <- fmap concat $ mapErrorsM (\s -> compileConcreteStreamlined (s `Set.member` testingCats) ta s) ss
  -- TODO: This should account for a name clash between a category declared in a
  -- TestsOnly .0rp and one declared in a non-TestOnly .0rx.
  let dm = mapByName ds
  checkDefined dm ex $ filter isValueConcrete (cs1 ++ ps1 ++ ts1)
  checkStreamlined
  return $ xx1 ++ xx2 ++ xx3 ++ xx4 ++ xx5 where
    testingCats = Set.fromList $ map getCategoryName ts1
    tmPublic  = foldM includeNewTypes defaultCategories [cs0,cs1]
    tmPrivate = tmPublic  >>= \tm -> foldM includeNewTypes tm [ps0,ps1]
    tmTesting = tmPrivate >>= \tm -> foldM includeNewTypes tm [ts0,ts1]
    nsPublic = ns0 `Set.union` ns2
    nsPrivate = ns1 `Set.union` nsPublic
    nsTesting = nsPrivate
    compileSourceP testing tm ns c = do
      tm' <- tm
      hxx <- compileCategoryDeclaration testing tm' ns c
      cxx <- if isValueConcrete c
                then return []
                else compileInterfaceDefinition testing c >>= return . (:[])
      return (hxx:cxx)
    compileSourceX (PrivateSource ns testing cs2 ds) = do
      tm <- if testing
               then tmTesting
               else tmPrivate
      let ns4 = if testing
                then nsTesting
                else nsPrivate
      let cs = if testing
                  then cs1 ++ ps1 ++ ts1
                  else cs1 ++ ps1
      checkLocals ds (ex ++ map getCategoryName (cs2 ++ cs))
      when testing $ checkTests ds (cs1 ++ ps1)
      let dm = mapByName ds
      checkDefined dm [] $ filter isValueConcrete cs2
      tm' <- includeNewTypes tm cs2
      -- Ensures that there isn't an inavertent collision when resolving
      -- dependencies for the module later on.
      tmTesting' <- tmTesting
      _ <- (includeNewTypes tmTesting' cs2)
      hxx <- mapErrorsM (compileCategoryDeclaration testing tm' ns4) cs2
      let interfaces = filter (not . isValueConcrete) cs2
      cxx1 <- mapErrorsM (compileInterfaceDefinition testing) interfaces
      cxx2 <- mapErrorsM (compileDefinition testing tm' (ns `Set.insert` ns4)) ds
      return (ds,hxx ++ cxx1 ++ cxx2)
    mergeGeneratedX ((ds,xx):xs2) = let (ds2,xx2) = mergeGeneratedX xs2 in (ds++ds2,xx++xx2)
    mergeGeneratedX _             = ([],[])
    compileDefinition testing tm ns4 d = do
      tm' <- mergeInternalInheritance tm d
      let refines = dcName d `Map.lookup` tm >>= return . getCategoryRefines
      compileConcreteDefinition testing tm' em ns4 refines d
    mapByName = Map.fromListWith (++) . map (\d -> (dcName d,[d]))
    ca = Set.fromList $ map getCategoryName $ filter isValueConcrete (cs1 ++ ps1 ++ ts1)
    checkLocals ds cs2 = mapErrorsM_ (checkLocal $ Set.fromList cs2) ds
    checkLocal cs2 d =
      when (not $ dcName d `Set.member` cs2) $
        compileErrorM ("Definition for " ++ show (dcName d) ++
                       formatFullContextBrace (dcContext d) ++
                       " does not correspond to a visible category in this module")
    checkTests ds ps = do
      let pa = Map.fromList $ map (\c -> (getCategoryName c,getCategoryContext c)) $ filter isValueConcrete ps
      mapErrorsM_ (checkTest pa) ds
    checkTest pa d =
      case dcName d `Map.lookup` pa of
           Nothing -> return ()
           Just c  ->
             compileErrorM ("Category " ++ show (dcName d) ++
                            formatFullContextBrace (dcContext d) ++
                            " was not declared as $TestsOnly$" ++ formatFullContextBrace c)
    checkDefined dm ex2 = mapErrorsM_ (checkSingle dm (Set.fromList ex2))
    checkSingle dm es t =
      case (getCategoryName t `Set.member` es, getCategoryName t `Map.lookup` dm) of
           (False,Just [_]) -> return ()
           (True,Nothing)   -> return ()
           (True,Just [d]) ->
             compileErrorM ("Category " ++ show (getCategoryName t) ++
                           formatFullContextBrace (getCategoryContext t) ++
                           " was declared external but is also defined at " ++ formatFullContext (dcContext d))
           (False,Nothing) ->
             compileErrorM ("Category " ++ show (getCategoryName t) ++
                           formatFullContextBrace (getCategoryContext t) ++
                           " has not been defined or declared external")
           (_,Just ds) ->
             ("Category " ++ show (getCategoryName t) ++
              formatFullContextBrace (getCategoryContext t) ++
              " is defined " ++ show (length ds) ++ " times") !!>
               (mapErrorsM_ (\d -> compileErrorM $ "Defined at " ++ formatFullContext (dcContext d)) ds)
    checkSupefluous es2
      | null es2 = return ()
      | otherwise = compileErrorM $ "External categories either not concrete or not present: " ++
                                    intercalate ", " (map show es2)
    checkStreamlined =  mapErrorsM_  streamlinedError $ Set.toList $ Set.difference (Set.fromList ss) (Set.fromList ex)
    streamlinedError n =
      compileErrorM $ "Category " ++ show n ++ " cannot be streamlined because it was not declared external"

compileTestsModule :: (Show c, CompileErrorM m) =>
  LanguageModule c -> Namespace -> [AnyCategory c] -> [DefinedCategory c] ->
  [TestProcedure c] -> m ([CxxOutput],CxxOutput,[FunctionName])
compileTestsModule cm ns cs ds ts = do
  let xs = PrivateSource {
      psNamespace = ns,
      psTesting = True,
      psCategory = cs,
      psDefine = ds
    }
  xx <- compileLanguageModule cm [xs]
  (main,fs) <- compileTestMain cm xs ts
  return (xx,main,fs)

compileTestMain :: (Show c, CompileErrorM m) =>
  LanguageModule c -> PrivateSource c -> [TestProcedure c] -> m (CxxOutput,[FunctionName])
compileTestMain (LanguageModule ns0 ns1 ns2 cs0 ps0 ts0 cs1 ps1 ts1 _ _ em) ts2 tests = do
  tm' <- tm
  (CompiledData req main) <- createTestFile tm' em tests
  return (CxxOutput Nothing testFilename NoNamespace (psNamespace ts2 `Set.insert` Set.unions [ns0,ns1,ns2]) req main,map tpName tests) where
  tm = foldM includeNewTypes defaultCategories [cs0,cs1,ps0,ps1,ts0,ts1,psCategory ts2]

compileModuleMain :: (Show c, CompileErrorM m) =>
  LanguageModule c -> [PrivateSource c] -> CategoryName -> FunctionName -> m CxxOutput
compileModuleMain (LanguageModule ns0 ns1 ns2 cs0 ps0 _ cs1 ps1 _ _ _ em) xa n f = do
  let resolved = filter (\d -> dcName d == n) $ concat $ map psDefine $ filter (not . psTesting) xa
  reconcile resolved
  tm' <- tm
  let cs = filter (\c -> getCategoryName c == n) $ concat $ map psCategory xa
  tm'' <- includeNewTypes tm' cs
  (ns,main) <- createMainFile tm'' em n f
  return $ CxxOutput Nothing mainFilename NoNamespace (ns `Set.insert` Set.unions [ns0,ns1,ns2]) (Set.fromList [n]) main where
    tm = foldM includeNewTypes defaultCategories [cs0,cs1,ps0,ps1]
    reconcile [_] = return ()
    reconcile []  = compileErrorM $ "No matches for main category " ++ show n ++ " ($TestsOnly$ sources excluded)"
    reconcile ds  =
      ("Multiple matches for main category " ++ show n) !!>
        mapErrorsM_ (\d -> compileErrorM $ "Defined at " ++ formatFullContext (dcContext d)) ds

compileCategoryDeclaration :: (Show c, CompileErrorM m) =>
  Bool -> CategoryMap c -> Set.Set Namespace -> AnyCategory c -> m CxxOutput
compileCategoryDeclaration testing _ ns t =
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

compileInterfaceDefinition :: CompileErrorM m => Bool -> AnyCategory c -> m CxxOutput
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

compileConcreteTemplate :: (Show c, CompileErrorM m) =>
  Bool -> CategoryMap c -> CategoryName -> m CxxOutput
compileConcreteTemplate testing ta n = do
  (_,t) <- getConcreteCategory ta ([],n)
  compileConcreteDefinition testing ta Map.empty Set.empty Nothing (defined t) <?? ("In generated template for " ++ show n) where
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

compileConcreteStreamlined :: (Show c, CompileErrorM m) =>
  Bool -> CategoryMap c -> CategoryName -> m [CxxOutput]
compileConcreteStreamlined testing ta n =  ("In streamlined compilation of " ++ show n) ??> do
  (_,t) <- getConcreteCategory ta ([],n)
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

compileConcreteDefinition :: (Show c, CompileErrorM m) =>
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
  cf <- collectAllM $ applyProcedureScope compileExecutableProcedure cp
  ce <- concatM [
      categoryConstructor t cm,
      categoryDispatch allFuncs,
      return $ mconcat $ map fst cf,
      concatM $ map (createMemberLazy r allFilters) cm
    ]
  tf <- collectAllM $ applyProcedureScope compileExecutableProcedure tp
  disallowTypeMembers tm
  te <- concatM [
      typeConstructor t tm,
      typeDispatch allFuncs,
      return $ mconcat $ map fst tf,
      concatM $ map (createMember r allFilters) tm
    ]
  vf <- collectAllM $ applyProcedureScope compileExecutableProcedure vp
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
      return $ indentCompiled $ mconcat $ map fst vf,
      fmap indentCompiled $ concatM $ map (createMember r allFilters) vm,
      fmap indentCompiled $ createParams,
      return $ indentCompiled $ onlyCode $ "const S<" ++ typeName n ++ "> parent;",
      return $ indentCompiled $ onlyCodes $ traceCreation (psProcedures vp),
      return $ onlyCode "};"
    ]
  bottom <- concatM $ [
      return $ defineValue,
      defineInternalValue n internalCount memberCount
    ] ++ map (return . snd) (cf ++ tf ++ vf)
  commonDefineAll testing t ns rs top bottom ce te fe
  where
    disallowTypeMembers :: (Show c, CompileErrorM m) =>
      [DefinedMember c] -> m ()
    disallowTypeMembers tm =
      collectAllM_ $ flip map tm
        (\m -> compileErrorM $ "Member " ++ show (dmName m) ++
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
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
      return $ onlyCode $ variableStoredType (dmType m) ++ " " ++ variableName (dmName m) ++ ";"
    createMemberLazy r filters m = do
      validateGeneralInstance r filters (vtType $ dmType m) <??
        ("In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m))
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

commonDefineAll :: CompileErrorM m =>
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

addNamespace :: AnyCategory c -> CompiledData [String] -> CompiledData [String]
addNamespace t cs
  | isStaticNamespace $ getCategoryNamespace t = mconcat [
      onlyCode $ "namespace " ++ show (getCategoryNamespace t) ++ " {",
      cs,
      onlyCode $ "}  // namespace " ++ show (getCategoryNamespace t),
      onlyCode $ "using namespace " ++ show (getCategoryNamespace t) ++ ";"
    ]
  | isPublicNamespace $ getCategoryNamespace t = mconcat [
      onlyCode $ "#ifdef " ++ publicNamespaceMacro,
      onlyCode $ "namespace " ++ publicNamespaceMacro ++ " {",
      onlyCode $ "#endif  // " ++ publicNamespaceMacro,
      cs,
      onlyCode $ "#ifdef " ++ publicNamespaceMacro,
      onlyCode $ "}  // namespace " ++ publicNamespaceMacro,
      onlyCode $ "using namespace " ++ publicNamespaceMacro ++ ";",
      onlyCode $ "#endif  // " ++ publicNamespaceMacro
    ]
  | isPrivateNamespace $ getCategoryNamespace t = mconcat [
      onlyCode $ "#ifdef " ++ privateNamespaceMacro,
      onlyCode $ "namespace " ++ privateNamespaceMacro ++ " {",
      onlyCode $ "#endif  // " ++ privateNamespaceMacro,
      cs,
      onlyCode $ "#ifdef " ++ privateNamespaceMacro,
      onlyCode $ "}  // namespace " ++ privateNamespaceMacro,
      onlyCode $ "using namespace " ++ privateNamespaceMacro ++ ";",
      onlyCode $ "#endif  // " ++ privateNamespaceMacro
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
                           "::*)(const S<TypeInstance>&, const ParamTuple&, const ValueTuple&);"
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
    | s == TypeScope     = "self, params, args"
    | s == ValueScope    = "self, params, args"
    | otherwise = undefined
  fallback
    | s == CategoryScope = "  return TypeCategory::Dispatch(label, params, args);"
    | s == TypeScope     = "  return TypeInstance::Dispatch(self, label, params, args);"
    | s == ValueScope    = "  return TypeValue::Dispatch(self, label, params, args);"
    | otherwise = undefined

commonDefineCategory :: CompileErrorM m =>
  AnyCategory c -> CompiledData [String] -> m (CompiledData [String])
commonDefineCategory t extra = do
  concatM $ [
      return $ onlyCode $ "struct " ++ categoryName name ++ " : public " ++ categoryBase ++ " {",
      return $ indentCompiled $ defineCategoryName CategoryScope name,
      return $ indentCompiled extra,
      return $ onlyCode "};"
    ]
  where
    name = getCategoryName t

commonDefineType :: CompileErrorM m =>
  AnyCategory c -> Maybe [ValueRefine c] -> CompiledData [String] -> m (CompiledData [String])
commonDefineType t rs extra = do
  let rs' = case rs of
                 Nothing -> getCategoryRefines t
                 Just rs2 -> rs2
  concatM [
      return $ CompiledData depends [],
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
      return $ indentCompiled $ defineCategoryName TypeScope name,
      return $ indentCompiled $ defineTypeName name (map vpParam $ getCategoryParams t),
      return $ indentCompiled $ onlyCode $ categoryName (getCategoryName t) ++ "& parent;",
      return $ indentCompiled createParams,
      return $ indentCompiled canConvertFrom,
      return $ indentCompiled $ typeArgsForParent rs',
      return $ indentCompiled extra,
      return $ onlyCode "};"
    ]
  where
    name = getCategoryName t
    depends = getCategoryDeps t
    createParams = mconcat $ map createParam $ getCategoryParams t
    createParam p = onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"
    canConvertFrom
      | isInstanceInterface t = emptyCode
      | otherwise = onlyCodes $ [
          "bool CanConvertFrom(const S<const TypeInstance>& from) const final {",
          -- TODO: This should be a typedef.
          "  std::vector<S<const TypeInstance>> args;",
          "  if (!from->TypeArgsForParent(parent, args)) return false;",
          -- TODO: Create a helper function for this error.
          "  if(args.size() != " ++ show (length params) ++ ") {",
          "    FAIL() << \"Wrong number of args (\" << args.size() << \")  for \" << CategoryName();",
          "  }"
        ] ++ checks ++ ["  return true;","}"]
    params = map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
    checks = concat $ map singleCheck $ zip ([0..] :: [Int]) params
    singleCheck (i,(p,Covariant))     = [checkCov i p]
    singleCheck (i,(p,Contravariant)) = [checkCon i p]
    singleCheck (i,(p,Invariant))     = [checkCov i p,checkCon i p]
    checkCov i p = "  if (!TypeInstance::CanConvert(args[" ++ show i ++ "], " ++ paramName p ++ ")) return false;"
    checkCon i p = "  if (!TypeInstance::CanConvert(" ++ paramName p ++ ", args[" ++ show i ++ "])) return false;"
    typeArgsForParent rs2
      | isInstanceInterface t = emptyCode
      | otherwise = onlyCodes $ [
          "bool TypeArgsForParent(" ++
          "const TypeCategory& category, " ++
          "std::vector<S<const TypeInstance>>& args) const final {"
        ] ++ allCats rs2 ++ ["  return false;","}"]
    myType = (getCategoryName t,map (singleType . JustParamName False . fst) params)
    refines rs2 = map (\r -> (tiName r,pValues $ tiParams r)) $ map vrType rs2
    allCats rs2 = concat $ map singleCat (myType:refines rs2)
    singleCat (t2,ps) = [
        "  if (&category == &" ++ categoryGetter t2 ++ "()) {",
        "    args = std::vector<S<const TypeInstance>>{" ++ expanded ++ "};",
        "    return true;",
        "  }"
      ]
      where
        expanded = intercalate ", " $ map expandLocalType ps

-- Similar to Procedure.expandGeneralInstance but doesn't account for scope.
expandLocalType :: GeneralInstance -> String
expandLocalType t
  | t == minBound = allGetter ++ "()"
  | t == maxBound = anyGetter ++ "()"
expandLocalType t = reduceMergeTree getAny getAll getSingle t where
  getAny ts = unionGetter     ++ combine ts
  getAll ts = intersectGetter ++ combine ts
  getSingle (JustTypeInstance (TypeInstance t2 ps)) =
    typeGetter t2 ++ "(T_get(" ++ intercalate ", " (map expandLocalType $ pValues ps) ++ "))"
  getSingle (JustParamName _ p)  = paramName p
  getSingle (JustInferredType p) = paramName p
  combine ps = "(L_get<" ++ typeBase ++ "*>(" ++ intercalate "," (map ("&" ++) ps) ++ "))"

defineCategoryName :: SymbolScope -> CategoryName -> CompiledData [String]
defineCategoryName TypeScope     _ = onlyCode $ "std::string CategoryName() const final { return parent.CategoryName(); }"
defineCategoryName ValueScope    _ = onlyCode $ "std::string CategoryName() const final { return parent->CategoryName(); }"
defineCategoryName _             t = onlyCode $ "std::string CategoryName() const final { return \"" ++ show t ++ "\"; }"

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
declareGetType t = ["S<" ++ typeBase ++ "> " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $getCategoryParams t) ++ ">::Type params);"]

defineGetType :: AnyCategory c -> [String]
defineGetType t = [
    "S<" ++ typeBase ++ "> " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
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
  return $ onlyCode $ "S<" ++ typeName t ++ "> " ++ typeCreator t ++
                      "(Params<" ++ show n ++ ">::Type params);"

defineInternalType :: Monad m =>
  CategoryName -> Int -> m (CompiledData [String])
defineInternalType t n
  | n < 1 =
      return $ onlyCodes [
        "S<" ++ typeName t ++ "> " ++ typeCreator t ++ "(Params<" ++ show n ++ ">::Type params) {",
        "  static const auto cached = S_get(new " ++ typeName t ++ "(" ++ categoryCreator t ++ "(), Params<" ++ show n ++ ">::Type()));",
        "  return cached;",
        "}"
      ]
  | otherwise =
      return $ onlyCodes [
        "S<" ++ typeName t ++ "> " ++ typeCreator t ++ "(Params<" ++ show n ++ ">::Type params) {",
        "  static auto& cache = *new WeakInstanceMap<" ++ show n ++ ", " ++ typeName t ++ ">();",
        "  static auto& cache_mutex = *new std::mutex;",
        "  std::lock_guard<std::mutex> lock(cache_mutex);",
        "  auto& cached = cache[GetKeyFromParams<" ++ show n ++ ">(params)];",
        "  S<" ++ typeName t ++ "> type = cached;",
        "  if (!type) { cached = type = S_get(new " ++ typeName t ++ "(" ++ categoryCreator t ++ "(), params)); }",
        "  return type;",
        "}"
      ]

declareInternalValue :: Monad m =>
  CategoryName -> Int -> Int -> m (CompiledData [String])
declareInternalValue t _ _ =
  return $ onlyCodes [
      "S<TypeValue> " ++ valueCreator t ++
      "(S<" ++ typeName t ++ "> parent, " ++
      "const ParamTuple& params, const ValueTuple& args);"
    ]

defineInternalValue :: Monad m =>
  CategoryName -> Int -> Int -> m (CompiledData [String])
defineInternalValue t _ _ =
  return $ onlyCodes [
      "S<TypeValue> " ++ valueCreator t ++ "(S<" ++ typeName t ++ "> parent, " ++
      "const ParamTuple& params, const ValueTuple& args) {",
      "  return S_get(new " ++ valueName t ++ "(parent, params, args));",
      "}"
    ]

createMainCommon :: String -> CompiledData [String] -> CompiledData [String] -> [String]
createMainCommon n (CompiledData req0 out0) (CompiledData req1 out1) =
  baseSourceIncludes ++ mainSourceIncludes ++ depIncludes (req0 `Set.union` req1) ++ out0 ++ [
      "int main(int argc, const char** argv) {",
      "  SetSignalHandler();",
      "  " ++ startFunctionTracing CategoryNone (FunctionName n)
    ] ++ map ("  " ++) out1 ++ ["}"] where
      depIncludes req2 = map (\i -> "#include \"" ++ headerFilename i ++ "\"") $
                           Set.toList req2

createMainFile :: (Show c, CompileErrorM m) =>
  CategoryMap c -> ExprMap c -> CategoryName -> FunctionName -> m (Namespace,[String])
createMainFile tm em n f = ("In the creation of the main binary procedure") ??> do
  ca <- compileMainProcedure tm em expr
  let file = noTestsOnlySourceGuard ++ createMainCommon "main" emptyCode (argv <> ca)
  (_,t) <- getConcreteCategory tm ([],n)
  return (getCategoryNamespace t,file) where
    funcCall = FunctionCall [] f (Positional []) (Positional [])
    mainType = JustTypeInstance $ TypeInstance n (Positional [])
    expr = Expression [] (TypeCall [] mainType funcCall) []
    argv = onlyCode "ProgramArgv program_argv(argc, argv);"

createTestFile :: (Show c, CompileErrorM m) =>
  CategoryMap c -> ExprMap c  -> [TestProcedure c] -> m (CompiledData [String])
createTestFile tm em ts = ("In the creation of the test binary procedure") ??> do
  ts' <- fmap mconcat $ mapErrorsM (compileTestProcedure tm em) ts
  (include,sel) <- selectTestFromArgv1 $ map tpName ts
  let (CompiledData req _) = ts' <> sel
  let file = testsOnlySourceGuard ++ createMainCommon "test" (onlyCodes include <> ts') (argv <> sel)
  return $ CompiledData req file where
    argv = onlyCodes [
        "const char* argv2[] = { \"testcase\" };",
        "ProgramArgv program_argv(sizeof argv2 / sizeof(char*), argv2);"
      ]

getCategoryMentions :: AnyCategory c -> [CategoryName]
getCategoryMentions t = fromRefines (getCategoryRefines t) ++
                        fromDefines (getCategoryDefines t) ++
                        fromFunctions (getCategoryFunctions t) ++
                        fromFilters (getCategoryFilters t) where
  fromRefines rs = Set.toList $ Set.unions $ map (categoriesFromRefine . vrType) rs
  fromDefines ds = Set.toList $ Set.unions $ map (categoriesFromDefine . vdType) ds
  fromDefine (DefinesInstance d ps) = d:(fromGenerals $ pValues ps)
  fromFunctions fs = concat $ map fromFunction fs
  fromFunction (ScopedFunction _ _ t2 _ as rs _ fs _) =
    [t2] ++ (fromGenerals $ map (vtType . pvType) (pValues as ++ pValues rs)) ++ fromFilters fs
  fromFilters fs = concat $ map (fromFilter . pfFilter) fs
  fromFilter (TypeFilter _ t2)  = Set.toList $ categoriesFromTypes t2
  fromFilter (DefinesFilter t2) = fromDefine t2
  fromGenerals = Set.toList . Set.unions . map categoriesFromTypes
