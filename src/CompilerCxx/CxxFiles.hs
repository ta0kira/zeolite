{- -----------------------------------------------------------------------------
Copyright 2019-2021,2023 Kevin P. Barry

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

{-# LANGUAGE CPP #-}

module CompilerCxx.CxxFiles (
  CxxOutput(..),
  FileContext(..),
  generateMainFile,
  generateNativeConcrete,
  generateNativeInterface,
  generateStreamlinedExtension,
  generateStreamlinedTemplate,
  generateTestFile,
  generateVerboseExtension,
) where

import Control.Arrow (second)
import Data.List (intercalate,sortBy)
import Data.Hashable (hash)
import Prelude hiding (pi)
import qualified Data.Map as Map
import qualified Data.Set as Set

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

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
import CompilerCxx.Procedure
import Module.CompileMetadata (CategorySpec(..))
import Types.Builtin
import Types.DefinedCategory
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
    coPossibleTraces :: Set.Set String,
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
  Bool -> Set.Set Namespace -> AnyCategory c -> m [CxxOutput]
generateNativeInterface testing ns t = do
  dec <- compileCategoryDeclaration testing ns t
  def <- generateCategoryDefinition testing (NativeInterface t)
  return (dec:def)

generateStreamlinedExtension :: (Ord c, Show c, CollectErrorsM m) =>
  FileContext c -> AnyCategory c -> CategorySpec c -> m [CxxOutput]
generateStreamlinedExtension (FileContext testing tm ns _) t spec = do
  dec <- compileCategoryDeclaration testing ns t
  def <- generateCategoryDefinition testing (StreamlinedExtension (getCategoryName t) tm ns spec)
  return (dec:def)

generateVerboseExtension :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> AnyCategory c -> m [CxxOutput]
generateVerboseExtension testing t =
  fmap (:[]) $ compileCategoryDeclaration testing Set.empty t

generateStreamlinedTemplate :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> CategoryMap c -> AnyCategory c -> CategorySpec c -> m [CxxOutput]
generateStreamlinedTemplate testing tm t spec =
  generateCategoryDefinition testing (StreamlinedTemplate (getCategoryName t) tm spec)

compileCategoryDeclaration :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> Set.Set Namespace -> AnyCategory c -> m CxxOutput
compileCategoryDeclaration testing ns t =
  return $ CxxOutput (Just $ getCategoryName t)
                     (headerFilename name)
                     (getCategoryNamespace t)
                     (getCategoryNamespace t `Set.insert` ns)
                     (cdRequired file)
                     (cdTraces file)
                     (cdOutput file) where
    file = mconcat $ [
        onlyDeps depends,
        onlyCodes guardTop,
        onlyCodes $ (if testing then testsOnlyCategoryGuard (getCategoryName t) else []),
        onlyCodes baseHeaderIncludes,
        addNamespace t content,
        onlyCodes guardBottom
      ]
    depends = getCategoryDeps t
    content = mconcat [categoryId,labels,getCategory2,getType]
    name = getCategoryName t
    guardTop = ["#ifndef " ++ guardName,"#define " ++ guardName]
    guardBottom = ["#endif  // " ++ guardName]
    guardName = "HEADER_" ++ guardNamespace ++ show name
    guardNamespace
      | isStaticNamespace $ getCategoryNamespace t = show (getCategoryNamespace t) ++ "_"
      | otherwise = ""
    functions = sortBy compareName $ filter ((== name) . sfType) $ getCategoryFunctions t
    compareName x y = sfName x `compare` sfName y
    categoryFunctions = filter ((== CategoryScope) . sfScope) functions
    typeFunctions = filter ((== TypeScope) . sfScope) functions
    valueFunctions = filter ((== ValueScope) . sfScope) functions
    labels = onlyCodes $ map label $ categoryFunctions ++ typeFunctions ++ valueFunctions
    label f = "extern " ++ functionLabelType f ++ " " ++ functionName f ++ ";"
    categoryId = onlyCodes [
        "static constexpr CategoryId " ++ categoryIdName name ++ " = " ++ show actualId ++ ";"
      ]
    actualId = abs $ hash $ show (getCategoryContext t) ++ show (getCategoryNamespace t) ++ show (getCategoryName t)
    getCategory2
      | isInstanceInterface t = emptyCode
      | otherwise             = declareGetCategory t
    getType
      | isInstanceInterface t = emptyCode
      | otherwise             = declareGetType t

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
    seType :: CategoryName,
    seCategories :: CategoryMap c,
    seNamespaces :: Set.Set Namespace,
    scSpec :: CategorySpec c
  } |
  StreamlinedTemplate {
    stName :: CategoryName,
    stCategories :: CategoryMap c,
    scSpec :: CategorySpec c
  }

generateCategoryDefinition :: (Ord c, Show c, CollectErrorsM m) =>
  Bool -> CategoryDefinition c -> m [CxxOutput]
generateCategoryDefinition testing = common where
  common :: (Ord c, Show c, CollectErrorsM m) => CategoryDefinition c -> m [CxxOutput]
  common (NativeInterface t) = fmap (:[]) singleSource where
    singleSource = do
      let filename = sourceFilename (getCategoryName t)
      let (cf,tf,vf) = partitionByScope sfScope $ getCategoryFunctions t
      (CompiledData req traces out) <- fmap (addNamespace t) $ concatM [
          defineFunctions t cf tf vf,
          declareInternalGetters t,
          defineInterfaceCategory t,
          defineInterfaceType     t,
          defineCategoryOverrides t [],
          defineTypeOverrides     t [],
          defineInternalGetters t,
          defineExternalGetters t
        ]
      let req' = req `Set.union` getCategoryMentions t
      return $ CxxOutput (Just $ getCategoryName t)
                         filename
                         (getCategoryNamespace t)
                         (Set.fromList [getCategoryNamespace t])
                         req'
                         traces
                         (allowTestsOnly $ addSourceIncludes $ addCategoryHeader t $ addIncludes req' out)
  common (StreamlinedExtension n ta ns (CategorySpec c rs ds)) = do
    ta' <- mergeInternalInheritance ta defined
    (_,t) <- getConcreteCategory ta' ([],n)
    sequence [streamlinedHeader t,streamlinedSource t] where
      defined = DefinedCategory {
          dcContext = c,
          dcPragmas = [],
          dcName = n,
          dcRefines = rs,
          dcDefines = ds,
          dcMembers = [],
          dcProcedures = [],
          dcFunctions = []
        }
      streamlinedHeader t = do
        let filename = headerStreamlined n
        let maybeValue = if hasPrimitiveValue n
                            then []
                            else [defineAbstractValue t]
        (CompiledData req traces out) <- fmap (addNamespace t) $ concatM $ [
            defineAbstractCategory t,
            return $ declareInternalType t (length $ getCategoryParams t),
            defineAbstractType t
          ] ++ maybeValue ++ [
            declareAbstractGetters t
          ]
        return $ CxxOutput (Just $ n)
                           filename
                           (getCategoryNamespace t)
                           (getCategoryNamespace t `Set.insert` ns)
                           req
                           traces
                           (headerGuard n $ allowTestsOnly $ addTemplateIncludes $ addCategoryHeader t $ addIncludes req out)
      streamlinedSource t = do
        let filename = sourceStreamlined n
        let (cf,tf,vf) = partitionByScope sfScope $ getCategoryFunctions t
        let maybeValue = if hasPrimitiveValue n
                            then []
                            else [defineValueOverrides t (getCategoryFunctions t)]
        (CompiledData req traces out) <- fmap (addNamespace t) $ concatM $ [
            defineFunctions t cf tf vf,
            defineCategoryOverrides t (getCategoryFunctions t),
            defineTypeOverrides     t (getCategoryFunctions t)
          ] ++ maybeValue ++ [
            defineExternalGetters t
          ]
        let req' = Set.unions [req,getCategoryMentions t]
        return $ CxxOutput (Just $ n)
                           filename
                           (getCategoryNamespace t)
                           (getCategoryNamespace t `Set.insert` ns)
                           req'
                           traces
                           (addSourceIncludes $ addStreamlinedHeader t $ addIncludes req' out)
  common (StreamlinedTemplate n tm (CategorySpec c rs ds)) = fmap (:[]) streamlinedTemplate where
    streamlinedTemplate = do
      tm' <- mergeInternalInheritance tm defined0
      (_,t) <- getConcreteCategory tm' ([],n)
      [cp,tp,vp] <- getProcedureScopes tm' Map.empty (defined $ getCategoryFunctions t)
      let maybeGetter = if hasPrimitiveValue (getCategoryName t)
                           then []
                           else [declareCustomValueGetter t]
      let maybeGetter2 = if hasPrimitiveValue (getCategoryName t)
                            then []
                            else [defineCustomValueGetter t]
      let maybeValue = if hasPrimitiveValue (getCategoryName t)
                          then []
                          else [defineCustomValue t vp]
      (CompiledData req traces out) <- fmap (addNamespace t) $ concatM $
        maybeGetter ++ [
          defineCustomCategory t cp,
          defineCustomType     t tp
        ] ++ maybeValue ++ [
          defineCustomGetters t
        ] ++ maybeGetter2
      let req' = Set.unions [req,getCategoryMentions t]
      return $ CxxOutput (Just $ getCategoryName t)
                         filename
                         (getCategoryNamespace t)
                         (Set.fromList [getCategoryNamespace t])
                         req'
                         traces
                         (addTemplateIncludes $ addStreamlinedHeader t $ addIncludes req' out)
    filename = templateStreamlined n
    defined0 = DefinedCategory {
        dcContext = c,
        dcPragmas = [],
        dcName = n,
        dcRefines = rs,
        dcDefines = ds,
        dcMembers = [],
        dcProcedures = [],
        dcFunctions = []
      }
    defined fs = DefinedCategory {
        dcContext = [],
        dcPragmas = [],
        dcName = n,
        dcRefines = rs,
        dcDefines = ds,
        dcMembers = [],
        dcProcedures = map defaultFail fs,
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
    failProcedure f = Procedure [] $ [
        asLineComment $ "TODO: Implement " ++ functionDebugName n f ++ "."
      ] ++ map asLineComment (formatFunctionTypes f) ++ [
        RawFailCall (functionDebugName n f ++ " is not implemented (see " ++ filename ++ ")")
      ]
    asLineComment = NoValueExpression [] . LineComment
  common (NativeConcrete t d@(DefinedCategory _ _ _ _ _ ms _ _) ta ns em) = fmap (:[]) singleSource where
    singleSource = do
      let filename = sourceFilename (getCategoryName t)
      ta' <- mergeInternalInheritance ta d
      let r = CategoryResolver ta'
      [cp,tp,vp] <- getProcedureScopes ta' em d
      let (_,tm,_) = partitionByScope dmScope ms
      disallowTypeMembers tm
      params <- getCategoryParamSet t
      let cf = map fst $ psProcedures cp
      let tf = map fst $ psProcedures tp
      let vf = map fst $ psProcedures vp
      (CompiledData req traces out) <- fmap (addNamespace t) $ concatM [
          defineFunctions t cf tf vf,
          declareInternalGetters t,
          defineConcreteCategory r cf ta' em t d,
          defineConcreteType tf t,
          defineConcreteValue r params vf t d,
          defineCategoryOverrides t cf,
          defineTypeOverrides     t tf,
          defineValueOverrides    t vf,
          defineCategoryFunctions t cp,
          defineTypeFunctions     t tp,
          defineValueFunctions    t vp,
          defineInternalGetters t,
          defineExternalGetters t
        ]
      let req' = Set.unions [req,getCategoryMentions t]
      return $ CxxOutput (Just $ getCategoryName t)
                         filename
                         (getCategoryNamespace t)
                         (getCategoryNamespace t `Set.insert` ns)
                         req'
                         traces
                         (allowTestsOnly $ addSourceIncludes $ addCategoryHeader t $ addIncludes req' out)

  defineFunctions t cf tf vf = createAllLabels where
    name = getCategoryName t
    createAllLabels = return $ onlyCodes $ concat $ map createLabels [cf,tf,vf]
    createLabels = map (uncurry createLabelForFunction) . zip [0..] . sortBy compareName . filter ((== name) . sfType)
    compareName x y = sfName x `compare` sfName y

  declareInternalGetters t = concatM [
      return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ ";",
      return $ declareInternalCategory t,
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ ";",
      return $ declareInternalType t (length $ getCategoryParams t),
      return $ valueGetter
    ] where
      valueGetter
        | isValueConcrete t = mconcat [
            onlyCode $ "struct " ++ valueName (getCategoryName t) ++ ";",
            declareInternalValue t
          ]
        | otherwise = emptyCode
  defineInternalGetters t = concatM [
      return $ defineInternalCategory t,
      return $ defineInternalType t (length $ getCategoryParams t),
      return $ valueGetter
    ] where
      valueGetter
        | isValueConcrete t = defineInternalValue t
        | otherwise = emptyCode

  declareCustomValueGetter t = concatM [
      return $ declareInternalValue t
    ]
  defineCustomValueGetter t = concatM [
      return $ defineInternalValue2 (valueCustom $ getCategoryName t) t
    ]

  declareAbstractGetters t = concatM [
      return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ ";",
      return $ declareInternalCategory t,
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ ";",
      return $ declareInternalType t (length $ getCategoryParams t)
    ]

  defineExternalGetters t = concatM [
      return $ defineGetCatetory t,
      return $ defineGetType     t
    ]
  defineCustomGetters t = concatM [
      return $ defineInternalCategory2 (categoryCustom (getCategoryName t)) t,
      return $ defineInternalType2     (typeCustom     (getCategoryName t)) t (length $ getCategoryParams t)
    ]

  defineInterfaceCategory t = concatM [
      return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ " : public " ++ categoryBase ++ " {",
      return declareCategoryOverrides,
      return $ onlyCode "};"
    ]
  defineInterfaceType t = concatM [
      return $ onlyCode $ "struct " ++ typeName (getCategoryName t) ++ " : public " ++ typeBase ++ " {",
      fmap indentCompiled $ inlineTypeConstructor t,
      fmap indentCompiled $ inlineTypeDestructor False t,
      return declareTypeOverrides,
      return $ indentCompiled $ createParams $ getCategoryParams t,
      return $ onlyCode $ "  " ++ categoryName (getCategoryName t) ++ "& parent;",
      return $ onlyCode "};"
    ]

  defineConcreteCategory r fs tm em t d = concatM [
      return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ " : public " ++ categoryBase ++ " {",
      fmap indentCompiled $ inlineCategoryConstructor t d tm em,
      return declareCategoryOverrides,
      fmap indentCompiled $ concatM $ map (declareProcedure t False) fs,
      fmap indentCompiled $ concatM $ map (createMemberLazy r) members,
      return $ onlyCode "};"
    ] where
      members = filter ((== CategoryScope). dmScope) $ dcMembers d

  defineConcreteType fs t = concatM [
      return $ onlyCode $ "struct " ++ className ++ " : public " ++ typeBase ++ ", std::enable_shared_from_this<" ++ className ++ "> {",
      fmap indentCompiled $ inlineTypeConstructor t,
      fmap indentCompiled $ inlineTypeDestructor False t,
      fmap indentCompiled $ inlineTypeParamSelf t,
      return declareTypeOverrides,
      fmap indentCompiled $ concatM $ map (declareProcedure t False) fs,
      return $ indentCompiled $ createParams $ getCategoryParams t,
      return $ onlyCode $ "  " ++ categoryName (getCategoryName t) ++ "& parent;",
      return $ onlyCode "};"
    ] where
      className = typeName (getCategoryName t)

  defineConcreteValue r params fs t d = concatM [
      return $ onlyCode $ "struct " ++ valueName (getCategoryName t) ++ " : public " ++ valueBase ++ " {",
      fmap indentCompiled $ inlineValueConstructor t d,
      fmap indentCompiled $ inlineValueParamSelf t,
      fmap indentCompiled $ inlineFlatCleanup d,
      return declareValueOverrides,
      fmap indentCompiled $ concatM $ map (declareProcedure t False) fs,
      fmap indentCompiled $ concatM $ map (createMember r params t) members,
      return $ onlyCode $ "  const S<const " ++ typeName (getCategoryName t) ++ "> parent;",
      return $ onlyCodes traceCreation,
      return $ onlyCode "};"
    ] where
      members = filter ((== ValueScope). dmScope) $ dcMembers d
      procedures = dcProcedures d
      traceCreation
        | any isTraceCreation $ concat $ map epPragmas procedures = [captureCreationTrace $ getCategoryName t]
        | otherwise = []

  defineAbstractCategory t = concatM [
      return $ onlyCode $ "struct " ++ categoryName (getCategoryName t) ++ " : public " ++ categoryBase ++ " {",
      return declareCategoryOverrides,
      fmap indentCompiled $ concatM $ map (declareProcedure t True) $ filter ((== CategoryScope). sfScope) $ getCategoryFunctions t,
      return $ onlyCode $ "  virtual inline ~" ++ categoryName (getCategoryName t) ++ "() {}",
      return $ onlyCode "};"
    ]

  defineAbstractType t = concatM [
      return $ onlyCode $ "struct " ++ className ++ " : public " ++ typeBase ++ ", std::enable_shared_from_this<" ++ className ++ "> {",
      fmap indentCompiled $ inlineTypeConstructor t,
      fmap indentCompiled $ inlineTypeDestructor True t,
      fmap indentCompiled $ inlineTypeParamSelf t,
      return declareTypeOverrides,
      fmap indentCompiled $ concatM $ map (declareProcedure t True) $ filter ((== TypeScope). sfScope) $ getCategoryFunctions t,
      return $ indentCompiled $ createParams $ getCategoryParams t,
      return $ onlyCode $ "  " ++ categoryName (getCategoryName t) ++ "& parent;",
      return $ onlyCode "};"
    ] where
      className = typeName (getCategoryName t)

  defineAbstractValue t = concatM [
      return $ onlyCode $ "struct " ++ valueName (getCategoryName t) ++ " : public " ++ valueBase ++ " {",
      fmap indentCompiled $ abstractValueConstructor t,
      fmap indentCompiled $ inlineValueParamSelf t,
      return declareValueOverrides,
      fmap indentCompiled $ concatM $ map (declareProcedure t True) $ filter ((== ValueScope). sfScope) $ getCategoryFunctions t,
      return $ onlyCode $ "  virtual inline ~" ++ valueName (getCategoryName t) ++ "() {}",
      return $ onlyCode $ "  const S<const " ++ typeName (getCategoryName t) ++ "> parent;",
      return $ onlyCode "};"
    ]

  defineCustomCategory :: (Ord c, Show c, CollectErrorsM m) => AnyCategory c -> ProcedureScope c -> m (CompiledData [String])
  defineCustomCategory t ps = concatM [
      return $ onlyCode $ "struct " ++ categoryCustom (getCategoryName t) ++ " : public " ++ categoryName (getCategoryName t) ++ " {",
      fmap indentCompiled $ concatM $ applyProcedureScope (defineProcedure t FinalInlineFunction) ps,
      return $ onlyCode "};"
    ]
  defineCustomType :: (Ord c, Show c, CollectErrorsM m) => AnyCategory c -> ProcedureScope c -> m (CompiledData [String])
  defineCustomType t ps = concatM [
      return $ onlyCode $ "struct " ++ typeCustom (getCategoryName t) ++ " : public " ++ typeName (getCategoryName t) ++ " {",
      fmap indentCompiled $ customTypeConstructor t,
      fmap indentCompiled $ concatM $ applyProcedureScope (defineProcedure t FinalInlineFunction) ps,
      return $ onlyCode "};"
    ]
  defineCustomValue :: (Ord c, Show c, CollectErrorsM m) => AnyCategory c -> ProcedureScope c -> m (CompiledData [String])
  defineCustomValue t ps = concatM [
      return $ onlyCode $ "struct " ++ valueCustom (getCategoryName t) ++ " : public " ++ valueName (getCategoryName t) ++ " {",
      fmap indentCompiled $ customValueConstructor t,
      fmap indentCompiled $ concatM $ applyProcedureScope (defineProcedure t FinalInlineFunction) ps,
      return $ onlyCode "};"
    ]

  defineCategoryFunctions :: (Ord c, Show c, CollectErrorsM m) => AnyCategory c -> ProcedureScope c -> m (CompiledData [String])
  defineCategoryFunctions t = concatM . applyProcedureScope (defineProcedure t $ OutOfLineFunction $ categoryName $ getCategoryName t)
  defineTypeFunctions :: (Ord c, Show c, CollectErrorsM m) => AnyCategory c -> ProcedureScope c -> m (CompiledData [String])
  defineTypeFunctions t = concatM . applyProcedureScope (defineProcedure t $ OutOfLineFunction $ typeName $ getCategoryName t)
  defineValueFunctions :: (Ord c, Show c, CollectErrorsM m) => AnyCategory c -> ProcedureScope c -> m (CompiledData [String])
  defineValueFunctions t = concatM . applyProcedureScope (defineProcedure t $ OutOfLineFunction $ valueName $ getCategoryName t)

  declareCategoryOverrides = onlyCodes [
      "  std::string CategoryName() const final;",
      "  ReturnTuple Dispatch(const CategoryFunction& label, const ParamsArgs& params_args) final;"
    ]
  declareTypeOverrides = onlyCodes [
      "  std::string CategoryName() const final;",
      "  void BuildTypeName(std::ostream& output) const final;",
      "  bool TypeArgsForParent(const CategoryId& category, std::vector<S<const TypeInstance>>& args) const final;",
      "  ReturnTuple Dispatch(const TypeFunction& label, const ParamsArgs& params_args) const final;",
      "  bool CanConvertFrom(const S<const TypeInstance>& from) const final;"
    ]
  declareValueOverrides = onlyCodes [
      "  std::string CategoryName() const final;",
      "  ReturnTuple Dispatch(const ValueFunction& label, const ParamsArgs& params_args) final;"
    ]

  defineCategoryOverrides t fs = return $ mconcat [
      onlyCode $ "std::string " ++ className ++ "::CategoryName() const { return \"" ++ show (getCategoryName t) ++ "\"; }",
      onlyCode $ "ReturnTuple " ++ className ++ "::Dispatch(const CategoryFunction& label, const ParamsArgs& params_args) {",
      createFunctionDispatch t CategoryScope fs,
      onlyCode "}"
    ] where
      className = categoryName (getCategoryName t)
  defineTypeOverrides t fs = do
    typeArgs <- createTypeArgsForParent t
    return $ mconcat [
        onlyCode $ "std::string " ++ className ++ "::CategoryName() const { return parent.CategoryName(); }",
        onlyCode $ "void " ++ className ++ "::BuildTypeName(std::ostream& output) const {",
        defineTypeName params,
        onlyCode "}",
        onlyCode $ "bool " ++ className ++ "::TypeArgsForParent(const CategoryId& category, std::vector<S<const TypeInstance>>& args) const {",
        typeArgs,
        onlyCode $ "}",
        onlyCode $ "ReturnTuple " ++ className ++ "::Dispatch(const TypeFunction& label, const ParamsArgs& params_args) const {",
        createFunctionDispatch t TypeScope fs,
        onlyCode $ "}",
        onlyCode $ "bool " ++ className ++ "::CanConvertFrom(const S<const TypeInstance>& from) const {",
        createCanConvertFrom t,
        onlyCode "}"
      ] where
      className = typeName (getCategoryName t)
      params = map vpParam $ getCategoryParams t
  defineValueOverrides t fs = return $ mconcat [
      onlyCode $ "std::string " ++ className ++ "::CategoryName() const { return parent->CategoryName(); }",
      onlyCode $ "ReturnTuple " ++ className ++ "::Dispatch(const ValueFunction& label, const ParamsArgs& params_args) {",
      createFunctionDispatch t ValueScope fs,
      onlyCode $ "}"
    ] where
      className = valueName (getCategoryName t)

  createMember r params t m = "In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m) ??> do
    m' <- replaceSelfMember (instanceFromCategory t) m
    validateGeneralInstance r params (vtType $ dmType m')
    return $ onlyCode $ variableStoredType (dmType m') ++ " " ++ variableName (dmName m') ++ ";"
  createMemberLazy r m = "In creation of " ++ show (dmName m) ++ " at " ++ formatFullContext (dmContext m) ??> do
    validateGeneralInstance r Set.empty (vtType $ dmType m)
    return $ onlyCode $ variableLazyType (dmType m) ++ " " ++ variableName (dmName m) ++ ";"

  createParams = mconcat . map createParam where
    createParam p = onlyCode $ paramType ++ " " ++ paramName (vpParam p) ++ ";"

  inlineCategoryConstructor t d tm em = do
    ctx <- getContextForInit testing tm em t d CategoryScope
    initMembers <- runDataCompiler (sequence $ map compileLazyInit members) ctx
    let initMembersStr = intercalate ", " $ cdOutput initMembers
    let initColon = if null initMembersStr then "" else " : "
    return $ mconcat [
        onlyCode $ "inline " ++ categoryName (getCategoryName t) ++ "()" ++ initColon ++ initMembersStr ++ " {",
        indentCompiled $ onlyCodes $ getCycleCheck (categoryName (getCategoryName t)),
        indentCompiled $ onlyCode $ startInitTracing (getCategoryName t) CategoryScope,
        onlyCode "}",
        clearCompiled initMembers -- Inherit required types.
      ] where
        members = filter ((== CategoryScope). dmScope) $ dcMembers d
  inlineTypeConstructor t = do
    let ps2 = map vpParam $ getCategoryParams t
    let argParent = categoryName (getCategoryName t) ++ "& p"
    let paramsPassed = "Params<" ++ show (length ps2) ++ ">::Type params"
    let allArgs = intercalate ", " [argParent,paramsPassed]
    let initParent = "parent(p)"
    let initPassed = map (\(i,p) -> paramName p ++ "(std::get<" ++ show i ++ ">(params))") $ zip ([0..] :: [Int]) ps2
    let allInit = intercalate ", " $ initParent:initPassed
    return $ mconcat [
        onlyCode $ "inline " ++ typeName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {",
        indentCompiled $ onlyCodes $ getCycleCheck (typeName (getCategoryName t)),
        indentCompiled $ onlyCode $ startInitTracing (getCategoryName t) TypeScope,
        onlyCode "}"
      ]
  inlineTypeDestructor abstract t = do
    let ps2 = map (paramName . vpParam) $ getCategoryParams t
    let params = "Params<" ++ show (length ps2) ++ ">::Type(" ++ intercalate "," ps2 ++ ")"
    let prefix = if abstract then "virtual " else "inline "
    return $ onlyCodes [
        prefix ++ "~" ++ typeName (getCategoryName t) ++ "() { " ++ typeRemover (getCategoryName t) ++ "(" ++ params ++ "); }"
      ]

  inlineFlatCleanup d = do
    let pragmas = filter isFlatCleanup $ dcPragmas d
    handle pragmas where
      handle [] = return emptyCode
      handle [FlatCleanup c v] = do
        let ms = filter ((== v) . dmName) members
        case ms of
             [m] -> return $ onlyCode $ "BoxedValue FlatCleanup() final { return std::move(" ++ variableName (dmName m) ++ "); }"
             _ -> compilerErrorM $ "FlatCleanup requires a non-weak boxed member" ++ formatFullContextBrace c
      handle ps = "Only one FlatCleanup is allowed" !!>
        (mapErrorsM $ map (\p -> "FlatCleanup using " ++ show (fcMember p) ++ formatFullContextBrace (fcContext p)) ps)
      members = filter ((/= WeakValue) . vtRequired . dmType) $
                filter (not . isStoredUnboxed . dmType) $
                filter ((== ValueScope) . dmScope) $ dcMembers d

  inlineValueConstructor t d = do
    let argParent = "S<const " ++ typeName (getCategoryName t) ++ "> p"
    let argsPassed = "const ParamsArgs& params_args"
    let allArgs = intercalate ", " [argParent,argsPassed]
    let initParent = "parent(std::move(p))"
    let initArgs = map (\(i,m) -> variableName (dmName m) ++ "(" ++ unwrappedArg i m ++ ")") $ zip ([0..] :: [Int]) members
    let allInit = intercalate ", " $ initParent:initArgs
    return $ onlyCode $ "inline " ++ valueName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}" where
      unwrappedArg i m = writeStoredVariable (dmType m) (UnwrappedSingle $ "params_args.GetArg(" ++ show i ++ ")")
      members = filter ((== ValueScope) . dmScope) $ dcMembers d

  abstractValueConstructor t = do
    let argParent = "S<const " ++ typeName (getCategoryName t) ++ "> p"
    let allArgs = intercalate ", " [argParent]
    let initParent = "parent(std::move(p))"
    let allInit = initParent
    return $ onlyCode $ "inline " ++ valueName (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"

  customTypeConstructor t = do
    let ps2 = map vpParam $ getCategoryParams t
    let argParent = categoryName (getCategoryName t) ++ "& p"
    let paramsPassed = "Params<" ++ show (length ps2) ++ ">::Type params"
    let allArgs = intercalate ", " [argParent,paramsPassed]
    let allInit = typeName (getCategoryName t) ++ "(p, params)"
    return $ onlyCode $ "inline " ++ typeCustom (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"

  customValueConstructor t = do
    let argParent = "S<const " ++ typeName (getCategoryName t) ++ "> p"
    let argsPassed = "const ParamsArgs& params_args"
    let allArgs = intercalate ", " [argParent,argsPassed]
    let allInit = valueName (getCategoryName t) ++ "(std::move(p))"
    return $ onlyCode $ "inline " ++ valueCustom (getCategoryName t) ++ "(" ++ allArgs ++ ") : " ++ allInit ++ " {}"

  inlineTypeParamSelf t = return $ onlyCodes [
      "inline S<const " ++ typeName (getCategoryName t) ++ "> Param_self() const {",
      "  return shared_from_this();",
      "}"
    ]

  inlineValueParamSelf t = return $ onlyCodes [
      "inline S<const " ++ typeName (getCategoryName t) ++ "> Param_self() const {",
      "  return parent;",
      "}"
    ]

  allowTestsOnly
    | testing   = (testsOnlySourceGuard ++)
    | otherwise = (noTestsOnlySourceGuard ++)
  addSourceIncludes = (baseSourceIncludes ++)
  addTemplateIncludes = (templateIncludes ++)
  addCategoryHeader t = (["#include \"" ++ headerFilename (getCategoryName t) ++ "\""] ++)
  addStreamlinedHeader t = (["#include \"" ++ headerStreamlined (getCategoryName t) ++ "\""] ++)
  addIncludes req = (map (\i -> "#include \"" ++ headerFilename i ++ "\"") (Set.toList req) ++)
  headerGuard t out = guardTop ++ out ++ guardBottom where
    guardTop = ["#ifndef " ++ guardName,"#define " ++ guardName]
    guardBottom = ["#endif  // " ++ guardName]
    guardName = "STREAMLINED_" ++ show t
  disallowTypeMembers :: (Ord c, Show c, CollectErrorsM m) => [DefinedMember c] -> m ()
  disallowTypeMembers tm =
    collectAllM_ $ flip map tm
      (\m -> compilerErrorM $ "Member " ++ show (dmName m) ++
                              " is not allowed to be @type-scoped" ++
                              formatFullContextBrace (dmContext m))
  getCycleCheck n2 = [
      "CycleCheck<" ++ n2 ++ ">::Check();",
      "CycleCheck<" ++ n2 ++ "> marker(*this);"
    ]

  defineProcedure t = compileExecutableProcedure testing (isImmutable t)
  declareProcedure t = procedureDeclaration (isImmutable t)

isImmutable :: AnyCategory c -> Bool
isImmutable = any isCategoryImmutable . getCategoryPragmas

formatFunctionTypes :: Show c => ScopedFunction c -> [String]
formatFunctionTypes (ScopedFunction c _ _ s _ as rs ps fa _) = [location,args,returns,params] ++ filters where
  location = show s ++ " function declared at " ++ formatFullContext c
  args    = "Arg Types:    (" ++ intercalate ", " (map singleArg $ pValues as)  ++ ")"
  returns = "Return Types: (" ++ intercalate ", " (map (show . pvType) $ pValues rs)  ++ ")"
  params  = "Type Params:  <" ++ intercalate ", " (map (show . vpParam) $ pValues ps) ++ ">"
  filters = map singleFilter fa
  singleFilter (ParamFilter _ n2 f) = "  " ++ show n2 ++ " " ++ show f
  singleArg (a,Just n) = show (pvType a) ++ " " ++ (calName n)
  singleArg (a,_)      = show (pvType a)

createMainCommon :: String -> CompiledData [String] -> CompiledData [String] -> [String]
createMainCommon n (CompiledData req0 _ out0) (CompiledData req1 _ out1) =
  baseSourceIncludes ++ mainSourceIncludes ++ depIncludes (req0 `Set.union` req1) ++ out0 ++ [
      "int main(int argc, const char** argv) {",
      "  SetSignalHandler();",
      "  " ++ startMainTracing n
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
  CategoryMap c -> ExprMap c  -> [String] -> Maybe ([c],TypeInstance) -> [TestProcedure c] ->
  m (CompiledData [String])
generateTestFile tm em args t ts = "In the creation of the test binary procedure" ??> do
  wrap <- case t of
               Just t2 -> compileWrapTestcase tm t2
               Nothing -> return emptyCode
  ts' <- fmap mconcat $ mapCompilerM (compileTestProcedure tm em) ts
  (include,sel) <- selectTestFromArgv1 $ map tpName ts
  let (CompiledData req traces _) = ts' <> sel
  let contentTop = mconcat [timeoutInclude,onlyCodes include,ts']
  let contentMain = mconcat [setTimeout,argv,callLog,wrap,sel]
  let file = testsOnlySourceGuard ++ createMainCommon "testcase" contentTop contentMain
  return $ CompiledData req traces file where
    args' = map escapeChars args
    argv = onlyCodes [
        "const char* argv2[] = { \"testcase\" " ++ concat (map (", " ++) args') ++ " };",
        "ProgramArgv program_argv(sizeof argv2 / sizeof(char*), argv2);"
      ]
    callLog = onlyCode "LogCallsToFile call_logger_((argc < 3)? \"\" : argv[2]);"
    timeoutInclude = onlyCodes [
        "#ifdef " ++ testTimeoutMacro,
        "#include <unistd.h>",
        "#endif  // " ++ testTimeoutMacro
      ]
    setTimeout = onlyCodes [
        "#ifdef " ++ testTimeoutMacro,
        "alarm(" ++ testTimeoutMacro ++ ");",
        "#endif  // " ++ testTimeoutMacro
      ]

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

createFunctionDispatch :: AnyCategory c -> SymbolScope -> [ScopedFunction c] -> CompiledData [String]
createFunctionDispatch t s fs = function where
  name = getCategoryName t
  function
    | null filtered = onlyCode fallback
    | otherwise = onlyCodes $ [typedef] ++ select
  filtered = filter ((== s) . sfScope) fs
  flatten f = f:(concat $ map flatten $ sfMerges f)
  flattened = concat $ map flatten filtered
  byCategory = map (second Set.toList) $ Map.toList $ Map.fromListWith Set.union $ map (\f -> (sfType f,Set.fromList [sfName f])) flattened
  typedef
    | s == CategoryScope = "  using CallType = ReturnTuple(" ++ categoryName name ++
                           "::*)(const ParamsArgs&);"
    | s == TypeScope     = "  using CallType = ReturnTuple(" ++ typeName name ++
                           "::*)(const ParamsArgs&) const;"
    | s == ValueScope    = "  using CallType = ReturnTuple(" ++ valueName name ++
                           "::*)(const ParamsArgs&)" ++ suffix ++ ";"
    | otherwise = undefined
  suffix
    | isImmutable t = " const"
    | otherwise     = ""
  funcName f
    | s == CategoryScope = categoryName name ++ "::" ++ callName f
    | s == TypeScope     = typeName name     ++ "::" ++ callName f
    | s == ValueScope    = valueName name    ++ "::" ++ callName f
    | otherwise = undefined
  select = [
      "  switch (label.collection) {"
    ] ++ categoryCases ++ [
      "    default:",
      "    " ++ fallback,
      "  }"
    ]
  categoryCases = concat $ map singleCase byCategory
  singleCase (n2,[f]) = [
      "    case " ++ categoryIdName n2 ++ ":",
      "      // " ++ show n2 ++ " only has one " ++ show s ++ " function.",
      "      return " ++ callName f ++ "(" ++ args ++ ");"
    ]
  singleCase (n2,fs2) = [
      "    case " ++ categoryIdName n2 ++ ":",
      "      static const CallType " ++ tableName n2 ++ "[] = {"
    ] ++ map (\f -> "        &" ++ funcName f ++ ",") fs2 ++ [
      "      };",
      "      return (this->*" ++ tableName n2 ++ "[label.function_num])(" ++ args ++ ");"
    ]
  args
    | s == CategoryScope = "params_args"
    | s == TypeScope     = "params_args"
    | s == ValueScope    = "params_args"
    | otherwise = undefined
  fallback
    | s == CategoryScope = "  return TypeCategory::Dispatch(label, params_args);"
    | s == TypeScope     = "  return TypeInstance::Dispatch(label, params_args);"
    | s == ValueScope    = "  return TypeValue::Dispatch(label, params_args);"
    | otherwise = undefined

createCanConvertFrom :: AnyCategory c -> CompiledData [String]
createCanConvertFrom t
  | isInstanceInterface t = onlyCode $ "  return " ++ typeBase ++ "::CanConvertFrom(from);"
  | otherwise = onlyCodes $ [
      "  std::vector<S<const TypeInstance>> args;",
      "  if (!from->TypeArgsForParent(" ++ categoryIdName (getCategoryName t) ++ ", args)) return false;",
      "  if(args.size() != " ++ show (length params) ++ ") {",
      "    FAIL() << \"Wrong number of args (\" << args.size() << \")  for \" << CategoryName();",
      "  }"
    ] ++ checks ++ ["  return true;"] where
      params = map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
      checks = concat $ map singleCheck $ zip ([0..] :: [Int]) params
      singleCheck (i,(p,Covariant))     = [checkCov i p]
      singleCheck (i,(p,Contravariant)) = [checkCon i p]
      singleCheck (i,(p,Invariant))     = [checkCov i p,checkCon i p]
      checkCov i p = "  if (!TypeInstance::CanConvert(args[" ++ show i ++ "], " ++ paramName p ++ ")) return false;"
      checkCon i p = "  if (!TypeInstance::CanConvert(" ++ paramName p ++ ", args[" ++ show i ++ "])) return false;"

createTypeArgsForParent :: CollectErrorsM m => AnyCategory c -> m (CompiledData [String])
createTypeArgsForParent t = do
  categoryCases <- fmap concat $ mapCompilerM singleCase (myType:refines)
  return $ onlyCodes $ [
      "  switch (category) {"
    ] ++ categoryCases ++ [
      "    default:",
      "      return false;",
      "  }"
    ] where
    params = map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
    self = singleFromCategory t
    myType = (getCategoryName t,map (singleType . JustParamName True . fst) params)
    refines = map (\r -> (tiName r,pValues $ tiParams r)) $ map vrType $ getCategoryRefines t
    singleCase (n2,ps) = do
      ps' <- mapCompilerM (reverseSelfInstance self) ps
      return [
          "    case " ++ categoryIdName n2 ++ ":",
          "      args = std::vector<S<const TypeInstance>>{" ++ intercalate ", " (map expandLocalType ps') ++ "};",
          "      return true;"
        ]

-- Similar to Procedure.expandGeneralInstance but doesn't account for scope.
expandLocalType :: GeneralInstance -> String
expandLocalType t
  | t == minBound = allGetter ++ "()"
  | t == maxBound = anyGetter ++ "()"
expandLocalType t = reduceMergeTree getAny getAll getSingle t where
  getAny ts = unionGetter     ++ combine ts
  getAll ts = intersectGetter ++ combine ts
  getSingle (JustTypeInstance (TypeInstance t2 ps)) =
    typeGetter t2 ++ "(Params<" ++ show (length $ pValues ps) ++ ">::Type(" ++ intercalate ", " (map expandLocalType $ pValues ps) ++ "))"
  getSingle (JustParamName _ p)  = paramName p
  getSingle (JustInferredType p) = paramName p
  combine ps = "(L_get<S<const " ++ typeBase ++ ">>(" ++ intercalate "," ps ++ "))"

defineCategoryName :: SymbolScope -> CategoryName -> CompiledData [String]
defineCategoryName TypeScope     _ = onlyCode $ "std::string CategoryName() const final { return parent.CategoryName(); }"
defineCategoryName ValueScope    _ = onlyCode $ "std::string CategoryName() const final { return parent->CategoryName(); }"
defineCategoryName _             t = onlyCode $ "std::string CategoryName() const final { return \"" ++ show t ++ "\"; }"

defineTypeName :: [ParamName] -> CompiledData [String]
defineTypeName ps = onlyCode $ "  return TypeInstance::TypeNameFrom(output, parent" ++ concat (map ((", " ++) . paramName) ps) ++ ");"

declareGetCategory :: AnyCategory c -> CompiledData [String]
declareGetCategory t = onlyCodes [categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "();"]

defineGetCatetory :: AnyCategory c -> CompiledData [String]
defineGetCatetory t = onlyCodes [
    categoryBase ++ "& " ++ categoryGetter (getCategoryName t) ++ "() {",
    "  return " ++ categoryCreator (getCategoryName t) ++ "();",
    "}"
  ]

declareGetType :: AnyCategory c -> CompiledData [String]
declareGetType t = onlyCodes [
    "S<const " ++ typeBase ++ "> " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $getCategoryParams t) ++ ">::Type params);"
  ]

defineGetType :: AnyCategory c -> CompiledData [String]
defineGetType t = onlyCodes [
    "S<const " ++ typeBase ++ "> " ++ typeGetter (getCategoryName t) ++ "(Params<" ++
            show (length $ getCategoryParams t) ++ ">::Type params) {",
    "  return " ++ typeCreator (getCategoryName t) ++ "(params);",
    "}"
  ]

declareInternalCategory :: AnyCategory c -> CompiledData [String]
declareInternalCategory t = onlyCodes [
    categoryName (getCategoryName t) ++ "& " ++ categoryCreator (getCategoryName t) ++ "();"
  ]

defineInternalCategory :: AnyCategory c -> CompiledData [String]
defineInternalCategory t = defineInternalCategory2 (categoryName (getCategoryName t)) t

defineInternalCategory2 :: String -> AnyCategory c -> CompiledData [String]
defineInternalCategory2 className t = onlyCodes [
    categoryName (getCategoryName t) ++ "& " ++ categoryCreator (getCategoryName t) ++ "() {",
    "  static auto& category = *new " ++ className ++ "();",
    "  return category;",
    "}"
  ]

declareInternalType :: AnyCategory c -> Int -> CompiledData [String]
declareInternalType t n = onlyCodes [
    "struct " ++ typeName (getCategoryName t) ++ ";",
    "S<const " ++ typeName (getCategoryName t) ++ "> " ++ typeCreator (getCategoryName t) ++
      "(const Params<" ++ show n ++ ">::Type& params);",
    "void " ++ typeRemover (getCategoryName t) ++
      "(const Params<" ++ show n ++ ">::Type& params);"
  ]

defineInternalType :: AnyCategory c -> Int -> CompiledData [String]
defineInternalType t = defineInternalType2 (typeName (getCategoryName t)) t

defineInternalType2 :: String -> AnyCategory c -> Int -> CompiledData [String]
defineInternalType2 className t n
  | n < 1 =
      onlyCodes [
        "S<const " ++ typeName (getCategoryName t) ++ "> " ++ typeCreator (getCategoryName t) ++ "(const Params<" ++ show n ++ ">::Type& params) {",
        "  static const auto cached = S_get(new " ++ className ++ "(" ++ categoryCreator (getCategoryName t) ++ "(), Params<" ++ show n ++ ">::Type()));",
        "  return cached;",
        "}",
        "void " ++ typeRemover (getCategoryName t) ++ "(const Params<" ++ show n ++ ">::Type& params) {}"
      ]
  | otherwise =
      onlyCodes [
        "static auto& " ++ cacheName ++ " = *new InstanceCache<" ++ show n ++ ", " ++ typeName (getCategoryName t) ++ ">([](const Params<" ++ show n ++ ">::Type& params) {",
        "    return S_get(new " ++ className ++ "(" ++ categoryCreator (getCategoryName t) ++ "(), params));",
        "  });",
        "S<const " ++ typeName (getCategoryName t) ++ "> " ++ typeCreator (getCategoryName t) ++ "(const Params<" ++ show n ++ ">::Type& params) {",
        "  return " ++ cacheName ++ ".GetOrCreate(params);",
        "}",
        "void " ++ typeRemover (getCategoryName t) ++ "(const Params<" ++ show n ++ ">::Type& params) {",
        "  " ++ cacheName ++ ".Remove(params);",
        "}"
      ] where
        cacheName = show (getCategoryName t) ++ "_instance_cache"

declareInternalValue :: AnyCategory c -> CompiledData [String]
declareInternalValue t =
  onlyCodes [
      "BoxedValue " ++ valueCreator (getCategoryName t) ++
      "(S<const " ++ typeName (getCategoryName t) ++ "> parent, " ++
      "const ParamsArgs& params_args);"
    ]

defineInternalValue :: AnyCategory c -> CompiledData [String]
defineInternalValue t = defineInternalValue2 (valueName (getCategoryName t)) t

defineInternalValue2 :: String -> AnyCategory c -> CompiledData [String]
defineInternalValue2 className t =
  onlyCodes [
      "BoxedValue " ++ valueCreator (getCategoryName t) ++
      "(S<const " ++ typeName (getCategoryName t) ++ "> parent, " ++
      "const ParamsArgs& params_args) {",
      "  return BoxedValue::New<" ++ className ++ ">(std::move(parent), params_args);",
      "}"
    ]

getCategoryMentions :: AnyCategory c -> Set.Set CategoryName
getCategoryMentions t = Set.fromList $ fromRefines (getCategoryRefines t) ++
                                       fromDefines (getCategoryDefines t) ++
                                       fromFunctions (getCategoryFunctions t) ++
                                       fromFilters (getCategoryFilters t) where
  fromRefines rs = Set.toList $ Set.unions $ map (categoriesFromRefine . vrType) rs
  fromDefines ds = Set.toList $ Set.unions $ map (categoriesFromDefine . vdType) ds
  fromDefine (DefinesInstance d ps) = d:(fromGenerals $ pValues ps)
  fromFunctions fs = concat $ map fromFunction fs
  fromFunction (ScopedFunction _ _ t2 _ _ as rs _ fs _) =
    [t2] ++ (fromGenerals $ map (vtType . pvType) (map fst (pValues as) ++ pValues rs)) ++ fromFilters fs
  fromFilters fs = concat $ map (fromFilter . pfFilter) fs
  fromFilter (TypeFilter _ t2)  = Set.toList $ categoriesFromTypes t2
  fromFilter (DefinesFilter t2) = fromDefine t2
  fromFilter ImmutableFilter = []
  fromGenerals = Set.toList . Set.unions . map categoriesFromTypes
