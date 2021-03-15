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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module CompilerCxx.Category (
  addNamespace,
  commonDefineCategory,
  commonDefineType,
  createFunctionDispatch,
  createLabelForFunction,
  declareGetCategory,
  declareGetType,
  declareInternalType,
  declareInternalValue,
  defineCategoryName,
  defineGetCatetory,
  defineGetType,
  defineInternalCategory,
  defineInternalType,
  defineInternalValue,
  defineTypeName,
  getCategoryMentions,
) where

import Data.List (intercalate)
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
import CompilerCxx.Code
import CompilerCxx.Naming
import CompilerCxx.Procedure
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


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
  dispatch (n2,_) = [
      "  if (label.collection == " ++ collectionName n2 ++ ") {",
      "    if (label.function_num < 0 || label.function_num >= sizeof " ++ tableName n2 ++ " / sizeof(CallType)) {",
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

commonDefineCategory :: CollectErrorsM m =>
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

commonDefineType :: CollectErrorsM m =>
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
