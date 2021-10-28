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

module CompilerCxx.Naming (
  allGetter,
  anyGetter,
  baseHeaderIncludes,
  baseSourceIncludes,
  callName,
  categoryCreator,
  categoryCustom,
  categoryGetter,
  categoryName,
  collectionName,
  functionDebugName,
  functionName,
  headerFilename,
  headerStreamlined,
  hiddenVariableName,
  initializerName,
  intersectGetter,
  mainFilename,
  mainSourceIncludes,
  paramName,
  privateNamespace,
  privateNamespaceMacro,
  publicNamespace,
  publicNamespaceMacro,
  qualifiedTypeGetter,
  sourceFilename,
  sourceStreamlined,
  tableName,
  templateIncludes,
  templateStreamlined,
  testFilename,
  testFunctionName,
  testTimeoutMacro,
  typeCreator,
  typeCustom,
  typeGetter,
  typeName,
  unionGetter,
  valueCreator,
  valueCustom,
  valueName,
  variableName,
) where

import Data.Hashable (Hashable,hash)
import Numeric (showHex)

import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


headerFilename :: CategoryName -> String
headerFilename n = "Category_" ++ show n ++ ".hpp"

sourceFilename :: CategoryName -> String
sourceFilename n = "Category_" ++ show n ++ ".cpp"

headerStreamlined :: CategoryName -> String
headerStreamlined n = "Streamlined_" ++ show n ++ ".hpp"

sourceStreamlined :: CategoryName -> String
sourceStreamlined n = "Streamlined_" ++ show n ++ ".cpp"

templateStreamlined :: CategoryName -> String
templateStreamlined n = "Extension_" ++ show n ++ ".cpp"

mainFilename :: String
mainFilename = "main.cpp"

testFilename :: String
testFilename = "test.cpp"

baseHeaderIncludes :: [String]
baseHeaderIncludes = ["#include \"category-header.hpp\""]

baseSourceIncludes :: [String]
baseSourceIncludes = ["#include \"category-source.hpp\"","#include \"internal.hpp\""]

templateIncludes :: [String]
templateIncludes = ["#include \"category-source.hpp\""]

mainSourceIncludes :: [String]
mainSourceIncludes = ["#include \"logging.hpp\""]

paramName :: ParamName -> String
paramName ParamSelf = "PARAM_SELF"
paramName p         = "Param_" ++ tail (show p) -- Remove leading '#'.

variableName :: VariableName -> String
variableName VariableSelf = "VAR_SELF"
variableName v            = "Var_" ++ show v

hiddenVariableName :: VariableName -> String
hiddenVariableName v = "Internal_" ++ show v

initializerName :: VariableName -> String
initializerName v = "Init_" ++ show v

categoryName :: CategoryName -> String
categoryName n = "Category_" ++ show n

categoryGetter :: CategoryName -> String
categoryGetter n = "GetCategory_" ++ show n

typeName :: CategoryName -> String
typeName n = "Type_" ++ show n

typeGetter :: CategoryName -> String
typeGetter n = "GetType_" ++ show n

intersectGetter :: String
intersectGetter = "Merge_Intersect"

unionGetter:: String
unionGetter = "Merge_Union"

allGetter :: String
allGetter = "GetMerged_All"

anyGetter :: String
anyGetter = "GetMerged_Any"

valueName :: CategoryName -> String
valueName n = "Value_" ++ show n

categoryCustom :: CategoryName -> String
categoryCustom n = "ExtCategory_" ++ show n

typeCustom :: CategoryName -> String
typeCustom n = "ExtType_" ++ show n

valueCustom :: CategoryName -> String
valueCustom n = "ExtValue_" ++ show n

callName :: FunctionName -> String
callName f = "Call_" ++ show f

functionDebugName :: CategoryName -> ScopedFunction c -> String
functionDebugName t f
  | sfScope f == CategoryScope = show t ++ ":" ++ show (sfName f)
  | otherwise                  = show t ++ "." ++ show (sfName f)

functionName :: ScopedFunction c -> String
functionName f = "Function_" ++ show (sfType f) ++ "_" ++ show (sfName f)

collectionName :: CategoryName -> String
collectionName n = "Functions_" ++ show n

testFunctionName :: FunctionName -> String
testFunctionName f = "Test_" ++ show f

tableName :: CategoryName -> String
tableName n = "Table_" ++ show n

categoryCreator :: CategoryName -> String
categoryCreator n = "CreateCategory_" ++ show n

typeCreator :: CategoryName -> String
typeCreator n = "CreateType_" ++ show n

valueCreator :: CategoryName -> String
valueCreator n = "CreateValue_" ++ show n

privateNamespace :: Hashable a => a -> String
privateNamespace = ("private_" ++) . flip showHex "" . abs . hash

publicNamespace :: Hashable a => a -> String
publicNamespace = ("public_" ++) . flip showHex "" . abs . hash

qualifiedTypeGetter :: AnyCategory c -> String
qualifiedTypeGetter t
  | isStaticNamespace $ getCategoryNamespace t =
    show (getCategoryNamespace t) ++ "::" ++ (typeGetter $ getCategoryName t)
  | otherwise = typeGetter $ getCategoryName t

testTimeoutMacro :: String
testTimeoutMacro = "ZEOLITE_TEST_TIMEOUT"

publicNamespaceMacro :: String
publicNamespaceMacro = "ZEOLITE_PUBLIC_NAMESPACE"

privateNamespaceMacro :: String
privateNamespaceMacro = "ZEOLITE_PRIVATE_NAMESPACE"
