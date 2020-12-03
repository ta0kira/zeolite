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

{-# LANGUAGE Safe #-}

module CompilerCxx.Naming (
  allGetter,
  anyGetter,
  baseHeaderIncludes,
  baseSourceIncludes,
  callName,
  categoryCreator,
  categoryGetter,
  categoryName,
  collectionName,
  functionName,
  headerFilename,
  headerStreamlined,
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
  testFilename,
  testFunctionName,
  typeCreator,
  typeGetter,
  typeName,
  unionGetter,
  valueCreator,
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

mainFilename :: String
mainFilename = "main.cpp"

testFilename :: String
testFilename = "test.cpp"

baseHeaderIncludes :: [String]
baseHeaderIncludes = ["#include \"category-header.hpp\""]

baseSourceIncludes :: [String]
baseSourceIncludes = ["#include \"category-source.hpp\""]

mainSourceIncludes :: [String]
mainSourceIncludes = ["#include \"logging.hpp\""]

paramName :: ParamName -> String
paramName p = "Param_" ++ tail (pnName p) -- Remove leading '#'.

variableName :: VariableName -> String
variableName v = "Var_" ++ show v

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

callName :: FunctionName -> String
callName f = "Call_" ++ show f

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

publicNamespaceMacro :: String
publicNamespaceMacro = "ZEOLITE_PUBLIC_NAMESPACE"

privateNamespaceMacro :: String
privateNamespaceMacro = "ZEOLITE_PRIVATE_NAMESPACE"
