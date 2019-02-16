{-# LANGUAGE Safe #-}

module CompilerCxx.Naming (
  baseHeaderIncludes,
  baseSourceIncludes,
  callName,
  categoryCreator,
  categoryGetter,
  categoryName,
  collectionName,
  functionName,
  headerFilename,
  initializerName,
  intersectGetter,
  paramName,
  sourceFilename,
  tableName,
  typeCreator,
  typeGetter,
  typeName,
  unionGetter,
  valueCreator,
  valueName,
  variableName,
) where

import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


headerFilename :: CategoryName -> String
headerFilename n = "Category_" ++ show n ++ ".hpp"

sourceFilename :: CategoryName -> String
sourceFilename n = "Category_" ++ show n ++ ".cpp"

baseHeaderIncludes :: [String]
baseHeaderIncludes = ["#include \"category-header.hpp\""]

baseSourceIncludes :: [String]
baseSourceIncludes = ["#include \"category-source.hpp\""]

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

valueName :: CategoryName -> String
valueName n = "Value_" ++ show n

callName :: FunctionName -> String
callName f = "Call_" ++ show f

functionName :: ScopedFunction c -> String
functionName f = "Function_" ++ show (sfType f) ++ "_" ++ show (sfName f)

collectionName :: CategoryName -> String
collectionName n = "Functions_" ++ show n

tableName :: CategoryName -> String
tableName n = "Table_" ++ show n

categoryCreator :: String
categoryCreator = "CreateCategory"

typeCreator :: String
typeCreator = "CreateType"

valueCreator :: String
valueCreator = "CreateValue"
