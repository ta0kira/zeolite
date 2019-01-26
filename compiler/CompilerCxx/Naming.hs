{-# LANGUAGE Safe #-}

module CompilerCxx.Naming (
  baseHeaderIncludes,
  baseSourceIncludes,
  callName,
  categoryBase,
  categoryGetter,
  categoryName,
  functionLabelType,
  functionName,
  headerFilename,
  headerFilename,
  initializerName,
  internalCategory,
  intersectGetter,
  paramName,
  paramType,
  proxyType,
  sourceFilename,
  typeBase,
  typeCreator,
  typeGetter,
  typeName,
  unionGetter,
  valueBase,
  valueCreator,
  valueName,
  variableName,
  variableType,
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

categoryBase :: String
categoryBase = "TypeCategory"

typeBase :: String
typeBase = "TypeInstance"

valueBase :: String
valueBase = "TypeValue"

paramType :: String
paramType = typeBase ++ "&"

variableType :: StorageType -> String
variableType WeakValue = "W<" ++ valueBase ++ ">"
variableType _         = "S<" ++ valueBase ++ ">"

proxyType :: String
proxyType = "S<" ++ valueBase ++ ">&"

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

internalCategory :: CategoryName -> String
internalCategory n = "Internal_" ++ show n

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

callName :: CategoryName -> FunctionName -> String
callName t f = "Call_" ++ show t ++ "_" ++ show f

functionName :: ScopedFunction c -> String
functionName f = "Function_" ++ show (sfType f) ++ "_" ++ show (sfName f)

typeCreator :: String
typeCreator = "CreateType"

valueCreator :: String
valueCreator = "CreateValue"

functionLabelType :: ScopedFunction c -> String
functionLabelType f =
  "Function<" ++ scope ++ "," ++ show pn ++ "," ++ show an ++ "," ++ show rn ++ ">" where
    pn = length $ psParams $ sfParams f
    an = length $ psParams $ sfArgs f
    rn = length $ psParams $ sfReturns f
    scope
      | sfScope f == CategoryScope = "SymbolScope::CategoryScope"
      | sfScope f == TypeScope     = "SymbolScope::TypeScope"
      | sfScope f == ValueScope    = "SymbolScope::ValueScope"
