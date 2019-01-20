{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerCxx (
  CxxOutput(..), -- TODO: Remove.
  CompiledData(..), -- TODO: Remove.
  compileCategoryDefinition,
) where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (execStateT)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CategoryCompiler
import CompilerState
import DefinedCategory
import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


data CxxOutput =
  CxxOutput {
    coFilename :: String,
    coFunctions :: [FunctionLabel],
    coOutput :: CompiledData
  }
  deriving (Show) -- TODO: Remove this.

data FunctionLabel =
  FunctionLabel {
    flName :: String,
    flScope :: SymbolScope,
    flParamCount :: Int,
    flArgCount :: Int,
    flReturnCount :: Int
  }
  deriving (Show) -- TODO: Remove this.

data CompiledData =
  CompiledData {
    cdRequired :: Set.Set TypeName,
    cdOutput :: [String]
  }
  deriving (Show) -- TODO: Remove this.

instance Mergeable CompiledData where
  mergeAny = foldr update (CompiledData Set.empty []) where
    update (CompiledData r1 o1) (CompiledData r2 o2) =
      CompiledData (Set.union r1 r2) (o1++o2)
  mergeAll = foldr update (CompiledData Set.empty []) where
    update (CompiledData r1 o1) (CompiledData r2 o2) =
      CompiledData (Set.union r1 r2) (o1++o2)

headerFilename :: TypeName -> String
headerFilename n = "Category_" ++ show n ++ ".hxx"

sourceFilename :: TypeName -> String
sourceFilename n = "Category_" ++ show n ++ ".cxx"

functionName :: ScopedFunction c -> String
functionName f = "Function_" ++ show (sfType f) ++ "_" ++ show (sfName f)

typeBase :: String
typeBase = "TypeInstance"

valueBase :: String
valueBase = "TypeValue"

paramType :: String
paramType = typeBase ++ "&"

variableType :: String
variableType = "S<" ++ valueBase ++ ">"

proxyType :: String
proxyType = variableType ++ "&"

paramName :: ParamName -> String
paramName p = "Param_" ++ tail (pnName p) -- Remove leading '`'.

variableName :: VariableName -> String
variableName v = "Var_" ++ show v

initializerName :: VariableName -> String
initializerName v = "Init_" ++ show v

callName :: ScopedFunction c -> String
callName f = "Call_" ++ show (sfType f) ++ "_" ++ show (sfName f)

categoryName :: TypeName -> String
categoryName n = "Category_" ++ show n

typeName :: TypeName -> String
typeName n = "Type_" ++ show n

valueName :: TypeName -> String
valueName n = "Value_" ++ show n

labelForFunction :: ScopedFunction c -> FunctionLabel
labelForFunction f = FunctionLabel name scope params args returns where
  name = functionName f
  scope = sfScope f
  params = length $ psParams $ sfParams f
  args = length $ psParams $ sfArgs f
  returns = length $ psParams $ sfReturns f

compileCategoryDefinition :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> DefinedCategory c -> m CxxOutput
compileCategoryDefinition tm (DefinedCategory c n ms ps fs) = do
  let filename = sourceFilename n
  (_,ca) <- getConcreteCategory tm (c,n)
  let filters = getCategoryFilterMap ca
  let r = categoriesToTypeResolver tm
  fa <- setInternalFunctions r filters (getCategoryFunctions ca) fs
  pa <- pairProceduresToFunctions fa ps
  ma <- mapMembers ms
  let t = TypeInstance (getCategoryName ca)
                       (ParamSet $ map (SingleType . JustParamName . vpParam) $ getCategoryParams ca)
  let (cp,tp,vp) = partitionByScope (sfScope . fst) pa
  output <- mergeAll [compileCategory t ms tm filters fa ma cp,
                      compileType     t ms tm filters fa ma tp,
                      compileValue    t ms tm filters fa ma vp]
  let labels = map labelForFunction $ filter ((== n) . sfType) $ Map.elems fa
  return $ CxxOutput filename labels output
  where
    compileCategory t ms tm filters fa ma cp = do
      let ms' = filter ((== CategoryScope) . dmScope) ms
      (CompiledData required output) <- mergeAll $ map (compileExecutableProcedure t tm filters fa ma) cp
      -- TODO: Add base class?
      let open = ["struct " ++ categoryName (tiName t) ++ " {"]
      -- TODO: Requires member initializers.
      let members = concat $ map createMember ms'
      let close = ["}"]
      return $ CompiledData required (open ++ indentCode (output ++ members) ++ close)
    compileType t ms tm filters fa ma tp = do
      let ms' = filter ((== TypeScope) . dmScope) ms
      (CompiledData required output) <- mergeAll $ map (compileExecutableProcedure t tm filters fa ma) tp
      let open = ["struct " ++ typeName (tiName t) ++ " : public " ++ typeBase ++ " {"]
      let parent = categoryName (tiName t) ++ "& parent;"
      -- TODO: Requires member initializers.
      let members = (parent:) $ concat $ map createMember ms'
      let params = concat $ map createParam $ map (jpnName . stType) $ psParams $ tiParams t
      let close = ["}"]
      return $ CompiledData required (open ++ indentCode (output ++ members ++ params) ++ close)
    compileValue t ms tm filters fa ma vp = do
      let ms' = filter ((== ValueScope) . dmScope) ms
      (CompiledData required output) <- mergeAll $ map (compileExecutableProcedure t tm filters fa ma) vp
      let open = ["struct " ++ valueName (tiName t) ++ " public " ++ valueBase ++ " {"]
      let parent = typeName (tiName t) ++ "& parent;"
      let members = (parent:) $ concat $ map createMember ms'
      let close = ["}"]
      return $ CompiledData required (open ++ indentCode (output ++ members) ++ close)
    createMember m = [variableType ++ " " ++ variableName (dmName m) ++ ";"]
    createParam p = [paramType ++ " " ++ paramName p ++ ";"]

runCompiler :: (Monad m, CompilerContext c m [String] a, Compiler a m b) =>
  b -> a -> m CompiledData
runCompiler x ctx = do
  ctx' <- execStateT (compile x) ctx
  required <- ccGetRequired ctx'
  output <- ccGetOutput ctx'
  return $ CompiledData  {
      cdRequired = required,
      cdOutput = output
    }

compileExecutableProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  TypeInstance -> CategoryMap c -> ParamFilters ->
  Map.Map FunctionName (ScopedFunction c) ->
  Map.Map VariableName (DefinedMember c) ->
  (ScopedFunction c,ExecutableProcedure c) -> m CompiledData
compileExecutableProcedure t tm pa fa ma
                 (ff@(ScopedFunction c1 _ _ s as1 rs1 ps va ms),
                  (ExecutableProcedure c2 n as2 rs2 p)) = do
  as' <- processParamPairs alwaysPairParams as1 (avNames as2)
  rs' <- if isUnnamedReturns rs2
            then return $ Left rs1
            else fmap (Right . Map.fromList) $ processParamPairs pairOutput rs1 (nrNames rs2)
  let va = filterMembers s ma
  va' <- updateArgVariables va as1 as2
  va'' <- updateReturnVariables va' rs1 rs2
  let pa' = if s == CategoryScope
               then getFunctionFilterMap ff
               else Map.union pa (getFunctionFilterMap ff)
  let ctx = ProcedureContext {
      pcType = t,
      pcCategories = tm,
      pcFilters = pa',
      pcFunctions = fa,
      pcVariables = va'',
      pcReturns = rs',
      pcRequiredTypes = Set.fromList [tiName t],
      pcOutput = []
    }
  (CompiledData required output) <- runCompiler p ctx
  return (CompiledData required (wrapProcedure (tiName t) ff as2 rs2 output))
  where
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)
    wrapProcedure n f as rs output = open ++ indentCode body ++ close where
      open = [header]
      body = defineParams ++ defineArgs ++ defineReturns ++ output
      close = ["}"]
      name = callName f
      header
        | sfScope f == ValueScope =
          "void " ++ name ++ "(Value self, " ++
          "Params<" ++ show (length $ psParams $ sfParams f) ++ ">::Type params, " ++
          "Args<" ++ show (length $ psParams $ sfArgs f) ++ ">::Type args, " ++
          "Returns<" ++ show (length $ psParams $ sfReturns f) ++ ">::Type returns) {"
        | otherwise =
          "void " ++ name ++ "(" ++
          "Params<" ++ show (length $ psParams $ sfParams f) ++ ">::Type params, " ++
          "Args<" ++ show (length $ psParams $ sfArgs f) ++ ">::Type args, " ++
          "Returns<" ++ show (length $ psParams $ sfReturns f) ++ ">::Type returns) {"
      defineParams = flip map (zip [0..] $ psParams $ sfParams f) $
        (\(i,p) -> paramType ++ " " ++ paramName (vpParam p) ++ " = std::get<" ++ show i ++ ">(params);")
      defineArgs = flip map (zip [0..] $ filter (not . isDiscardedInput) $ psParams $ avNames as) $
        (\(i,n) -> proxyType ++ " " ++ variableName (ivName n) ++ " = std::get<" ++ show i ++ ">(args);")
      defineReturns
        | isUnnamedReturns rs = []
        | otherwise = flip map (zip [0..] $ psParams $ nrNames rs) $
        (\(i,n) -> proxyType ++ " " ++ variableName (ovName n) ++ " = std::get<" ++ show i ++ ">(returns);")

indentCode = map ("  " ++)

instance (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  Compiler (ProcedureContext c) m (Procedure c) where
  compile (Procedure c ss) = do
    return ()
