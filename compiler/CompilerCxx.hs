{-# LANGUAGE Safe #-}

module CompilerCxx (
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

data FunctionLabel =
  FunctionLabel {
    flName :: String,
    flScope :: SymbolScope,
    flParamCount :: Int,
    flArgCount :: Int,
    flReturnCount :: Int
  }

data CompiledData =
  CompiledData {
    cdRequired :: Set.Set TypeName,
    cdOutput :: [String]
  }

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
  output <- mergeAll $ map (compileExecutableProcedure t tm filters fa ma) pa
  let labels = map labelForFunction $ filter ((== n) . sfType) $ Map.elems fa
  return $ CxxOutput filename labels output

runCompiler :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  (ProcedureContext c) -> CompilerState (ProcedureContext c) m () -> m CompiledData
runCompiler ctx s = do
  ctx' <- execStateT s ctx
  reqired <- ccGetRequired ctx'
  output <- ccGetOutput ctx'
  return $ CompiledData  {
      cdRequired = reqired,
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
  vs <- updateVariables s ma rs1 rs2
  let ctx = ProcedureContext {
      pcType = t,
      pcCategories = tm,
      pcFilters = Map.union pa (getFunctionFilterMap ff),
      pcFunctions = fa,
      pcVariables = vs,
      pcReturns = rs',
      pcRequiredTypes = Set.fromList [tiName t],
      pcOutput = []
    }
  runCompiler ctx (compileProcedure p)
  where
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)

compileProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  Procedure c -> CompilerState (ProcedureContext c) m ()
compileProcedure (Procedure c ss) = do
  undefined
