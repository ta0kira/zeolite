{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerCxx (
) where

import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import qualified Data.Set as Set

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
    coLines :: [String]
  }

compileCategoryDefinition :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> DefinedCategory c -> m CxxOutput
compileCategoryDefinition tm (DefinedCategory c n ms ps fs) = do
  -- TODO: Create helpers to come up with names.
  let filename = "Category_" ++ show n ++ ".cxx"
  (_,ca) <- getConcreteCategory tm (c,n)
  fa <- setInternalFunctions (getCategoryFunctions ca) fs
  pa <- pairProceduresToFunctions fa ps
  ma <- mapMembers ms
  let filters = getFilterMap ca
  tx <- collectAllOrErrorM $ map (compileProcedure tm filters fa ma) pa
  -- TODO: Turn types into includes.
  let types = Set.unions $ map fst tx
  let content = concat $ map snd tx
  return $ CxxOutput filename content

setInternalFunctions :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  [ScopedFunction c] -> [ScopedFunction c] -> m (Map.Map FunctionName (ScopedFunction c))
setInternalFunctions fs0 fs = do undefined

pairProceduresToFunctions :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  Map.Map FunctionName (ScopedFunction c) -> [ExecutableProcedure c] ->
  m [(ScopedFunction c,ExecutableProcedure c)]
pairProceduresToFunctions fa ps = do undefined

mapMembers :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  [DefinedMember c] -> m (Map.Map VariableName (DefinedMember c))
mapMembers ms = do undefined

updateVariables :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  SymbolScope -> Map.Map VariableName (DefinedMember c) ->
  ReturnValues c -> m (Map.Map VariableName (OutputType c))
updateVariables s ma rs = updated where
  updated
    | isUnnamedReturns rs = filtered
    | otherwise = do
      let ma' = filtered
      undefined
  filtered = do undefined

compileProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> ParamFilters -> Map.Map FunctionName (ScopedFunction c) ->
  Map.Map VariableName (DefinedMember c) ->
  (ScopedFunction c,ExecutableProcedure c) -> m (Set.Set TypeName,[String])
compileProcedure tm pa fa ma
                 ((ScopedFunction c1 _ t s as1 rs1 ps va ms),
                  (ExecutableProcedure c2 n as2 rs2 p)) = do
  as' <- processParamPairs alwaysPairParams as1 (avNames as2)
  rs' <- if isUnnamedReturns rs2
            then return $ Left rs1
            else fmap (Right . Map.fromList) $ processParamPairs pairOutput rs1 (nrNames rs2)
  vs <- updateVariables s ma rs2
  let ctx = ProcedureContext {
      pcCategories = tm,
      pcFilters = pa,
      pcFunctions = fa,
      pcVariables = vs,
      pcReturns = rs',
      pcRequiredTypes = Set.fromList [t],
      pcOutput = []
    }
  undefined
  where
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)

data OutputType c =
  OutputType {
    otContext :: [c],
    otType :: ValueType
  }

data ProcedureContext c =
  ProcedureContext {
    pcCategories :: CategoryMap c,
    pcFilters :: ParamFilters,
    pcFunctions :: Map.Map FunctionName (ScopedFunction c),
    pcVariables :: Map.Map VariableName (OutputType c),
    pcReturns :: Either (ParamSet (PassedValue c)) (Map.Map VariableName (PassedValue c)),
    pcRequiredTypes :: Set.Set TypeName,
    pcOutput :: [String]
  }

instance (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CompilerContext c m [String] (ProcedureContext c) where
  ccResolver = return . categoriesToTypeResolver . pcCategories
  ccAllFilters = return . pcFilters
  ccRequiresType = undefined
  ccGetFunction ctx c t n = undefined
  ccGetVariable ctx c n = undefined
  ccAddVariable ctx c n t = undefined
  ccWrite ctx ss = return $
    ProcedureContext {
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx ++ ss
    }
  ccGetOutput = return . pcOutput
  ccUpdateAssigned ctx n = undefined
  ccCheckReturn ctx c vs = undefined
