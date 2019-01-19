{-# LANGUAGE Safe #-}

module CompilerCxx (
) where

import Control.Monad (when)
import Control.Monad.Trans (lift)
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
    coLines :: [String]
  }

headerFilename :: TypeName -> String
headerFilename n = "Category_" ++ show n ++ ".hxx"

sourceFilename :: TypeName -> String
sourceFilename n = "Category_" ++ show n ++ ".cxx"

compileCategoryDefinition :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> DefinedCategory c -> m CxxOutput
compileCategoryDefinition tm (DefinedCategory c n ms ps fs) = do
  let filename = sourceFilename n
  (_,ca) <- getConcreteCategory tm (c,n)
  let filters = getFilterMap ca
  let r = categoriesToTypeResolver tm
  fa <- setInternalFunctions r filters (getCategoryFunctions ca) fs
  pa <- pairProceduresToFunctions fa ps
  ma <- mapMembers ms
  let t = TypeInstance (getCategoryName ca)
                       (ParamSet $ map (SingleType . JustParamName . vpParam) $ getCategoryParams ca)
  tx <- collectAllOrErrorM $ map (compileProcedure t tm filters fa ma) pa
  let types = Set.unions $ map fst tx
  -- TODO: The content also needs to include:
  --       - Includes for type headers.
  --       - Labels for all functions.
  --       - Dispatching of all functions.
  let content = concat $ map snd tx
  return $ CxxOutput filename content

compileProcedure :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  TypeInstance -> CategoryMap c -> ParamFilters ->
  Map.Map FunctionName (ScopedFunction c) ->
  Map.Map VariableName (DefinedMember c) ->
  (ScopedFunction c,ExecutableProcedure c) -> m (Set.Set TypeName,[String])
compileProcedure t tm pa fa ma
                 ((ScopedFunction c1 _ _ s as1 rs1 ps va ms),
                  (ExecutableProcedure c2 n as2 rs2 p)) = do
  as' <- processParamPairs alwaysPairParams as1 (avNames as2)
  rs' <- if isUnnamedReturns rs2
            then return $ Left rs1
            else fmap (Right . Map.fromList) $ processParamPairs pairOutput rs1 (nrNames rs2)
  vs <- updateVariables s ma rs1 rs2
  let ctx = ProcedureContext {
      pcType = t,
      pcCategories = tm,
      -- TODO: Needs to also pull in filters from ScopedFunction.
      pcFilters = pa,
      pcFunctions = fa,
      pcVariables = vs,
      pcReturns = rs',
      pcRequiredTypes = Set.fromList [tiName t],
      pcOutput = []
    }
  undefined
  where
    pairOutput (PassedValue c1 t) (OutputValue c2 n) = return $ (n,PassedValue (c2++c1) t)
