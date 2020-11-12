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

module CompilerCxx.CategoryContext (
  getContextForInit,
  getMainContext,
  getProcedureContext,
) where

import Prelude hiding (pi)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Compilation.CompilerState
import Compilation.ProcedureContext
import Compilation.ScopeContext
import CompilerCxx.Code
import Types.DefinedCategory
import Types.GeneralType
import Types.Positional
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


getContextForInit :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> ExprMap c -> AnyCategory c -> DefinedCategory c ->
  SymbolScope -> m (ProcedureContext c)
getContextForInit tm em t d s = do
  let ps = Positional $ getCategoryParams t
  -- NOTE: This is always ValueScope for initializer checks.
  let ms = filter ((== ValueScope) . dmScope) $ dcMembers d
  let pa = if s == CategoryScope
              then []
              else getCategoryFilters t
  let sa = Map.fromList $ zip (map vpParam $ getCategoryParams t) (repeat TypeScope)
  let r = CategoryResolver tm
  fa <- setInternalFunctions r t (dcFunctions d)
  let typeInstance = TypeInstance (getCategoryName t) $ fmap (singleType . JustParamName False . vpParam) ps
  let builtin = Map.filter ((== LocalScope) . vvScope) $ builtinVariables typeInstance
  members <- mapMembers $ filter ((<= s) . dmScope) (dcMembers d)
  return $ ProcedureContext {
      pcScope = s,
      pcType = getCategoryName t,
      pcExtParams = ps,
      pcIntParams = Positional [],
      pcMembers = ms,
      pcCategories = tm,
      pcAllFilters = getFilterMap (pValues ps) pa,
      pcExtFilters = pa,
      pcIntFilters = [],
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = Map.union builtin members,
      pcReturns = ValidatePositions (Positional []),
      pcJumpType = NextStatement,
      pcIsNamed = False,
      pcPrimNamed = [],
      pcRequiredTypes = Set.empty,
      pcOutput = [],
      pcDisallowInit = True,
      pcLoopSetup = NotInLoop,
      pcCleanupSetup = [],
      pcInCleanup = False,
      pcExprMap = em,
      pcNoTrace = False
    }

getProcedureContext :: (Show c, CompileErrorM m, MergeableM m) =>
  ScopeContext c -> ScopedFunction c -> ExecutableProcedure c -> m (ProcedureContext c)
getProcedureContext (ScopeContext tm t ps pi ms pa fi fa va em)
                    ff@(ScopedFunction _ _ _ s as1 rs1 ps1 fs _)
                    (ExecutableProcedure _ _ _ _ as2 rs2 _) = do
  rs' <- if isUnnamedReturns rs2
            then return $ ValidatePositions rs1
            else fmap (ValidateNames (fmap ovName $ nrNames rs2) rs1 . Map.fromList) $ processPairs pairOutput rs1 (nrNames rs2)
  va' <- updateArgVariables va as1 as2
  va'' <- updateReturnVariables va' rs1 rs2
  let pa' = if s == CategoryScope
               then fs
               else pa ++ fs
  let localScopes = Map.fromList $ zip (map vpParam $ pValues ps1) (repeat LocalScope)
  let typeScopes = Map.fromList $ zip (map vpParam $ pValues ps) (repeat TypeScope)
  let valueScopes = Map.fromList $ zip (map vpParam $ pValues pi) (repeat ValueScope)
  let sa = case s of
                CategoryScope -> localScopes
                TypeScope -> Map.union typeScopes localScopes
                ValueScope -> Map.unions [localScopes,typeScopes,valueScopes]
                _ -> undefined
  let localFilters = getFunctionFilterMap ff
  let typeFilters = getFilterMap (pValues ps) pa
  let valueFilters = getFilterMap (pValues pi) fi
  let allFilters = case s of
                   CategoryScope -> localFilters
                   TypeScope -> Map.union localFilters typeFilters
                   ValueScope -> Map.unions [localFilters,typeFilters,valueFilters]
                   _ -> undefined
  let ns0 = if isUnnamedReturns rs2
               then []
               else zipWith3 ReturnVariable [0..] (map ovName $ pValues $ nrNames rs2) (map pvType $ pValues rs1)
  let ns = filter (isPrimType . rvType) ns0
  return $ ProcedureContext {
      pcScope = s,
      pcType = t,
      pcExtParams = ps,
      pcIntParams = pi,
      pcMembers = ms,
      pcCategories = tm,
      pcAllFilters = allFilters,
      pcExtFilters = pa',
      -- fs is duplicated so value initialization checks work properly.
      pcIntFilters = fi ++ fs,
      pcParamScopes = sa,
      pcFunctions = fa,
      pcVariables = va'',
      pcReturns = rs',
      pcJumpType = NextStatement,
      pcIsNamed = not (isUnnamedReturns rs2),
      pcPrimNamed = ns,
      pcRequiredTypes = Set.empty,
      pcOutput = [],
      pcDisallowInit = False,
      pcLoopSetup = NotInLoop,
      pcCleanupSetup = [],
      pcInCleanup = False,
      pcExprMap = em,
      pcNoTrace = False
    }
  where
    pairOutput (PassedValue c1 t2) (OutputValue c2 n2) = return $ (n2,PassedValue (c2++c1) t2)

getMainContext :: CompileErrorM m => CategoryMap c -> ExprMap c -> m (ProcedureContext c)
getMainContext tm em = return $ ProcedureContext {
    pcScope = LocalScope,
    pcType = CategoryNone,
    pcExtParams = Positional [],
    pcIntParams = Positional [],
    pcMembers = [],
    pcCategories = tm,
    pcAllFilters = Map.empty,
    pcExtFilters = [],
    pcIntFilters = [],
    pcParamScopes = Map.empty,
    pcFunctions = Map.empty,
    pcVariables = Map.empty,
    pcReturns = ValidatePositions (Positional []),
    pcJumpType = NextStatement,
    pcIsNamed = False,
    pcPrimNamed = [],
    pcRequiredTypes = Set.empty,
    pcOutput = [],
    pcDisallowInit = False,
    pcLoopSetup = NotInLoop,
    pcCleanupSetup = [],
    pcInCleanup = False,
    pcExprMap = em,
    pcNoTrace = False
  }
