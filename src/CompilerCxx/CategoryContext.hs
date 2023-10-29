{- -----------------------------------------------------------------------------
Copyright 2019-2021,2023 Kevin P. Barry

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

import Base.CompilerError
import Base.GeneralType
import Base.Positional
import Compilation.CompilerState
import Compilation.ProcedureContext
import Compilation.ScopeContext
import CompilerCxx.Code (isStoredUnboxed)
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


getContextForInit :: (Show c, CollectErrorsM m) =>
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
  fm <- getFilterMap (pValues ps) pa
  let typeInstance = TypeInstance (getCategoryName t) $ fmap (singleType . JustParamName False . vpParam) ps
  let builtin = Map.filter ((== LocalScope) . vvScope) $ builtinVariables typeInstance
  let readOnly = Map.fromListWith (++) $ map (\m -> (dmName m,[])) $ dcMembers d
  members <- mapMembers readOnly Map.empty $ filter ((<= s) . dmScope) (dcMembers d)
  return $ ProcedureContext {
      _pcScope = s,
      _pcType = getCategoryName t,
      _pcExtParams = ps,
      _pcMembers = ms,
      _pcCategories = tm,
      _pcAllFilters = fm,
      _pcExtFilters = pa,
      _pcParamScopes = sa,
      _pcFunctions = fa,
      _pcVariables = Map.union builtin members,
      _pcReturns = ValidatePositions (Positional []),
      _pcDeferred = emptyDeferred,
      _pcJumpType = NextStatement,
      _pcIsNamed = False,
      _pcPrimNamed = [],
      _pcRequiredTypes = Set.empty,
      _pcOutput = [],
      _pcDisallowInit = True,
      _pcLoopSetup = NotInLoop,
      _pcCleanupBlocks = [],
      _pcInCleanup = False,
      _pcUsedVars = [],
      _pcExprMap = em,
      _pcReservedMacros = [],
      _pcNoTrace = False,
      _pcTraces = [],
      _pcParentCall = Nothing
    }

getProcedureContext :: (Show c, CollectErrorsM m) =>
  ScopeContext c -> ScopedFunction c -> ExecutableProcedure c -> m (ProcedureContext c)
getProcedureContext (ScopeContext tm t ps ms pa fa va em)
                    ff@(ScopedFunction _ _ _ s as1 rs1 ps1 fs _)
                    (ExecutableProcedure _ _ _ _ as2 rs2 _) = do
  rs' <- if isUnnamedReturns rs2
            then return $ ValidatePositions rs1
            else fmap (ValidateNames (fmap ovName $ nrNames rs2) rs1 . foldr (uncurry addDeferred) emptyDeferred) $ processPairs pairOutput rs1 (nrNames rs2)
  va' <- updateArgVariables va as1 as2
  va'' <- updateReturnVariables va' rs1 rs2
  let pa' = if s == CategoryScope
               then fs
               else pa ++ fs
  let localScopes = Map.fromList $ zip (map vpParam $ pValues ps1) (repeat LocalScope)
  let typeScopes = Map.fromList $ zip (map vpParam $ pValues ps) (repeat TypeScope)
  let sa = case s of
                CategoryScope -> localScopes
                TypeScope     -> Map.union typeScopes localScopes
                ValueScope    -> Map.union typeScopes localScopes
                _ -> undefined
  localFilters <- getFunctionFilterMap ff
  typeFilters <- getFilterMap (pValues ps) pa
  let allFilters = case s of
                   CategoryScope -> localFilters
                   TypeScope     -> Map.union localFilters typeFilters
                   ValueScope    -> Map.union localFilters typeFilters
                   _ -> undefined
  let ns0 = if isUnnamedReturns rs2
               then []
               else zipWith3 ReturnVariable [0..] (map ovName $ pValues $ nrNames rs2) (map pvType $ pValues rs1)
  let ns = filter (isStoredUnboxed . rvType) ns0
  return $ ProcedureContext {
      _pcScope = s,
      _pcType = t,
      _pcExtParams = ps,
      _pcMembers = ms,
      _pcCategories = tm,
      _pcAllFilters = allFilters,
      _pcExtFilters = pa',
      _pcParamScopes = sa,
      _pcFunctions = fa,
      _pcVariables = va'',
      _pcReturns = rs',
      _pcDeferred = emptyDeferred,
      _pcJumpType = NextStatement,
      _pcIsNamed = not (isUnnamedReturns rs2),
      _pcPrimNamed = ns,
      _pcRequiredTypes = Set.empty,
      _pcOutput = [],
      _pcDisallowInit = False,
      _pcLoopSetup = NotInLoop,
      _pcCleanupBlocks = [],
      _pcInCleanup = False,
      _pcUsedVars = [],
      _pcExprMap = em,
      _pcReservedMacros = [],
      _pcNoTrace = False,
      _pcTraces = [],
      _pcParentCall = parentCall
    }
  where
    pairOutput (PassedValue c1 t2) (OutputValue c2 n2) = return $ (n2,PassedValue (c2++c1) t2)
    psList = map vpParam $ pValues ps1
    asList = sequence $ map maybeArgName $ pValues $ avNames as2
    maybeArgName (InputValue _ n) = Just n
    maybeArgName _                = Nothing
    parentCall = case asList of
                      Just as3 -> Just (Positional psList,Positional as3)
                      Nothing  -> Nothing

getMainContext :: CollectErrorsM m => CategoryMap c -> ExprMap c -> m (ProcedureContext c)
getMainContext tm em = return $ ProcedureContext {
    _pcScope = LocalScope,
    _pcType = CategoryNone,
    _pcExtParams = Positional [],
    _pcMembers = [],
    _pcCategories = tm,
    _pcAllFilters = Map.empty,
    _pcExtFilters = [],
    _pcParamScopes = Map.empty,
    _pcFunctions = Map.empty,
    _pcVariables = Map.empty,
    _pcReturns = ValidatePositions (Positional []),
    _pcDeferred = emptyDeferred,
    _pcJumpType = NextStatement,
    _pcIsNamed = False,
    _pcPrimNamed = [],
    _pcRequiredTypes = Set.empty,
    _pcOutput = [],
    _pcDisallowInit = False,
    _pcLoopSetup = NotInLoop,
    _pcCleanupBlocks = [],
    _pcInCleanup = False,
    _pcUsedVars = [],
    _pcExprMap = em,
    _pcReservedMacros = [],
    _pcNoTrace = False,
    _pcTraces = [],
    _pcParentCall = Nothing
  }
