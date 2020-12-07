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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Compilation.ProcedureContext (
  ExprMap,
  ProcedureContext(..),
  ReturnValidation(..),
  updateArgVariables,
  updateReturnVariables,
) where

import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.MergeTree
import Compilation.CompilerState
import Types.DefinedCategory
import Types.GeneralType
import Types.Positional
import Types.Pragma (MacroName)
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


data ProcedureContext c =
  ProcedureContext {
    pcScope :: SymbolScope,
    pcType :: CategoryName,
    pcExtParams :: Positional (ValueParam c),
    pcIntParams :: Positional (ValueParam c),
    pcMembers :: [DefinedMember c],
    pcCategories :: CategoryMap c,
    pcAllFilters :: ParamFilters,
    pcExtFilters :: [ParamFilter c],
    pcIntFilters :: [ParamFilter c],
    pcParamScopes :: Map.Map ParamName SymbolScope,
    pcFunctions :: Map.Map FunctionName (ScopedFunction c),
    pcVariables :: Map.Map VariableName (VariableValue c),
    pcReturns :: ReturnValidation c,
    pcJumpType :: JumpType,
    pcIsNamed :: Bool,
    pcPrimNamed :: [ReturnVariable],
    pcRequiredTypes :: Set.Set CategoryName,
    pcOutput :: [String],
    pcDisallowInit :: Bool,
    pcLoopSetup :: LoopSetup [String],
    pcCleanupSetup :: [CleanupSetup (ProcedureContext c) [String]],
    pcInCleanup :: Bool,
    pcExprMap :: ExprMap c,
    pcReservedMacros :: [(MacroName,[c])],
    pcNoTrace :: Bool
  }

type ExprMap c = Map.Map MacroName (Expression c)

data ReturnValidation c =
  ValidatePositions {
    vpReturns :: Positional (PassedValue c)
  } |
  ValidateNames {
    vnNames :: Positional VariableName,
    vnTypes :: Positional (PassedValue c),
    vnRemaining :: Map.Map VariableName (PassedValue c)
  }

instance (Show c, CollectErrorsM m) =>
  CompilerContext c m [String] (ProcedureContext c) where
  ccCurrentScope = return . pcScope
  ccResolver = return . AnyTypeResolver . CategoryResolver . pcCategories
  ccSameType ctx = return . (== same) where
    same = TypeInstance (pcType ctx) (fmap (singleType . JustParamName False . vpParam) $ pcExtParams ctx)
  ccAllFilters = return . pcAllFilters
  ccGetParamScope ctx p = do
    case p `Map.lookup` pcParamScopes ctx of
            (Just s) -> return s
            _ -> compilerErrorM $ "Param " ++ show p ++ " does not exist"
  ccRequiresTypes ctx ts = return $
    ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcExtParams = pcExtParams ctx,
      pcIntParams = pcIntParams ctx,
      pcMembers = pcMembers ctx,
      pcCategories = pcCategories ctx,
      pcAllFilters = pcAllFilters ctx,
      pcExtFilters = pcExtFilters ctx,
      pcIntFilters = pcIntFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcJumpType = pcJumpType ctx,
      pcIsNamed = pcIsNamed ctx,
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = Set.union (pcRequiredTypes ctx) ts,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx,
      pcCleanupSetup = pcCleanupSetup ctx,
      pcInCleanup = pcInCleanup ctx,
      pcExprMap = pcExprMap ctx,
      pcReservedMacros = pcReservedMacros ctx,
      pcNoTrace = pcNoTrace ctx
    }
  ccGetRequired = return . pcRequiredTypes
  ccGetCategoryFunction ctx c Nothing n = ccGetCategoryFunction ctx c (Just $ pcType ctx) n
  ccGetCategoryFunction ctx c (Just t) n = getFunction where
    getFunction
      -- Same category as the procedure itself.
      | t == pcType ctx = checkFunction $ n `Map.lookup` pcFunctions ctx
      -- A different category than the procedure.
      | otherwise = do
        (_,ca) <- getCategory (pcCategories ctx) (c,t)
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        checkFunction $ n `Map.lookup` fa
    checkFunction (Just f) = do
      when (pcDisallowInit ctx && t == pcType ctx && pcScope ctx == CategoryScope) $
        compilerErrorM $ "Function " ++ show n ++
                       " disallowed during initialization" ++ formatFullContextBrace c
      when (sfScope f /= CategoryScope) $
        compilerErrorM $ "Function " ++ show n ++ " in " ++ show t ++ " cannot be used as a category function"
      return f
    checkFunction _ =
      compilerErrorM $ "Category " ++ show t ++
                     " does not have a category function named " ++ show n ++
                     formatFullContextBrace c
  ccGetTypeFunction ctx c t n = getFunction t where
    getFunction (Just t2) = reduceMergeTree getFromAny getFromAll getFromSingle t2
    getFunction Nothing = do
      let ps = fmap (singleType . JustParamName False . vpParam) $ pcExtParams ctx
      getFunction (Just $ singleType $ JustTypeInstance $ TypeInstance (pcType ctx) ps)
    getFromAny _ =
      compilerErrorM $ "Use explicit type conversion to call " ++ show n ++ " from " ++ show t
    getFromAll ts = do
      collectFirstM ts <!!
        "Function " ++ show n ++ " not available for type " ++ show t ++ formatFullContextBrace c
    getFromSingle (JustParamName _ p) = do
      fa <- ccAllFilters ctx
      fs <- case p `Map.lookup` fa of
                (Just fs) -> return fs
                _ -> compilerErrorM $ "Param " ++ show p ++ " does not exist"
      let ts = map tfType $ filter isRequiresFilter fs
      let ds = map dfType $ filter isDefinesFilter  fs
      collectFirstM (map (getFunction . Just) ts ++ map checkDefine ds) <!!
        "Function " ++ show n ++ " not available for param " ++ show p ++ formatFullContextBrace c
    getFromSingle (JustTypeInstance t2)
      -- Same category as the procedure itself.
      | tiName t2 == pcType ctx =
        subAndCheckFunction (tiName t2) (fmap vpParam $ pcExtParams ctx) (tiParams t2) $ n `Map.lookup` pcFunctions ctx
      -- A different category than the procedure.
      | otherwise = do
        (_,ca) <- getCategory (pcCategories ctx) (c,tiName t2)
        let params = Positional $ map vpParam $ getCategoryParams ca
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        subAndCheckFunction (tiName t2) params (tiParams t2) $ n `Map.lookup` fa
    getFromSingle _ = compilerErrorM $ "Type " ++ show t ++ " contains unresolved types"
    checkDefine t2 = do
      (_,ca) <- getCategory (pcCategories ctx) (c,diName t2)
      let params = Positional $ map vpParam $ getCategoryParams ca
      let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
      subAndCheckFunction (diName t2) params (diParams t2) $ n `Map.lookup` fa
    subAndCheckFunction t2 ps1 ps2 (Just f) = do
      when (pcDisallowInit ctx && t2 == pcType ctx) $
        compilerErrorM $ "Function " ++ show n ++
                       " disallowed during initialization" ++ formatFullContextBrace c
      when (sfScope f == CategoryScope) $
        compilerErrorM $ "Function " ++ show n ++ " in " ++ show t2 ++
                       " is a category function" ++ formatFullContextBrace c
      paired <- processPairs alwaysPair ps1 ps2 <??
        "In external function call at " ++ formatFullContext c
      let assigned = Map.fromList paired
      uncheckedSubFunction assigned f
    subAndCheckFunction t2 _ _ _ =
      compilerErrorM $ "Category " ++ show t2 ++
                     " does not have a type or value function named " ++ show n ++
                     formatFullContextBrace c
  ccCheckValueInit ctx c (TypeInstance t as) ts ps
    | t /= pcType ctx =
      compilerErrorM $ "Category " ++ show (pcType ctx) ++ " cannot initialize values from " ++
                     show t ++ formatFullContextBrace c
    | otherwise = "In initialization at " ++ formatFullContext c ??> do
      let t' = TypeInstance (pcType ctx) as
      r <- ccResolver ctx
      allFilters <- ccAllFilters ctx
      pa  <- fmap Map.fromList $ processPairs alwaysPair (fmap vpParam $ pcExtParams ctx) as
      pa2 <- fmap Map.fromList $ processPairs alwaysPair (fmap vpParam $ pcIntParams ctx) ps
      let pa' = Map.union pa pa2
      validateTypeInstance r allFilters t'
      -- Check internal param substitution.
      let mapped = Map.fromListWith (++) $ map (\f -> (pfParam f,[pfFilter f])) (pcIntFilters ctx)
      let positional = map (getFilters mapped) (map vpParam $ pValues $ pcIntParams ctx)
      assigned <- fmap Map.fromList $ processPairs alwaysPair (fmap vpParam $ pcIntParams ctx) ps
      subbed <- fmap Positional $ mapErrorsM (assignFilters assigned) positional
      processPairs_ (validateAssignment r allFilters) ps subbed
      -- Check initializer types.
      ms <- fmap Positional $ mapErrorsM (subSingle pa') (pcMembers ctx)
      processPairs_ (checkInit r allFilters) ms (Positional $ zip ([1..] :: [Int]) $ pValues ts)
      return ()
      where
        getFilters fm n =
          case n `Map.lookup` fm of
              (Just fs) -> fs
              _ -> []
        assignFilters fm fs = do
          mapErrorsM (uncheckedSubFilter $ getValueForParam fm) fs
        checkInit r fa (MemberValue c2 n t0) (i,t1) = do
          checkValueAssignment r fa t1 t0 <??
            "In initializer " ++ show i ++ " for " ++ show n ++ formatFullContextBrace c2
        subSingle pa (DefinedMember c2 _ t2 n _) = do
          t2' <- uncheckedSubValueType (getValueForParam pa) t2
          return $ MemberValue c2 n t2'
  ccGetVariable ctx c n =
    case n `Map.lookup` pcVariables ctx of
          (Just v) -> return v
          _ -> compilerErrorM $ "Variable " ++ show n ++ " is not defined" ++
                               formatFullContextBrace c
  ccAddVariable ctx c n t = do
    case n `Map.lookup` pcVariables ctx of
          Nothing -> return ()
          (Just v) -> compilerErrorM $ "Variable " ++ show n ++
                                      formatFullContextBrace c ++
                                      " is already defined: " ++ show v
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = Map.insert n t (pcVariables ctx),
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccCheckVariableInit ctx c n =
    case pcReturns ctx of
         ValidateNames _ _ na -> when (n `Map.member` na) $
           compilerErrorM $ "Named return " ++ show n ++ " might not be initialized" ++ formatFullContextBrace c
         _ -> return ()
  ccWrite ctx ss = return $
    ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcExtParams = pcExtParams ctx,
      pcIntParams = pcIntParams ctx,
      pcMembers = pcMembers ctx,
      pcCategories = pcCategories ctx,
      pcAllFilters = pcAllFilters ctx,
      pcExtFilters = pcExtFilters ctx,
      pcIntFilters = pcIntFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcJumpType = pcJumpType ctx,
      pcIsNamed = pcIsNamed ctx,
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx ++ ss,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx,
      pcCleanupSetup = pcCleanupSetup ctx,
      pcInCleanup = pcInCleanup ctx,
      pcExprMap = pcExprMap ctx,
      pcReservedMacros = pcReservedMacros ctx,
      pcNoTrace = pcNoTrace ctx
    }
  ccGetOutput = return . pcOutput
  ccClearOutput ctx = return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = [],
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccUpdateAssigned ctx n = update (pcReturns ctx) where
    update (ValidateNames ns ts ra) = return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = ValidateNames ns ts $ Map.delete n ra,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
    update _ = return ctx
  ccInheritReturns ctx cs = return $ ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcExtParams = pcExtParams ctx,
      pcIntParams = pcIntParams ctx,
      pcMembers = pcMembers ctx,
      pcCategories = pcCategories ctx,
      pcAllFilters = pcAllFilters ctx,
      pcExtFilters = pcExtFilters ctx,
      pcIntFilters = pcIntFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = returns,
      pcJumpType = jump,
      pcIsNamed = pcIsNamed ctx,
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx,
      pcCleanupSetup = pcCleanupSetup ctx,
      pcInCleanup = pcInCleanup ctx,
      pcExprMap = pcExprMap ctx,
      pcReservedMacros = pcReservedMacros ctx,
      pcNoTrace = pcNoTrace ctx
    }
    where
      (returns,jump) = combineSeries (pcReturns ctx,pcJumpType ctx) inherited
      combineSeries (r@(ValidatePositions _),j1) (_,j2) = (r,max j1 j2)
      combineSeries (_,j1) (r@(ValidatePositions _),j2) = (r,max j1 j2)
      combineSeries (ValidateNames ns ts ra1,j1) (ValidateNames _ _ ra2,j2) = (ValidateNames ns ts $ Map.intersection ra1 ra2,max j1 j2)
      inherited = foldr combineParallel (ValidateNames (Positional []) (Positional []) Map.empty,JumpMax) $ zip (map pcReturns cs) (map pcJumpType cs)
      combineParallel (_,j1) (r,j2)
        -- Ignore a branch if it jumps to a higher scope.
        | (if pcInCleanup ctx then j1 > JumpReturn else j1 > NextStatement) = (r,min j1 j2)
      combineParallel (ValidateNames ns ts ra1,j1) (ValidateNames _ _ ra2,j2) = (ValidateNames ns ts $ Map.union ra1 ra2,min j1 j2)
      combineParallel (r@(ValidatePositions _),j1) (_,j2) = (r,min j1 j2)
      combineParallel (_,j1) (r@(ValidatePositions _),j2) = (r,min j1 j2)
  ccRegisterReturn ctx c vs = do
    when (pcInCleanup ctx) $ compilerErrorM $ "Explicit return at " ++ formatFullContext c ++ " not allowed in cleanup"
    returns <- check (pcReturns ctx)
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = returns,
        pcJumpType = JumpReturn,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
    where
      check (ValidatePositions rs) = do
        let vs' = case vs of
                       Nothing -> Positional []
                       Just vs2 -> vs2
        -- Check for a count match first, to avoid the default error message.
        processPairs_ alwaysPair (fmap pvType rs) vs' <??
          "In procedure return at " ++ formatFullContext c
        processPairs_ checkReturnType rs (Positional $ zip ([0..] :: [Int]) $ pValues vs') <??
          "In procedure return at " ++ formatFullContext c
        return (ValidatePositions rs)
        where
          checkReturnType ta0@(PassedValue _ t0) (n,t) = do
            r <- ccResolver ctx
            pa <- ccAllFilters ctx
            checkValueAssignment r pa t t0 <!!
              "Cannot convert " ++ show t ++ " to " ++ show ta0 ++ " in return " ++
               show n ++ " at " ++ formatFullContext c
      check (ValidateNames ns ts ra) = do
        case vs of
             Just _  -> check (ValidatePositions ts) >> return ()
             Nothing -> mapErrorsM_ alwaysError $ Map.toList ra
        return (ValidateNames ns ts Map.empty)
      alwaysError (n,t) = compilerErrorM $ "Named return " ++ show n ++ " (" ++ show t ++
                                          ") might not have been set before return at " ++
                                          formatFullContext c
  ccPrimNamedReturns = return . pcPrimNamed
  ccIsUnreachable ctx
    | pcInCleanup ctx = return $ pcJumpType ctx > JumpReturn
    | otherwise       = return $ pcJumpType ctx > NextStatement
  ccIsNamedReturns = return . pcIsNamed
  ccSetJumpType ctx j =
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = j,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccStartLoop ctx l =
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = l,
        pcCleanupSetup = LoopBoundary:(pcCleanupSetup ctx),
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccGetLoop = return . pcLoopSetup
  ccStartCleanup ctx = do
    let vars = protectReturns (pcReturns ctx) (pcVariables ctx)
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = vars,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = True,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
    where
      protectReturns (ValidateNames ns _ _) vs = foldr protect vs (pValues ns)
      protectReturns _                      vs = vs
      protect n vs =
        case n `Map.lookup` vs of
             Just (VariableValue c s@LocalScope t _) -> Map.insert n (VariableValue c s t False) vs
             _ -> vs
  ccPushCleanup ctx cs =
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = cs:(pcCleanupSetup ctx),
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccGetCleanup ctx j = return combined where
    combined
      | j == JumpReturn                   = combine $ filter    (not . isLoopBoundary) $ pcCleanupSetup ctx
      | j == JumpBreak || j == JumpReturn = combine $ takeWhile (not . isLoopBoundary) $ pcCleanupSetup ctx
      | otherwise = CleanupSetup [] []
    combine cs = CleanupSetup (concat $ map csReturnContext cs) (concat $ map csCleanup cs)
  ccExprLookup ctx c n =
    case n `Map.lookup` pcExprMap ctx of
         Nothing -> compilerErrorM $ "Env expression " ++ show n ++ " is not defined" ++ formatFullContextBrace c
         Just e -> do
           checkReserved (pcReservedMacros ctx) [(n,c)]
           return e
    where
      checkReserved [] _ = return ()
      checkReserved (m@(n2,_):ms) rs
        | n2 /= n = checkReserved ms (m:rs)
        | otherwise = (mapErrorsM_ singleError (m:rs)) <!!
            "Expression macro " ++ show n ++ " references itself"
      singleError (n2,c2) = compilerErrorM $ show n2 ++ " expanded at " ++ formatFullContext c2
  ccReserveExprMacro ctx c n =
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = ((n,c):pcReservedMacros ctx),
        pcNoTrace = pcNoTrace ctx
      }
  ccReleaseExprMacro ctx _ n =
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = filter ((/= n) . fst) $ pcReservedMacros ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccSetNoTrace ctx t =
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcExtParams = pcExtParams ctx,
        pcIntParams = pcIntParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcAllFilters = pcAllFilters ctx,
        pcExtFilters = pcExtFilters ctx,
        pcIntFilters = pcIntFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcJumpType = pcJumpType ctx,
        pcIsNamed = pcIsNamed ctx,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcInCleanup = pcInCleanup ctx,
        pcExprMap = pcExprMap ctx,
        pcReservedMacros = pcReservedMacros ctx,
        pcNoTrace = t
      }
  ccGetNoTrace = return . pcNoTrace

updateReturnVariables :: (Show c, CollectErrorsM m) =>
  (Map.Map VariableName (VariableValue c)) ->
  Positional (PassedValue c) -> ReturnValues c ->
  m (Map.Map VariableName (VariableValue c))
updateReturnVariables ma rs1 rs2 = updated where
  updated
    | isUnnamedReturns rs2 = return ma
    | otherwise = do
      rs <- processPairs alwaysPair rs1 (nrNames rs2)
      foldr update (return ma) rs where
        update (PassedValue c t,r) va = do
          va' <- va
          case ovName r `Map.lookup` va' of
               Nothing -> return $ Map.insert (ovName r) (VariableValue c LocalScope t True) va'
               (Just v) -> compilerErrorM $ "Variable " ++ show (ovName r) ++
                                          formatFullContextBrace (ovContext r) ++
                                          " is already defined" ++
                                          formatFullContextBrace (vvContext v)

updateArgVariables :: (Show c, CollectErrorsM m) =>
  (Map.Map VariableName (VariableValue c)) ->
  Positional (PassedValue c) -> ArgValues c ->
  m (Map.Map VariableName (VariableValue c))
updateArgVariables ma as1 as2 = do
  as <- processPairs alwaysPair as1 (avNames as2)
  let as' = filter (not . isDiscardedInput . snd) as
  foldr update (return ma) as' where
    update (PassedValue c t,a) va = do
      va' <- va
      case ivName a `Map.lookup` va' of
            Nothing -> return $ Map.insert (ivName a) (VariableValue c LocalScope t False) va'
            (Just v) -> compilerErrorM $ "Variable " ++ show (ivName a) ++
                                       formatFullContextBrace (ivContext a) ++
                                       " is already defined" ++
                                       formatFullContextBrace (vvContext v)
