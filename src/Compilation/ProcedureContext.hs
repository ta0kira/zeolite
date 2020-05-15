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

import Base.CompileError
import Base.Mergeable
import Compilation.CompilerState
import Types.DefinedCategory
import Types.GeneralType
import Types.Positional
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
    pcPrimNamed :: [ReturnVariable],
    pcRequiredTypes :: Set.Set CategoryName,
    pcOutput :: [String],
    pcDisallowInit :: Bool,
    pcLoopSetup :: LoopSetup [String],
    pcCleanupSetup :: CleanupSetup (ProcedureContext c) [String],
    pcExprMap :: ExprMap c,
    pcNoTrace :: Bool
  }

type ExprMap c = Map.Map String (Expression c)

data ReturnValidation c =
  ValidatePositions {
    vpReturns :: Positional (PassedValue c)
  } |
  ValidateNames {
    vnTypes :: Positional (PassedValue c),
    vnRemaining :: Map.Map VariableName (PassedValue c)
  } |
  UnreachableCode

instance (Show c, MergeableM m, CompileErrorM m) =>
  CompilerContext c m [String] (ProcedureContext c) where
  ccCurrentScope = return . pcScope
  ccResolver = return . AnyTypeResolver . CategoryResolver . pcCategories
  ccSameType ctx = return . (== same) where
    same = TypeInstance (pcType ctx) (fmap (SingleType . JustParamName . vpParam) $ pcExtParams ctx)
  ccAllFilters = return . pcAllFilters
  ccGetParamScope ctx p = do
    case p `Map.lookup` pcParamScopes ctx of
            (Just s) -> return s
            _ -> compileErrorM $ "Param " ++ show p ++ " does not exist"
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
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = Set.union (pcRequiredTypes ctx) ts,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx,
      pcCleanupSetup = pcCleanupSetup ctx,
      pcExprMap = pcExprMap ctx,
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
        compileErrorM $ "Function " ++ show n ++
                       " disallowed during initialization" ++ formatFullContextBrace c
      when (sfScope f /= CategoryScope) $
        compileErrorM $ "Function " ++ show n ++ " in " ++ show t ++ " cannot be used as a category function"
      return f
    checkFunction _ =
      compileErrorM $ "Category " ++ show t ++
                     " does not have a category function named " ++ show n ++
                     formatFullContextBrace c
  ccGetTypeFunction ctx c t n = getFunction t where
    getFunction (Just t2@(TypeMerge MergeUnion _)) =
      compileErrorM $ "Use explicit type conversion to call " ++ show n ++ " for union type " ++
                     show t2 ++ formatFullContextBrace c
    getFunction (Just ta@(TypeMerge MergeIntersect ts)) =
      collectOneOrErrorM (map getFunction $ map Just ts) `reviseErrorM`
        ("Function " ++ show n ++ " not available for type " ++ show ta ++ formatFullContextBrace c)
    getFunction (Just (SingleType (JustParamName p))) = do
      fa <- ccAllFilters ctx
      fs <- case p `Map.lookup` fa of
                (Just fs) -> return fs
                _ -> compileErrorM $ "Param " ++ show p ++ " does not exist"
      let ts = map tfType $ filter isRequiresFilter fs
      let ds = map dfType $ filter isDefinesFilter  fs
      collectOneOrErrorM (map (getFunction . Just . SingleType) ts ++ map checkDefine ds) `reviseErrorM`
        ("Function " ++ show n ++ " not available for param " ++ show p ++ formatFullContextBrace c)
    getFunction (Just (SingleType (JustTypeInstance t2)))
      -- Same category as the procedure itself.
      | tiName t2 == pcType ctx =
        checkFunction (tiName t2) (fmap vpParam $ pcExtParams ctx) (tiParams t2) $ n `Map.lookup` pcFunctions ctx
      -- A different category than the procedure.
      | otherwise = do
        (_,ca) <- getCategory (pcCategories ctx) (c,tiName t2)
        let params = Positional $ map vpParam $ getCategoryParams ca
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        checkFunction (tiName t2) params (tiParams t2) $ n `Map.lookup` fa
    getFunction Nothing = do
      let ps = fmap (SingleType . JustParamName . vpParam) $ pcExtParams ctx
      getFunction (Just $ SingleType $ JustTypeInstance $ TypeInstance (pcType ctx) ps)
    checkDefine t2 = do
      (_,ca) <- getCategory (pcCategories ctx) (c,diName t2)
      let params = Positional $ map vpParam $ getCategoryParams ca
      let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
      checkFunction (diName t2) params (diParams t2) $ n `Map.lookup` fa
    checkFunction t2 ps1 ps2 (Just f) = do
      when (pcDisallowInit ctx && t2 == pcType ctx) $
        compileErrorM $ "Function " ++ show n ++
                       " disallowed during initialization" ++ formatFullContextBrace c
      when (sfScope f == CategoryScope) $
        compileErrorM $ "Function " ++ show n ++ " in " ++ show t2 ++
                       " is a category function" ++ formatFullContextBrace c
      paired <- processPairs alwaysPair ps1 ps2 `reviseErrorM`
        ("In external function call at " ++ formatFullContext c)
      let assigned = Map.fromList paired
      uncheckedSubFunction assigned f
    checkFunction t2 _ _ _ =
      compileErrorM $ "Category " ++ show t2 ++
                     " does not have a type or value function named " ++ show n ++
                     formatFullContextBrace c
  ccCheckValueInit ctx c (TypeInstance t as) ts ps
    | t /= pcType ctx =
      compileErrorM $ "Category " ++ show (pcType ctx) ++ " cannot initialize values from " ++
                     show t ++ formatFullContextBrace c
    | otherwise = flip reviseErrorM ("In initialization at " ++ formatFullContext c) $ do
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
      subbed <- fmap Positional $ collectAllOrErrorM $ map (assignFilters assigned) positional
      processPairs_ (validateAssignment r allFilters) ps subbed
      -- Check initializer types.
      ms <- fmap Positional $ collectAllOrErrorM $ map (subSingle pa') (pcMembers ctx)
      processPairs_ (checkInit r allFilters) ms (Positional $ zip ([1..] :: [Int]) $ pValues ts)
      return ()
      where
        getFilters fm n =
          case n `Map.lookup` fm of
              (Just fs) -> fs
              _ -> []
        assignFilters fm fs = do
          collectAllOrErrorM $ map (uncheckedSubFilter $ getValueForParam fm) fs
        checkInit r fa (MemberValue c2 n t0) (i,t1) = do
          checkValueTypeMatch r fa t1 t0 `reviseErrorM`
            ("In initializer " ++ show i ++ " for " ++ show n ++ formatFullContextBrace c2)
        subSingle pa (DefinedMember c2 _ t2 n _) = do
          t2' <- uncheckedSubValueType (getValueForParam pa) t2
          return $ MemberValue c2 n t2'
  ccGetVariable ctx c n =
    case n `Map.lookup` pcVariables ctx of
          (Just v) -> return v
          _ -> compileErrorM $ "Variable " ++ show n ++ " is not defined" ++
                              formatFullContextBrace c
  ccAddVariable ctx c n t = do
    case n `Map.lookup` pcVariables ctx of
          Nothing -> return ()
          (Just v) -> compileErrorM $ "Variable " ++ show n ++
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
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccCheckVariableInit ctx c n =
    case pcReturns ctx of
         ValidateNames _ na -> when (n `Map.member` na) $
           compileErrorM $ "Named return " ++ show n ++ " might not be initialized" ++ formatFullContextBrace c
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
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx ++ ss,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx,
      pcCleanupSetup = pcCleanupSetup ctx,
      pcExprMap = pcExprMap ctx,
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
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = [],
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccUpdateAssigned ctx n = update (pcReturns ctx) where
    update (ValidateNames ts ra) = return $ ProcedureContext {
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
        pcReturns = ValidateNames ts $ Map.delete n ra,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
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
      pcReturns = combineSeries (pcReturns ctx) inherited,
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx,
      pcCleanupSetup = pcCleanupSetup ctx,
      pcExprMap = pcExprMap ctx,
      pcNoTrace = pcNoTrace ctx
    }
    where
      inherited = foldr combineParallel UnreachableCode (map pcReturns cs)
      combineSeries _ UnreachableCode = UnreachableCode
      combineSeries UnreachableCode _ = UnreachableCode
      combineSeries r@(ValidatePositions _) _ = r
      combineSeries _ r@(ValidatePositions _) = r
      combineSeries (ValidateNames ts ra1) (ValidateNames _ ra2) = ValidateNames ts $ Map.intersection ra1 ra2
      combineParallel UnreachableCode r = r
      combineParallel r UnreachableCode = r
      combineParallel (ValidateNames ts ra1) (ValidateNames _ ra2) = ValidateNames ts $ Map.union ra1 ra2
      combineParallel r@(ValidatePositions _) _ = r
      combineParallel _ r@(ValidatePositions _) = r
  ccRegisterReturn ctx c vs = do
    check (pcReturns ctx)
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
        pcReturns = UnreachableCode,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
        pcNoTrace = pcNoTrace ctx
      }
    where
      check (ValidatePositions rs) = do
        let vs' = case vs of
                       Nothing -> Positional []
                       Just vs2 -> vs2
        -- Check for a count match first, to avoid the default error message.
        processPairs_ alwaysPair (fmap pvType rs) vs' `reviseErrorM`
          ("In procedure return at " ++ formatFullContext c)
        processPairs_ checkReturnType rs (Positional $ zip ([0..] :: [Int]) $ pValues vs') `reviseErrorM`
          ("In procedure return at " ++ formatFullContext c)
        return ()
        where
          checkReturnType ta0@(PassedValue _ t0) (n,t) = do
            r <- ccResolver ctx
            pa <- ccAllFilters ctx
            checkValueTypeMatch r pa t t0 `reviseErrorM`
              ("Cannot convert " ++ show t ++ " to " ++ show ta0 ++ " in return " ++
               show n ++ " at " ++ formatFullContext c)
      check (ValidateNames ts ra) =
        case vs of
             Just _ -> check (ValidatePositions ts)
             Nothing -> mergeAllM $ map alwaysError $ Map.toList ra where
               alwaysError (n,t) = compileErrorM $ "Named return " ++ show n ++ " (" ++ show t ++
                                                  ") might not have been set before return at " ++
                                                  formatFullContext c
      check _ = return ()
  ccPrimNamedReturns = return . pcPrimNamed
  ccIsUnreachable ctx = return $ match (pcReturns ctx) where
    match UnreachableCode = True
    match _                 = False
  ccIsNamedReturns ctx = return $ match (pcReturns ctx) where
    match (ValidateNames _ _) = True
    match _                   = False
  ccSetNoReturn ctx =
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
        pcReturns = UnreachableCode,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
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
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = l,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccGetLoop = return . pcLoopSetup
  ccPushCleanup ctx (CleanupSetup cs ss) =
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
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = CleanupSetup (cs ++ (csReturnContext $ pcCleanupSetup ctx))
                                      (ss ++ (csCleanup $ pcCleanupSetup ctx)),
        pcExprMap = pcExprMap ctx,
        pcNoTrace = pcNoTrace ctx
      }
  ccGetCleanup = return . pcCleanupSetup
  ccExprLookup ctx c n =
    case n `Map.lookup` pcExprMap ctx of
         Nothing -> compileErrorM $ "Env expression " ++ n ++ " is not defined" ++ formatFullContextBrace c
         Just e -> return e
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
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx,
        pcCleanupSetup = pcCleanupSetup ctx,
        pcExprMap = pcExprMap ctx,
        pcNoTrace = t
      }
  ccGetNoTrace = return . pcNoTrace

updateReturnVariables :: (Show c, CompileErrorM m, MergeableM m) =>
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
               (Just v) -> compileErrorM $ "Variable " ++ show (ovName r) ++
                                          formatFullContextBrace (ovContext r) ++
                                          " is already defined" ++
                                          formatFullContextBrace (vvContext v)

updateArgVariables :: (Show c, CompileErrorM m, MergeableM m) =>
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
            (Just v) -> compileErrorM $ "Variable " ++ show (ivName a) ++
                                       formatFullContextBrace (ivContext a) ++
                                       " is already defined" ++
                                       formatFullContextBrace (vvContext v)
