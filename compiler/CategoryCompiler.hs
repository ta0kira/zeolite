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

module CategoryCompiler (
  ProcedureContext(..),
  ReturnValidation(..),
  updateArgVariables,
  updateReturnVariables,
) where

import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CompilerState
import DefinedCategory
import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


data ProcedureContext c =
  ProcedureContext {
    pcScope :: SymbolScope,
    pcType :: CategoryName,
    pcExtParams :: ParamSet (ValueParam c),
    pcIntParams :: ParamSet (ValueParam c),
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
    pcLoopSetup :: LoopSetup [String]
  }

data ReturnValidation c =
  ValidatePositions {
    vpReturns :: ParamSet (PassedValue c)
  } |
  ValidateNames {
    vnTypes :: ParamSet (PassedValue c),
    vnReturns :: Map.Map VariableName (PassedValue c)
  } |
  NoValidation |
  UnreachableReturn

instance (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CompilerContext c m [String] (ProcedureContext c) where
  ccCurrentScope = return . pcScope
  ccResolver = return . categoriesToTypeResolver . pcCategories
  ccSameType ctx = return . (== same) where
    same = TypeInstance (pcType ctx) (fmap (SingleType . JustParamName . vpParam) $ pcExtParams ctx)
  ccAllFilters = return . pcAllFilters
  ccGetParamScope ctx p = do
    case p `Map.lookup` pcParamScopes ctx of
            (Just s) -> return s
            _ -> compileError $ "Param " ++ show p ++ " does not exist"
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
      pcLoopSetup = pcLoopSetup ctx
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
        let params = ParamSet $ map vpParam $ getCategoryParams ca
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        checkFunction $ n `Map.lookup` fa
    checkFunction (Just f) = do
      when (pcDisallowInit ctx && t == pcType ctx && pcScope ctx == CategoryScope) $
        compileError $ "Function " ++ show n ++
                       " disallowed during initialization [" ++ formatFullContext c ++ "]"
      when (sfScope f /= CategoryScope) $
        compileError $ "Function " ++ show n ++ " in " ++ show t ++ " cannot be used as a category function"
      return f
    checkFunction _ =
      compileError $ "Category " ++ show t ++
                     " does not have a function named " ++ show n ++ " [" ++
                     formatFullContext c ++ "]"
  ccGetTypeFunction ctx c t n = getFunction t where
    getFunction (Just t@(TypeMerge MergeUnion _)) =
      compileError $ "Use explicit type conversion to call " ++ show n ++ " for union type " ++
                     show t ++ " [" ++ formatFullContext c ++ "]"
    getFunction (Just t@(TypeMerge MergeIntersect ts)) =
      collectOneOrErrorM $ map getFunction $ map Just ts
    getFunction (Just (SingleType (JustParamName p))) = do
      fa <- ccAllFilters ctx
      fs <- case p `Map.lookup` fa of
                (Just fs) -> return fs
                _ -> compileError $ "Param " ++ show p ++ " does not exist"
      let ts = map tfType $ filter isRequiresFilter fs
      let ds = map dfType $ filter isDefinesFilter  fs
      collectOneOrErrorM $
        [compileError $ "Function " ++ show n ++ " not available for param " ++ show p ++
         " [" ++ formatFullContext c ++ "]"] ++
        (map getFunction $ map (Just . SingleType) ts) ++ (map checkDefine ds)
    getFunction (Just (SingleType (JustTypeInstance t)))
      -- Same category as the procedure itself.
      | tiName t == pcType ctx =
        checkFunction (tiName t) (fmap vpParam $ pcExtParams ctx) (tiParams t) $ n `Map.lookup` pcFunctions ctx
      -- A different category than the procedure.
      | otherwise = do
        (_,ca) <- getCategory (pcCategories ctx) (c,tiName t)
        let params = ParamSet $ map vpParam $ getCategoryParams ca
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        checkFunction (tiName t) params (tiParams t) $ n `Map.lookup` fa
    getFunction Nothing = do
      let ps = fmap (SingleType . JustParamName . vpParam) $ pcExtParams ctx
      getFunction (Just $ SingleType $ JustTypeInstance $ TypeInstance (pcType ctx) ps)
    checkDefine t = do
      (_,ca) <- getCategory (pcCategories ctx) (c,diName t)
      let params = ParamSet $ map vpParam $ getCategoryParams ca
      let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
      checkFunction (diName t) params (diParams t) $ n `Map.lookup` fa
    checkFunction t2 ps1 ps2 (Just f) = do
      when (pcDisallowInit ctx && t2 == pcType ctx) $
        compileError $ "Function " ++ show n ++
                       " disallowed during initialization [" ++ formatFullContext c ++ "]"
      when (sfScope f == CategoryScope) $
        compileError $ "Function " ++ show n ++ " in " ++ show t2 ++
                       " is a category function [" ++ formatFullContext c ++ "]"
      paired <- processParamPairs alwaysPairParams ps1 ps2 `reviseError`
        ("In external function call at " ++ formatFullContext c)
      let assigned = Map.fromList paired
      uncheckedSubFunction assigned f
    checkFunction t2 _ _ _ =
      compileError $ "Category " ++ show t2 ++
                     " does not have a function named " ++ show n ++ " [" ++
                     formatFullContext c ++ "]"
  ccCheckValueInit ctx c (TypeInstance t as) ts ps
    | t /= pcType ctx =
      compileError $ "Category " ++ show (pcType ctx) ++ " cannot initialize values from " ++
                     show t ++ " [" ++ formatFullContext c ++ "]"
    | otherwise = flip reviseError ("In initialization at " ++ formatFullContext c) $ do
      let t' = TypeInstance (pcType ctx) as
      r <- ccResolver ctx
      allFilters <- ccAllFilters ctx
      pa  <- fmap Map.fromList $ processParamPairs alwaysPairParams (fmap vpParam $ pcExtParams ctx) as
      pa2 <- fmap Map.fromList $ processParamPairs alwaysPairParams (fmap vpParam $ pcIntParams ctx) ps
      let pa' = Map.union pa pa2
      validateTypeInstance r allFilters t'
      -- Check internal param substitution.
      let mapped = Map.fromListWith (++) $ map (\f -> (pfParam f,[pfFilter f])) (pcIntFilters ctx)
      let positional = map (getFilters mapped) (map vpParam $ psParams $ pcIntParams ctx)
      assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams (fmap vpParam $ pcIntParams ctx) ps
      subbed <- fmap ParamSet $ collectAllOrErrorM $ map (assignFilters assigned) positional
      processParamPairs (validateAssignment r allFilters) ps subbed
      -- Check initializer types.
      ms <- fmap ParamSet $ collectAllOrErrorM $ map (subSingle pa') (pcMembers ctx)
      processParamPairs (checkInit r allFilters) ms (ParamSet $ zip [1..] $ psParams ts)
      return ()
      where
        getFilters fm n =
          case n `Map.lookup` fm of
              (Just fs) -> fs
              _ -> []
        assignFilters fm fs = do
          collectAllOrErrorM $ map (uncheckedSubFilter $ getValueForParam fm) fs
        checkInit r fa (MemberValue c n t0) (i,t1) = do
          checkValueTypeMatch r fa t1 t0 `reviseError`
            ("In initializer " ++ show i ++ " for " ++ show n ++ " [" ++ formatFullContext c ++ "]")
        subSingle pa (DefinedMember c _ t n _) = do
          t' <- uncheckedSubValueType (getValueForParam pa) t
          return $ MemberValue c n t'
  ccGetVariable ctx c n =
    case n `Map.lookup` pcVariables ctx of
          (Just v) -> return v
          _ -> compileError $ "Variable " ++ show n ++ " is not defined [" ++
                              formatFullContext c ++ "]"
  ccAddVariable ctx c n t = do
    case n `Map.lookup` pcVariables ctx of
          Nothing -> return ()
          (Just v) -> compileError $ "Variable " ++ show n ++ " [" ++
                                    formatFullContext c ++
                                    "] is already defined: " ++ show v
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
        pcLoopSetup = pcLoopSetup ctx
      }
  ccCheckVariableInit ctx c n =
    case pcReturns ctx of
         ValidateNames _ na -> when (n `Map.member` na) $
           compileError $ "Named return " ++ show n ++ " might not be initialized [" ++ formatFullContext c ++ "]"
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
      pcLoopSetup = pcLoopSetup ctx
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
        pcLoopSetup = pcLoopSetup ctx
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
        pcLoopSetup = pcLoopSetup ctx
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
      pcReturns = foldr update UnreachableReturn (map pcReturns cs),
      pcPrimNamed = pcPrimNamed ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx,
      pcLoopSetup = pcLoopSetup ctx
    }
    where
      update r@(ValidatePositions _) _ = r
      update NoValidation r = r
      update r NoValidation = r
      update UnreachableReturn r = r
      update r UnreachableReturn = r
      update (ValidateNames ts ra1) (ValidateNames _ ra2) = ValidateNames ts $ Map.union ra1 ra2
      update _ _ = UnreachableReturn
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
        pcReturns = UnreachableReturn,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx
      }
    where
      check (ValidatePositions rs) = do
        processParamPairs checkReturnType rs (ParamSet $ zip [1..] $ psParams vs) `reviseError`
          ("In procedure return at " ++ formatFullContext c)
        return ()
        where
          checkReturnType ta0@(PassedValue c0 t0) (n,t) = do
            r <- ccResolver ctx
            pa <- ccAllFilters ctx
            checkValueTypeMatch r pa t t0 `reviseError`
              ("Cannot convert " ++ show t ++ " to " ++ show ta0 ++ " in return " ++
               show n ++ " at " ++ formatFullContext c)
      check (ValidateNames ts ra)
        | not $ null $ psParams vs = check (ValidatePositions ts)
        | otherwise =
          mergeAllM $ map alwaysError $ Map.toList ra
          where
            alwaysError (n,t) = compileError $ "Named return " ++ show n ++ " (" ++ show t ++
                                               ") might not have been set before return at " ++
                                               formatFullContext c
      check _ = return ()
  ccPrimNamedReturns = return . pcPrimNamed
  ccIsUnreachable ctx = return $ match (pcReturns ctx) where
    match UnreachableReturn = True
    match _                 = False
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
        pcReturns = UnreachableReturn,
        pcPrimNamed = pcPrimNamed ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx,
        pcLoopSetup = pcLoopSetup ctx
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
        pcLoopSetup = l
      }
  ccGetLoop = return . pcLoopSetup

updateReturnVariables :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  (Map.Map VariableName (VariableValue c)) ->
  ParamSet (PassedValue c) -> ReturnValues c ->
  m (Map.Map VariableName (VariableValue c))
updateReturnVariables ma rs1 rs2 = updated where
  updated
    | isUnnamedReturns rs2 = return ma
    | otherwise = do
      rs <- processParamPairs alwaysPairParams rs1 (nrNames rs2)
      foldr update (return ma) rs where
        update (PassedValue c t,r) va = do
          va' <- va
          case ovName r `Map.lookup` va' of
               Nothing -> return $ Map.insert (ovName r) (VariableValue c LocalScope t True) va'
               (Just v) -> compileError $ "Variable " ++ show (ovName r) ++
                                          " [" ++ formatFullContext (ovContext r) ++
                                          "] is already defined [" ++
                                          formatFullContext (vvContext v) ++ "]"

updateArgVariables :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  (Map.Map VariableName (VariableValue c)) ->
  ParamSet (PassedValue c) -> ArgValues c ->
  m (Map.Map VariableName (VariableValue c))
updateArgVariables ma as1 as2 = do
  as <- processParamPairs alwaysPairParams as1 (avNames as2)
  let as' = filter (not . isDiscardedInput . snd) as
  foldr update (return ma) as' where
    update (PassedValue c t,a) va = do
      va' <- va
      case ivName a `Map.lookup` va' of
            Nothing -> return $ Map.insert (ivName a) (VariableValue c LocalScope t False) va'
            (Just v) -> compileError $ "Variable " ++ show (ivName a) ++
                                       " [" ++ formatFullContext (ivContext a) ++
                                       "] is already defined [" ++
                                       formatFullContext (vvContext v) ++ "]"
