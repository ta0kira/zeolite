{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Compilation.ProcedureContext (
  ExprMap,
  ProcedureContext(..),
  ReturnValidation(..),
  updateArgVariables,
  updateReturnVariables,
) where

import Control.Monad (foldM,when)
import Lens.Micro hiding (mapped)
import Lens.Micro.TH
import Data.Maybe (fromJust,isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.MergeTree
import Base.Positional
import Compilation.CompilerState
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


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
  deriving (Show)

data ProcedureContext c =
  ProcedureContext {
    _pcScope :: SymbolScope,
    _pcType :: CategoryName,
    _pcExtParams :: Positional (ValueParam c),
    _pcMembers :: [DefinedMember c],
    _pcCategories :: CategoryMap c,
    _pcAllFilters :: ParamFilters,
    _pcExtFilters :: [ParamFilter c],
    _pcParamScopes :: Map.Map ParamName SymbolScope,
    _pcFunctions :: Map.Map FunctionName (ScopedFunction c),
    _pcVariables :: Map.Map VariableName (VariableValue c),
    _pcReturns :: ReturnValidation c,
    _pcJumpType :: JumpType,
    _pcIsNamed :: Bool,
    _pcPrimNamed :: [ReturnVariable],
    _pcRequiredTypes :: Set.Set CategoryName,
    _pcOutput :: [String],
    _pcDisallowInit :: Bool,
    _pcLoopSetup :: LoopSetup [String],
    _pcCleanupBlocks :: [Maybe (CleanupBlock c [String])],
    _pcInCleanup :: Bool,
    _pcUsedVars :: [UsedVariable c],
    _pcExprMap :: ExprMap c,
    _pcReservedMacros :: [(MacroName,[c])],
    _pcNoTrace :: Bool
  }

$(makeLenses ''ProcedureContext)

instance (Show c, CollectErrorsM m) =>
  CompilerContext c m [String] (ProcedureContext c) where
  ccCurrentScope = return . (^. pcScope)
  ccResolver = return . AnyTypeResolver . CategoryResolver . (^. pcCategories)
  ccSameType ctx t
    | ctx ^. pcScope == CategoryScope = return False
    | otherwise = ccSelfType ctx >>= return . (== t)
  ccSelfType ctx
    | ctx ^. pcScope == CategoryScope = compilerErrorM $ "Param " ++ show ParamSelf ++ " not found"
    | otherwise = return $ TypeInstance (ctx ^. pcType)
                                        (fmap (singleType . JustParamName False . vpParam) $ ctx ^. pcExtParams)
  ccAllFilters = return . (^. pcAllFilters)
  ccGetParamScope ctx p = do
    case p `Map.lookup` (ctx ^. pcParamScopes) of
            (Just s) -> return s
            _ -> compilerErrorM $ "Param " ++ show p ++ " not found"
  ccAddRequired ctx ts = return $ ctx & pcRequiredTypes <>~ ts
  ccGetRequired = return . (^. pcRequiredTypes)
  ccGetCategoryFunction ctx c Nothing n = ccGetCategoryFunction ctx c (Just $ ctx ^. pcType) n
  ccGetCategoryFunction ctx c (Just t) n = getFunction where
    getFunction
      -- Same category as the procedure itself.
      | t == ctx ^. pcType = checkFunction $ n `Map.lookup` (ctx ^. pcFunctions)
      -- A different category than the procedure.
      | otherwise = do
        (_,ca) <- getCategory (ctx ^. pcCategories) (c,t)
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        checkFunction $ n `Map.lookup` fa
    checkFunction (Just f) = do
      when (ctx ^. pcDisallowInit && t == ctx ^. pcType && ctx ^. pcScope == CategoryScope) $
        compilerErrorM $ "Function " ++ show n ++
                       " disallowed during initialization" ++ formatFullContextBrace c
      when (sfScope f /= CategoryScope) $
        compilerErrorM $ "Function " ++ show n ++ " in " ++ show t ++ " cannot be used as a category function"
      return f
    checkFunction _ =
      compilerErrorM $ "Category " ++ show t ++
                       " does not have a category function named " ++ show n ++
                       formatFullContextBrace c
  ccGetTypeFunction ctx c t n = do
    t' <- case ctx ^. pcScope of
               CategoryScope -> case t of
                                     Nothing -> compilerErrorM $ "Category " ++ show (ctx ^. pcType) ++
                                                                  " does not have a category function named " ++ show n ++
                                                                  formatFullContextBrace c
                                     Just t0 -> return t0
               _ -> do
                 self <- fmap (singleType . JustTypeInstance) $ ccSelfType ctx
                 case t of
                      Just t0 -> replaceSelfInstance self t0
                      Nothing -> return self
    getFunction t' t' where
      getFunction t0 t2 = reduceMergeTree getFromAny getFromAll (getFromSingle t0) t2
      getFromAny _ =
        compilerErrorM $ "Use explicit type conversion to call " ++ show n ++ " from " ++ show t
      getFromAll ts = do
        t' <- case t of
                   Just t2 -> return t2
                   Nothing -> fmap (singleType . JustTypeInstance) $ ccSelfType ctx
        collectFirstM ts <!!
          "Function " ++ show n ++ " not available for type " ++ show t' ++ formatFullContextBrace c
      getFromSingle t0 (JustParamName _ p) = do
        fa <- ccAllFilters ctx
        fs <- case p `Map.lookup` fa of
                   (Just fs) -> return fs
                   _ -> compilerErrorM $ "Param " ++ show p ++ " not found"
        let ts = map tfType $ filter isRequiresFilter fs
        let ds = map dfType $ filter isDefinesFilter  fs
        collectFirstM (map (getFunction t0) ts ++ map (checkDefine t0) ds) <!!
          "Function " ++ show n ++ " not available for param " ++ show p ++ formatFullContextBrace c
      getFromSingle t0 (JustTypeInstance t2)
        -- Same category as the procedure itself.
        | tiName t2 == ctx ^. pcType =
          subAndCheckFunction t0 (tiName t2) (fmap vpParam $ ctx ^. pcExtParams) (tiParams t2) $ n `Map.lookup` (ctx ^. pcFunctions)
        -- A different category than the procedure.
        | otherwise = do
          (_,ca) <- getCategory (ctx ^. pcCategories) (c,tiName t2)
          let params = Positional $ map vpParam $ getCategoryParams ca
          let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
          subAndCheckFunction t0 (tiName t2) params (tiParams t2) $ n `Map.lookup` fa
      getFromSingle _ _ = compilerErrorM $ "Type " ++ show t ++ " contains unresolved types"
      checkDefine t0 t2 = do
        (_,ca) <- getCategory (ctx ^. pcCategories) (c,diName t2)
        let params = Positional $ map vpParam $ getCategoryParams ca
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        subAndCheckFunction t0 (diName t2) params (diParams t2) $ n `Map.lookup` fa
      subAndCheckFunction t0 t2 ps1 ps2 (Just f) = do
        when (ctx ^. pcDisallowInit && t2 == ctx ^. pcType) $
          compilerErrorM $ "Function " ++ show n ++
                           " disallowed during initialization" ++ formatFullContextBrace c
        when (sfScope f == CategoryScope) $
          compilerErrorM $ "Function " ++ show n ++ " in " ++ show t2 ++
                           " is a category function" ++ formatFullContextBrace c
        paired <- processPairs alwaysPair ps1 ps2 <??
          "In external function call at " ++ formatFullContext c
        let assigned = Map.fromList paired
        uncheckedSubFunction assigned f >>= replaceSelfFunction (fixTypeParams t0)
      subAndCheckFunction _ t2 _ _ _ =
        compilerErrorM $ "Category " ++ show t2 ++
                         " does not have a type or value function named " ++ show n ++
                         formatFullContextBrace c
  ccCheckValueInit ctx c (TypeInstance t as) ts
    | t /= ctx ^. pcType =
      compilerErrorM $ "Category " ++ show (ctx ^. pcType) ++ " cannot initialize values from " ++
                     show t ++ formatFullContextBrace c
    | otherwise = "In initialization at " ++ formatFullContext c ??> do
      let t' = TypeInstance (ctx ^. pcType) as
      r <- ccResolver ctx
      allFilters <- ccAllFilters ctx
      pa  <- fmap Map.fromList $ processPairs alwaysPair (fmap vpParam $ ctx ^. pcExtParams) as
      validateTypeInstanceForCall r allFilters t'
      -- Check initializer types.
      ms <- fmap Positional $ mapCompilerM (subSingle pa) (ctx ^. pcMembers)
      processPairs_ (checkInit r allFilters) ms (Positional $ zip ([1..] :: [Int]) $ pValues ts)
      return ()
      where
        checkInit r fa (MemberValue c2 n t0) (i,t1) = do
          checkValueAssignment r fa t1 t0 <??
            "In initializer " ++ show i ++ " for " ++ show n ++ formatFullContextBrace c2
        subSingle pa (DefinedMember c2 _ t2 n _) = do
          t2' <- uncheckedSubValueType (getValueForParam pa) t2
          return $ MemberValue c2 n t2'
  ccGetVariable ctx (UsedVariable c n) =
    case n `Map.lookup` (ctx ^. pcVariables) of
           (Just (VariableValue _ _ _ (VariableHidden []))) ->
             compilerErrorM $ "Variable " ++ show n ++ formatFullContextBrace c ++
                              " is hidden"
           (Just (VariableValue _ _ _ (VariableHidden c2))) ->
             compilerErrorM $ "Variable " ++ show n ++ formatFullContextBrace c ++
                              " was explicitly hidden at " ++ formatFullContext c2
           (Just v) -> return v
           _ -> compilerErrorM $ "Variable " ++ show n ++ formatFullContextBrace c ++ " is not defined"
  ccAddVariable ctx (UsedVariable c n) t = do
    case n `Map.lookup` (ctx ^. pcVariables) of
          Nothing -> return ()
          (Just v) -> compilerErrorM $ "Variable " ++ show n ++
                                       formatFullContextBrace c ++
                                       " is already defined: " ++ show v
    return $ ctx & pcVariables <>~ Map.fromList [(n,t)]
  ccSetReadOnly ctx v@(UsedVariable c n) = do
    (VariableValue c2 s t _) <- ccGetVariable ctx v
    return $ ctx & pcVariables %~ Map.insert n (VariableValue c2 s t (VariableReadOnly c))
  ccSetHidden ctx v@(UsedVariable c n) = do
    (VariableValue c2 s t _) <- ccGetVariable ctx v
    return $ ctx & pcVariables %~ Map.insert n (VariableValue c2 s t (VariableHidden c))
  ccCheckVariableInit ctx vs
    -- Returns are checked during cleanup inlining.
    | ctx ^. pcInCleanup = return ()
    | otherwise =
        case ctx ^. pcReturns of
             ValidateNames _ _ na -> mapCompilerM_ (checkSingle na) vs
             _ -> return ()
        where
          checkSingle na (UsedVariable c n) =
             when (n `Map.member` na) $
               compilerErrorM $ "Named return " ++ show n ++
                                " might not be initialized" ++ formatFullContextBrace c
  ccWrite ctx ss = return $ ctx & pcOutput <>~ ss
  ccGetOutput = return . (^. pcOutput)
  ccClearOutput ctx = return $ ctx & pcOutput .~ mempty
  ccUpdateAssigned ctx n = return $ ctx & pcReturns %~ update where
    update (ValidateNames ns ts ra) = ValidateNames ns ts $ Map.delete n ra
    update rs                       = rs
  ccAddUsed ctx v
    | ctx ^. pcInCleanup = return $ ctx & pcUsedVars <>~ [v]
    | otherwise          = return ctx
  ccInheritReturns ctx cs = return $ ctx & pcReturns .~ returns & pcJumpType .~ jump & pcUsedVars .~ used where
    used = ctx ^. pcUsedVars ++ (concat $ map (^. pcUsedVars) cs)
    (returns,jump) = combineSeries (ctx ^. pcReturns,ctx ^. pcJumpType) inherited
    combineSeries (r@(ValidatePositions _),j1) (_,j2) = (r,max j1 j2)
    combineSeries (_,j1) (r@(ValidatePositions _),j2) = (r,max j1 j2)
    combineSeries (ValidateNames ns ts ra1,j1) (ValidateNames _ _ ra2,j2) = (ValidateNames ns ts $ Map.intersection ra1 ra2,max j1 j2)
    inherited = foldr combineParallel (ValidateNames (Positional []) (Positional []) Map.empty,JumpMax) $ zip (map (^. pcReturns) cs) (map (^. pcJumpType) cs)
    -- Ignore a branch if it jumps to a higher scope.
    combineParallel (_,j1) (r,j2) | j1 > NextStatement = (r,min j1 j2)
    combineParallel (ValidateNames ns ts ra1,j1) (ValidateNames _ _ ra2,j2) = (ValidateNames ns ts $ Map.union ra1 ra2,min j1 j2)
    combineParallel (r@(ValidatePositions _),j1) (_,j2) = (r,min j1 j2)
    combineParallel (_,j1) (r@(ValidatePositions _),j2) = (r,min j1 j2)
  ccInheritUsed ctx ctx2 = return $ ctx & pcUsedVars <>~ (ctx2 ^. pcUsedVars)
  ccRegisterReturn ctx c vs = do
    returns <- check (ctx ^. pcReturns)
    return $ ctx & pcReturns .~ returns & pcJumpType .~ JumpReturn
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
             Nothing -> mapCompilerM_ alwaysError $ Map.toList ra
        return (ValidateNames ns ts Map.empty)
      alwaysError (n,_) = compilerErrorM $ "Named return " ++ show n ++
                                           " might not be initialized" ++
                                           formatFullContextBrace c
  ccPrimNamedReturns = return . (^. pcPrimNamed)
  ccIsUnreachable ctx
    | ctx ^. pcInCleanup = return $ ctx ^. pcJumpType > JumpReturn
    | otherwise          = return $ ctx ^. pcJumpType > NextStatement
  ccIsNamedReturns = return . (^. pcIsNamed)
  ccSetJumpType ctx c j
    | ctx ^. pcInCleanup && j == JumpBreak =
        compilerErrorM $ "Explicit break at " ++ formatFullContext c ++ " not allowed in cleanup"
    | ctx ^. pcInCleanup && j == JumpContinue =
        compilerErrorM $ "Explicit continue at " ++ formatFullContext c ++ " not allowed in cleanup"
    | ctx ^. pcInCleanup && j == JumpReturn =
        compilerErrorM $ "Explicit return at " ++ formatFullContext c ++ " not allowed in cleanup"
    | otherwise = return $ ctx & pcJumpType %~ max j
  ccStartLoop ctx l = return $ ctx & pcLoopSetup .~ l & pcCleanupBlocks %~ (Nothing:)
  ccGetLoop = return . (^. pcLoopSetup)
  ccStartCleanup ctx c = do
    let vars = protectReturns (ctx ^. pcReturns) (ctx ^. pcVariables)
    return $ ctx & pcVariables .~ vars & pcInCleanup .~ True where
    protectReturns (ValidateNames ns _ _) vs = foldr protect vs (pValues ns)
    protectReturns _                      vs = vs
    protect n vs =
      case n `Map.lookup` vs of
           Just (VariableValue c2 s@LocalScope t _) -> Map.insert n (VariableValue c2 s t (VariableReadOnly c)) vs
           _ -> vs
  ccPushCleanup ctx ctx2 = return $ ctx & pcCleanupBlocks %~ (Just cleanup:) where
      cleanup = CleanupBlock (ctx2 ^. pcOutput) (ctx2 ^. pcUsedVars) (ctx2 ^. pcJumpType) (ctx2 ^. pcRequiredTypes)
  ccGetCleanup ctx j = return combined where
    combined
      | j == NextStatement =
          case ctx ^. pcCleanupBlocks of
               ((Just b):_) -> b
               _            -> emptyCleanupBlock
      | j == JumpReturn                     = combine $ map fromJust $ filter    isJust $ ctx ^. pcCleanupBlocks
      | j == JumpBreak || j == JumpContinue = combine $ map fromJust $ takeWhile isJust $ ctx ^. pcCleanupBlocks
      | otherwise = emptyCleanupBlock
    combine cs = CleanupBlock (concat $ map csCleanup cs)
                              (concat $ map csUsesVars cs)
                              (foldr max NextStatement (map csJumpType cs))
                              (Set.unions $ map csRequires cs)
  ccExprLookup ctx c n =
    case n `Map.lookup` (ctx ^. pcExprMap) of
         Nothing -> compilerErrorM $ "Env expression " ++ show n ++ " is not defined" ++ formatFullContextBrace c
         Just e -> do
           checkReserved (ctx ^. pcReservedMacros) [(n,c)]
           return e
    where
      checkReserved [] _ = return ()
      checkReserved (m@(n2,_):ms) rs
        | n2 /= n = checkReserved ms (m:rs)
        | otherwise = (mapCompilerM_ singleError (m:rs)) <!!
            "Expression macro " ++ show n ++ " references itself"
      singleError (n2,c2) = compilerErrorM $ show n2 ++ " expanded at " ++ formatFullContext c2
  ccReserveExprMacro ctx c n = return $ ctx & pcReservedMacros %~ ((n,c):)
  ccReleaseExprMacro ctx _ n = return $ ctx & pcReservedMacros %~ (filter ((/= n) . fst))
  ccSetNoTrace ctx t = return $ ctx & pcNoTrace .~ t
  ccGetNoTrace = return . (^. pcNoTrace)

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
               Nothing -> return $ Map.insert (ovName r) (VariableValue c LocalScope t VariableDefault) va'
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
  foldM update ma as' where
    update va (PassedValue _ t,a) = do
      let c = ivContext a
      case ivName a `Map.lookup` va of
            Nothing -> return $ Map.insert (ivName a) (VariableValue c LocalScope t (VariableReadOnly c)) va
            (Just v) -> compilerErrorM $ "Variable " ++ show (ivName a) ++
                                         formatFullContextBrace c ++
                                         " is already defined" ++
                                         formatFullContextBrace (vvContext v)
