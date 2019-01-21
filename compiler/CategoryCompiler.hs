{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CategoryCompiler (
  ProcedureContext(..),
  ReturnValidation(..),
  mapMembers,
  pairProceduresToFunctions,
  setInternalFunctions,
  updateArgVariables,
  updateReturnVariables,
) where

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
    pcType :: TypeInstance,
    pcCategories :: CategoryMap c,
    pcFilters :: ParamFilters,
    pcParamScopes :: Map.Map ParamName SymbolScope,
    pcFunctions :: Map.Map FunctionName (ScopedFunction c),
    pcVariables :: Map.Map VariableName (VariableValue c),
    pcReturns :: ReturnValidation c,
    pcRequiredTypes :: Set.Set TypeName,
    pcOutput :: [String]
  }

data ReturnValidation c =
  ValidatePositions {
    vpReturns :: ParamSet (PassedValue c)
  } |
  ValidateNames {
    vnReturns :: Map.Map VariableName (PassedValue c)
  } |
  NoValidation

instance (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CompilerContext c m [String] (ProcedureContext c) where
  ccCurrentScope = return . pcScope
  ccResolver = return . categoriesToTypeResolver . pcCategories
  ccAllFilters = return . pcFilters
  ccGetParamScope ctx p = do
    case p `Map.lookup` pcParamScopes ctx of
            (Just s) -> return s
            _ -> compileError $ "Param " ++ show p ++ " does not exist"
  ccRequiresTypes ctx ts = return $
    ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = Set.union (pcRequiredTypes ctx) ts,
      pcOutput = pcOutput ctx
    }
  ccGetRequired = return . pcRequiredTypes
  ccGetFunction ctx c n (Just t@(TypeMerge MergeUnion _)) =
    compileError $ "Cannot resolve function " ++ show n ++ " for union type " ++ show t
  ccGetFunction ctx c n (Just t@(TypeMerge MergeIntersect ts)) =
    collectOneOrErrorM $ map (ccGetFunction ctx c n) $ map Just ts
  ccGetFunction ctx c n (Just (SingleType (JustParamName p))) = do
    fs <- case p `Map.lookup` pcFilters ctx of
               (Just fs) -> return fs
               _ -> compileError $ "Param " ++ show p ++ " does not exist"
    let ts = map tfType $ filter isRequiresFilter fs
    collectOneOrErrorM $ map (ccGetFunction ctx c n) $ map (Just . SingleType) ts
  ccGetFunction ctx c n (Just (SingleType (JustTypeInstance t)))
    -- Same category as the procedure itself.
    | tiName t == tiName (pcType ctx) =
      case n `Map.lookup` pcFunctions ctx of
           (Just f) -> return f
           _ -> compileError $ "Category " ++ show (tiName t) ++
                               " does not have a function named " ++ show n
    -- A different category than the procedure.
    | otherwise = do
      (_,ca) <- getCategory (pcCategories ctx) (c,tiName t)
      let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
      case n `Map.lookup` fa of
           (Just f) -> return f
           _ -> compileError $ "Category " ++ show (tiName t) ++
                               " does not have a function named " ++ show n
  ccGetFunction ctx c n Nothing =
    ccGetFunction ctx c n (Just $ SingleType $ JustTypeInstance $ pcType ctx)
  ccGetVariable ctx c n =
      case n `Map.lookup` pcVariables ctx of
           (Just v) -> return v
           _ -> compileError $ "Variable " ++ show n ++ " is not defined"
  ccAddVariable ctx c n t = do
      case n `Map.lookup` pcVariables ctx of
           Nothing -> return ()
           (Just v) -> compileError $ "Variable " ++ show n ++ " [" ++
                                      formatFullContext c ++
                                      "] is already defined: " ++ show v
      return $ ProcedureContext {
          pcScope = pcScope ctx,
          pcType = pcType ctx,
          pcCategories = pcCategories ctx,
          pcFilters = pcFilters ctx,
          pcParamScopes = pcParamScopes ctx,
          pcFunctions = pcFunctions ctx,
          pcVariables = Map.insert n t (pcVariables ctx),
          pcReturns = pcReturns ctx,
          pcRequiredTypes = pcRequiredTypes ctx,
          pcOutput = pcOutput ctx
        }
  ccWrite ctx ss = return $
    ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx ++ ss
    }
  ccGetOutput = return . pcOutput
  ccUpdateAssigned ctx n = update (pcReturns ctx) where
    update (ValidateNames ra) = return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcCategories = pcCategories ctx,
        pcFilters = pcFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = ValidateNames $ Map.delete n ra,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx
      }
    update _ = return ctx
  ccInheritReturns ctx cs = return $ ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = foldr update NoValidation (map pcReturns cs),
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx
    }
    where
      update r@(ValidatePositions _) _ = r
      update NoValidation r = r
      update r NoValidation = r
      update (ValidateNames ra1) (ValidateNames ra2) = ValidateNames $ Map.union ra1 ra2
  ccRegisterReturn ctx c vs = do
    check (pcReturns ctx)
    return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcCategories = pcCategories ctx,
        pcFilters = pcFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = NoValidation,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx
      }
    where
      check (ValidatePositions rs) = do
        processParamPairs checkReturnType rs (ParamSet $ zip [1..] $ psParams vs)
        return ()
        where
          checkReturnType ta0@(PassedValue c0 t0) (n,t) = do
            r <- ccResolver ctx
            pa <- ccAllFilters ctx
            checkValueTypeMatch r pa t t0 `reviseError`
              ("Cannot convert " ++ show t ++ " to " ++ show ta0 ++ " in return " ++
               show n ++ " at " ++ formatFullContext c)
      check (ValidateNames ra)
        | not $ null $ psParams vs =
          compileError $ "Positional returns not allowed when returns are named [" ++
                         formatFullContext c ++ "]"
        | otherwise =
          mergeAllM $ map alwaysError $ Map.toList ra
          where
            alwaysError (n,t) = compileError $ "Named return " ++ show n ++ " (" ++ show t ++
                                               ") might not have been set before return at " ++
                                               formatFullContext c
      check _ = return ()

setInternalFunctions :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  TypeResolver m -> AnyCategory c -> [ScopedFunction c] ->
  m (Map.Map FunctionName (ScopedFunction c))
setInternalFunctions r t fs = foldr update (return start) fs where
  start = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions t
  filters = getCategoryFilterMap t
  update f@(ScopedFunction c n t2 s as rs ps fs ms) fa = do
    validateCategoryFunction r t f
    fa' <- fa
    case n `Map.lookup` fa' of
         Nothing -> return $ Map.insert n f fa'
         (Just f0@(ScopedFunction c2 _ _ _ _ _ _ _ ms2)) -> do
           flip reviseError ("In function merge:\n---\n" ++ show f0 ++
                             "\n  ->\n" ++ show f ++ "\n---\n") $ do
             f0' <- parsedToFunctionType f0
             f' <- parsedToFunctionType f
             checkFunctionConvert r filters f0' f'
           return $ Map.insert n (ScopedFunction (c++c2) n t2 s as rs ps fs (ms++ms2)) fa'

pairProceduresToFunctions :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  Map.Map FunctionName (ScopedFunction c) -> [ExecutableProcedure c] ->
  m [(ScopedFunction c,ExecutableProcedure c)]
pairProceduresToFunctions fa ps = do
  pa <- foldr updateProcedure (return Map.empty) ps
  let allNames = Set.union (Map.keysSet fa) (Map.keysSet pa)
  foldr (updatePairs fa pa) (return []) $ Set.toList allNames
  where
    updateProcedure p pa = do
      pa' <- pa
      case epName p `Map.lookup` pa' of
           Nothing -> return ()
           -- TODO: The error might show things in the wrong order.
           (Just p0) -> compileError $ "Procedure " ++ show (epName p) ++ " [" ++
                                       formatFullContext (epContext p) ++
                                       "] is already defined [" ++
                                       formatFullContext (epContext p0) ++ "]"
      return $ Map.insert (epName p) p pa'
    updatePairs fa pa n ps = do
      ps' <- ps
      p <- getPair (n `Map.lookup` fa) (n `Map.lookup` pa)
      return (p:ps')
    getPair (Just f) Nothing =
      compileError $ "Function " ++ show (sfName f) ++
                     " [" ++ formatFullContext (sfContext f) ++
                     "] has no procedure definition"
    getPair Nothing (Just p) =
      compileError $ "Procedure " ++ show (epName p) ++
                     " [" ++ formatFullContext (epContext p) ++
                     "] has no procedure definition"
    getPair (Just f) (Just p) = do
      processParamPairs alwaysPairParams (sfArgs f) (avNames $ epArgs p) `reviseError`
        ("Procedure for " ++ show (sfName f) ++ " [" ++
         formatFullContext (avContext $ epArgs p) ++
         "] has the wrong number of arguments [" ++
         formatFullContext (sfContext f) ++ "]")
      if isUnnamedReturns (epReturns p)
         then return ()
         else do
           processParamPairs alwaysPairParams (sfArgs f) (nrNames $ epReturns p) `reviseError`
             ("Procedure for " ++ show (sfName f) ++ " [" ++
              formatFullContext (nrContext $ epReturns p) ++
              "] has the wrong number of returns [" ++
              formatFullContext (sfContext f) ++ "]")
           return ()
      return (f,p)

mapMembers :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  [DefinedMember c] -> m (Map.Map VariableName (VariableValue c))
mapMembers ms = foldr update (return Map.empty) ms where
  update m ma = do
    ma' <- ma
    case dmName m `Map.lookup` ma' of
         Nothing ->  return ()
         -- TODO: The error might show things in the wrong order.
         (Just m0) -> compileError $ "Member " ++ show (dmName m) ++ " [" ++
                                     formatFullContext (dmContext m) ++
                                     "] is already defined [" ++
                                     formatFullContext (vvContext m0) ++ "]"
    return $ Map.insert (dmName m) (VariableValue (dmContext m) (dmScope m) (dmType m)) ma'

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
               Nothing -> return $ Map.insert (ovName r) (VariableValue c LocalScope t) va'
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
            Nothing -> return $ Map.insert (ivName a) (VariableValue c LocalScope t) va'
            (Just v) -> compileError $ "Variable " ++ show (ivName a) ++
                                       " [" ++ formatFullContext (ivContext a) ++
                                       "] is already defined [" ++
                                       formatFullContext (vvContext v) ++ "]"
