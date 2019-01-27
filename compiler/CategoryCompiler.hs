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
    pcParams :: ParamSet ParamName,
    pcMembers :: [DefinedMember c],
    pcCategories :: CategoryMap c,
    pcFilters :: ParamFilters,
    pcParamScopes :: Map.Map ParamName SymbolScope,
    pcFunctions :: Map.Map FunctionName (ScopedFunction c),
    pcVariables :: Map.Map VariableName (VariableValue c),
    pcReturns :: ReturnValidation c,
    pcRequiredTypes :: Set.Set CategoryName,
    pcOutput :: [String],
    pcDisallowInit :: Bool
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
      pcParams = pcParams ctx,
      pcMembers = pcMembers ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = Set.union (pcRequiredTypes ctx) ts,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx
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
      compileError $ "Cannot resolve function " ++ show n ++ " for union type " ++
                     show t ++ " [" ++ formatFullContext c ++ "]"
    getFunction (Just t@(TypeMerge MergeIntersect ts)) =
      collectOneOrErrorM $ map getFunction $ map Just ts
    getFunction (Just (SingleType (JustParamName p))) = do
      fs <- case p `Map.lookup` pcFilters ctx of
                (Just fs) -> return fs
                _ -> compileError $ "Param " ++ show p ++ " does not exist"
      let ts = map tfType $ filter isRequiresFilter fs
      let ds = map dfType $ filter isDefinesFilter fs
      collectOneOrErrorM $ (map getFunction $ map (Just . SingleType) ts) ++ (map checkDefine ds)
    getFunction (Just (SingleType (JustTypeInstance t)))
      -- Same category as the procedure itself.
      | tiName t == pcType ctx =
        checkFunction (tiName t) (pcParams ctx) (tiParams t) $ n `Map.lookup` pcFunctions ctx
      -- A different category than the procedure.
      | otherwise = do
        (_,ca) <- getCategory (pcCategories ctx) (c,tiName t)
        let params = ParamSet $ map vpParam $ getCategoryParams ca
        let fa = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions ca
        checkFunction (tiName t) params (tiParams t) $ n `Map.lookup` fa
    getFunction Nothing = do
      let ps = fmap (SingleType . JustParamName) $ pcParams ctx
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
        compileError $ "Function " ++ show n ++ " in " ++ show t2 ++ " is a category function"
      paired <- processParamPairs alwaysPairParams ps1 ps2 `reviseError`
        ("In external function call at " ++ formatFullContext c)
      let assigned = Map.fromList paired
      uncheckedSubFunction assigned f
    checkFunction t2 _ _ _ =
      compileError $ "Category " ++ show t2 ++
                     " does not have a function named " ++ show n ++ " [" ++
                     formatFullContext c ++ "]"
  ccGetValueInit ctx c (TypeInstance t as)
    | pcDisallowInit ctx =
      compileError $ "Value initialization not allowed here [" ++ formatFullContext c ++ "]"
    | t /= pcType ctx =
      compileError $ "Category " ++ show (pcType ctx) ++ " cannot initialize values from " ++
                     show t ++ " [" ++ formatFullContext c ++ "]"
    | otherwise = do
      let t' = TypeInstance (pcType ctx) as
      r <- ccResolver ctx
      fa <- ccAllFilters ctx
      validateTypeInstance r fa t'
      pa <- fmap Map.fromList $ processParamPairs alwaysPairParams (pcParams ctx) as
      fmap ParamSet $ collectAllOrErrorM $ map (subSingle pa) (pcMembers ctx)
      where
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
          pcParams = pcParams ctx,
          pcMembers = pcMembers ctx,
          pcCategories = pcCategories ctx,
          pcFilters = pcFilters ctx,
          pcParamScopes = pcParamScopes ctx,
          pcFunctions = pcFunctions ctx,
          pcVariables = Map.insert n t (pcVariables ctx),
          pcReturns = pcReturns ctx,
          pcRequiredTypes = pcRequiredTypes ctx,
          pcOutput = pcOutput ctx,
          pcDisallowInit = pcDisallowInit ctx
        }
  ccWrite ctx ss = return $
    ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcParams = pcParams ctx,
      pcMembers = pcMembers ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx ++ ss,
      pcDisallowInit = pcDisallowInit ctx
    }
  ccGetOutput = return . pcOutput
  ccClearOutput ctx = return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcParams = pcParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcFilters = pcFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = pcReturns ctx,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = [],
        pcDisallowInit = pcDisallowInit ctx
      }
  ccUpdateAssigned ctx n = update (pcReturns ctx) where
    update (ValidateNames ra) = return $ ProcedureContext {
        pcScope = pcScope ctx,
        pcType = pcType ctx,
        pcParams = pcParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcFilters = pcFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = ValidateNames $ Map.delete n ra,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx
      }
    update _ = return ctx
  ccInheritReturns ctx cs = return $ ProcedureContext {
      pcScope = pcScope ctx,
      pcType = pcType ctx,
      pcParams = pcParams ctx,
      pcMembers = pcMembers ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcParamScopes = pcParamScopes ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = foldr update NoValidation (map pcReturns cs),
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx,
      pcDisallowInit = pcDisallowInit ctx
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
        pcParams = pcParams ctx,
        pcMembers = pcMembers ctx,
        pcCategories = pcCategories ctx,
        pcFilters = pcFilters ctx,
        pcParamScopes = pcParamScopes ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = NoValidation,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx,
        pcDisallowInit = pcDisallowInit ctx
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
           processParamPairs alwaysPairParams (sfReturns f) (nrNames $ epReturns p) `reviseError`
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
