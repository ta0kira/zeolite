{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module CompilerCxx (
) where

import Control.Monad (when)
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

headerFilename :: TypeName -> String
headerFilename n = "Category_" ++ show n ++ ".hxx"

sourceFilename :: TypeName -> String
sourceFilename n = "Category_" ++ show n ++ ".cxx"

data ProcedureContext c =
  ProcedureContext {
    pcType :: TypeInstance,
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
  ccRequiresType ctx n = return $
    ProcedureContext {
      pcType = pcType ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = n `Set.insert` pcRequiredTypes ctx,
      pcOutput = pcOutput ctx
    }
  ccGetFunction ctx c n (Just t@(TypeMerge MergeUnion _)) =
    compileError $ "Cannot resolve function " ++ show n ++ " for union type " ++ show t
  ccGetFunction ctx c n (Just t@(TypeMerge MergeIntersect ts)) =
    collectOneOrErrorM $ map (ccGetFunction ctx c n) $ map Just ts
  ccGetFunction ctx c n (Just (SingleType (JustParamName p))) = do
    fs <- case p `Map.lookup` pcFilters ctx of
               (Just fs) -> return fs
               _ -> compileError $ "Param " ++ show n ++ " does not exist"
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
    collectOneOrErrorM [ccGetFunction ctx c n (Just $ SingleType $ JustTypeInstance $ pcType ctx),
                        getBuiltin c n]
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
          pcType = pcType ctx,
          pcCategories = pcCategories ctx,
          pcFilters = pcFilters ctx,
          pcFunctions = pcFunctions ctx,
          pcVariables = Map.insert n t (pcVariables ctx),
          pcReturns = pcReturns ctx,
          pcRequiredTypes = pcRequiredTypes ctx,
          pcOutput = pcOutput ctx
        }
  ccWrite ctx ss = return $
    ProcedureContext {
      pcType = pcType ctx,
      pcCategories = pcCategories ctx,
      pcFilters = pcFilters ctx,
      pcFunctions = pcFunctions ctx,
      pcVariables = pcVariables ctx,
      pcReturns = pcReturns ctx,
      pcRequiredTypes = pcRequiredTypes ctx,
      pcOutput = pcOutput ctx ++ ss
    }
  ccGetOutput = return . pcOutput
  ccUpdateAssigned ctx n = update (pcReturns ctx) where
    update (Left _) = return ctx
    update (Right ra) = return $ ProcedureContext {
        pcType = pcType ctx,
        pcCategories = pcCategories ctx,
        pcFilters = pcFilters ctx,
        pcFunctions = pcFunctions ctx,
        pcVariables = pcVariables ctx,
        pcReturns = Right $ Map.delete n ra,
        pcRequiredTypes = pcRequiredTypes ctx,
        pcOutput = pcOutput ctx
      }
  ccCheckReturn ctx c vs = check (pcReturns ctx) where
    check (Left rs) = do
      processParamPairs checkReturnType rs (ParamSet $ zip [1..] $ psParams vs)
      return ()
      where
        checkReturnType ta0@(PassedValue c0 t0) (n,t) = do
          r <- ccResolver ctx
          pa <- ccAllFilters ctx
          checkValueTypeMatch r pa t t0 `reviseError`
            ("Cannot convert " ++ show t ++ " to " ++ show ta0 ++ " in return " ++
             show n ++ " [" ++ formatFullContext c ++ "]")
    check (Right ra)
      | not $ null $ psParams vs =
        compileError $ "Positional returns not allowed when returns are named [" ++
                       formatFullContext c ++ "]"
      | otherwise =
        mergeAll $ map alwaysError $ Map.toList ra
        where
          alwaysError (n,t) = compileError $ "Named return " ++ show n ++ " (" ++
                                             show t ++ ") might not have been set"

getBuiltin :: (Show c, CompileErrorM m, Monad m) =>
  [c] -> FunctionName -> m (ScopedFunction c)
getBuiltin c n = undefined

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
  -- TODO: Turn types into includes.
  let types = Set.unions $ map fst tx
  let content = concat $ map snd tx
  return $ CxxOutput filename content

setInternalFunctions :: (Show c, Monad m, CompileErrorM m, MergeableM m) =>
  TypeResolver m () -> ParamFilters -> [ScopedFunction c] -> [ScopedFunction c] ->
  m (Map.Map FunctionName (ScopedFunction c))
setInternalFunctions r fm fs0 fs = foldr update (return start) fs where
  start = Map.fromList $ map (\f -> (sfName f,f)) fs0
  update f fa = do
    fa' <- fa
    case sfName f `Map.lookup` fa' of
         Nothing -> return $ Map.insert (sfName f) f fa'
         (Just f0) -> do
           flip reviseError ("In function merge:\n---\n" ++ show f0 ++
                             "\n  ->\n" ++ show f ++ "\n---\n") $ do
             f0' <- parsedToFunctionType f0
             f' <- parsedToFunctionType f
             checkFunctionConvert r fm f0' f'
           return $ Map.insert (sfName f) f fa'

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
  vs <- updateVariables s ma rs2
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
