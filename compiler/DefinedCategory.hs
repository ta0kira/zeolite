{-# LANGUAGE Safe #-}

module DefinedCategory (
  DefinedCategory(..),
  DefinedMember(..),
  VariableValue(..),
  isInitialized,
  mapMembers,
  pairProceduresToFunctions,
  setInternalFunctions,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Function
import Procedure
import TypeCategory
import TypeInstance
import TypesBase


data DefinedCategory c =
  DefinedCategory {
    dcContext :: [c],
    dcName :: CategoryName,
    dcParams :: [ValueParam c],
    dcParamFilter :: [ParamFilter c],
    dcMembers :: [DefinedMember c],
    dcProcedures :: [ExecutableProcedure c],
    dcFunctions :: [ScopedFunction c]
  }
  deriving (Show) -- TODO: Implement Show.

data DefinedMember c =
  DefinedMember {
    dmContext :: [c],
    dmScope :: SymbolScope,
    dmType :: ValueType,
    dmName :: VariableName,
    dmInit :: Maybe (Expression c)
  }
  deriving (Show) -- TODO: Implement Show.

isInitialized :: DefinedMember c -> Bool
isInitialized = check . dmInit where
  check Nothing = False
  check _       = True

data VariableValue c =
  VariableValue {
    vvContext :: [c],
    vvScope :: SymbolScope,
    vvType :: ValueType,
    vvWritable :: Bool
  }

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
           return $ Map.insert n (ScopedFunction (c++c2) n t2 s as rs ps fs ([f0]++ms++ms2)) fa'

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
                     "] does not correspond to a function"
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
    return $ Map.insert (dmName m) (VariableValue (dmContext m) (dmScope m) (dmType m) True) ma'
