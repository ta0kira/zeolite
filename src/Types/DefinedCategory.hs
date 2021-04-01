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

{-# LANGUAGE Safe #-}

module Types.DefinedCategory (
  DefinedCategory(..),
  DefinedMember(..),
  PragmaDefined(..),
  VariableRule(..),
  VariableValue(..),
  isInitialized,
  isMembersHidden,
  isMembersReadOnly,
  mapMembers,
  mergeInternalInheritance,
  pairProceduresToFunctions,
  replaceSelfMember,
  setInternalFunctions,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.Positional
import Types.Function
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


data DefinedCategory c =
  DefinedCategory {
    dcContext :: [c],
    dcName :: CategoryName,
    dcPragmas :: [PragmaDefined c],
    dcRefines :: [ValueRefine c],
    dcDefines :: [ValueDefine c],
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

data PragmaDefined c =
  MembersReadOnly {
    mroContext :: [c],
    mroMembers :: [VariableName]
  } |
  MembersHidden {
    mhContext :: [c],
    mhMembers :: [VariableName]
  }
  deriving (Show)

isMembersReadOnly :: PragmaDefined c -> Bool
isMembersReadOnly (MembersReadOnly _ _) = True
isMembersReadOnly _                     = False

isMembersHidden :: PragmaDefined c -> Bool
isMembersHidden (MembersHidden _ _) = True
isMembersHidden _                   = False

data VariableRule c =
  VariableDefault |
  VariableReadOnly {
    vroContext :: [c]
  } |
  VariableHidden {
    vhContext :: [c]
  }

data VariableValue c =
  VariableValue {
    vvContext :: [c],
    vvScope :: SymbolScope,
    vvType :: ValueType,
    vvReadOnlyAt :: VariableRule c
  }

instance Show c => Show (VariableValue c) where
  show (VariableValue c _ t ro) = show t ++ formatFullContextBrace c ++ format ro where
    format (VariableReadOnly c2) = " (read-only at " ++ formatFullContext c2 ++ ")"
    format (VariableHidden c2)   = " (hidden at " ++ formatFullContext c2 ++ ")"
    format _ = ""

setInternalFunctions :: (Show c, CollectErrorsM m, TypeResolver r) =>
  r -> AnyCategory c -> [ScopedFunction c] ->
  m (Map.Map FunctionName (ScopedFunction c))
setInternalFunctions r t fs = do
  fm <- getCategoryFilterMap t
  foldr (update fm) (return start) fs where
  start = Map.fromList $ map (\f -> (sfName f,f)) $ getCategoryFunctions t
  pm = getCategoryParamMap t
  update fm f@(ScopedFunction c n t2 s as rs ps fs2 ms) fa = do
    validateCategoryFunction r t f
    fa' <- fa
    case n `Map.lookup` fa' of
         Nothing -> return $ Map.insert n f fa'
         (Just f0@(ScopedFunction c2 _ _ _ _ _ _ _ ms2)) -> do
           ("In function merge:\n---\n" ++ show f0 ++
             "\n  ->\n" ++ show f ++ "\n---\n") ??> do
              f0' <- parsedToFunctionType f0
              f' <- parsedToFunctionType f
              checkFunctionConvert r fm pm f0' f'
           return $ Map.insert n (ScopedFunction (c++c2) n t2 s as rs ps fs2 ([f0]++ms++ms2)) fa'

pairProceduresToFunctions :: (Show c, CollectErrorsM m) =>
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
           (Just p0) -> compilerErrorM $ "Procedure " ++ show (epName p) ++
                                       formatFullContextBrace (epContext p) ++
                                       " is already defined" ++
                                       formatFullContextBrace (epContext p0)
      return $ Map.insert (epName p) p pa'
    updatePairs fa2 pa n ps2 = do
      ps2' <- ps2
      p <- getPair (n `Map.lookup` fa2) (n `Map.lookup` pa)
      return (p:ps2')
    getPair (Just f) Nothing =
      compilerErrorM $ "Function " ++ show (sfName f) ++
                     formatFullContextBrace (sfContext f) ++
                     " has no procedure definition"
    getPair Nothing (Just p) =
      compilerErrorM $ "Procedure " ++ show (epName p) ++
                     formatFullContextBrace (epContext p) ++
                     " does not correspond to a function"
    getPair (Just f) (Just p) = do
      processPairs_ alwaysPair (sfArgs f) (avNames $ epArgs p) <!!
        ("Procedure for " ++ show (sfName f) ++
         formatFullContextBrace (avContext $ epArgs p) ++
         " has the wrong number of arguments" ++
         formatFullContextBrace (sfContext f))
      if isUnnamedReturns (epReturns p)
         then return ()
         else do
           processPairs_ alwaysPair (sfReturns f) (nrNames $ epReturns p) <!!
             ("Procedure for " ++ show (sfName f) ++
              formatFullContextBrace (nrContext $ epReturns p) ++
              " has the wrong number of returns" ++
              formatFullContextBrace (sfContext f))
           return ()
      return (f,p)
    getPair _ _ = undefined

mapMembers :: (Show c, CollectErrorsM m) =>
  Map.Map VariableName [c] -> Map.Map VariableName [c] -> [DefinedMember c] ->
  m (Map.Map VariableName (VariableValue c))
mapMembers readOnly hidden ms = foldr update (return Map.empty) ms where
  update m ma = do
    ma' <- ma
    case dmName m `Map.lookup` ma' of
         Nothing ->  return ()
         -- TODO: The error might show things in the wrong order.
         (Just m0) -> compilerErrorM $ "Member " ++ show (dmName m) ++
                                     formatFullContextBrace (dmContext m) ++
                                     " is already defined" ++
                                     formatFullContextBrace (vvContext m0)
    return $ Map.insert (dmName m) (VariableValue (dmContext m) (dmScope m) (dmType m) (memberRule m)) ma'
  memberRule m =
    case (dmName m `Map.lookup` hidden,dmName m `Map.lookup` readOnly) of
         (Just c,_) -> VariableHidden   c
         (_,Just c) -> VariableReadOnly c
         _ -> VariableDefault

-- TODO: Most of this duplicates parts of flattenAllConnections.
mergeInternalInheritance :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> DefinedCategory c -> m (CategoryMap c)
mergeInternalInheritance tm d = do
  let rs2 = dcRefines d
  let ds2 = dcDefines d
  (_,t@(ValueConcrete c ns n ps rs ds vs fs)) <- getConcreteCategory tm (dcContext d,dcName d)
  let c2 = ValueConcrete c ns n ps (rs++rs2) (ds++ds2) vs fs
  let tm' = Map.insert (dcName d) c2 tm
  let r = CategoryResolver tm'
  fm <- getCategoryFilterMap t
  let pm = getCategoryParamMap t
  rs' <- mergeRefines r fm (rs++rs2)
  noDuplicateRefines [] n rs'
  ds' <- mergeDefines r fm (ds++ds2)
  noDuplicateDefines [] n ds'
  fs' <- mergeFunctions r tm' pm fm rs' ds' fs
  let c2' = ValueConcrete c ns n ps rs' ds' vs fs'
  let tm0 = (dcName d) `Map.delete` tm
  checkCategoryInstances tm0 [c2']
  return $ Map.insert (dcName d) c2' tm

replaceSelfMember :: (Show c, CollectErrorsM m) =>
  GeneralInstance -> DefinedMember c -> m (DefinedMember c)
replaceSelfMember self (DefinedMember c s t n i) = do
  t' <- replaceSelfValueType self t
  return $ DefinedMember c s t' n i
