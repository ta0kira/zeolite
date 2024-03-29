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

module Types.DefinedCategory (
  DefinedCategory(..),
  DefinedMember(..),
  PragmaDefined(..),
  VariableRule(..),
  VariableValue(..),
  isInitialized,
  isFlatCleanup,
  isMembersHidden,
  isMembersReadOnly,
  isMembersReadOnlyExcept,
  mapMembers,
  mergeInternalInheritance,
  pairProceduresToFunctions,
  replaceSelfMember,
  setInternalFunctions,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.Positional
import Types.Function
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


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
  MembersReadOnlyExcept {
    mroeContext :: [c],
    mroeMembers :: [VariableName]
  } |
  MembersHidden {
    mhContext :: [c],
    mhMembers :: [VariableName]
  } |
  FlatCleanup {
    fcContext :: [c],
    fcMember :: VariableName
  }
  deriving (Show)

isMembersReadOnly :: PragmaDefined c -> Bool
isMembersReadOnly (MembersReadOnly _ _) = True
isMembersReadOnly _                     = False

isMembersReadOnlyExcept :: PragmaDefined c -> Bool
isMembersReadOnlyExcept (MembersReadOnlyExcept _ _) = True
isMembersReadOnlyExcept _                           = False

isMembersHidden :: PragmaDefined c -> Bool
isMembersHidden (MembersHidden _ _) = True
isMembersHidden _                   = False

isFlatCleanup :: PragmaDefined c -> Bool
isFlatCleanup (FlatCleanup _ _) = True
isFlatCleanup _                 = False

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
  update fm f@(ScopedFunction c n t2 s v as rs ps fs2 ms) fa = do
    validateCategoryFunction r t f
    fa' <- fa
    case n `Map.lookup` fa' of
         Nothing -> return $ Map.insert n f fa'
         (Just f0@(ScopedFunction c2 _ _ _ _ _ _ _ _ ms2)) -> do
           ("In function merge:\n---\n" ++ show f0 ++
             "\n  ->\n" ++ show f ++ "\n---\n") ??> do
              f0' <- parsedToFunctionType f0
              f' <- parsedToFunctionType f
              case s of
                   CategoryScope -> checkFunctionConvert r Map.empty Map.empty f0' f'
                   _             -> checkFunctionConvert r fm pm f0' f'
           return $ Map.insert n (ScopedFunction (c++c2) n t2 s v as rs ps fs2 ([f0]++ms++ms2)) fa'

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
      processPairs_ alwaysPair (fmap (pvType . fst) $ sfArgs f) (fmap inputValueName $ avNames $ epArgs p) <!!
        ("Procedure for " ++ show (sfName f) ++
         formatFullContextBrace (avContext $ epArgs p) ++
         " has the wrong number of arguments" ++
         formatFullContextBrace (sfContext f))
      if isUnnamedReturns (epReturns p)
         then return ()
         else do
           processPairs_ alwaysPair (fmap pvType $ sfReturns f) (fmap ovName $ nrNames $ epReturns p) <!!
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
mergeInternalInheritance cm@(CategoryMap km tm) d = "In definition of " ++ show (dcName d) ++ formatFullContextBrace (dcContext d) ??> do
  let rs2 = dcRefines d
  let ds2 = dcDefines d
  (_,t@(ValueConcrete c ns n pg fv ps rs ds vs fs)) <- getConcreteCategory cm (dcContext d,dcName d)
  let c2 = ValueConcrete c ns n pg fv ps (rs++rs2) (ds++ds2) vs fs
  let tm' = Map.insert (dcName d) c2 tm
  let r = CategoryResolver (CategoryMap km tm')
  fm <- getCategoryFilterMap t
  let pm = getCategoryParamMap t
  rs2' <- fmap concat $ mapCompilerM (flattenRefine r) rs2
  rs' <- mergeRefines r fm (rs++rs2')
  noDuplicateRefines (dcContext d) n rs'
  ds' <- mergeDefines r fm (ds++ds2)
  noDuplicateDefines (dcContext d) n ds'
  let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
  mapCompilerM_ (checkRefinesVariance r vm) rs2
  mapCompilerM_ (checkDefinesVariance r vm) ds2
  pg2 <- fmap concat $ mapCompilerM getRefinesPragmas rs2
  pg3 <- fmap concat $ mapCompilerM getDefinesPragmas ds2
  let fs2 = mergeInternalFunctions fs (dcFunctions d)
  fs' <- mergeFunctions r (CategoryMap km tm') pm fm rs' ds' fs2
  let c2' = ValueConcrete c ns n (pg++pg2++pg3) fv ps rs' ds' vs fs'
  let tm0 = (dcName d) `Map.delete` tm
  checkCategoryInstances (CategoryMap km tm0) [c2']
  return $ CategoryMap km $ Map.insert (dcName d) c2' tm
  where
    getRefinesPragmas rf = do
      (_,t) <- getCategory cm (vrContext rf,tiName $ vrType rf)
      return $ map (prependCategoryPragmaContext $ vrContext rf) $ getCategoryPragmas t
    getDefinesPragmas df = do
      (_,t) <- getCategory cm (vdContext df,diName $ vdType df)
      return $ map (prependCategoryPragmaContext $ vdContext df) $ getCategoryPragmas t
    mergeInternalFunctions fs1 = Map.elems . foldr single (funcMap fs1)
    funcMap = Map.fromList . map (\f -> (sfName f,f))
    single f fm =
      case sfName f `Map.lookup` fm of
           Nothing -> Map.insert (sfName f) f fm
           Just f2 -> Map.insert (sfName f) (ScopedFunction {
               sfContext = sfContext f,
               sfName = sfName f,
               sfType = sfType f,
               sfScope = sfScope f,
               sfVisibility = sfVisibility f,
               sfArgs = sfArgs f,
               sfReturns = sfReturns f,
               sfParams = sfParams f,
               sfFilters = sfFilters f,
               sfMerges = sfMerges f ++ [f2]
             }) fm
    checkRefinesVariance r vm (ValueRefine c t) =
      validateInstanceVariance r vm Covariant (singleType $ JustTypeInstance t) <??
        "In " ++ show t ++ formatFullContextBrace c
    checkDefinesVariance r vm (ValueDefine c t) =
      validateDefinesVariance r vm Covariant t <??
        "In " ++ show t ++ formatFullContextBrace c
    flattenRefine r ra@(ValueRefine c t) = do
      (_,t2) <- getValueCategory cm (c,tiName t)
      rs <- mapCompilerM (singleRefine r ra) (getCategoryRefines t2)
      return (ra:rs)
    singleRefine r (ValueRefine c t) (ValueRefine c2 t2) = do
      ps <- trRefines r t (tiName t2)
      return $ ValueRefine (c++c2) (TypeInstance (tiName t2) ps)

replaceSelfMember :: (Show c, CollectErrorsM m) =>
  GeneralInstance -> DefinedMember c -> m (DefinedMember c)
replaceSelfMember self (DefinedMember c s t n i) = do
  t' <- replaceSelfValueType self t
  return $ DefinedMember c s t' n i
