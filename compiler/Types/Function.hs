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

{-# LANGUAGE Safe #-}

module Types.Function (
  FunctionType(..),
  assignFunctionParams,
  checkFunctionConvert,
  validatateFunctionType,
) where

import Data.List (group,intercalate,sort)
import Control.Monad (when)
import qualified Data.Map as Map

import Base.CompileError
import Base.Mergeable
import Types.GeneralType
import Types.Positional
import Types.TypeInstance
import Types.Variance


data FunctionType =
  FunctionType {
    ftArgs :: Positional ValueType,
    ftReturns :: Positional ValueType,
    ftParams :: Positional ParamName,
    ftFilters :: Positional [TypeFilter]
  }
  deriving (Eq)

instance Show FunctionType where
  show (FunctionType as rs ps fa) =
    "<" ++ intercalate "," (map show $ pValues ps) ++ "> " ++
    concat (concat $ map showFilters $ zip (pValues ps) (pValues fa)) ++
    "(" ++ intercalate "," (map show $ pValues as) ++ ") -> " ++
    "(" ++ intercalate "," (map show $ pValues rs) ++ ")"
    where
      showFilters (n,fs) = map (\f -> show n ++ " " ++ show f ++ " ") fs

validatateFunctionType :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> ParamVariances -> FunctionType -> m ()
validatateFunctionType r fm vm (FunctionType as rs ps fa) = do
  mergeAllM $ map checkCount $ group $ sort $ pValues ps
  mergeAllM $ map checkHides $ pValues ps
  paired <- processPairs alwaysPair ps fa
  let allFilters = Map.union fm (Map.fromList paired)
  expanded <- fmap concat $ processPairs (\n fs -> return $ zip (repeat n) fs) ps fa
  mergeAllM $ map (checkFilterType allFilters) expanded
  mergeAllM $ map checkFilterVariance expanded
  mergeAllM $ map (checkArg allFilters) $ pValues as
  mergeAllM $ map (checkReturn allFilters) $ pValues rs
  where
    allVariances = Map.union vm (Map.fromList $ zip (pValues ps) (repeat Invariant))
    checkCount xa@(x:_:_) =
      compileError $ "Param " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkCount _ = return ()
    checkHides n =
      when (n `Map.member` fm) $
        compileError $ "Param " ++ show n ++ " hides another param in a higher scope"
    checkFilterType fa (n,f) =
      validateTypeFilter r fa f `reviseError` ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(TypeFilter FilterRequires t)) =
      validateInstanceVariance r allVariances Contravariant (SingleType t) `reviseError`
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(TypeFilter FilterAllows t)) =
      validateInstanceVariance r allVariances Covariant (SingleType t) `reviseError`
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(DefinesFilter t)) =
      validateDefinesVariance r allVariances Contravariant t `reviseError`
        ("In filter " ++ show n ++ " " ++ show f)
    checkArg fa ta@(ValueType _ t) = flip reviseError ("In argument " ++ show ta) $ do
      when (isWeakValue ta) $ compileError "Weak values not allowed as argument types"
      validateGeneralInstance r fa t
      validateInstanceVariance r allVariances Contravariant t
    checkReturn fa ta@(ValueType _ t) = flip reviseError ("In return " ++ show ta) $ do
      when (isWeakValue ta) $ compileError "Weak values not allowed as return types"
      validateGeneralInstance r fa t
      validateInstanceVariance r allVariances Covariant t

assignFunctionParams :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> Positional GeneralInstance ->
  FunctionType -> m FunctionType
assignFunctionParams r fm ts ff@(FunctionType as rs ps fa) = do
  mergeAllM $ map (validateGeneralInstance r fm) $ pValues ts
  assigned <- fmap Map.fromList $ processPairs alwaysPair ps ts
  let allAssigned = Map.union assigned (Map.fromList $ map (\n -> (n,SingleType $ JustParamName n)) $ Map.keys fm)
  fa' <- fmap Positional $ collectAllOrErrorM $ map (assignFilters allAssigned) (pValues fa)
  processPairs (validateAssignment r fm) ts fa'
  as' <- fmap Positional $ collectAllOrErrorM $
         map (uncheckedSubValueType $ getValueForParam allAssigned) (pValues as)
  rs' <- fmap Positional $ collectAllOrErrorM $
         map (uncheckedSubValueType $ getValueForParam allAssigned) (pValues rs)
  return $ FunctionType as' rs' (Positional []) (Positional [])
  where
    assignFilters fm fs = collectAllOrErrorM $ map (uncheckedSubFilter $ getValueForParam fm) fs

checkFunctionConvert :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> FunctionType -> FunctionType -> m ()
checkFunctionConvert r fm ff1@(FunctionType as1 rs1 ps1 fa1) ff2 = do
  mapped <- fmap Map.fromList $ processPairs alwaysPair ps1 fa1
  let fm' = Map.union fm mapped
  let asTypes = Positional $ map (SingleType . JustParamName) $ pValues ps1
  -- Substitute params from ff2 into ff1.
  (FunctionType as2 rs2 _ _) <- assignFunctionParams r fm' asTypes ff2
  fixed <- processPairs alwaysPair ps1 fa1
  let fm' = Map.union fm (Map.fromList fixed)
  processPairs (validateArg fm') as1 as2
  processPairs (validateReturn fm') rs1 rs2
  return ()
  where
    validateArg fm a1 a2 = checkValueTypeMatch r fm a1 a2
    validateReturn fm r1 r2 = checkValueTypeMatch r fm r2 r1
