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

module Types.Function (
  FunctionType(..),
  assignFunctionParams,
  checkFunctionConvert,
  validatateFunctionType,
) where

import Data.List (group,intercalate,sort)
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.Positional
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

validatateFunctionType :: (CollectErrorsM m, TypeResolver r) =>
  r -> Set.Set ParamName -> ParamVariances -> FunctionType -> m ()
validatateFunctionType r params vm (FunctionType as rs ps fa) = do
  mapCompilerM_ checkCount $ group $ sort $ pValues ps
  mapCompilerM_ checkHides $ pValues ps
  let allParams = Set.union params (Set.fromList $ pValues ps)
  expanded <- fmap concat $ processPairs (\n fs -> return $ zip (repeat n) fs) ps fa
  mapCompilerM_ (checkFilterType allParams) expanded
  mapCompilerM_ checkFilterVariance expanded
  mapCompilerM_ (checkArg allParams) $ pValues as
  mapCompilerM_ (checkReturn allParams) $ pValues rs
  where
    allVariances = Map.union vm (Map.fromList $ zip (pValues ps) (repeat Invariant))
    checkCount xa@(x:_:_) =
      compilerErrorM $ "Function parameter " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkCount _ = return ()
    checkHides n =
      when (n `Set.member` params) $
        compilerErrorM $ "Function parameter " ++ show n ++ " hides a category-level parameter"
    checkFilterType fa2 (n,f) =
      validateTypeFilter r fa2 f <?? ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(TypeFilter FilterRequires t)) =
      validateInstanceVariance r allVariances Contravariant t <??
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(TypeFilter FilterAllows t)) =
      validateInstanceVariance r allVariances Covariant t <??
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(DefinesFilter t)) =
      validateDefinesVariance r allVariances Contravariant t <??
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (_,ImmutableFilter) = return ()
    checkArg fa2 ta@(ValueType _ t) = ("In argument " ++ show ta) ??> do
      when (isWeakValue ta) $ compilerErrorM "Weak values not allowed as argument types"
      validateGeneralInstance r fa2 t
      validateInstanceVariance r allVariances Contravariant t
    checkReturn fa2 ta@(ValueType _ t) = ("In return " ++ show ta) ??> do
      when (isWeakValue ta) $ compilerErrorM "Weak values not allowed as return types"
      validateGeneralInstance r fa2 t
      validateInstanceVariance r allVariances Covariant t

assignFunctionParams :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ParamValues -> Positional GeneralInstance ->
  FunctionType -> m FunctionType
assignFunctionParams r fm pm ts (FunctionType as rs ps fa) = do
  mapCompilerM_ (validateGeneralInstanceForCall r fm) $ pValues ts
  assigned <- fmap Map.fromList $ processPairs alwaysPair ps ts
  let pa = pm `Map.union` assigned
  fa' <- fmap Positional $ mapCompilerM (assignFilters pa) (pValues fa)
  processPairs_ (validateAssignment r fm) ts fa'
  as' <- fmap Positional $
         mapCompilerM (uncheckedSubValueType $ getValueForParam pa) (pValues as)
  rs' <- fmap Positional $
         mapCompilerM (uncheckedSubValueType $ getValueForParam pa) (pValues rs)
  return $ FunctionType as' rs' (Positional []) (Positional [])
  where
    assignFilters fm2 fs = mapCompilerM (uncheckedSubFilter $ getValueForParam fm2) fs

checkFunctionConvert :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ParamValues -> FunctionType -> FunctionType -> m ()
checkFunctionConvert r fm pm (FunctionType as1 rs1 ps1 fa1) ff2 = do
  mapped <- fmap Map.fromList $ processPairs alwaysPair ps1 fa1
  let fm' = Map.union fm mapped
  let asTypes = Positional $ map (singleType . JustParamName False) $ pValues ps1
  -- Substitute params from ff2 into ff1.
  (FunctionType as2 rs2 _ _) <- assignFunctionParams r fm' pm asTypes ff2
  fixed <- processPairs alwaysPair ps1 fa1
  let fm'' = Map.union fm (Map.fromList fixed)
  processPairs_ (validateArg fm'') as1 as2
  processPairs_ (validateReturn fm'') rs1 rs2
  where
    validateArg fm2 a1 a2 = checkValueAssignment r fm2 a1 a2
    validateReturn fm2 r1 r2 = checkValueAssignment r fm2 r2 r1
