{-# LANGUAGE Safe #-}

module Function (
  FunctionType(..),
  assignFunctionParams,
  checkFunctionConvert,
  validatateFunctionType,
) where

import Data.List (intercalate)
import Control.Monad (when)
import qualified Data.Map as Map

import TypeInstance
import TypesBase

data FunctionType =
  FunctionType {
    ftArgs :: ParamSet ValueType,
    ftReturns :: ParamSet ValueType,
    ftParams :: ParamSet ParamName,
    ftFilters :: ParamSet [TypeFilter]
  }
  deriving (Eq)

instance Show FunctionType where
  show (FunctionType as rs ps fa) =
    "<" ++ intercalate "," (map show $ psParams ps) ++ "> " ++
    concat (concat $ map showFilters $ zip (psParams ps) (psParams fa)) ++
    "(" ++ intercalate "," (map show $ psParams as) ++ ") -> " ++
    "(" ++ intercalate "," (map show $ psParams rs) ++ ")"
    where
      showFilters (n,fs) = map (\f -> show n ++ " " ++ show f ++ " ") fs

validatateFunctionType ::  (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> ParamVariances -> FunctionType -> m ()
validatateFunctionType r fm vm (FunctionType as rs ps fa) = do
  -- TODO: Also need to check for duplicates within the function.
  mergeAll $ map checkDuplicate $ psParams ps
  paired <- processParamPairs alwaysPairParams ps fa
  let allFilters = Map.union fm (Map.fromList paired)
  expanded <- fmap concat $ processParamPairs (\n fs -> return $ zip (repeat n) fs) ps fa
  mergeAll $ map checkFilterType expanded
  mergeAll $ map checkFilterVariance expanded
  mergeAll $ map (checkArg allFilters) $ psParams as
  mergeAll $ map (checkReturn allFilters) $ psParams rs
  where
    allVariances = Map.union vm (Map.fromList $ zip (psParams ps) (repeat Invariant))
    checkDuplicate n =
      when (n `Map.member` fm) $
        compileError $ "Param " ++ show n ++ " is already defined"
    checkFilterType (n,f) =
      validateTypeFilter r fm f `reviseError` ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(TypeFilter FilterRequires t)) =
      validateInstanceVariance r vm Contravariant (SingleType t) `reviseError`
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(TypeFilter FilterAllows t)) =
      validateInstanceVariance r vm Covariant (SingleType t) `reviseError`
        ("In filter " ++ show n ++ " " ++ show f)
    checkFilterVariance (n,f@(DefinesFilter t)) =
      validateDefinesVariance r vm Contravariant t `reviseError`
        ("In filter " ++ show n ++ " " ++ show f)
    checkArg fa ta@(ValueType _ t) = flip reviseError ("In arg " ++ show ta) $ do
      validateGeneralInstance r fa t
      validateInstanceVariance r allVariances Contravariant t
    checkReturn fa ta@(ValueType _ t) = flip reviseError ("In return " ++ show ta) $ do
      validateGeneralInstance r fa t
      validateInstanceVariance r allVariances Covariant t

assignFunctionParams :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> ParamSet GeneralInstance ->
  FunctionType -> m FunctionType
assignFunctionParams r fm ts (FunctionType as rs ps fa) = do
  assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams ps ts
  fa' <- fmap ParamSet $ collectAllOrErrorM $ map (assignFilters assigned) (psParams fa)
  processParamPairs (validateAssignment r fm) ts fa'
  as' <- fmap ParamSet $ collectAllOrErrorM $
         map (uncheckedSubValueType $getValueForParam assigned) (psParams as)
  rs' <- fmap ParamSet $ collectAllOrErrorM $
         map (uncheckedSubValueType $getValueForParam assigned) (psParams rs)
  return $ FunctionType as' rs' (ParamSet []) (ParamSet [])
  where
    assignFilters fm fs = collectAllOrErrorM $ map (uncheckedSubFilter $ getValueForParam fm) fs

checkFunctionConvert :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> FunctionType -> FunctionType -> m ()
checkFunctionConvert r fm ff1 ff2@(FunctionType as2 rs2 ps2 fa2) = do
  mapped <- fmap Map.fromList $ processParamPairs alwaysPairParams ps2 fa2
  let fm' = Map.union fm mapped
  let asTypes = ParamSet $ map (SingleType . JustParamName) $ psParams ps2
  -- Substitute params from ff2 into ff1.
  (FunctionType as1 rs1 _ _) <- assignFunctionParams r fm' asTypes ff1
  processParamPairs (validateArg fm') as1 as2
  processParamPairs (validateReturn fm') rs1 rs2
  return ()
  where
    validateArg fm a1 a2 = checkValueTypeMatch r fm a2 a1
    validateReturn fm r1 r2 = checkValueTypeMatch r fm r1 r2
