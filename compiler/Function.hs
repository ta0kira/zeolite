{-# LANGUAGE Safe #-}

module Function (
  FunctionType(..),
  assignFunctionParams,
  checkFunctionConvert,
  validatateFunctionType,
) where

import Data.List (group,intercalate,sort)
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

validatateFunctionType :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m -> ParamFilters -> ParamVariances -> FunctionType -> m ()
validatateFunctionType r fm vm (FunctionType as rs ps fa) = do
  mergeAllM $ map checkCount $ group $ sort $ psParams ps
  mergeAllM $ map checkHides $ psParams ps
  paired <- processParamPairs alwaysPairParams ps fa
  let allFilters = Map.union fm (Map.fromList paired)
  expanded <- fmap concat $ processParamPairs (\n fs -> return $ zip (repeat n) fs) ps fa
  mergeAllM $ map (checkFilterType allFilters) expanded
  mergeAllM $ map checkFilterVariance expanded
  mergeAllM $ map (checkArg allFilters) $ psParams as
  mergeAllM $ map (checkReturn allFilters) $ psParams rs
  where
    allVariances = Map.union vm (Map.fromList $ zip (psParams ps) (repeat Invariant))
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

assignFunctionParams :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m -> ParamFilters -> ParamSet GeneralInstance ->
  FunctionType -> m FunctionType
assignFunctionParams r fm ts ff@(FunctionType as rs ps fa) = do
  assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams ps ts
  let allAssigned = Map.union assigned (Map.fromList $ map (\n -> (n,SingleType $ JustParamName n)) $ Map.keys fm)
  fa' <- fmap ParamSet $ collectAllOrErrorM $ map (assignFilters allAssigned) (psParams fa)
  processParamPairs (validateAssignment r fm) ts fa'
  as' <- fmap ParamSet $ collectAllOrErrorM $
         map (uncheckedSubValueType $ getValueForParam allAssigned) (psParams as)
  rs' <- fmap ParamSet $ collectAllOrErrorM $
         map (uncheckedSubValueType $ getValueForParam allAssigned) (psParams rs)
  return $ FunctionType as' rs' (ParamSet []) (ParamSet [])
  where
    assignFilters fm fs = collectAllOrErrorM $ map (uncheckedSubFilter $ getValueForParam fm) fs

checkFunctionConvert :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m -> ParamFilters -> FunctionType -> FunctionType -> m ()
checkFunctionConvert r fm ff1@(FunctionType as1 rs1 ps1 fa1) ff2 = do
  mapped <- fmap Map.fromList $ processParamPairs alwaysPairParams ps1 fa1
  let fm' = Map.union fm mapped
  let asTypes = ParamSet $ map (SingleType . JustParamName) $ psParams ps1
  -- Substitute params from ff2 into ff1.
  (FunctionType as2 rs2 _ _) <- assignFunctionParams r fm' asTypes ff2
  fixed <- processParamPairs alwaysPairParams ps1 fa1
  let fm' = Map.union fm (Map.fromList fixed)
  processParamPairs (validateArg fm') as1 as2
  processParamPairs (validateReturn fm') rs1 rs2
  return ()
  where
    validateArg fm a1 a2 = checkValueTypeMatch r fm a1 a2
    validateReturn fm r1 r2 = checkValueTypeMatch r fm r2 r1
