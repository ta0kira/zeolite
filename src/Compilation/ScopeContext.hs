{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

module Compilation.ScopeContext (
  ProcedureScope(..),
  ScopeContext(..),
  applyProcedureScope,
  builtinVariables,
  getProcedureScopes,
) where

import Control.Monad (when)
import Prelude hiding (pi)
import qualified Data.Map as Map

import Base.CompileError
import Base.Mergeable
import Compilation.ProcedureContext
import Types.DefinedCategory
import Types.GeneralType
import Types.Positional
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


data ScopeContext c =
  ScopeContext {
    scCategories :: CategoryMap c,
    scName :: CategoryName,
    scExternalParams :: Positional (ValueParam c),
    scInternalparams :: Positional (ValueParam c),
    scMembers :: [DefinedMember c],
    scExternalFilters :: [ParamFilter c],
    scInternalFilters :: [ParamFilter c],
    scFunctions :: Map.Map FunctionName (ScopedFunction c),
    scVariables :: Map.Map VariableName (VariableValue c),
    scExprMap :: ExprMap c
  }

data ProcedureScope c =
  ProcedureScope {
    psContext :: ScopeContext c,
    psProcedures :: [(ScopedFunction c,ExecutableProcedure c)]
  }

applyProcedureScope ::
  (ScopeContext c -> ScopedFunction c -> ExecutableProcedure c -> a) -> ProcedureScope c -> [a]
applyProcedureScope f (ProcedureScope ctx fs) = map (uncurry (f ctx)) fs

getProcedureScopes :: (Show c, CompileErrorM m, MergeableM m) =>
  CategoryMap c -> ExprMap c -> DefinedCategory c -> m [ProcedureScope c]
getProcedureScopes ta em (DefinedCategory c n pi _ _ fi ms ps fs) = do
  (_,t) <- getConcreteCategory ta (c,n)
  let params = Positional $ getCategoryParams t
  let params2 = Positional pi
  let typeInstance = TypeInstance n $ fmap (singleType . JustParamName False . vpParam) params
  let filters = getCategoryFilters t
  let filters2 = fi
  let r = CategoryResolver ta
  fa <- setInternalFunctions r t fs
  checkInternalParams pi fi (getCategoryParams t) (Map.elems fa) r (getCategoryFilterMap t)
  pa <- pairProceduresToFunctions fa ps
  let (cp,tp,vp) = partitionByScope (sfScope . fst) pa
  let (cm,tm,vm) = partitionByScope dmScope ms
  let cm0 = builtins typeInstance CategoryScope
  let tm0 = builtins typeInstance TypeScope
  let vm0 = builtins typeInstance ValueScope
  cm' <- mapMembers cm
  tm' <- mapMembers $ cm ++ tm
  vm' <- mapMembers $ cm ++ tm ++ vm
  let cv = Map.union cm0 cm'
  let tv = Map.union tm0 tm'
  let vv = Map.union vm0 vm'
  let ctxC = ScopeContext ta n params params2 vm filters filters2 fa cv em
  let ctxT = ScopeContext ta n params params2 vm filters filters2 fa tv em
  let ctxV = ScopeContext ta n params params2 vm filters filters2 fa vv em
  return [ProcedureScope ctxC cp,ProcedureScope ctxT tp,ProcedureScope ctxV vp]
  where
    builtins t s0 = Map.filter ((<= s0) . vvScope) $ builtinVariables t
    checkInternalParams pi2 fi2 pe fs2 r fa = do
      let pm = Map.fromList $ map (\p -> (vpParam p,vpContext p)) pi2
      mergeAllM $ map (checkFunction pm) fs2
      mergeAllM $ map (checkParam pm) pe
      let fa' = Map.union fa $ getFilterMap pi2 fi2
      mergeAllM $ map (checkFilter r fa') fi2
    checkFilter r fa (ParamFilter c2 n2 f) =
      validateTypeFilter r fa f <?? ("In " ++ show n2 ++ " " ++ show f ++ formatFullContextBrace c2)
    checkFunction pm f =
      when (sfScope f == ValueScope) $
        mergeAllM $ map (checkParam pm) $ pValues $ sfParams f
    checkParam pm p =
      case vpParam p `Map.lookup` pm of
           Nothing -> return ()
           (Just c2) -> compileErrorM $ "Internal param " ++ show (vpParam p) ++
                                        formatFullContextBrace (vpContext p) ++
                                        " is already defined at " ++
                                        formatFullContext c2

-- TODO: This is probably in the wrong module.
builtinVariables :: TypeInstance -> Map.Map VariableName (VariableValue c)
builtinVariables t = Map.fromList [
    (VariableName "self",VariableValue [] ValueScope (ValueType RequiredValue $ singleType $ JustTypeInstance t) False)
  ]
