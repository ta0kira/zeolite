{- -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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

import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.Positional
import Compilation.ProcedureContext
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


data ScopeContext c =
  ScopeContext {
    scCategories :: CategoryMap c,
    scName :: CategoryName,
    scParams :: Positional (ValueParam c),
    scValueMembers :: [DefinedMember c],
    scFilters :: [ParamFilter c],
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

getProcedureScopes :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> ExprMap c -> DefinedCategory c -> m [ProcedureScope c]
getProcedureScopes ta em (DefinedCategory c n pragmas _ _ ms ps fs) = do
  (_,t) <- getConcreteCategory ta (c,n)
  let params = Positional $ getCategoryParams t
  let typeInstance = TypeInstance n $ fmap (singleType . JustParamName False . vpParam) params
  let filters = getCategoryFilters t
  let r = CategoryResolver ta
  fa <- setInternalFunctions r t fs
  pa <- pairProceduresToFunctions fa ps
  let (cp,tp,vp) = partitionByScope (sfScope . fst) pa
  tp' <- mapCompilerM (firstM $ replaceSelfFunction (instanceFromCategory t)) tp
  vp' <- mapCompilerM (firstM $ replaceSelfFunction (instanceFromCategory t)) vp
  let (cm,tm,vm) = partitionByScope dmScope ms
  tm' <- mapCompilerM (replaceSelfMember (instanceFromCategory t)) tm
  vm' <- mapCompilerM (replaceSelfMember (instanceFromCategory t)) vm
  let cm0 = builtins typeInstance CategoryScope
  let tm0 = builtins typeInstance TypeScope
  let vm0 = builtins typeInstance ValueScope
  cm2 <- mapMembers readOnly hidden cm
  tm2 <- mapMembers readOnly hidden $ cm ++ tm'
  vm2 <- mapMembers readOnly hidden $ cm ++ tm' ++ vm'
  mapCompilerM_ checkPragma pragmas
  let cv = Map.union cm0 cm2
  let tv = Map.union tm0 tm2
  let vv = Map.union vm0 vm2
  let ctxC = ScopeContext ta n params vm' filters fa cv em
  let ctxT = ScopeContext ta n params vm' filters fa tv em
  let ctxV = ScopeContext ta n params vm' filters fa vv em
  return [ProcedureScope ctxC cp,ProcedureScope ctxT tp',ProcedureScope ctxV vp']
  where
    checkPragma (MembersReadOnly c2 vs) = do
      let missing = Set.toList $ Set.fromList vs `Set.difference` allMembers
      mapCompilerM_ (\v -> compilerErrorM $ "Member " ++ show v ++
                                            " does not exist (marked ReadOnly at " ++
                                            formatFullContext c2 ++ ")") missing
    checkPragma (MembersHidden c2 vs) = do
      let missing = Set.toList $ Set.fromList vs `Set.difference` allMembers
      mapCompilerM_ (\v -> compilerErrorM $ "Member " ++ show v ++
                                            " does not exist (marked Hidden at " ++
                                            formatFullContext c2 ++ ")") missing
    allMembers = Set.fromList $ map dmName ms
    readOnly = Map.fromListWith (++) $ concat $ map (\m -> zip (mroMembers m) (repeat $ mroContext m)) $ filter isMembersReadOnly pragmas
    hidden   = Map.fromListWith (++) $ concat $ map (\m -> zip (mhMembers m)  (repeat $ mhContext m))  $ filter isMembersHidden   pragmas
    firstM f (x,y) = do
      x' <- f x
      return (x',y)
    builtins t s0 = Map.filter ((<= s0) . vvScope) $ builtinVariables t

-- TODO: This is probably in the wrong module.
builtinVariables :: TypeInstance -> Map.Map VariableName (VariableValue c)
builtinVariables t = Map.fromList [
    (VariableName "self",VariableValue [] ValueScope (ValueType RequiredValue $ singleType $ JustTypeInstance t) (VariableReadOnly []))
  ]
