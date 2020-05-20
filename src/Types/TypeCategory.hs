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

module Types.TypeCategory (
  AnyCategory(..),
  CategoryMap,
  CategoryResolver(..),
  FunctionName(..),
  Namespace(..),
  ParamFilter(..),
  PassedValue(..),
  ScopedFunction(..),
  SymbolScope(..),
  ValueDefine(..),
  ValueParam(..),
  ValueRefine(..),
  checkCategoryInstances,
  checkConnectedTypes,
  checkConnectionCycles,
  checkParamVariances,
  declareAllTypes, -- TODO: Remove?
  flattenAllConnections,
  formatFullContext,
  formatFullContextBrace,
  getCategory,
  getCategoryContext,
  getCategoryDefines,
  getCategoryDeps,
  getCategoryFilterMap,
  getCategoryFilters,
  getCategoryFunctions,
  getCategoryName,
  getCategoryNamespace,
  getCategoryParams,
  getCategoryRefines,
  getConcreteCategory,
  getFilterMap,
  getFunctionFilterMap,
  getInstanceCategory,
  getValueCategory,
  includeNewTypes,
  inferParamTypes,
  isInstanceInterface,
  isDynamicNamespace,
  isNoNamespace,
  isStaticNamespace,
  isValueConcrete,
  isValueInterface,
  mergeDefines,
  mergeFunctions,
  mergeInferredTypes,
  mergeRefines,
  noDuplicateDefines,
  noDuplicateRefines,
  parsedToFunctionType,
  partitionByScope,
  setCategoryNamespace,
  topoSortCategories,
  uncheckedSubFunction,
  validateCategoryFunction,
) where

import Control.Arrow (second)
import Control.Monad (when)
import Data.List (group,groupBy,intercalate,sort,sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.MergeTree
import Base.Mergeable
import Types.Function
import Types.GeneralType
import Types.Positional
import Types.TypeInstance
import Types.Variance


data AnyCategory c =
  ValueInterface {
    viContext :: [c],
    viNamespace :: Namespace,
    viName :: CategoryName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c],
    viParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  } |
  InstanceInterface {
    iiContext :: [c],
    iiNamespace :: Namespace,
    iiName :: CategoryName,
    iiParams :: [ValueParam c],
    iiParamFilter :: [ParamFilter c],
    iiFunctions :: [ScopedFunction c]
  } |
  ValueConcrete {
    vcContext :: [c],
    vcNamespace :: Namespace,
    vcName :: CategoryName,
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcDefines :: [ValueDefine c],
    vcParamFilter :: [ParamFilter c],
    vcFunctions :: [ScopedFunction c]
  }

formatFullContext :: Show a => [a] -> String
formatFullContext cs = intercalate " -> " (map show cs)

formatFullContextBrace :: Show a => [a] -> String
formatFullContextBrace [] = ""
formatFullContextBrace cs = " [" ++ intercalate " -> " (map show cs) ++ "]"

instance Show c => Show (AnyCategory c) where
  show = format where
    format (ValueInterface cs ns n ps rs vs fs) =
      "@value interface " ++ show n ++ formatParams ps ++ namespace ns ++ " { " ++ formatContext cs ++ "\n" ++
      (intercalate "\n\n" $
         map (\r -> "  " ++ formatRefine r) rs ++
         map (\v -> "  " ++ formatValue v) vs ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    format (InstanceInterface cs ns n ps vs fs) =
      "@type interface " ++ show n ++ formatParams ps ++ namespace ns ++ " { " ++ formatContext cs ++
      (intercalate "\n\n" $
         map (\v -> "  " ++ formatValue v) vs ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    format (ValueConcrete cs ns n ps rs ds vs fs) =
      "concrete " ++ show n ++ formatParams ps ++ namespace ns ++ " { " ++ formatContext cs ++ "\n" ++
      (intercalate "\n\n" $
         map (\r -> "  " ++ formatRefine r) rs ++
         map (\d -> "  " ++ formatDefine d) ds ++
         map (\v -> "  " ++ formatValue v) vs ++
         map (\f -> formatConcreteFunc f) fs) ++
      "\n}\n"
    namespace ns
      | isStaticNamespace ns = " /*" ++ show ns ++ "*/"
      | otherwise = ""
    formatContext cs = "/*" ++ formatFullContext cs ++ "*/"
    formatParams ps = let (con,inv,cov) = (foldr partitionParam ([],[],[]) ps) in
      "<" ++ intercalate "," con ++ "|" ++
             intercalate "," inv ++ "|" ++
             intercalate "," cov ++ ">"
    partitionParam (ValueParam _ p Contravariant) (con,inv,cov) = ((show p):con,inv,cov)
    partitionParam (ValueParam _ p Invariant)     (con,inv,cov) = (con,(show p):inv,cov)
    partitionParam (ValueParam _ p Covariant)     (con,inv,cov) = (con,inv,(show p):cov)
    formatRefine r = "refines " ++ show (vrType r) ++ " " ++ formatContext (vrContext r)
    formatDefine d = "defines " ++ show (vdType d) ++ " " ++ formatContext (vdContext d)
    formatValue v = show (pfParam v) ++ " " ++ show (pfFilter v) ++
                    " " ++ formatContext (pfContext v)
    formatInterfaceFunc f = showFunctionInContext "" "  " f
    formatConcreteFunc f = showFunctionInContext (show (sfScope f) ++ " ") "  " f

getCategoryName :: AnyCategory c -> CategoryName
getCategoryName (ValueInterface _ _ n _ _ _ _)  = n
getCategoryName (InstanceInterface _ _ n _ _ _) = n
getCategoryName (ValueConcrete _ _ n _ _ _ _ _) = n

getCategoryContext :: AnyCategory c -> [c]
getCategoryContext (ValueInterface c _ _ _ _ _ _)  = c
getCategoryContext (InstanceInterface c _ _ _ _ _) = c
getCategoryContext (ValueConcrete c _ _ _ _ _ _ _) = c

getCategoryNamespace :: AnyCategory c -> Namespace
getCategoryNamespace (ValueInterface _ ns _ _ _ _ _)  = ns
getCategoryNamespace (InstanceInterface _ ns _ _ _ _) = ns
getCategoryNamespace (ValueConcrete _ ns _ _ _ _ _ _) = ns

setCategoryNamespace :: Namespace -> AnyCategory c -> AnyCategory c
setCategoryNamespace ns (ValueInterface c _ n ps rs vs fs)   = (ValueInterface c ns n ps rs vs fs)
setCategoryNamespace ns (InstanceInterface c _ n ps vs fs)   = (InstanceInterface c ns n ps vs fs)
setCategoryNamespace ns (ValueConcrete c _ n ps rs ds vs fs) = (ValueConcrete c ns n ps rs ds vs fs)

getCategoryParams :: AnyCategory c -> [ValueParam c]
getCategoryParams (ValueInterface _ _ _ ps _ _ _)  = ps
getCategoryParams (InstanceInterface _ _ _ ps _ _) = ps
getCategoryParams (ValueConcrete _ _ _ ps _ _ _ _) = ps

getCategoryRefines :: AnyCategory c -> [ValueRefine c]
getCategoryRefines (ValueInterface _ _ _ _ rs _ _)  = rs
getCategoryRefines (InstanceInterface _ _ _ _ _ _)  = []
getCategoryRefines (ValueConcrete _ _ _ _ rs _ _ _) = rs

getCategoryDefines :: AnyCategory c -> [ValueDefine c]
getCategoryDefines (ValueInterface _ _ _ _ _ _ _)  = []
getCategoryDefines (InstanceInterface _ _ _ _ _ _)  = []
getCategoryDefines (ValueConcrete _ _ _ _ _ ds _ _) = ds

getCategoryFilters :: AnyCategory c -> [ParamFilter c]
getCategoryFilters (ValueInterface _ _ _ _ _ vs _)  = vs
getCategoryFilters (InstanceInterface _ _ _ _ vs _) = vs
getCategoryFilters (ValueConcrete _ _ _ _ _ _ vs _) = vs

getCategoryFunctions :: AnyCategory c -> [ScopedFunction c]
getCategoryFunctions (ValueInterface _ _ _ _ _ _ fs)  = fs
getCategoryFunctions (InstanceInterface _ _ _ _ _ fs) = fs
getCategoryFunctions (ValueConcrete _ _ _ _ _ _ _ fs) = fs

getCategoryDeps :: AnyCategory c -> Set.Set CategoryName
getCategoryDeps t = Set.fromList $ filter (/= getCategoryName t) $ refines ++ defines ++ filters ++ functions where
  refines = concat $ map (fromInstance . SingleType . JustTypeInstance . vrType) $ getCategoryRefines t
  defines = concat $ map (fromDefine . vdType) $ getCategoryDefines t
  filters = concat $ map (fromFilter . pfFilter) $ getCategoryFilters t
  functions = concat $ map fromFunction $ getCategoryFunctions t
  fromInstance (TypeMerge _ ps)                                    = concat $ map fromInstance ps
  fromInstance (SingleType (JustTypeInstance (TypeInstance n ps))) = n:(concat $ map fromInstance $ pValues ps)
  fromInstance _                                                   = []
  fromDefine (DefinesInstance n ps) = n:(concat $ map fromInstance $ pValues ps)
  fromFilter (TypeFilter _ t2@(JustTypeInstance _)) = fromInstance (SingleType t2)
  fromFilter (DefinesFilter t2)                     = fromDefine t2
  fromFilter _                                      = []
  fromType (ValueType _ t2) = fromInstance t2
  fromFunction f = args ++ returns ++ filters2 where
    args = concat $ map (fromType . pvType) $ pValues $ sfArgs f
    returns = concat $ map (fromType . pvType) $ pValues $ sfReturns f
    filters2 = concat $ map (fromFilter . pfFilter) $ sfFilters f

isValueInterface :: AnyCategory c -> Bool
isValueInterface (ValueInterface _ _ _ _ _ _ _) = True
isValueInterface _ = False

isInstanceInterface :: AnyCategory c -> Bool
isInstanceInterface (InstanceInterface _ _ _ _ _ _) = True
isInstanceInterface _ = False

isValueConcrete :: AnyCategory c -> Bool
isValueConcrete (ValueConcrete _ _ _ _ _ _ _ _) = True
isValueConcrete _ = False

data Namespace =
  StaticNamespace {
    snName :: String
  } |
  NoNamespace |
  DynamicNamespace
  deriving (Eq,Ord)

instance Show Namespace where
  show (StaticNamespace n) = n
  show _                   = ""

isStaticNamespace :: Namespace -> Bool
isStaticNamespace (StaticNamespace _) = True
isStaticNamespace _                   = False

isNoNamespace :: Namespace -> Bool
isNoNamespace NoNamespace = True
isNoNamespace _           = False

isDynamicNamespace :: Namespace -> Bool
isDynamicNamespace DynamicNamespace = True
isDynamicNamespace _                = False

data ValueRefine c =
  ValueRefine {
    vrContext :: [c],
    vrType :: TypeInstance
  }

instance Show c => Show (ValueRefine c) where
  show (ValueRefine c t) = show t ++ formatFullContextBrace c

data ValueDefine c =
  ValueDefine {
    vdContext :: [c],
    vdType :: DefinesInstance
  }

instance Show c => Show (ValueDefine c) where
  show (ValueDefine c t) = show t ++ formatFullContextBrace c

data ValueParam c =
  ValueParam {
    vpContext :: [c],
    vpParam :: ParamName,
    vpVariance :: Variance
  }

instance Show c => Show (ValueParam c) where
  show (ValueParam c t v) = show t ++ " (" ++ show v ++ ")" ++ formatFullContextBrace c

data ParamFilter c =
  ParamFilter {
    pfContext :: [c],
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }

instance Show c => Show (ParamFilter c) where
  show (ParamFilter c n f) = show n ++ " " ++ show f ++ formatFullContextBrace c

newtype CategoryResolver c =
  CategoryResolver {
    crCategories :: CategoryMap c
  }

instance (Show c) => TypeResolver (CategoryResolver c) where
    trRefines (CategoryResolver tm) (TypeInstance n1 ps1) n2
      | n1 == n2 = do
        (_,t) <- getValueCategory tm ([],n1)
        processPairs_ alwaysPair (Positional $ map vpParam $ getCategoryParams t) ps1
        return ps1
      | otherwise = do
        (_,t) <- getValueCategory tm ([],n1)
        let params = map vpParam $ getCategoryParams t
        assigned <- fmap Map.fromList $ processPairs alwaysPair (Positional params) ps1
        let pa = Map.fromList $ map (\r -> (tiName r,tiParams r)) $ map vrType $ getCategoryRefines t
        ps2 <- case n2 `Map.lookup` pa of
                    (Just x) -> return x
                    _ -> compileErrorM $ "Category " ++ show n1 ++ " does not refine " ++ show n2
        fmap Positional $ mapErrorsM (subAllParams assigned) $ pValues ps2
    trDefines (CategoryResolver tm) (TypeInstance n1 ps1) n2 = do
      (_,t) <- getValueCategory tm ([],n1)
      let params = map vpParam $ getCategoryParams t
      assigned <- fmap Map.fromList $ processPairs alwaysPair (Positional params) ps1
      let pa = Map.fromList $ map (\r -> (diName r,diParams r)) $ map vdType $ getCategoryDefines t
      ps2 <- case n2 `Map.lookup` pa of
                  (Just x) -> return x
                  _ -> compileErrorM $ "Category " ++ show n1 ++ " does not define " ++ show n2
      fmap Positional $ mapErrorsM (subAllParams assigned) $ pValues ps2
    trVariance (CategoryResolver tm) n = do
      (_,t) <- getCategory tm ([],n)
      return $ Positional $ map vpVariance $ getCategoryParams t
    trTypeFilters (CategoryResolver tm) (TypeInstance n ps) = do
      (_,t) <- getValueCategory tm ([],n)
      checkFilters t ps
    trDefinesFilters (CategoryResolver tm) (DefinesInstance n ps) = do
      (_,t) <- getInstanceCategory tm ([],n)
      checkFilters t ps
    trConcrete (CategoryResolver tm) n = do
      (_,t) <- getCategory tm ([],n)
      return (isValueConcrete t)

data SymbolScope =
  LocalScope |
  CategoryScope |
  TypeScope |
  ValueScope
  deriving (Eq,Ord)

instance Show SymbolScope where
  show CategoryScope = "@category"
  show TypeScope     = "@type"
  show ValueScope    = "@value"
  show LocalScope    = "@local"

partitionByScope :: (a -> SymbolScope) -> [a] -> ([a],[a],[a])
partitionByScope f = foldr bin empty where
  empty = ([],[],[])
  bin x (cs,ts,vs)
    | f x == CategoryScope = (x:cs,ts,vs)
    | f x == TypeScope     = (cs,x:ts,vs)
    | f x == ValueScope    = (cs,ts,x:vs)
    | otherwise = (cs,ts,vs)

checkFilters :: (CompileErrorM m, MergeableM m) =>
  AnyCategory c -> Positional GeneralInstance -> m (Positional [TypeFilter])
checkFilters t ps = do
  let params = map vpParam $ getCategoryParams t
  assigned <- fmap Map.fromList $ processPairs alwaysPair (Positional params) ps
  fs <- mapErrorsM (subSingleFilter assigned . \f -> (pfParam f,pfFilter f))
                                  (getCategoryFilters t)
  let fa = Map.fromListWith (++) $ map (second (:[])) fs
  fmap Positional $ mapErrorsM (assignFilter fa) params where
    subSingleFilter pa (n,(TypeFilter v t2)) = do
      (SingleType t3) <- uncheckedSubInstance (getValueForParam pa) (SingleType t2)
      return (n,(TypeFilter v t3))
    subSingleFilter pa (n,(DefinesFilter (DefinesInstance n2 ps2))) = do
      ps3 <- mapErrorsM (uncheckedSubInstance $ getValueForParam pa) (pValues ps2)
      return (n,(DefinesFilter (DefinesInstance n2 (Positional ps3))))
    assignFilter fa n =
      case n `Map.lookup` fa of
            (Just x) -> return x
            _ -> return []

subAllParams :: (MergeableM m, CompileErrorM m) =>
  Map.Map ParamName GeneralInstance -> GeneralInstance -> m GeneralInstance
subAllParams pa = uncheckedSubInstance (getValueForParam pa)

type CategoryMap c = Map.Map CategoryName (AnyCategory c)

getCategory :: (Show c, CompileErrorM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getCategory tm (c,n) =
  case n `Map.lookup` tm of
       (Just t) -> return (c,t)
       _ -> compileErrorM $ "Type " ++ show n ++ context ++ " not found"
  where
    context
      | null c = ""
      | otherwise = formatFullContextBrace c

getValueCategory :: (Show c, CompileErrorM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getValueCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueInterface t || isValueConcrete t
     then return (c2,t)
     else compileErrorM $ "Category " ++ show n ++
                         " cannot be used as a value" ++
                         formatFullContextBrace c

getInstanceCategory :: (Show c, CompileErrorM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getInstanceCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isInstanceInterface t
     then return (c2,t)
     else compileErrorM $ "Category " ++ show n ++
                         " cannot be used as a type interface" ++
                         formatFullContextBrace c

getConcreteCategory :: (Show c, CompileErrorM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getConcreteCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueConcrete t
     then return (c2,t)
     else compileErrorM $ "Category " ++ show n ++
                         " cannot be used as concrete" ++
                         formatFullContextBrace c

includeNewTypes :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
includeNewTypes tm0 ts = do
  checkConnectionCycles tm0 ts
  checkConnectedTypes tm0 ts
  checkParamVariances tm0 ts
  ts2 <- topoSortCategories tm0 ts
  ts3 <- flattenAllConnections tm0 ts2
  checkCategoryInstances tm0 ts3
  declareAllTypes tm0 ts3

declareAllTypes :: (Show c, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
declareAllTypes tm0 = foldr (\t tm -> tm >>= update t) (return tm0) where
  update t tm =
    case getCategoryName t `Map.lookup` tm of
        (Just t2) -> compileErrorM $ "Type " ++ show (getCategoryName t) ++
                                     formatFullContextBrace (getCategoryContext t) ++
                                     " has already been declared" ++
                                     formatFullContextBrace (getCategoryContext t2)
        _ -> return $ Map.insert (getCategoryName t) t tm

getFilterMap :: [ValueParam c] -> [ParamFilter c] -> ParamFilters
getFilterMap ps fs = getFilters $ zip (Set.toList pa) (repeat []) where
  pa = Set.fromList $ map vpParam ps
  getFilters pa0 = let fs' = map (\f -> (pfParam f,pfFilter f)) fs in
                       Map.fromListWith (++) $ map (second (:[])) fs' ++ pa0

getCategoryFilterMap :: AnyCategory c -> ParamFilters
getCategoryFilterMap t = getFilterMap (getCategoryParams t) (getCategoryFilters t)

-- TODO: Use this where it's needed in this file.
getFunctionFilterMap :: ScopedFunction c -> ParamFilters
getFunctionFilterMap f = getFilterMap (pValues $ sfParams f) (sfFilters f)

checkConnectedTypes :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectedTypes tm0 ts = do
  tm <- declareAllTypes tm0 ts
  mergeAllM (map (checkSingle tm) ts)
  where
    checkSingle tm (ValueInterface c _ n _ rs _ _) = do
      let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
      is <- mapErrorsM (getCategory tm) ts2
      mergeAllM (map (valueRefinesInstanceError c n) is)
      mergeAllM (map (valueRefinesConcreteError c n) is)
    checkSingle tm (ValueConcrete c _ n _ rs ds _ _) = do
      let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
      let ts3 = map (\d -> (vdContext d,diName $ vdType d)) ds
      is1 <- mapErrorsM (getCategory tm) ts2
      is2 <- mapErrorsM (getCategory tm) ts3
      mergeAllM (map (concreteRefinesInstanceError c n) is1)
      mergeAllM (map (concreteDefinesValueError c n) is2)
      mergeAllM (map (concreteRefinesConcreteError c n) is1)
      mergeAllM (map (concreteDefinesConcreteError c n) is2)
    checkSingle _ _ = return ()
    valueRefinesInstanceError c n (c2,t)
      | isInstanceInterface t =
        compileErrorM $ "Value interface " ++ show n ++ formatFullContextBrace c ++
                        " cannot refine type interface " ++
                        show (iiName t) ++ formatFullContextBrace c2
      | otherwise = return ()
    valueRefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compileErrorM $ "Value interface " ++ show n ++ formatFullContextBrace c ++
                        " cannot refine concrete type " ++
                        show (getCategoryName t) ++ formatFullContextBrace c2
      | otherwise = return ()
    concreteRefinesInstanceError c n (c2,t)
      | isInstanceInterface t =
        compileErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                        " cannot refine instance interface " ++
                        show (getCategoryName t) ++ formatFullContextBrace c2 ++
                        " => use defines instead"
      | otherwise = return ()
    concreteDefinesValueError c n (c2,t)
      | isValueInterface t =
        compileErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                      " cannot define value interface " ++
                      show (getCategoryName t) ++ formatFullContextBrace c2 ++
                      " => use refines instead"
      | otherwise = return ()
    concreteRefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compileErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                      " cannot refine concrete type " ++
                      show (getCategoryName t) ++ formatFullContextBrace c2
      | otherwise = return ()
    concreteDefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compileErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                      " cannot define concrete type " ++
                      show (getCategoryName t) ++ formatFullContextBrace c2
      | otherwise = return ()

checkConnectionCycles :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectionCycles tm0 ts = mergeAllM (map (checker []) ts) where
  tm = Map.union tm0 $ Map.fromList $ zip (map getCategoryName ts) ts
  checker us (ValueInterface c _ n _ rs _ _) = do
    failIfCycle n c us
    let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- mapErrorsM (getValueCategory tm) ts2
    mergeAllM (map (checker (us ++ [n]) . snd) is)
  checker us (ValueConcrete c _ n _ rs _ _ _) = do
    failIfCycle n c us
    let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- mapErrorsM (getValueCategory tm) ts2
    mergeAllM (map (checker (us ++ [n]) . snd) is)
  checker _ _ = return ()
  failIfCycle n c us =
    when (n `Set.member` (Set.fromList us)) $
      compileErrorM $ "Category " ++ show n ++ formatFullContextBrace c ++
                     " refers back to itself: " ++
                     intercalate " -> " (map show (us ++ [n]))

checkParamVariances :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkParamVariances tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = CategoryResolver tm
  mergeAllM (map (checkCategory r) ts)
  where
    checkCategory r (ValueInterface c _ n ps rs fa _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAllM (map (checkRefine r vm) rs)
      mergeAllM $ map (checkFilterVariance r vm) fa
    checkCategory r (ValueConcrete c _ n ps rs ds fa _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAllM (map (checkRefine r vm) rs)
      mergeAllM (map (checkDefine r vm) ds)
      mergeAllM $ map (checkFilterVariance r vm) fa
    checkCategory r (InstanceInterface c _ n ps fa _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAllM $ map (checkFilterVariance r vm) fa
    noDuplicates c n ps = mergeAllM (map checkCount $ group $ sort $ map vpParam ps) where
      checkCount xa@(x:_:_) =
        compileErrorM $ "Param " ++ show x ++ " occurs " ++ show (length xa) ++
                      " times in " ++ show n ++ formatFullContextBrace c
      checkCount _ = return ()
    checkRefine r vm (ValueRefine c t) =
      validateInstanceVariance r vm Covariant (SingleType $ JustTypeInstance t) `reviseErrorM`
        (show t ++ formatFullContextBrace c)
    checkDefine r vm (ValueDefine c t) =
      validateDefinesVariance r vm Covariant t `reviseErrorM`
        (show t ++ formatFullContextBrace c)
    checkFilterVariance r vs (ParamFilter c n f@(TypeFilter FilterRequires t)) =
      flip reviseErrorM ("In filter " ++ show n ++ " " ++ show f ++ formatFullContextBrace c) $ do
        case n `Map.lookup` vs of
             Just Contravariant -> compileErrorM $ "Contravariant param " ++ show n ++
                                                  " cannot have a requires filter"
             Nothing -> compileErrorM $ "Param " ++ show n ++ " is undefined"
             _ -> return ()
        validateInstanceVariance r vs Contravariant (SingleType t)
    checkFilterVariance r vs (ParamFilter c n f@(TypeFilter FilterAllows t)) =
      flip reviseErrorM ("In filter " ++ show n ++ " " ++ show f ++ formatFullContextBrace c) $ do
        case n `Map.lookup` vs of
             Just Covariant -> compileErrorM $ "Covariant param " ++ show n ++
                                              " cannot have an allows filter"
             Nothing -> compileErrorM $ "Param " ++ show n ++ " is undefined"
             _ -> return ()
        validateInstanceVariance r vs Covariant (SingleType t)
    checkFilterVariance r vs (ParamFilter c n f@(DefinesFilter t)) =
      flip reviseErrorM ("In filter " ++ show n ++ " " ++ show f ++ formatFullContextBrace c) $ do
        case n `Map.lookup` vs of
             Just Contravariant -> compileErrorM $ "Contravariant param " ++ show n ++
                                                  " cannot have a defines filter"
             Nothing -> compileErrorM $ "Param " ++ show n ++ " is undefined"
             _ -> return ()
        validateDefinesVariance r vs Contravariant t

checkCategoryInstances :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkCategoryInstances tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = CategoryResolver tm
  mergeAllM $ map (checkSingle r) ts
  where
    checkSingle r t = do
      let pa = Set.fromList $ map vpParam $ getCategoryParams t
      let fm = getCategoryFilterMap t
      mergeAllM $ map (checkFilterParam pa) (getCategoryFilters t)
      mergeAllM $ map (checkRefine r fm) (getCategoryRefines t)
      mergeAllM $ map (checkDefine r fm) (getCategoryDefines t)
      mergeAllM $ map (checkFilter r fm) (getCategoryFilters t)
      mergeAllM $ map (validateCategoryFunction r t) (getCategoryFunctions t)
    checkFilterParam pa (ParamFilter c n _) =
      when (not $ n `Set.member` pa) $
        compileErrorM $ "Param " ++ show n ++ formatFullContextBrace c ++ " does not exist"
    checkRefine r fm (ValueRefine c t) =
      validateTypeInstance r fm t `reviseErrorM`
        (show t ++ formatFullContextBrace c)
    checkDefine r fm (ValueDefine c t) =
      validateDefinesInstance r fm t `reviseErrorM`
        (show t ++ formatFullContextBrace c)
    checkFilter r fm (ParamFilter c n f) =
      validateTypeFilter r fm f `reviseErrorM`
        (show n ++ " " ++ show f ++ formatFullContextBrace c)

validateCategoryFunction :: (Show c, MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> AnyCategory c -> ScopedFunction c -> m ()
validateCategoryFunction r t f = do
  let fm = getCategoryFilterMap t
  let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
  flip reviseErrorM ("In function:\n---\n" ++ show f ++ "\n---\n") $ do
    funcType <- parsedToFunctionType f
    case sfScope f of
         CategoryScope -> validatateFunctionType r Map.empty Map.empty funcType
         TypeScope     -> validatateFunctionType r fm vm funcType
         ValueScope    -> validatateFunctionType r fm vm funcType
         _             -> return ()

topoSortCategories :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
topoSortCategories tm0 ts = do
  tm <- declareAllTypes tm0 ts
  fmap fst $ update tm (Map.keysSet tm0) ts
  where
    update tm ta (t:ts2) = do
      if getCategoryName t `Set.member` ta
         then update tm ta ts2
         else do
           refines <- mapErrorsM (\r -> getCategory tm (vrContext r,tiName $ vrType r)) $ getCategoryRefines t
           defines <- mapErrorsM (\d -> getCategory tm (vdContext d,diName $ vdType d)) $ getCategoryDefines t
           (ts3,ta2) <- update tm (getCategoryName t `Set.insert` ta) (map snd $ refines ++ defines)
           (ts4,ta3) <- update tm ta2 ts2
           return (ts3 ++ [t] ++ ts4,ta3)
    update _ ta _ = return ([],ta)

mergeObjects :: (MergeableM m, CompileErrorM m) =>
  (a -> a -> m ()) -> [a] -> m [a]
mergeObjects f = merge [] where
  merge cs [] = return cs
  merge cs (x:xs) = do
    ys <- collectOneOrErrorM $ map check (cs ++ xs) ++ [return [x]]
    merge (cs ++ ys) xs where
      check x2 = x2 `f` x >> return []

mergeRefines :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> [ValueRefine c] -> m [ValueRefine c]
mergeRefines r f = mergeObjects check where
  check (ValueRefine _ t1@(TypeInstance n1 _)) (ValueRefine _ t2@(TypeInstance n2 _))
    | n1 /= n2 = compileErrorM $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise =
      noInferredTypes $ checkGeneralMatch r f Covariant
                        (SingleType $ JustTypeInstance $ t1)
                        (SingleType $ JustTypeInstance $ t2)

mergeDefines :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> [ValueDefine c] -> m [ValueDefine c]
mergeDefines r f = mergeObjects check where
  check (ValueDefine _ t1@(DefinesInstance n1 _)) (ValueDefine _ t2@(DefinesInstance n2 _))
    | n1 /= n2 = compileErrorM $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise = do
      checkDefinesMatch r f t1 t2
      return ()

noDuplicateRefines :: (Show c, MergeableM m, CompileErrorM m) =>
  [c] -> CategoryName -> [ValueRefine c] -> m ()
noDuplicateRefines c n rs = do
  let names = map (\r -> (tiName $ vrType r,r)) rs
  noDuplicateCategories c n names

noDuplicateDefines :: (Show c, MergeableM m, CompileErrorM m) =>
  [c] -> CategoryName -> [ValueDefine c] -> m ()
noDuplicateDefines c n ds = do
  let names = map (\d -> (diName $ vdType d,d)) ds
  noDuplicateCategories c n names

noDuplicateCategories :: (Show c, Show a, MergeableM m, CompileErrorM m) =>
  [c] -> CategoryName -> [(CategoryName,a)] -> m ()
noDuplicateCategories c n ns =
  mergeAllM $ map checkCount $ groupBy (\x y -> fst x == fst y) $
                               sortBy (\x y -> fst x `compare` fst y) ns where
    checkCount xa@(x:_:_) =
      compileErrorM $ "Category " ++ show (fst x) ++ " occurs " ++ show (length xa) ++
                      " times in " ++ show n ++ formatFullContextBrace c ++ " :\n---\n" ++
                      intercalate "\n---\n" (map (show . snd) xa)
    checkCount _ = return ()

flattenAllConnections :: (Show c, MergeableM m, CompileErrorM m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
flattenAllConnections tm0 ts = do
  -- We need to process all refines before type-checking can be done.
  tm1 <- foldr preMerge (return tm0) (reverse ts)
  let r = CategoryResolver tm1
  (ts',_) <- foldr (update r) (return ([],tm0)) (reverse ts)
  return ts'
  where
    preMerge t u = do
      tm <- u
      t' <- preMergeSingle tm t
      return $ Map.insert (getCategoryName t') t' tm
    preMergeSingle tm (ValueInterface c ns n ps rs vs fs) = do
      rs' <- fmap concat $ mapErrorsM (getRefines tm) rs
      return $ ValueInterface c ns n ps rs' vs fs
    preMergeSingle tm (ValueConcrete c ns n ps rs ds vs fs) = do
      rs' <- fmap concat $ mapErrorsM (getRefines tm) rs
      return $ ValueConcrete c ns n ps rs' ds vs fs
    preMergeSingle _ t = return t
    update r t u = do
      (ts2,tm) <- u
      t' <- updateSingle r tm t `reviseErrorM`
              ("In category " ++ show (getCategoryName t) ++
               formatFullContextBrace (getCategoryContext t))
      return (ts2 ++ [t'],Map.insert (getCategoryName t') t' tm)
    updateSingle r tm t@(ValueInterface c ns n ps rs vs fs) = do
      let fm = getCategoryFilterMap t
      rs' <- fmap concat $ mapErrorsM (getRefines tm) rs
      rs'' <- mergeRefines r fm rs'
      noDuplicateRefines c n rs''
      checkMerged r fm rs rs''
      -- Only merge from direct parents.
      fs' <- mergeFunctions r tm fm rs [] fs
      return $ ValueInterface c ns n ps rs'' vs fs'
    -- TODO: Remove duplication below and/or have separate tests.
    updateSingle r tm t@(ValueConcrete c ns n ps rs ds vs fs) = do
      let fm = getCategoryFilterMap t
      rs' <- fmap concat $ mapErrorsM (getRefines tm) rs
      rs'' <- mergeRefines r fm rs'
      noDuplicateRefines c n rs''
      checkMerged r fm rs rs''
      ds' <- mergeDefines r fm ds
      noDuplicateDefines c n ds'
      -- Only merge from direct parents.
      fs' <- mergeFunctions r tm fm rs ds fs
      return $ ValueConcrete c ns n ps rs'' ds' vs fs'
    updateSingle _ _ t = return t
    getRefines tm ra@(ValueRefine c t@(TypeInstance n _)) = do
      (_,v) <- getValueCategory tm (c,n)
      let refines = getCategoryRefines v
      pa <- assignParams tm c t
      fmap (ra:) $ mapErrorsM (subAll c pa) refines
    subAll c pa (ValueRefine c1 t1) = do
      (SingleType (JustTypeInstance t2)) <-
        uncheckedSubInstance (getValueForParam pa) (SingleType (JustTypeInstance t1))
      return $ ValueRefine (c ++ c1) t2
    assignParams tm c (TypeInstance n ps) = do
      (_,v) <- getValueCategory tm (c,n)
      let ns = map vpParam $ getCategoryParams v
      paired <- processPairs alwaysPair (Positional ns) ps
      return $ Map.fromList paired
    checkMerged r fm rs rs2 = do
      let rm = Map.fromList $ map (\t -> (tiName $ vrType t,t)) rs
      mergeAllM $ map (\t -> checkConvert r fm (tiName (vrType t) `Map.lookup` rm) t) rs2
    checkConvert r fm (Just ta1@(ValueRefine _ t1)) ta2@(ValueRefine _ t2) = do
      noInferredTypes $ checkGeneralMatch r fm Covariant
                        (SingleType $ JustTypeInstance t1)
                        (SingleType $ JustTypeInstance t2) `reviseErrorM`
                        ("Cannot refine " ++ show ta1 ++ " from inherited " ++ show ta2)
      return ()
    checkConvert _ _ _ _ = return ()

mergeFunctions :: (Show c, MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> CategoryMap c -> ParamFilters -> [ValueRefine c] ->
  [ValueDefine c] -> [ScopedFunction c] -> m [ScopedFunction c]
mergeFunctions r tm fm rs ds fs = do
  inheritValue <- fmap concat $ mapErrorsM (getRefinesFuncs tm) rs
  inheritType  <- fmap concat $ mapErrorsM (getDefinesFuncs tm) ds
  let inheritByName  = Map.fromListWith (++) $ map (\f -> (sfName f,[f])) $ inheritValue ++ inheritType
  let explicitByName = Map.fromListWith (++) $ map (\f -> (sfName f,[f])) fs
  let allNames = Set.toList $ Set.union (Map.keysSet inheritByName) (Map.keysSet explicitByName)
  mapErrorsM (mergeByName r fm inheritByName explicitByName) allNames where
    getRefinesFuncs tm2 ra@(ValueRefine c (TypeInstance n ts2)) = flip reviseErrorM (show ra) $ do
      (_,t) <- getValueCategory tm2 (c,n)
      let ps = map vpParam $ getCategoryParams t
      let fs2 = getCategoryFunctions t
      paired <- processPairs alwaysPair (Positional ps) ts2
      let assigned = Map.fromList paired
      mapErrorsM (uncheckedSubFunction assigned) fs2
    getDefinesFuncs tm2 da@(ValueDefine c (DefinesInstance n ts2)) = flip reviseErrorM (show da) $  do
      (_,t) <- getInstanceCategory tm2 (c,n)
      let ps = map vpParam $ getCategoryParams t
      let fs2 = getCategoryFunctions t
      paired <- processPairs alwaysPair (Positional ps) ts2
      let assigned = Map.fromList paired
      mapErrorsM (uncheckedSubFunction assigned) fs2
    mergeByName r2 fm2 im em n =
      tryMerge r2 fm2 n (n `Map.lookup` im) (n `Map.lookup` em)
    -- Inherited without an override.
    tryMerge _ _ n (Just is) Nothing
      | length is == 1 = return $ head is
      | otherwise = compileErrorM $ "Function " ++ show n ++ " is inherited " ++
                                   show (length is) ++ " times:\n---\n" ++
                                   intercalate "\n---\n" (map show is)
    -- Not inherited.
    tryMerge r2 fm2 n Nothing es = tryMerge r2 fm2 n (Just []) es
    -- Explicit override, possibly inherited.
    tryMerge r2 fm2 n (Just is) (Just es)
      | length es /= 1 = compileErrorM $ "Function " ++ show n ++ " is declared " ++
                                        show (length es) ++ " times:\n---\n" ++
                                        intercalate "\n---\n" (map show es)
      | otherwise = do
        let ff@(ScopedFunction c n2 t s as rs2 ps fa ms) = head es
        mergeAllM $ map (checkMerge r2 fm2 ff) is
        return $ ScopedFunction c n2 t s as rs2 ps fa (ms ++ is)
        where
          checkMerge r3 fm3 f1 f2
            | sfScope f1 /= sfScope f2 =
              compileErrorM $ "Cannot merge " ++ show (sfScope f2) ++ " with " ++
                             show (sfScope f1) ++ " in function merge:\n---\n" ++
                             show f2 ++ "\n  ->\n" ++ show f1
            | otherwise =
              flip reviseErrorM ("In function merge:\n---\n" ++ show f2 ++
                                "\n  ->\n" ++ show f1 ++ "\n---\n") $ do
                f1' <- parsedToFunctionType f1
                f2' <- parsedToFunctionType f2
                checkFunctionConvert r3 fm3 f2' f1'

data FunctionName =
  FunctionName {
    fnName :: String
  } |
  BuiltinPresent |
  BuiltinReduce |
  BuiltinRequire |
  BuiltinStrong |
  BuiltinTypename
  deriving (Eq,Ord)

instance Show FunctionName where
  show (FunctionName n) = n
  show BuiltinPresent = "present"
  show BuiltinReduce = "reduce"
  show BuiltinRequire = "require"
  show BuiltinStrong = "strong"
  show BuiltinTypename = "typename"

data ScopedFunction c =
  ScopedFunction {
    sfContext :: [c],
    sfName :: FunctionName,
    sfType :: CategoryName,
    sfScope :: SymbolScope,
    sfArgs :: Positional (PassedValue c),
    sfReturns :: Positional (PassedValue c),
    sfParams :: Positional (ValueParam c),
    sfFilters :: [ParamFilter c],
    sfMerges :: [ScopedFunction c]
  }

instance Show c => Show (ScopedFunction c) where
  show f = showFunctionInContext (show (sfScope f) ++ " ") "" f

showFunctionInContext :: Show c => String -> String -> ScopedFunction c -> String
showFunctionInContext s indent (ScopedFunction cs n t _ as rs ps fa ms) =
  indent ++ s ++ "/*" ++ show t ++ "*/ " ++ show n ++
  showParams (pValues ps) ++ " " ++ formatContext cs ++ "\n" ++
  concat (map (\v -> indent ++ formatValue v ++ "\n") fa) ++
  indent ++ "(" ++ intercalate "," (map (show . pvType) $ pValues as) ++ ") -> " ++
  "(" ++ intercalate "," (map (show . pvType) $ pValues rs) ++ ")" ++ showMerges (flatten ms)
  where
    showParams [] = ""
    showParams ps2 = "<" ++ intercalate "," (map (show . vpParam) ps2) ++ ">"
    formatContext cs2 = "/*" ++ formatFullContext cs2 ++ "*/"
    formatValue v = "  " ++ show (pfParam v) ++ " " ++ show (pfFilter v) ++
                    " " ++ formatContext (pfContext v)
    flatten [] = Set.empty
    flatten ms2 = Set.unions $ (Set.fromList $ map sfType ms2):(map (flatten . sfMerges) ms2)
    showMerges ms2
      | null (Set.toList ms2) = " /*not merged*/"
      | otherwise = " /*merged from: " ++ intercalate ", " (map show $ Set.toList ms2) ++ "*/"

data PassedValue c =
  PassedValue {
    pvContext :: [c],
    pvType :: ValueType
  }

instance Show c => Show (PassedValue c) where
  show (PassedValue c t) = show t ++ formatFullContextBrace c

parsedToFunctionType :: (Show c, MergeableM m, CompileErrorM m) =>
  ScopedFunction c -> m FunctionType
parsedToFunctionType (ScopedFunction c n _ _ as rs ps fa _) = do
  let as' = Positional $ map pvType $ pValues as
  let rs' = Positional $ map pvType $ pValues rs
  let ps' = Positional $ map vpParam $ pValues ps
  mergeAllM $ map checkFilter fa
  let fm = Map.fromListWith (++) $ map (\f -> (pfParam f,[pfFilter f])) fa
  let fa' = Positional $ map (getFilters fm) $ pValues ps'
  return $ FunctionType as' rs' ps' fa'
  where
    pa = Set.fromList $ map vpParam $ pValues ps
    checkFilter f =
      when (not $ (pfParam f) `Set.member` pa) $
      compileErrorM $ "Filtered param " ++ show (pfParam f) ++
                     " is not defined for function " ++ show n ++
                     formatFullContextBrace c
    getFilters fm2 n2 =
      case n2 `Map.lookup` fm2 of
           (Just fs) -> fs
           _ -> []

uncheckedSubFunction :: (Show c, MergeableM m, CompileErrorM m) =>
  Map.Map ParamName GeneralInstance -> ScopedFunction c -> m (ScopedFunction c)
uncheckedSubFunction pa ff@(ScopedFunction c n t s as rs ps fa ms) =
  flip reviseErrorM ("In function:\n---\n" ++ show ff ++ "\n---\n") $ do
    let fixed = Map.fromList $ map (\n2 -> (n2,SingleType $ JustParamName n2)) $ map vpParam $ pValues ps
    let pa' = Map.union pa fixed
    as' <- fmap Positional $ mapErrorsM (subPassed pa') $ pValues as
    rs' <- fmap Positional $ mapErrorsM (subPassed pa') $ pValues rs
    fa' <- mapErrorsM (subFilter pa') fa
    ms' <- mapErrorsM (uncheckedSubFunction pa) ms
    return $ (ScopedFunction c n t s as' rs' ps fa' ms')
    where
      subPassed pa2 (PassedValue c2 t2) = do
        t' <- uncheckedSubValueType (getValueForParam pa2) t2
        return $ PassedValue c2 t'
      subFilter pa2 (ParamFilter c2 n2 f) = do
        f' <- uncheckedSubFilter (getValueForParam pa2) f
        return $ ParamFilter c2 n2 f'

inferParamTypes :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> ParamFilters -> Map.Map ParamName GeneralInstance ->
  [(ValueType,ValueType)] -> m (Map.Map ParamName GeneralInstance)
inferParamTypes r f ff ps ts = do
  ts2 <- mapErrorsM subAll ts
  ff2 <- fmap Map.fromList $ mapErrorsM filterSub $ Map.toList ff
  gs  <- mergeAllM $ map (uncurry $ checkValueTypeMatch r f) ts2
  let gs2 = concat $ map (filtersToGuess ff2) $ Map.elems ps
  let gs3 = mergeAll $ gs:(map mergeLeaf gs2)
  gs4 <- mergeInferredTypes r f gs3
  let ga = Map.fromList $ zip (map itgParam gs4) (map itgGuess gs4)
  return $ ga `Map.union` ps where
    subAll (t1,t2) = do
      t2' <- uncheckedSubValueType (weakGetValueForParam ps) t2
      return (t1,t2')
    filterSub (k,fs) = do
      fs' <- mapErrorsM (uncheckedSubFilter (weakGetValueForParam ps)) fs
      return (k,fs')
    filtersToGuess f2 (SingleType (JustInferredType p)) =
      case p `Map.lookup` f2 of
           Nothing -> []
           Just fs -> concat $ map (filterToGuess p) fs
    filtersToGuess _ _ = []
    filterToGuess p (TypeFilter FilterRequires t) =
      [InferredTypeGuess p (SingleType t) Contravariant]
    filterToGuess p (TypeFilter FilterAllows t) =
      [InferredTypeGuess p (SingleType t) Covariant]
    filterToGuess _ _ = []

mergeInferredTypes :: (MergeableM m, CompileErrorM m, TypeResolver r) =>
  r -> ParamFilters -> MergeTree InferredTypeGuess -> m [InferredTypeGuess]
mergeInferredTypes r f = reduceMergeTree anyOp allOp leafOp where
  leafOp i = noInferred (itgGuess i) >> return [i]
  anyOp = mergeCommon anyCheck
  allOp = mergeCommon allCheck
  mergeCommon check is = do
    let ia = Map.fromListWith (++) $ zip (map itgParam is) (map (:[]) is)
    mergeAllM $ map (tryMerge check) $ Map.toList ia
  tryMerge check (i,is) = do
    is' <- mergeObjects check is
    case is' of
         []   -> undefined  -- Shouldn't happen.
         [i2] -> return [i2]
         is2  -> compileErrorM $ "Could not reconcile guesses for " ++ show i ++
                                 ": " ++ show is2
  noInferred (TypeMerge _ ts) = mergeAllM $ map noInferred ts
  noInferred (SingleType (JustTypeInstance (TypeInstance _ (Positional ts)))) = mergeAllM $ map noInferred ts
  noInferred (SingleType (JustInferredType i)) = compileErrorM $ "Failed to infer " ++ show i
  noInferred _ = return ()
  anyCheck (InferredTypeGuess _ g1 v1) (InferredTypeGuess _ g2 _) =
    -- Find the least-general guess: If g1 can be replaced with g2, prefer g1.
    noInferredTypes $ checkGeneralMatch r f v1 g1 g2
  allCheck (InferredTypeGuess _ g1 _) (InferredTypeGuess _ g2 v2) =
    -- Find the most-general guess: If g2 can be replaced with g1, prefer g1.
    noInferredTypes $ checkGeneralMatch r f v2 g2 g1
