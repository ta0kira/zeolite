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
{-# LANGUAGE TypeFamilies #-}

module Types.TypeCategory (
  AnyCategory(..),
  CallArgLabel(..),
  CategoryMap(..),
  CategoryResolver(..),
  FunctionName(..),
  FunctionVisibility(..),
  Namespace(..),
  ParamFilter(..),
  PassedValue(..),
  PatternMatch(..),
  PragmaCategory(..),
  ScopedFunction(..),
  SymbolScope(..),
  ValueDefine(..),
  ValueParam(..),
  ValueRefine(..),
  checkCategoryInstances,
  checkConnectedTypes,
  checkConnectionCycles,
  checkFunctionCallVisibility,
  checkParamVariances,
  declareAllTypes, -- TODO: Remove?
  emptyCategoryMap,
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
  getCategoryParamMap,
  getCategoryParamSet,
  getCategoryParams,
  getCategoryPragmas,
  getCategoryRefines,
  getConcreteCategory,
  getFilterMap,
  getFunctionFilterMap,
  getInstanceCategory,
  getValueCategory,
  guessesFromFilters,
  includeNewTypes,
  inferParamTypes,
  instanceFromCategory,
  isCategoryImmutable,
  isInstanceInterface,
  isNoNamespace,
  isPrivateNamespace,
  isPublicNamespace,
  isStaticNamespace,
  isValueConcrete,
  isValueInterface,
  matchesCallArgLabel,
  mergeDefines,
  mergeFunctions,
  mergeInferredTypes,
  mergeRefines,
  noDuplicateDefines,
  noDuplicateRefines,
  parsedToFunctionType,
  partitionByScope,
  prependCategoryPragmaContext,
  replaceSelfFunction,
  setCategoryNamespace,
  singleFromCategory,
  toCategoryMap,
  topoSortCategories,
  uncheckedSubFunction,
  validateCategoryFunction,
) where

import Control.Arrow (second)
import Control.Monad ((>=>),foldM,when)
import Data.List (group,intercalate,nub,nubBy,sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.GeneralType
import Base.MergeTree
import Base.Mergeable
import Base.Positional
import Types.Function
import Types.TypeInstance
import Types.Variance


data AnyCategory c =
  ValueInterface {
    viContext :: [c],
    viNamespace :: Namespace,
    viName :: CategoryName,
    viPragmas :: [PragmaCategory c],
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c],
    viFunctions :: [ScopedFunction c]
  } |
  InstanceInterface {
    iiContext :: [c],
    iiNamespace :: Namespace,
    iiName :: CategoryName,
    iiPragmas :: [PragmaCategory c],
    iiParams :: [ValueParam c],
    iiFunctions :: [ScopedFunction c]
  } |
  ValueConcrete {
    vcContext :: [c],
    vcNamespace :: Namespace,
    vcName :: CategoryName,
    vcPragmas :: [PragmaCategory c],
    vcVisibility :: [FunctionVisibility c],
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcDefines :: [ValueDefine c],
    vcParamFilter :: [ParamFilter c],
    vcFunctions :: [ScopedFunction c]
  }

data PragmaCategory c =
  CategoryImmutable {
    ciContext :: [c]
  }

instance Show c => Show (PragmaCategory c) where
  show (CategoryImmutable c) = "immutable /*" ++ formatFullContext c ++ "*/"

isCategoryImmutable :: PragmaCategory c -> Bool
isCategoryImmutable (CategoryImmutable _) = True

prependCategoryPragmaContext :: [c] -> PragmaCategory c -> PragmaCategory c
prependCategoryPragmaContext c (CategoryImmutable c2) = CategoryImmutable (c++c2)

formatFullContext :: Show a => [a] -> String
formatFullContext cs = intercalate " -> " (map show cs)

formatFullContextBrace :: Show a => [a] -> String
formatFullContextBrace [] = ""
formatFullContextBrace cs = " [" ++ intercalate " -> " (map show cs) ++ "]"

instance Show c => Show (AnyCategory c) where
  show = format where
    format (ValueInterface cs ns n pg ps rs fs) =
      "@value interface " ++ show n ++ formatParams ps ++ namespace ns ++ " { " ++ formatContext cs ++ "\n" ++
      (intercalate "\n\n" $
         map (\p -> "  " ++ show p) pg ++
         map (\r -> "  " ++ formatRefine r) rs ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    format (InstanceInterface cs ns n pg ps fs) =
      "@type interface " ++ show n ++ formatParams ps ++ namespace ns ++ " { " ++ formatContext cs ++
      (intercalate "\n\n" $
         map (\p -> "  " ++ show p) pg ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    format (ValueConcrete cs ns n pg _ ps rs ds vs fs) =
      "concrete " ++ show n ++ formatParams ps ++ namespace ns ++ " { " ++ formatContext cs ++ "\n" ++
      (intercalate "\n\n" $
         map (\p -> "  " ++ show p) pg ++
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
getCategoryName (ValueInterface _ _ n _ _ _ _)      = n
getCategoryName (InstanceInterface _ _ n _ _ _)     = n
getCategoryName (ValueConcrete _ _ n _ _ _ _ _ _ _) = n

getCategoryContext :: AnyCategory c -> [c]
getCategoryContext (ValueInterface c _ _ _ _ _ _)      = c
getCategoryContext (InstanceInterface c _ _ _ _ _)     = c
getCategoryContext (ValueConcrete c _ _ _ _ _ _ _ _ _) = c

getCategoryNamespace :: AnyCategory c -> Namespace
getCategoryNamespace (ValueInterface _ ns _ _ _ _ _)      = ns
getCategoryNamespace (InstanceInterface _ ns _ _ _ _)     = ns
getCategoryNamespace (ValueConcrete _ ns _ _ _ _ _ _ _ _) = ns

getCategoryPragmas :: AnyCategory c -> [PragmaCategory c]
getCategoryPragmas (ValueInterface _ _ _ pg _ _ _)      = pg
getCategoryPragmas (InstanceInterface _ _ _ pg _ _)     = pg
getCategoryPragmas (ValueConcrete _ _ _ pg _ _ _ _ _ _) = pg

setCategoryNamespace :: Namespace -> AnyCategory c -> AnyCategory c
setCategoryNamespace ns (ValueInterface c _ n pg ps rs fs)        = (ValueInterface c ns n pg ps rs fs)
setCategoryNamespace ns (InstanceInterface c _ n pg ps fs)        = (InstanceInterface c ns n pg ps fs)
setCategoryNamespace ns (ValueConcrete c _ n pg fv ps rs ds vs fs) = (ValueConcrete c ns n pg fv ps rs ds vs fs)

getCategoryVisibilities :: AnyCategory c -> [FunctionVisibility c]
getCategoryVisibilities (ValueInterface _ _ _ _ _ _ _)       = []
getCategoryVisibilities (InstanceInterface _ _ _ _ _ _)      = []
getCategoryVisibilities (ValueConcrete _ _ _ _ fv _ _ _ _ _) = fv

getCategoryParams :: AnyCategory c -> [ValueParam c]
getCategoryParams (ValueInterface _ _ _ _ ps _ _)      = ps
getCategoryParams (InstanceInterface _ _ _ _ ps _)     = ps
getCategoryParams (ValueConcrete _ _ _ _ _ ps _ _ _ _) = ps

getCategoryRefines :: AnyCategory c -> [ValueRefine c]
getCategoryRefines (ValueInterface _ _ _ _ _ rs _)      = rs
getCategoryRefines (InstanceInterface _ _ _ _ _ _)      = []
getCategoryRefines (ValueConcrete _ _ _ _ _ _ rs _ _ _) = rs

getCategoryDefines :: AnyCategory c -> [ValueDefine c]
getCategoryDefines (ValueInterface _ _ _ _ _ _ _)       = []
getCategoryDefines (InstanceInterface _ _ _ _ _ _)      = []
getCategoryDefines (ValueConcrete _ _ _ _ _ _ _ ds _ _) = ds

getCategoryFilters :: AnyCategory c -> [ParamFilter c]
getCategoryFilters (ValueInterface _ _ _ _ _ _ _)       = []
getCategoryFilters (InstanceInterface _ _ _ _ _ _)      = []
getCategoryFilters (ValueConcrete _ _ _ _ _ _ _ _ vs _) = vs

getCategoryFunctions :: AnyCategory c -> [ScopedFunction c]
getCategoryFunctions (ValueInterface _ _ _ _ _ _ fs)      = fs
getCategoryFunctions (InstanceInterface _ _ _ _ _ fs)     = fs
getCategoryFunctions (ValueConcrete _ _ _ _ _ _ _ _ _ fs) = fs

singleFromCategory :: AnyCategory c -> TypeInstance
singleFromCategory t = TypeInstance n (Positional ps) where
  n = getCategoryName t
  ps = map (singleType . JustParamName True . vpParam) $ getCategoryParams t

instanceFromCategory :: AnyCategory c -> GeneralInstance
instanceFromCategory = singleType . JustTypeInstance . singleFromCategory

getCategoryDeps :: AnyCategory c -> Set.Set CategoryName
getCategoryDeps t = Set.fromList $ filter (/= getCategoryName t) $ refines ++ defines ++ filters ++ functions where
  refines = concat $ map (fromInstance . singleType . JustTypeInstance . vrType) $ getCategoryRefines t
  defines = concat $ map (fromDefine . vdType) $ getCategoryDefines t
  filters = concat $ map (fromFilter . pfFilter) $ getCategoryFilters t
  functions = concat $ map fromFunction $ getCategoryFunctions t
  fromInstance = reduceMergeTree concat concat fromSingle
  fromSingle (JustTypeInstance (TypeInstance n ps)) = n:(concat $ map fromInstance $ pValues ps)
  fromSingle _ = []
  fromDefine (DefinesInstance n ps) = n:(concat $ map fromInstance $ pValues ps)
  fromFilter (TypeFilter _ t2)  = fromInstance t2
  fromFilter (DefinesFilter t2) = fromDefine t2
  fromFilter ImmutableFilter = []
  fromType (ValueType _ t2) = fromInstance t2
  fromFunction f = args ++ returns ++ filters2 where
    args = concat $ map (fromType . pvType . fst) $ pValues $ sfArgs f
    returns = concat $ map (fromType . pvType) $ pValues $ sfReturns f
    filters2 = concat $ map (fromFilter . pfFilter) $ sfFilters f

isValueInterface :: AnyCategory c -> Bool
isValueInterface (ValueInterface _ _ _ _ _ _ _) = True
isValueInterface _ = False

isInstanceInterface :: AnyCategory c -> Bool
isInstanceInterface (InstanceInterface _ _ _ _ _ _) = True
isInstanceInterface _ = False

isValueConcrete :: AnyCategory c -> Bool
isValueConcrete (ValueConcrete _ _ _ _ _ _ _ _ _ _) = True
isValueConcrete _ = False

data Namespace =
  StaticNamespace {
    snName :: String
  } |
  NoNamespace |
  PublicNamespace |
  PrivateNamespace
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

isPublicNamespace :: Namespace -> Bool
isPublicNamespace PublicNamespace = True
isPublicNamespace _                = False

isPrivateNamespace :: Namespace -> Bool
isPrivateNamespace PrivateNamespace = True
isPrivateNamespace _                = False

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

instance Show c => TypeResolver (CategoryResolver c) where
    trRefines (CategoryResolver tm) ta@(TypeInstance n1 ps1) n2
      | n1 == n2 = do
        (_,t) <- getValueCategory tm ([],n1)
        processPairs_ alwaysPair (Positional $ map vpParam $ getCategoryParams t) ps1
        return ps1
      | otherwise = do
        let self = singleType $ JustTypeInstance ta
        (_,t) <- getValueCategory tm ([],n1)
        let params = map vpParam $ getCategoryParams t
        assigned <- fmap Map.fromList $ processPairs alwaysPair (Positional params) ps1
        let pa = Map.fromList $ map (\r -> (tiName r,tiParams r)) $ map vrType $ getCategoryRefines t
        ps2 <- case n2 `Map.lookup` pa of
                    (Just x) -> return x
                    _ -> compilerErrorM $ "Category " ++ show n1 ++ " does not refine " ++ show n2
        fmap Positional $ mapCompilerM (subAllParams assigned >=> replaceSelfInstance self) $ pValues ps2
    trDefines (CategoryResolver tm) ta@(TypeInstance n1 ps1) n2 = do
      let self = singleType $ JustTypeInstance ta
      (_,t) <- getValueCategory tm ([],n1)
      let params = map vpParam $ getCategoryParams t
      assigned <- fmap Map.fromList $ processPairs alwaysPair (Positional params) ps1
      let pa = Map.fromList $ map (\r -> (diName r,diParams r)) $ map vdType $ getCategoryDefines t
      ps2 <- case n2 `Map.lookup` pa of
                  (Just x) -> return x
                  _ -> compilerErrorM $ "Category " ++ show n1 ++ " does not define " ++ show n2
      fmap Positional $ mapCompilerM (subAllParams assigned >=> replaceSelfInstance self) $ pValues ps2
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
    trImmutable (CategoryResolver tm) n = do
      (_,t) <- getCategory tm ([],n)
      return $ any isCategoryImmutable (getCategoryPragmas t)

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

checkFilters :: CollectErrorsM m =>
  AnyCategory c -> Positional GeneralInstance -> m (Positional [TypeFilter])
checkFilters t ps = do
  assigned <- fmap (Map.insert ParamSelf selfType . Map.fromList) $ processPairs alwaysPair (Positional params) ps
  fs <- mapCompilerM (subSingleFilter assigned . \f -> (pfParam f,pfFilter f)) allFilters
  let fa = Map.fromListWith (++) $ map (second (:[])) fs
  fmap Positional $ mapCompilerM (assignFilter fa) params where
    params = map vpParam $ getCategoryParams t
    allFilters = getCategoryFilters t ++ map (ParamFilter (getCategoryContext t) ParamSelf) (getSelfFilters t)
    subSingleFilter pa (n,(TypeFilter v t2)) = do
      t3<- uncheckedSubInstance (getValueForParam pa) t2
      return (n,(TypeFilter v t3))
    subSingleFilter pa (n,(DefinesFilter (DefinesInstance n2 ps2))) = do
      ps3 <- mapCompilerM (uncheckedSubInstance $ getValueForParam pa) (pValues ps2)
      return (n,(DefinesFilter (DefinesInstance n2 (Positional ps3))))
    subSingleFilter _ f@(_,ImmutableFilter) = return f
    assignFilter fa n =
      case n `Map.lookup` fa of
            (Just x) -> return x
            _ -> return []

getSelfFilters :: AnyCategory c -> [TypeFilter]
getSelfFilters t = selfFilters where
  params = map vpParam $ getCategoryParams t
  selfParams = Positional $ map (singleType . JustParamName False) params
  selfFilters
    | isInstanceInterface t = [
        DefinesFilter $ DefinesInstance (getCategoryName t) selfParams
      ] ++ inheritedFilters
    | otherwise = [
        TypeFilter FilterRequires $ singleType $ JustTypeInstance $ TypeInstance (getCategoryName t) selfParams
      ] ++ inheritedFilters
  inheritedFilters = map (DefinesFilter . vdType) (getCategoryDefines t) ++
                     map (TypeFilter FilterRequires . singleType . JustTypeInstance . vrType) (getCategoryRefines t)

subAllParams :: CollectErrorsM m =>
  ParamValues -> GeneralInstance -> m GeneralInstance
subAllParams pa = uncheckedSubInstance (getValueForParam pa)

data CategoryMap c =
  CategoryMap {
    cmKnown :: Map.Map CategoryName [c],
    cmAvailable :: Map.Map CategoryName (AnyCategory c)
  }

emptyCategoryMap :: CategoryMap c
emptyCategoryMap = toCategoryMap []

toCategoryMap :: [(CategoryName,AnyCategory c)] -> CategoryMap c
toCategoryMap = CategoryMap Map.empty . Map.fromList

getCategory :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getCategory (CategoryMap km tm) (c,n) = handle (n `Map.lookup` tm) (n `Map.lookup` km) where
  handle (Just t) _ = return (c,t)
  handle _ (Just c2) = compilerErrorM $ "Type " ++ show n ++ formatFullContextBrace c2 ++
                                        " not visible here" ++ formatFullContextBrace c
  handle _ _ = compilerErrorM $ "Type " ++ show n ++ " not found" ++ formatFullContextBrace c

getValueCategory :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getValueCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueInterface t || isValueConcrete t
     then return (c2,t)
     else compilerErrorM $ "Category " ++ show n ++
                           " cannot be used as a value" ++
                           formatFullContextBrace c

getInstanceCategory :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getInstanceCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isInstanceInterface t
     then return (c2,t)
     else compilerErrorM $ "Category " ++ show n ++
                           " cannot be used as a type interface" ++
                           formatFullContextBrace c

getConcreteCategory :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getConcreteCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueConcrete t
     then return (c2,t)
     else compilerErrorM $ "Category " ++ show n ++
                           " cannot be used as concrete" ++
                           formatFullContextBrace c

includeNewTypes :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
includeNewTypes tm0 ts = do
  checkConnectionCycles tm0 ts
  checkConnectedTypes tm0 ts
  checkParamVariances tm0 ts
  ts2 <- topoSortCategories tm0 ts
  ts3 <- flattenAllConnections tm0 ts2
  checkCategoryInstances tm0 ts3
  declareAllTypes tm0 ts3

declareAllTypes :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
declareAllTypes tm0 = foldr (\t tm -> tm >>= update t) (return tm0) where
  update t (CategoryMap km tm) =
    case getCategoryName t `Map.lookup` tm of
        (Just t2) -> compilerErrorM $ "Type " ++ show (getCategoryName t) ++
                                      formatFullContextBrace (getCategoryContext t) ++
                                      " has already been declared" ++
                                      formatFullContextBrace (getCategoryContext t2)
        _ -> return $ CategoryMap km $ Map.insert (getCategoryName t) t tm

getFilterMap :: CollectErrorsM m => [ValueParam c] -> [ParamFilter c] -> m ParamFilters
getFilterMap ps fs = do
  mirrored <- fmap concat $ mapCompilerM maybeMirror fs
  return $ getFilters mirrored $ zip (Set.toList pa) (repeat []) where
    pa = Set.fromList $ map vpParam ps
    maybeMirror fa@(ParamFilter c p1 (TypeFilter d p2)) = do
      p <- tryCompilerM $ matchOnlyLeaf p2
      case p of
          Just (JustParamName _ p') ->
            if p' `Set.member` pa
               then return [fa,(ParamFilter c p' (TypeFilter (flipFilter d) (singleType (JustParamName False p1))))]
               else return [fa]
          _ -> return [fa]
    maybeMirror fa = return [fa]
    getFilters fs2 pa0 = let fs' = map (\f -> (pfParam f,pfFilter f)) fs2 in
                             Map.fromListWith (++) $ map (second (:[])) fs' ++ pa0

getCategoryFilterMap :: CollectErrorsM m => AnyCategory c -> m ParamFilters
getCategoryFilterMap t = do
  defaultMap <- getFilterMap (getCategoryParams t) (getCategoryFilters t)
  return $ Map.insert ParamSelf (getSelfFilters t) defaultMap

getCategoryParamSet :: CollectErrorsM m => AnyCategory c -> m (Set.Set ParamName)
getCategoryParamSet = return . Set.fromList . ([ParamSelf] ++) . map vpParam . getCategoryParams

-- TODO: Use this where it's needed in this file.
getFunctionFilterMap :: CollectErrorsM m => ScopedFunction c -> m ParamFilters
getFunctionFilterMap f = getFilterMap (pValues $ sfParams f) (sfFilters f)

getCategoryParamMap :: AnyCategory c -> ParamValues
getCategoryParamMap t = let ps = map vpParam $ getCategoryParams t in
  Map.fromList $ zip ps (map (singleType . JustParamName False) ps) ++ [(ParamSelf,selfType)]

disallowBoundedParams :: CollectErrorsM m => ParamFilters -> m ()
disallowBoundedParams = mapCompilerM_ checkBounds . Map.toList where
  checkBounds (p,fs) = do
    let (lb,ub) = foldr splitBounds (minBound,maxBound) fs
    when (lb /= minBound && ub /= maxBound) $
      "Param " ++ show p ++ " cannot have both lower and upper bounds" !!>
        collectAllM_ [
            compilerErrorM $ "Lower bound: " ++ show lb,
            compilerErrorM $ "Upper bound: " ++ show ub
          ]
  splitBounds (TypeFilter FilterRequires t) (lb,ub) = (lb,t<&&>ub)
  splitBounds (TypeFilter FilterAllows   t) (lb,ub) = (t<||>lb,ub)
  splitBounds _ bs = bs

checkConnectedTypes :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectedTypes cm0 ts = do
  cm <- declareAllTypes cm0 ts
  collectAllM_ (map (checkSingle cm) ts)
  where
    checkSingle cm (ValueInterface c _ n _ _ rs _) = do
      let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
      is <- mapCompilerM (getCategory cm) ts2
      collectAllM_ (map (valueRefinesInstanceError c n) is)
      collectAllM_ (map (valueRefinesConcreteError c n) is)
    checkSingle cm (ValueConcrete c _ n _ _ _ rs ds _ _) = do
      let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
      let ts3 = map (\d -> (vdContext d,diName $ vdType d)) ds
      is1 <- mapCompilerM (getCategory cm) ts2
      is2 <- mapCompilerM (getCategory cm) ts3
      collectAllM_ (map (concreteRefinesInstanceError c n) is1)
      collectAllM_ (map (concreteDefinesValueError c n) is2)
      collectAllM_ (map (concreteRefinesConcreteError c n) is1)
      collectAllM_ (map (concreteDefinesConcreteError c n) is2)
    checkSingle _ _ = return ()
    valueRefinesInstanceError c n (c2,t)
      | isInstanceInterface t =
        compilerErrorM $ "Value interface " ++ show n ++ formatFullContextBrace c ++
                         " cannot refine type interface " ++
                         show (iiName t) ++ formatFullContextBrace c2
      | otherwise = return ()
    valueRefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compilerErrorM $ "Value interface " ++ show n ++ formatFullContextBrace c ++
                         " cannot refine concrete type " ++
                         show (getCategoryName t) ++ formatFullContextBrace c2
      | otherwise = return ()
    concreteRefinesInstanceError c n (c2,t)
      | isInstanceInterface t =
        compilerErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                         " cannot refine instance interface " ++
                         show (getCategoryName t) ++ formatFullContextBrace c2 ++
                         " => use defines instead"
      | otherwise = return ()
    concreteDefinesValueError c n (c2,t)
      | isValueInterface t =
        compilerErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                         " cannot define value interface " ++
                         show (getCategoryName t) ++ formatFullContextBrace c2 ++
                         " => use refines instead"
      | otherwise = return ()
    concreteRefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compilerErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                         " cannot refine concrete type " ++
                         show (getCategoryName t) ++ formatFullContextBrace c2
      | otherwise = return ()
    concreteDefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compilerErrorM $ "Concrete type " ++ show n ++ formatFullContextBrace c ++
                         " cannot define concrete type " ++
                         show (getCategoryName t) ++ formatFullContextBrace c2
      | otherwise = return ()

checkConnectionCycles :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectionCycles (CategoryMap km tm0) ts = collectAllM_ (map (checker []) ts) where
  tm = Map.union tm0 $ Map.fromList $ zip (map getCategoryName ts) ts
  cm = CategoryMap km tm
  checker us (ValueInterface c _ n _ _ rs _) = do
    failIfCycle n c us
    let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- mapCompilerM (getValueCategory cm) ts2
    collectAllM_ (map (checker (us ++ [n]) . snd) is)
  checker us (ValueConcrete c _ n _ _ _ rs _ _ _) = do
    failIfCycle n c us
    let ts2 = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- mapCompilerM (getValueCategory cm) ts2
    collectAllM_ (map (checker (us ++ [n]) . snd) is)
  checker _ _ = return ()
  failIfCycle n c us =
    when (n `Set.member` (Set.fromList us)) $
      compilerErrorM $ "Category " ++ show n ++ formatFullContextBrace c ++
                       " refers back to itself: " ++
                       intercalate " -> " (map show (us ++ [n]))

checkParamVariances :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkParamVariances cm0 ts = do
  cm <- declareAllTypes cm0 ts
  let r = CategoryResolver cm
  mapCompilerM_ (checkCategory r) ts
  mapCompilerM_ checkBounds ts
  where
    categoryContext t =
      "In " ++ show (getCategoryName t) ++ formatFullContextBrace (getCategoryContext t)
    checkBounds t = categoryContext t ??> (getCategoryFilterMap t >>= disallowBoundedParams)
    checkCategory r t@(ValueInterface c _ n _ ps rs _) = categoryContext t ??> do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      collectAllM_ (map (checkRefine r vm) rs)
    checkCategory r t@(ValueConcrete c _ n _ fv ps rs ds _ _) = categoryContext t ??> do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      collectAllM_ (map (checkRefine r vm) rs)
      collectAllM_ (map (checkDefine r vm) ds)
      collectAllM_ (map (checkVisibility r vm) fv)
    checkCategory _ t@(InstanceInterface c _ n _ ps _) = categoryContext t ??> do
      noDuplicates c n ps
    noDuplicates c n ps = collectAllM_ (map checkCount $ group $ sort $ map vpParam ps) where
      checkCount xa@(x:_:_) =
        compilerErrorM $ "Param " ++ show x ++ " occurs " ++ show (length xa) ++
                         " times in " ++ show n ++ formatFullContextBrace c
      checkCount _ = return ()
    checkRefine r vm (ValueRefine c t) =
      validateInstanceVariance r vm Covariant (singleType $ JustTypeInstance t) <??
        "In " ++ show t ++ formatFullContextBrace c
    checkDefine r vm (ValueDefine c t) =
      validateDefinesVariance r vm Covariant t <??
        "In " ++ show t ++ formatFullContextBrace c
    checkVisibility _ _ FunctionVisibilityDefault = return ()
    checkVisibility r vm (FunctionVisibility _ ts2) =
      collectAllM_ (map (checkVisibilitySingle r vm) ts2)
    checkVisibilitySingle r vm (c,t) =
      validateInstanceVariance r vm Covariant t <??
        "In visibility " ++ show t ++ formatFullContextBrace c

checkCategoryInstances :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkCategoryInstances cm0 ts = do
  cm <- declareAllTypes cm0 ts
  let r = CategoryResolver cm
  mapCompilerM_ (checkSingle r) ts
  where
    checkSingle r t = do
      pa <- getCategoryParamSet t
      mapCompilerM_ (checkFilterParam pa)  (getCategoryFilters t)
      mapCompilerM_ (checkRefine r pa)     (getCategoryRefines t)
      mapCompilerM_ (checkDefine r pa)     (getCategoryDefines t)
      mapCompilerM_ (checkFilter r pa)     (getCategoryFilters t)
      mapCompilerM_ (checkVisibility r pa) (getCategoryVisibilities t)
      mapCompilerM_ (validateCategoryFunction r t) (getCategoryFunctions t)
    checkVisibility r fm (FunctionVisibility c ts2) =
      mapCompilerM_ (checkVisibilitySingle r fm) ts2 <??
        "In visibility at " ++ formatFullContext c
    checkVisibility _ _ _ = return ()
    checkVisibilitySingle r fm (c,t) =
      validateGeneralInstance r fm t <??
        "In " ++ show t ++ formatFullContextBrace c
    checkFilterParam pa (ParamFilter c n _) =
      when (not $ n `Set.member` pa) $
        compilerErrorM $ "Param " ++ show n ++ formatFullContextBrace c ++ " not found"
    checkRefine r fm (ValueRefine c t) =
      validateTypeInstance r fm t <??
        "In " ++ show t ++ formatFullContextBrace c
    checkDefine r fm (ValueDefine c t) =
      validateDefinesInstance r fm t <??
        "In " ++ show t ++ formatFullContextBrace c
    checkFilter r fm (ParamFilter c n f) =
      validateTypeFilter r fm f <??
        "In " ++ show n ++ " " ++ show f ++ formatFullContextBrace c

validateCategoryFunction :: (Show c, CollectErrorsM m, TypeResolver r) =>
  r -> AnyCategory c -> ScopedFunction c -> m ()
validateCategoryFunction r t f = do
  pa <- getCategoryParamSet t
  let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
  message ??> do
    funcType <- parsedToFunctionType f
    case sfScope f of
         CategoryScope -> validatateFunctionType r Set.empty Map.empty funcType
         TypeScope     -> validatateFunctionType r pa vm funcType
         ValueScope    -> validatateFunctionType r pa vm funcType
         _             -> return ()
    getFunctionFilterMap f >>= disallowBoundedParams
    checkVis (sfScope f) (sfVisibility f) where
      checkVis CategoryScope va@(FunctionVisibility _ _) =
        compilerErrorM $ "Category functions must not have restricted visibility: " ++ show va
      checkVis _ _ = return ()
      message
        | getCategoryName t == sfType f = "In function:\n---\n" ++ show f ++ "\n---\n"
        | otherwise = "In function inherited from " ++ show (sfType f) ++
                      formatFullContextBrace (getCategoryContext t) ++ ":\n---\n" ++ show f ++ "\n---\n"

topoSortCategories :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
topoSortCategories cm0 ts = do
  cm <- declareAllTypes cm0 ts
  fmap fst $ update cm (Map.keysSet $ cmAvailable cm0) ts
  where
    update cm ta (t:ts2) = do
      if getCategoryName t `Set.member` ta
         then update cm ta ts2
         else do
           refines <- mapCompilerM (\r -> getCategory cm (vrContext r,tiName $ vrType r)) $ getCategoryRefines t
           defines <- mapCompilerM (\d -> getCategory cm (vdContext d,diName $ vdType d)) $ getCategoryDefines t
           (ts3,ta2) <- update cm (getCategoryName t `Set.insert` ta) (map snd $ refines ++ defines)
           (ts4,ta3) <- update cm ta2 ts2
           return (ts3 ++ [t] ++ ts4,ta3)
    update _ ta _ = return ([],ta)

mergeRefines :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> [ValueRefine c] -> m [ValueRefine c]
mergeRefines r f = mergeObjectsM check where
  check (ValueRefine _ t1@(TypeInstance n1 _)) (ValueRefine _ t2@(TypeInstance n2 _))
    | n1 /= n2 = compilerErrorM $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise =
      noInferredTypes $ checkGeneralMatch r f Covariant
                        (singleType $ JustTypeInstance $ t1)
                        (singleType $ JustTypeInstance $ t2)

mergeDefines :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> [ValueDefine c] -> m [ValueDefine c]
mergeDefines r f = mergeObjectsM check where
  check (ValueDefine _ t1@(DefinesInstance n1 _)) (ValueDefine _ t2@(DefinesInstance n2 _))
    | n1 /= n2 = compilerErrorM $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise = checkDefinesMatch r f t2 t1 >> return ()

noDuplicateRefines :: (Show c, CollectErrorsM m) =>
  [c] -> CategoryName -> [ValueRefine c] -> m ()
noDuplicateRefines c n rs = do
  let names = map (\r -> (tiName $ vrType r,r)) rs
  noDuplicateCategories c n names

noDuplicateDefines :: (Show c, CollectErrorsM m) =>
  [c] -> CategoryName -> [ValueDefine c] -> m ()
noDuplicateDefines c n ds = do
  let names = map (\d -> (diName $ vdType d,d)) ds
  noDuplicateCategories c n names

noDuplicateCategories :: (Show c, Show a, CollectErrorsM m) =>
  [c] -> CategoryName -> [(CategoryName,a)] -> m ()
noDuplicateCategories c n ns = do
  let byName = Map.fromListWith (++) $ map (second (:[])) ns
  mapCompilerM_ checkCount $ Map.toList byName where
    checkCount (_,[_]) = return ()
    checkCount (n2,xs) =
      compilerErrorM $ show n2 ++ " occurs " ++ show (length xs) ++
                       " times in " ++ show n ++ formatFullContextBrace c ++ ":\n---\n" ++
                       intercalate "\n---\n" (map show xs)

flattenAllConnections :: (Show c, CollectErrorsM m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
flattenAllConnections (CategoryMap km tm0) ts = do
  -- We need to process all refines before type-checking can be done.
  tm1 <- foldr preMerge (return tm0) (reverse ts)
  let r = CategoryResolver (CategoryMap km tm1)
  (ts',_) <- foldr (update r) (return ([],tm0)) (reverse ts)
  return ts'
  where
    preMerge t u = do
      tm <- u
      t' <- preMergeSingle tm t
      return $ Map.insert (getCategoryName t') t' tm
    preMergeSingle tm (ValueInterface c ns n pg ps rs fs) = do
      rs' <- fmap concat $ mapCompilerM (getRefines tm) rs
      return $ ValueInterface c ns n pg ps rs' fs
    preMergeSingle tm (ValueConcrete c ns n pg fv ps rs ds vs fs) = do
      rs' <- fmap concat $ mapCompilerM (getRefines tm) rs
      return $ ValueConcrete c ns n pg fv ps rs' ds vs fs
    preMergeSingle _ t = return t
    update r t u = do
      (ts2,tm) <- u
      t' <- updateSingle r tm t <??
              "In category " ++ show (getCategoryName t) ++
                formatFullContextBrace (getCategoryContext t)
      return (ts2 ++ [t'],Map.insert (getCategoryName t') t' tm)
    updateSingle r tm t@(ValueInterface c ns n pg ps rs fs) = do
      fm <- getCategoryFilterMap t
      let pm = getCategoryParamMap t
      rs' <- fmap concat $ mapCompilerM (getRefines tm) rs
      rs'' <- mergeRefines r fm rs'
      noDuplicateRefines c n rs''
      checkMerged r fm rs rs''
      pg2 <- fmap concat $ mapCompilerM (getRefinesPragmas tm) rs
      -- Only merge from direct parents.
      fs' <- mergeFunctions r (CategoryMap km tm) pm fm rs [] fs
      return $ ValueInterface c ns n (pg++pg2) ps rs'' fs'
    -- TODO: Remove duplication below and/or have separate tests.
    updateSingle r tm t@(ValueConcrete c ns n pg fv ps rs ds vs fs) = do
      fm <- getCategoryFilterMap t
      let pm = getCategoryParamMap t
      rs' <- fmap concat $ mapCompilerM (getRefines tm) rs
      rs'' <- mergeRefines r fm rs'
      noDuplicateRefines c n rs''
      checkMerged r fm rs rs''
      ds' <- mergeDefines r fm ds
      noDuplicateDefines c n ds'
      pg2 <- fmap concat $ mapCompilerM (getRefinesPragmas tm) rs
      pg3 <- fmap concat $ mapCompilerM (getDefinesPragmas tm) ds
      -- Only merge from direct parents.
      fs' <- mergeFunctions r (CategoryMap km tm) pm fm rs ds fs
      return $ ValueConcrete c ns n (pg++pg2++pg3) fv ps rs'' ds' vs fs'
    updateSingle _ _ t = return t
    getRefines tm ra@(ValueRefine c t@(TypeInstance n _)) = do
      (_,v) <- getValueCategory (CategoryMap km tm) (c,n)
      let refines = getCategoryRefines v
      pa <- assignParams tm c t
      fmap (ra:) $ mapCompilerM (subAll c pa) refines
    subAll c pa (ValueRefine c1 t1) = do
      t2 <- uncheckedSubSingle (getValueForParam pa) t1
      return $ ValueRefine (c ++ c1) t2
    assignParams tm c (TypeInstance n ps) = do
      (_,v) <- getValueCategory (CategoryMap km tm) (c,n)
      let ns = map vpParam $ getCategoryParams v
      paired <- processPairs alwaysPair (Positional ns) ps
      return $ Map.insert ParamSelf selfType $ Map.fromList paired
    checkMerged r fm rs rs2 = do
      let rm = Map.fromList $ map (\t -> (tiName $ vrType t,t)) rs
      mapCompilerM_ (\t -> checkConvert r fm (tiName (vrType t) `Map.lookup` rm) t) rs2
    checkConvert r fm (Just ta1@(ValueRefine _ t1)) ta2@(ValueRefine _ t2) = do
      noInferredTypes $ checkGeneralMatch r fm Covariant
                        (singleType $ JustTypeInstance t1)
                        (singleType $ JustTypeInstance t2) <!!
                          "Cannot refine " ++ show ta1 ++ " from inherited " ++ show ta2
      return ()
    checkConvert _ _ _ _ = return ()
    getRefinesPragmas tm rf = do
      (_,t) <- getCategory (CategoryMap km tm) (vrContext rf,tiName $ vrType rf)
      return $ map (prependCategoryPragmaContext $ vrContext rf) $ getCategoryPragmas t
    getDefinesPragmas tm df = do
      (_,t) <- getCategory (CategoryMap km tm) (vdContext df,diName $ vdType df)
      return $ map (prependCategoryPragmaContext $ vdContext df) $ getCategoryPragmas t

mergeFunctions :: (Show c, CollectErrorsM m, TypeResolver r) =>
  r -> CategoryMap c -> ParamValues -> ParamFilters -> [ValueRefine c] ->
  [ValueDefine c] -> [ScopedFunction c] -> m [ScopedFunction c]
mergeFunctions r cm pm fm rs ds fs = do
  inheritValue <- fmap concat $ mapCompilerM getRefinesFuncs rs
  inheritType  <- fmap concat $ mapCompilerM getDefinesFuncs ds
  let inheritByName  = fmap (nubBy sameFunction) $ Map.fromListWith (++) $ map (\f -> (sfName f,[f])) $ inheritValue ++ inheritType
  let explicitByName = Map.fromListWith (++) $ map (\f -> (sfName f,[f])) fs
  let allNames = Set.toList $ Set.union (Map.keysSet inheritByName) (Map.keysSet explicitByName)
  mapCompilerM (mergeByName inheritByName explicitByName) allNames where
    getRefinesFuncs (ValueRefine c (TypeInstance n ts2)) = do
      (_,t) <- getValueCategory cm (c,n)
      let ps = map vpParam $ getCategoryParams t
      let fs2 = getCategoryFunctions t
      paired <- processPairs alwaysPair (Positional ps) ts2
      let assigned = Map.fromList $ (ParamSelf,selfType):paired
      mapCompilerM (unfixedSubFunction assigned) fs2
    getDefinesFuncs (ValueDefine c (DefinesInstance n ts2)) = do
      (_,t) <- getInstanceCategory cm (c,n)
      let ps = map vpParam $ getCategoryParams t
      let fs2 = getCategoryFunctions t
      paired <- processPairs alwaysPair (Positional ps) ts2
      let assigned = Map.fromList $ (ParamSelf,selfType):paired
      mapCompilerM (unfixedSubFunction assigned) fs2
    mergeByName im em n =
      tryMerge n (n `Map.lookup` im) (n `Map.lookup` em)
    -- Inherited without an override.
    tryMerge n (Just is) Nothing
      | length is == 1 = return $ head is
      | otherwise = compilerErrorM $ "Function " ++ show n ++ " is inherited " ++
                                     show (length is) ++ " times:\n---\n" ++
                                     intercalate "\n---\n" (map show is)
    -- Not inherited.
    tryMerge n Nothing es = tryMerge n (Just []) es
    -- Explicit override, possibly inherited.
    tryMerge n (Just is) (Just es)
      | length es /= 1 = compilerErrorM $ "Function " ++ show n ++ " is declared " ++
                                          show (length es) ++ " times:\n---\n" ++
                                          intercalate "\n---\n" (map show es)
      | otherwise = do
        let ff@(ScopedFunction c n2 t s v as rs2 ps fa ms) = head es
        mapCompilerM_ (checkMerge ff) is
        return $ ScopedFunction c n2 t s v as rs2 ps fa (ms ++ is)
        where
          checkMerge f1 f2
            | sfScope f1 /= sfScope f2 =
              compilerErrorM $ "Cannot merge " ++ show (sfScope f2) ++ " with " ++
                               show (sfScope f1) ++ " in function merge:\n---\n" ++
                               show f2 ++ "\n  ->\n" ++ show f1
            | otherwise =
              "In function merge:\n---\n" ++ show f2 ++ "\n  ->\n" ++ show f1 ++ "\n---\n" ??> do
                checkMergeVis (sfVisibility f1) (sfVisibility f2)
                f1' <- parsedToFunctionType f1
                f2' <- parsedToFunctionType f2
                case sfScope f1 of
                     CategoryScope -> checkFunctionConvert r Map.empty Map.empty f2' f1'
                     _             -> checkFunctionConvert r fm pm f2' f1'
                processPairs_ checkArgNames (sfArgs f1) (sfArgs f2)
          checkMergeVis FunctionVisibilityDefault _ = return ()
          checkMergeVis v1 v2 =
            compilerErrorM $ "Cannot supersede " ++ show v2 ++ " with " ++ show v1
          checkArgNames (_,n1) (_,n2)
            | fmap calName n1 == fmap calName n2 = return ()
          checkArgNames t1 t2 =
            compilerErrorM $ "Expected arg label from " ++ showArgName t2 ++
                            " to match " ++ showArgName t1
          showArgName (t',Nothing) = show t'
          showArgName (t',Just n') = show (pvType t') ++ " " ++ show n'

data FunctionName =
  FunctionName {
    fnName :: String
  } |
  BuiltinPresent |
  BuiltinReduce |
  BuiltinRequire |
  BuiltinStrong |
  BuiltinIdentify |
  BuiltinTypename
  deriving (Eq,Ord)

instance Show FunctionName where
  show (FunctionName n) = n
  show BuiltinPresent  = "present"
  show BuiltinReduce   = "reduce"
  show BuiltinRequire  = "require"
  show BuiltinStrong   = "strong"
  show BuiltinIdentify = "identify"
  show BuiltinTypename = "typename"

data CallArgLabel c =
  CallArgLabel {
    calContext :: [c],
    calName ::String
  }
  deriving (Eq,Ord)

instance Show c => Show (CallArgLabel c) where
  show (CallArgLabel c n) = n ++ formatFullContextBrace c

matchesCallArgLabel :: CallArgLabel c -> String -> Bool
matchesCallArgLabel (CallArgLabel _ n1) n2 = init n1 == n2

data FunctionVisibility c =
  FunctionVisibility {
    fvContext :: [c],
    fvTypes :: [([c],GeneralInstance)]
  } |
  FunctionVisibilityDefault
  deriving (Eq)

instance Show c => Show (FunctionVisibility c) where
  show FunctionVisibilityDefault = "visibility _"
  show (FunctionVisibility c ts) = "visibility " ++ intercalate ", " (map (show . snd) ts) ++ formatFullContextBrace c

data ScopedFunction c =
  ScopedFunction {
    sfContext :: [c],
    sfName :: FunctionName,
    sfType :: CategoryName,
    sfScope :: SymbolScope,
    sfVisibility :: FunctionVisibility c,
    sfArgs :: Positional (PassedValue c, Maybe (CallArgLabel c)),
    sfReturns :: Positional (PassedValue c),
    sfParams :: Positional (ValueParam c),
    sfFilters :: [ParamFilter c],
    sfMerges :: [ScopedFunction c]
  }

instance Show c => Show (ScopedFunction c) where
  show f = showFunctionInContext (show (sfScope f) ++ " ") "" f

sameFunction :: ScopedFunction c -> ScopedFunction c -> Bool
sameFunction (ScopedFunction _ n1 t1 s1 _ _ _ _ _ _) (ScopedFunction _ n2 t2 s2 _ _ _ _ _ _) =
  all id [n1 == n2, t1 == t2, s1 == s2]

showFunctionInContext :: Show c => String -> String -> ScopedFunction c -> String
showFunctionInContext s indent (ScopedFunction cs n t _ _ as rs ps fa ms) =
  indent ++ s ++ "/*" ++ show t ++ "*/ " ++ show n ++
  showParams (pValues ps) ++ " " ++ formatContext cs ++ "\n" ++
  concat (map (\v -> indent ++ formatValue v ++ "\n") fa) ++
  indent ++ "(" ++ intercalate "," (map showArg $ pValues as) ++ ") -> " ++
  "(" ++ intercalate "," (map (show . pvType) $ pValues rs) ++ ")" ++ showMerges (flatten ms)
  where
    showArg (a,Nothing) = show (pvType a)
    showArg (a,Just n2) = show (pvType a) ++ " " ++ show (calName n2)
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

parsedToFunctionType :: (Show c, CollectErrorsM m) =>
  ScopedFunction c -> m FunctionType
parsedToFunctionType (ScopedFunction c n _ _ _ as rs ps fa _) = do
  let as' = Positional $ map (pvType . fst) $ pValues as
  let rs' = Positional $ map pvType $ pValues rs
  let ps' = Positional $ map vpParam $ pValues ps
  mapCompilerM_ checkFilter fa
  let fm = Map.fromListWith (++) $ map (\f -> (pfParam f,[pfFilter f])) fa
  let fa' = Positional $ map (getFilters fm) $ pValues ps'
  return $ FunctionType as' rs' ps' fa'
  where
    pa = Set.fromList $ map vpParam $ pValues ps
    checkFilter f =
      when (not $ (pfParam f) `Set.member` pa) $
      compilerErrorM $ "Filtered param " ++ show (pfParam f) ++
                       " is not defined for function " ++ show n ++
                       formatFullContextBrace c
    getFilters fm2 n2 =
      case n2 `Map.lookup` fm2 of
           (Just fs) -> fs
           _ -> []

uncheckedSubFunction :: (Show c, CollectErrorsM m) =>
  ParamValues -> ScopedFunction c -> m (ScopedFunction c)
uncheckedSubFunction = unfixedSubFunction . fmap fixTypeParams

unfixedSubFunction :: (Show c, CollectErrorsM m) =>
  ParamValues -> ScopedFunction c -> m (ScopedFunction c)
unfixedSubFunction pa ff@(ScopedFunction c n t s v as rs ps fa ms) =
  "In function:\n---\n" ++ show ff ++ "\n---\n" ??> do
    let unresolved = Map.fromList $ map (\n2 -> (n2,singleType $ JustParamName False n2)) $ map vpParam $ pValues ps
    let pa' = pa `Map.union` unresolved
    as' <- fmap Positional $ mapCompilerM (subPassedNamed pa') $ pValues as
    rs' <- fmap Positional $ mapCompilerM (subPassed pa') $ pValues rs
    fa' <- mapCompilerM (subFilter pa') fa
    ms' <- mapCompilerM (uncheckedSubFunction pa) ms
    v' <- subVisibility pa' v
    return $ (ScopedFunction c n t s v' as' rs' ps fa' ms')
    where
      subPassedNamed pa2 (a,n2) = do
        a2 <- subPassed pa2 a
        return (a2,n2)
      subPassed pa2 (PassedValue c2 t2) = do
        t' <- uncheckedSubValueType (getValueForParam pa2) t2
        return $ PassedValue c2 t'
      subFilter pa2 (ParamFilter c2 n2 f) = do
        f' <- uncheckedSubFilter (getValueForParam pa2) f
        return $ ParamFilter c2 n2 f'
      subVisibility _ FunctionVisibilityDefault = return FunctionVisibilityDefault
      subVisibility pa2 (FunctionVisibility c2 ts) = do
        ts' <- mapCompilerM (subVisibilitySingle pa2) ts
        return $ FunctionVisibility c2 ts'
      subVisibilitySingle pa2 (c2,t2) = do
        t2' <- uncheckedSubInstance (getValueForParam pa2) t2
        return (c2,t2')

replaceSelfFunction :: (Show c, CollectErrorsM m) =>
  GeneralInstance -> ScopedFunction c -> m (ScopedFunction c)
replaceSelfFunction self ff@(ScopedFunction c n t s v as rs ps fa ms) =
  "In function:\n---\n" ++ show ff ++ "\n---\n" ??> do
    as' <- fmap Positional $ mapCompilerM subPassedNamed $ pValues as
    rs' <- fmap Positional $ mapCompilerM subPassed $ pValues rs
    fa' <- mapCompilerM subFilter fa
    ms' <- mapCompilerM (replaceSelfFunction self) ms
    v' <- subVisibility v
    return $ (ScopedFunction c n t s v' as' rs' ps fa' ms')
    where
      subPassedNamed (a,n2) = do
        a2 <- subPassed a
        return (a2,n2)
      subPassed (PassedValue c2 t2) = do
        t' <- replaceSelfValueType self t2
        return $ PassedValue c2 t'
      subFilter (ParamFilter c2 n2 f) = do
        f' <- replaceSelfFilter self f
        return $ ParamFilter c2 n2 f'
      subVisibility FunctionVisibilityDefault = return FunctionVisibilityDefault
      subVisibility (FunctionVisibility c2 ts) = do
        ts' <- mapCompilerM subVisibilitySingle ts
        return $ FunctionVisibility c2 ts'
      subVisibilitySingle (c2,t2) = do
        t2' <- replaceSelfInstance self t2
        return (c2,t2')

checkFunctionCallVisibility :: (Show c, CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ScopedFunction c -> GeneralInstance -> m ()
checkFunctionCallVisibility r fs f = check (sfVisibility f) where
  check FunctionVisibilityDefault _ = return ()
  check (FunctionVisibility _ ts) t0 = "Cannot call " ++ show (sfName f) ++ " in context of " ++ show t0 !!>
    collectFirstM_ (map (checkSingle t0) ts)
  checkSingle t0 (c,t) = "In visibility " ++ show t ++ formatFullContextBrace c ??>
    checkGeneralMatch r fs Covariant t0 t

data PatternMatch =
  TypePattern {
    tpVariance :: Variance,
    tpData :: ValueType,
    tpPattern :: ValueType
  } |
  DefinesPattern {
    dpData :: TypeInstance,
    dpPattern :: DefinesInstance
  }

instance Show PatternMatch where
  show (TypePattern Covariant     l r) = show l ++ " -> "  ++ show r
  show (TypePattern Contravariant l r) = show l ++ " <- "  ++ show r
  show (TypePattern Invariant     l r) = show l ++ " <-> " ++ show r
  show (DefinesPattern l r) = show l ++ " -> " ++ show r

inferParamTypes :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ParamValues -> [PatternMatch] ->
  m (MergeTree InferredTypeGuess)
inferParamTypes r f ps = fmap mergeAll . mapCompilerM single where
  single (TypePattern v t1 t2) = do
    t2' <- uncheckedSubValueType (getValueForParam ps) t2
    checkValueTypeMatch r f v t1 t2'
  single (DefinesPattern t1 (DefinesInstance n ps2)) = do
    ps3 <- trDefines r t1 n
    ps2' <- fmap Positional $ mapCompilerM (uncheckedSubInstance (getValueForParam ps)) $ pValues ps2
    checkDefinesMatch r f (DefinesInstance n ps3) (DefinesInstance n ps2')

data GuessRange a =
  GuessRange {
    grLower :: Maybe a,
    grUpper :: Maybe a
  }
  deriving (Eq,Ord)

instance Show a => Show (GuessRange a) where
  show (GuessRange Nothing   Nothing)   = "literally anything is possible"
  show (GuessRange Nothing   (Just hi)) = "something at or below " ++ show hi
  show (GuessRange (Just lo) Nothing)   = "something at or above " ++ show lo
  show (GuessRange (Just lo) (Just hi)) = "something between " ++ show lo ++ " and " ++ show hi

data GuessUnion =
  GuessUnion {
    guGuesses :: [GuessRange GeneralInstance]
  }

guessesFromFilters :: CollectErrorsM m =>
  ParamFilters -> ValueType -> ValueType -> m [PatternMatch]
guessesFromFilters fm (ValueType _ t1) (ValueType _ t2) = tryParam >>= fromFilters where
  tryParam = tryCompilerM $ matchOnlyLeaf t2
  fromFilters (Just (JustParamName _ n)) =
    case n `Map.lookup` fm of
         Just fs -> fmap concat $ mapCompilerM toGuess fs
         Nothing -> return []
  fromFilters _ = return []
  toGuess (TypeFilter FilterRequires t3) =
    return [TypePattern Covariant (ValueType RequiredValue t1) (ValueType RequiredValue t3)]
  toGuess (TypeFilter FilterAllows t3) =
    return [TypePattern Contravariant (ValueType RequiredValue t1) (ValueType RequiredValue t3)]
  toGuess (DefinesFilter t3) = do
    maybeInstance <- tryCompilerM $ matchOnlyLeaf t1
    case maybeInstance of
         Just (JustTypeInstance t) -> return [DefinesPattern t t3]
         _ -> return []
  toGuess _ = return []

mergeInferredTypes :: (CollectErrorsM m, TypeResolver r) =>
  r -> ParamFilters -> ParamFilters -> ParamValues -> MergeTree InferredTypeGuess -> m ParamValues
mergeInferredTypes _ _ ff _ _ | null (Map.toList ff) = return Map.empty
mergeInferredTypes r f ff ps gs0 = do
  let gs0' = mapTypeGuesses gs0
  gs1 <- mapCompilerM (\(i,is) -> fmap ((,) i) $ (reduce >=> simplifyUnion) is) $ Map.toList gs0'
  gs2 <- filterGuesses gs1
  takeBest gs2 where
    reduce is = fmap guGuesses $ reduceMergeTree anyOp allOp leafOp is
    leafOp (InferredTypeGuess _ t Covariant)     = return $ GuessUnion [GuessRange (Just t) Nothing]
    leafOp (InferredTypeGuess _ t Contravariant) = return $ GuessUnion [GuessRange Nothing  (Just t)]
    leafOp (InferredTypeGuess _ t _)             = return $ GuessUnion [GuessRange (Just t) (Just t)]
    anyOp = fmap (GuessUnion . concat . map guGuesses) . collectAllM
    allOp = collectAllM >=> prodAll
    prodAll [] = return $ GuessUnion []
    prodAll [GuessUnion gs] = return $ GuessUnion $ nub gs
    prodAll ((GuessUnion g1):(GuessUnion g2):gs) = do
      g <- g1 `guessProd` g2
      prodAll (GuessUnion g:gs)
    guessProd xs ys = fmap concat $ collectAllM $ do
      x <- nub xs
      y <- nub ys
      [x `guessIntersect` y]
    guessIntersect (GuessRange loX hiX) (GuessRange loY hiY) = do
      q1 <- loX `convertsTo` hiY
      q2 <- loY `convertsTo` hiX
      if q1 && q2
         then do
           loZ <- tryMerge Covariant     loX loY
           hiZ <- tryMerge Contravariant hiX hiY
           return [GuessRange loZ hiZ]
         else return []
    convertsTo Nothing _ = return True
    convertsTo _ Nothing = return True
    convertsTo (Just t1) (Just t2) = isCompilerSuccessM $ checkGeneralMatch r f Covariant t1 t2
    tryMerge _ Nothing t2 = return t2
    tryMerge _ t1 Nothing = return t1
    tryMerge v (Just t1) (Just t2) = collectFirstM [
        checkGeneralMatch r f v t1 t2 >> return (Just t2),
        checkGeneralMatch r f v t2 t1 >> return (Just t1),
        return $ case v of
                      Covariant     -> Just $ mergeAny [t1,t2]
                      Contravariant -> Just $ mergeAll [t1,t2]
                      _ -> undefined
      ]
    simplifyUnion [] = return []
    simplifyUnion (g:gs) = do
      ga <- tryRangeUnion [] g gs
      case ga of
           Just gs2 -> simplifyUnion gs2
           Nothing -> do
             gs2 <- simplifyUnion gs
             return (g:gs2)
    -- Returns Just a new list if there was a merge, and Nothing otherwise.
    tryRangeUnion ms g1@(GuessRange loX hiX) (g2@(GuessRange loY hiY):gs) = do
      l1 <- loX `convertsTo` loY
      l2 <- loY `convertsTo` loX
      let loZ = case (l1,l2) of
                     (True,_) -> Just loX
                     (_,True) -> Just loY
                     _ -> Nothing
      h1 <- hiX `convertsTo` hiY
      h2 <- hiY `convertsTo` hiX
      let hiZ = case (h1,h2) of
                     (True,_) -> Just hiY
                     (_,True) -> Just hiX
                     _ -> Nothing
      case (loZ,hiZ) of
           (Just lo,Just hi) -> return $ Just $ ms ++ [GuessRange lo hi] ++ gs
           _                 -> tryRangeUnion (ms ++ [g2]) g1 gs
    tryRangeUnion _ _ _ = return Nothing
    takeBest [gs] = return $ Map.fromList gs
    takeBest [] = compilerErrorM genericError
    takeBest gs = "Unable to merge alternative param guesses" !!> do
      mapCompilerM_ showAmbiguous (zip ([1..] :: [Int]) gs)
      emptyErrorM
    showAmbiguous (n,gs) = "Param guess set " ++ show n !!>
      (mapErrorsM $ map (\(i,t) -> "Guess for param " ++ show i ++ ": " ++ show t) gs)
    filterGuesses gs = do
      gs' <- mapCompilerM extractGuesses gs
      let mult = foldM (\xs ys -> [xs++[y] | y <- ys]) [] gs'
      let gs2 = map filterGuess mult
      collectFirstM_ gs2 <!! genericError
      collectAnyM gs2
    genericError = "No guesses available for params " ++ intercalate ", " (map show $ Map.keys ff)
    filterGuess gs = checkSubFilters gs >> return gs
    extractGuesses (i,is) = do
      let is2 = map (extractSingle i) is
      -- resetBackgroundM prevents duplicate messages.
      resetBackgroundM $ collectFirstM_ is2 <!! "No feasible guesses for param " ++ show i
      fmap nub $ collectAnyM is2
    extractSingle i (GuessRange (Just lo) Nothing) = return (i,lo)
    extractSingle i (GuessRange Nothing (Just hi)) = return (i,hi)
    extractSingle i g@(GuessRange (Just lo) (Just hi)) = do
      p <- (Just hi) `convertsTo` (Just lo)
      if p
         then return (i,lo)
         else do
           compilerBackgroundM $ "Arbitrarily using lower bound " ++ show lo ++
                                 " for " ++ show i ++ " (" ++ show g ++ ")"
           return (i,lo)
    extractSingle i g@(GuessRange Nothing Nothing) =
      compilerErrorM $ "Ambiguous guess for param " ++ show i ++ ": " ++ show g
    checkSubFilters gs = "In validation of inference guess: " ++ describeGuess gs ??> do
      let ps' = foldr (uncurry Map.insert) ps gs
      ff' <- uncheckedSubFilters (getValueForParam ps') ff
      mapCompilerM_ (validateSingleParam ff') gs
    validateSingleParam ff2 (i,t) = do
      fs <- ff2 `filterLookup` i
      validateAssignment r f t fs
    describeGuess = intercalate ", " . map (\(i,t) -> show i ++ " = " ++ show t)
