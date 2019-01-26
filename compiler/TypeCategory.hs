{-# LANGUAGE Safe #-}

module TypeCategory (
  AnyCategory(..),
  CategoryMap(..),
  FunctionName(..),
  ParamFilter(..),
  PassedValue(..),
  ScopedFunction(..),
  ValueDefine(..),
  ValueParam(..),
  ValueRefine(..),
  categoriesToTypeResolver,
  checkCategoryInstances,
  checkConnectedTypes,
  checkConnectionCycles,
  checkParamVariances,
  declareAllTypes, -- TODO: Remove?
  flattenAllConnections,
  formatFullContext,
  getCategory,
  getCategoryContext,
  getCategoryDefines,
  getCategoryFilterMap,
  getCategoryFilters,
  getCategoryFunctions,
  getCategoryName,
  getCategoryParams,
  getCategoryRefines,
  getConcreteCategory,
  getFunctionFilterMap,
  getInstanceCategory,
  getValueCategory,
  includeNewTypes,
  isInstanceInterface,
  isValueConcrete,
  isValueInterface,
  parsedToFunctionType,
  topoSortCategories,
  uncheckedSubFunction,
  validateCategoryFunction,
) where

import Control.Arrow (second)
import Control.Monad (when)
import Data.List (group,groupBy,intercalate,sort,sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Function
import TypeInstance
import TypesBase


data AnyCategory c =
  ValueInterface {
    viContext :: [c],
    viName :: CategoryName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c],
    viParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  } |
  InstanceInterface {
    iiContext :: [c],
    iiName :: CategoryName,
    iiParams :: [ValueParam c],
    iiParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  } |
  ValueConcrete {
    vcContext :: [c],
    vcName :: CategoryName,
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcDefines :: [ValueDefine c],
    vcParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  }

formatFullContext :: Show a => [a] -> String
formatFullContext cs = intercalate " -> " (map show cs)

instance Show c => Show (AnyCategory c) where
  show = format where
    format (ValueInterface cs n ps rs vs fs) =
      "@value interface " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "\n" ++
      (intercalate "\n\n" $
         map (\r -> "  " ++ formatRefine r) rs ++
         map (\v -> "  " ++ formatValue v) vs ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    format (InstanceInterface cs n ps vs fs) =
      "@type interface " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++
      (intercalate "\n\n" $
         map (\v -> "  " ++ formatValue v) vs ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    format (ValueConcrete cs n ps rs ds vs fs) =
      "concrete " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "\n" ++
      (intercalate "\n\n" $
         map (\r -> "  " ++ formatRefine r) rs ++
         map (\d -> "  " ++ formatDefine d) ds ++
         map (\v -> "  " ++ formatValue v) vs ++
         map (\f -> formatInterfaceFunc f) fs) ++
      "\n}\n"
    formatContext cs = "/*" ++ formatFullContext cs ++ "*/"
    formatParams ps = let (con,inv,cov) = (foldr partitionParam ([],[],[]) ps) in
      "<" ++ intercalate "," con ++ "|" ++
             intercalate "," inv ++ "|" ++
             intercalate "," cov ++ ">"
    -- NOTE: This assumes that the params are ordered by contravariant,
    -- invariant, and covariant.
    partitionParam p (con,inv,cov)
      | vpVariance p == Contravariant = ((show $ vpParam p):con,inv,cov)
      | vpVariance p == Invariant     = (con,(show $ vpParam p):inv,cov)
      | vpVariance p == Covariant     = (con,inv,(show $ vpParam p):cov)
    formatRefine r = "refines " ++ show (vrType r) ++ " " ++ formatContext (vrContext r)
    formatDefine d = "defines " ++ show (vdType d) ++ " " ++ formatContext (vdContext d)
    formatValue v = show (pfParam v) ++ " " ++ show (pfFilter v) ++
                    " " ++ formatContext (pfContext v)
    formatInterfaceFunc f = showFunctionInContext "" "  " f
    formatConcreteFunc f = showFunctionInContext (showScope (sfScope f) ++ " ") "  " f

showScope :: SymbolScope -> String
showScope CategoryScope = "@category"
showScope TypeScope     = "@type"
showScope ValueScope    = "@value"
showScope LocalScope    = "@local"

getCategoryName :: AnyCategory c -> CategoryName
getCategoryName (ValueInterface _ n _ _ _ _) = n
getCategoryName (InstanceInterface _ n _ _ _) = n
getCategoryName (ValueConcrete _ n _ _ _ _ _) = n

getCategoryContext :: AnyCategory c -> [c]
getCategoryContext (ValueInterface c _ _ _ _ _) = c
getCategoryContext (InstanceInterface c _ _ _ _) = c
getCategoryContext (ValueConcrete c _ _ _ _ _ _) = c

getCategoryParams :: AnyCategory c -> [ValueParam c]
getCategoryParams (ValueInterface _ _ ps _ _ _) = ps
getCategoryParams (InstanceInterface _ _ ps _ _) = ps
getCategoryParams (ValueConcrete _ _ ps _ _ _ _) = ps

getCategoryRefines :: AnyCategory c -> [ValueRefine c]
getCategoryRefines (ValueInterface _ _ _ rs _ _) = rs
getCategoryRefines (InstanceInterface _ _ _ _ _) = []
getCategoryRefines (ValueConcrete _ _ _ rs _ _ _) = rs

getCategoryDefines :: AnyCategory c -> [ValueDefine c]
getCategoryDefines (ValueInterface _ _ _ _ _ _) = []
getCategoryDefines (InstanceInterface _ _ _ _ _) = []
getCategoryDefines (ValueConcrete _ _ _ _ ds _ _) = ds

getCategoryFilters :: AnyCategory c -> [ParamFilter c]
getCategoryFilters (ValueInterface _ _ _ _ vs _) = vs
getCategoryFilters (InstanceInterface _ _ _ vs _) = vs
getCategoryFilters (ValueConcrete _ _ _ _ _ vs _) = vs

getCategoryFunctions :: AnyCategory c -> [ScopedFunction c]
getCategoryFunctions (ValueInterface _ _ _ _ _ fs) = fs
getCategoryFunctions (InstanceInterface _ _ _ _ fs) = fs
getCategoryFunctions (ValueConcrete _ _ _ _ _ _ fs) = fs

isValueInterface :: AnyCategory c -> Bool
isValueInterface (ValueInterface _ _ _ _ _ _) = True
isValueInterface _ = False

isInstanceInterface :: AnyCategory c -> Bool
isInstanceInterface (InstanceInterface _ _ _ _ _) = True
isInstanceInterface _ = False

isValueConcrete :: AnyCategory c -> Bool
isValueConcrete (ValueConcrete _ _ _ _ _ _ _) = True
isValueConcrete _ = False

data ValueRefine c =
  ValueRefine {
    vrContext :: [c],
    vrType :: TypeInstance
  }

instance Show c => Show (ValueRefine c) where
  show (ValueRefine c t) = show t ++ " [" ++ formatFullContext c ++ "]"

data ValueDefine c =
  ValueDefine {
    vdContext :: [c],
    vdType :: DefinesInstance
  }

instance Show c => Show (ValueDefine c) where
  show (ValueDefine c t) = show t ++ " [" ++ formatFullContext c ++ "]"

data ValueParam c =
  ValueParam {
    vpContext :: [c],
    vpParam :: ParamName,
    vpVariance :: Variance
  }

instance Show c => Show (ValueParam c) where
  show (ValueParam c t v) = show t ++ " (" ++ show v ++ ") [" ++ formatFullContext c ++ "]"

data ParamFilter c =
  ParamFilter {
    pfContext :: [c],
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }

instance Show c => Show (ParamFilter c) where
  show (ParamFilter c n f) = show n ++ " " ++ show f ++ " [" ++ formatFullContext c ++ "]"


type CategoryMap c = Map.Map CategoryName (AnyCategory c)

getCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getCategory tm (c,n) =
  case n `Map.lookup` tm of
       (Just t) -> return (c,t)
       _ -> compileError $ "Type " ++ show n ++ context ++ " not found"
  where
    context
      | null c = ""
      | otherwise = " [" ++ formatFullContext c ++ "]"

getValueCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getValueCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueInterface t || isValueConcrete t
     then return (c2,t)
     else compileError $ "Category " ++ show n ++
                         " cannot be used as a value [" ++
                         formatFullContext c ++ "]"

getInstanceCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getInstanceCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isInstanceInterface t
     then return (c2,t)
     else compileError $ "Category " ++ show n ++
                         " cannot be used as a type interface [" ++
                         formatFullContext c ++ "]"

getConcreteCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],CategoryName) -> m ([c],AnyCategory c)
getConcreteCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueConcrete t
     then return (c2,t)
     else compileError $ "Category " ++ show n ++
                         " cannot be used as concrete [" ++
                         formatFullContext c ++ "]"

includeNewTypes :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
includeNewTypes tm0 ts = do
  checkConnectionCycles ts
  checkConnectedTypes tm0 ts
  checkParamVariances tm0 ts
  ts <- topoSortCategories tm0 ts
  ts <- flattenAllConnections tm0 ts
  checkCategoryInstances tm0 ts
  declareAllTypes tm0 ts

declareAllTypes :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
declareAllTypes tm0 = foldr (\t tm -> tm >>= update t) (return tm0) where
  update t tm =
    case getCategoryName t `Map.lookup` tm of
        (Just t2) -> compileError $ "Type " ++ show (getCategoryName t) ++
                                    " [" ++ formatFullContext (getCategoryContext t) ++
                                    "] has already been declared [" ++
                                    showExisting t2 ++ "]"
        _ -> return $ Map.insert (getCategoryName t) t tm
  showExisting t
    | isBuiltinCategory (getCategoryName t) = "builtin type"
    | otherwise = formatFullContext (getCategoryContext t)

getCategoryFilterMap :: AnyCategory c -> ParamFilters
getCategoryFilterMap t = getFilters $ zip (Set.toList pa) (repeat []) where
  pa = Set.fromList $ map vpParam $ getCategoryParams t
  getFilters ps = let fs = map (\f -> (pfParam f,pfFilter f)) (getCategoryFilters t) in
                      Map.fromListWith (++) $ map (second (:[])) fs ++ ps

-- TODO: Use this where it's needed in this file.
getFunctionFilterMap :: ScopedFunction c -> ParamFilters
getFunctionFilterMap f = getFilters $ zip (Set.toList pa) (repeat []) where
  pa = Set.fromList $ map vpParam $ psParams $ sfParams f
  getFilters ps = let fs = map (\f -> (pfParam f,pfFilter f)) (sfFilters f) in
                      Map.fromListWith (++) $ map (second (:[])) fs ++ ps

checkConnectedTypes :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectedTypes tm0 ts = do
  tm <- declareAllTypes tm0 ts
  mergeAllM (map (checkSingle tm) ts)
  where
    checkSingle tm (ValueInterface c n _ rs _ _) = do
      let ts = map (\r -> (vrContext r,tiName $ vrType r)) rs
      is <- collectAllOrErrorM $ map (getCategory tm) ts
      mergeAllM (map (valueRefinesInstanceError c n) is)
      mergeAllM (map (valueRefinesConcreteError c n) is)
    checkSingle tm (ValueConcrete c n _ rs ds _ _) = do
      let ts1 = map (\r -> (vrContext r,tiName $ vrType r)) rs
      let ts2 = map (\d -> (vdContext d,diName $ vdType d)) ds
      is1 <- collectAllOrErrorM $ map (getCategory tm) ts1
      is2 <- collectAllOrErrorM $ map (getCategory tm) ts2
      mergeAllM (map (concreteRefinesInstanceError c n) is1)
      mergeAllM (map (concreteDefinesValueError c n) is2)
      mergeAllM (map (concreteRefinesConcreteError c n) is1)
      mergeAllM (map (concreteDefinesConcreteError c n) is2)
    checkSingle _ _ = return ()
    valueRefinesInstanceError c n (c2,t)
      | isInstanceInterface t =
        compileError $ "Value interface " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                      " cannot refine type interface " ++
                      show (iiName t) ++ " [" ++ formatFullContext c2 ++ "]"
      | otherwise = return ()
    valueRefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compileError $ "Value interface " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                      " cannot refine concrete type " ++
                      show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]"
      | otherwise = return ()
    concreteRefinesInstanceError c n (c2,t)
      | isInstanceInterface t =
        compileError $ "Concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                      " cannot refine instance interface " ++
                      show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]" ++
                      " => use defines instead"
      | otherwise = return ()
    concreteDefinesValueError c n (c2,t)
      | isValueInterface t =
        compileError $ "Concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                      " cannot define value interface " ++
                      show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]" ++
                      " => use refines instead"
      | otherwise = return ()
    concreteRefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compileError $ "Concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                      " cannot refine concrete type " ++
                      show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]"
      | otherwise = return ()
    concreteDefinesConcreteError c n (c2,t)
      | isValueConcrete t =
        compileError $ "Concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                      " cannot define concrete type " ++
                      show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]"
      | otherwise = return ()

checkConnectionCycles :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  [AnyCategory c] -> m ()
checkConnectionCycles ts = mergeAllM (map (checker []) ts) where
  tm = Map.fromList $ zip (map getCategoryName ts) ts
  checker us (ValueInterface c n _ rs _ _) = do
    failIfCycle n c us
    let ts = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getValueCategory tm) ts
    mergeAllM (map (checker (us ++ [n]) . snd) is)
  checker us (ValueConcrete c n _ rs _ _ _) = do
    failIfCycle n c us
    let ts = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getValueCategory tm) ts
    mergeAllM (map (checker (us ++ [n]) . snd) is)
  checker _ _ = return ()
  failIfCycle n c us =
    when (n `Set.member` (Set.fromList us)) $
      compileError $ "Category " ++ show n ++ " [" ++
                     formatFullContext c ++ "] refers back to itself: " ++
                     intercalate " -> " (map show (us ++ [n]))

checkParamVariances :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkParamVariances tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = categoriesToTypeResolver tm
  mergeAllM (map (checkCategory r) ts)
  where
    checkCategory r (ValueInterface c n ps rs _ _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAllM (map (checkRefine r vm) rs)
    checkCategory r (ValueConcrete c n ps rs ds _ _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAllM (map (checkRefine r vm) rs)
      mergeAllM (map (checkDefine r vm) ds)
    checkCategory _ (InstanceInterface c n ps _ _) = noDuplicates c n ps
    noDuplicates c n ps = mergeAllM (map checkCount $ group $ sort $ map vpParam ps) where
      checkCount xa@(x:_:_) =
        compileError $ "Param " ++ show x ++ " occurs " ++ show (length xa) ++
                      " times in " ++ show n ++ " [" ++ formatFullContext c ++ "]"
      checkCount _ = return ()
    checkRefine r vm (ValueRefine c t) =
      validateInstanceVariance r vm Covariant (SingleType $ JustTypeInstance t) `reviseError`
        (show t ++ " [" ++ formatFullContext c ++ "]")
    checkDefine r vm (ValueDefine c t) =
      validateDefinesVariance r vm Covariant t `reviseError`
        (show t ++ " [" ++ formatFullContext c ++ "]")

checkCategoryInstances :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkCategoryInstances tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = categoriesToTypeResolver tm
  mergeAllM $ map (checkSingle r) ts
  where
    checkSingle r t = do
      let pa = Set.fromList $ map vpParam $ getCategoryParams t
      let fm = getCategoryFilterMap t
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
      mergeAllM $ map (checkFilterParam pa) (getCategoryFilters t)
      mergeAllM $ map (checkRefine r fm) (getCategoryRefines t)
      mergeAllM $ map (checkDefine r fm) (getCategoryDefines t)
      mergeAllM $ map (checkFilter r fm) (getCategoryFilters t)
      mergeAllM $ map (validateCategoryFunction r t) (getCategoryFunctions t)
    checkFilterParam pa (ParamFilter c n _) =
      when (not $ n `Set.member` pa) $
        compileError $ "Param " ++ show n ++ " [" ++ formatFullContext c ++ "] does not exist"
    checkRefine r fm (ValueRefine c t) =
      validateTypeInstance r fm t `reviseError`
        (show t ++ " [" ++ formatFullContext c ++ "]")
    checkDefine r fm (ValueDefine c t) =
      validateDefinesInstance r fm t `reviseError`
        (show t ++ " [" ++ formatFullContext c ++ "]")
    checkFilter r fm (ParamFilter c n f) =
      validateTypeFilter r fm f `reviseError`
        (show n ++ " " ++ show f ++ " [" ++ formatFullContext c ++ "]")

validateCategoryFunction :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m -> AnyCategory c -> ScopedFunction c -> m ()
validateCategoryFunction r t f = do
  let fm = getCategoryFilterMap t
  let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
  flip reviseError ("In function:\n---\n" ++ show f ++ "\n---\n") $ do
    funcType <- parsedToFunctionType f
    if sfScope f == CategoryScope
        then validatateFunctionType r Map.empty vm funcType
        else validatateFunctionType r fm vm funcType

topoSortCategories :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
topoSortCategories tm0 ts = do
  tm <- declareAllTypes tm0 ts
  (ts',_) <- foldr (update tm) (return ([],Map.keysSet tm0)) ts
  return ts'
  where
    update tm t u = do
      (ts,ta) <- u
      if getCategoryName t `Set.member` ta
         then return (ts,ta)
         else do
          refines <- collectAllOrErrorM $
                    map (\r -> getCategory tm (vrContext r,tiName $ vrType r)) $ getCategoryRefines t
          defines <- collectAllOrErrorM $
                    map (\d -> getCategory tm (vdContext d,diName $ vdType d)) $ getCategoryDefines t
          (ts',ta') <- foldr (update tm) u (map snd $ refines ++ defines)
          let ts'' = ts' ++ [t]
          let ta'' = Set.insert (getCategoryName t) ta'
          return (ts'',ta'')

mergeObjects :: (MergeableM m, CompileErrorM m, Monad m) =>
  (a -> a -> m ()) -> [a] -> m [a]
mergeObjects f = return . merge [] where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    -- TODO: Should f just perform merging? In case we want to preserve info
    -- about what was merged, e.g., return m [(p,a)].
    checker x2 = f x2 x
    ys = if isCompileError $ mergeAnyM (map checker (cs ++ xs))
            then [x] -- x is not redundant => keep.
            else []  -- x is redundant => remove.

mergeRefines :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m -> ParamFilters -> [ValueRefine c] -> m [ValueRefine c]
mergeRefines r f = mergeObjects check where
  check (ValueRefine _ t1@(TypeInstance n1 _)) (ValueRefine _ t2@(TypeInstance n2 _))
    | n1 /= n2 = compileError $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise = do
      checkGeneralMatch r f Covariant (SingleType $ JustTypeInstance $ t1)
                                      (SingleType $ JustTypeInstance $ t2)
      return ()

flattenAllConnections :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
flattenAllConnections tm0 ts = do
  -- We need to process all refines before type-checking can be done.
  tm1 <- foldr preMerge (return tm0) (reverse ts)
  let r = categoriesToTypeResolver tm1
  (ts',_) <- foldr (update r) (return ([],tm0)) (reverse ts)
  return ts'
  where
    preMerge t u = do
      tm <- u
      t' <- preMergeSingle tm t
      return $ Map.insert (getCategoryName t') t' tm
    preMergeSingle tm t@(ValueInterface c n ps rs vs fs) = do
      rs' <- fmap concat $ collectAllOrErrorM $ map (getRefines tm) rs
      return $ ValueInterface c n ps rs' vs fs
    preMergeSingle _ t = return t
    update r t u = do
      (ts,tm) <- u
      t' <- updateSingle r tm t `reviseError`
              ("In category " ++ show (getCategoryName t) ++ " [" ++
               formatFullContext (getCategoryContext t) ++ "]")
      return (ts ++ [t'],Map.insert (getCategoryName t') t' tm)
    updateSingle r tm t@(ValueInterface c n ps rs vs fs) = do
      noDuplicateRefines c n rs
      let fm = getCategoryFilterMap t
      rs' <- fmap concat $ collectAllOrErrorM $ map (getRefines tm) rs
      rs'' <- mergeRefines r fm rs'
      noDuplicateRefines c n rs''
      checkMerged r fm rs rs''
      -- Only merge from direct parents.
      fs' <- mergeFuncs r tm fm rs [] fs
      return $ ValueInterface c n ps rs'' vs fs'
    -- TODO: Remove duplication below and/or have separate tests.
    updateSingle r tm t@(ValueConcrete c n ps rs ds vs fs) = do
      noDuplicateRefines c n rs
      noDuplicateDefines c n ds
      let fm = getCategoryFilterMap t
      rs' <- fmap concat $ collectAllOrErrorM $ map (getRefines tm) rs
      rs'' <- mergeRefines r fm rs'
      noDuplicateRefines c n rs''
      checkMerged r fm rs rs''
      -- Only merge from direct parents.
      fs' <- mergeFuncs r tm fm rs ds fs
      return $ ValueConcrete c n ps rs'' ds vs fs'
    updateSingle _ _ t = return t
    noDuplicateRefines c n rs = do
      let names = map (\r -> (tiName $ vrType r,r)) rs
      noDuplicates c n names
    noDuplicateDefines c n ds = do
      let names = map (\d -> (diName $ vdType d,d)) ds
      noDuplicates c n names
    noDuplicates c n ns =
      mergeAllM $ map checkCount $ groupBy (\x y -> fst x == fst y) $
                                  sortBy (\x y -> fst x `compare` fst y) ns where
        checkCount xa@(x:_:_) =
          compileError $ "Category " ++ show (fst x) ++ " occurs " ++ show (length xa) ++
                         " times in " ++ show n ++ " [" ++ formatFullContext c ++ "] :\n---\n" ++
                         intercalate "\n---\n" (map (show . snd) xa)
        checkCount _ = return ()
    getRefines tm ra@(ValueRefine c t@(TypeInstance n ps)) = do
      (_,v) <- getValueCategory tm (c,n)
      let refines = getCategoryRefines v
      pa <- assignParams tm c t
      fmap (ra:) $ collectAllOrErrorM $ map (subAll c pa) refines
    subAll c pa (ValueRefine c1 t1) = do
      (SingleType (JustTypeInstance t2)) <-
        uncheckedSubInstance (getValueForParam pa) (SingleType (JustTypeInstance t1))
      return $ ValueRefine (c ++ c1) t2
    assignParams tm c (TypeInstance n ps) = do
      (_,v) <- getValueCategory tm (c,n)
      let ns = map vpParam $ getCategoryParams v
      paired <- processParamPairs alwaysPairParams (ParamSet ns) ps
      return $ Map.fromList paired
    checkMerged r fm rs rs2 = do
      let rm = Map.fromList $ map (\t -> (tiName $ vrType t,t)) rs
      mergeAllM $ map (\t -> checkConvert r fm (tiName (vrType t) `Map.lookup` rm) t) rs2
    checkConvert r fm (Just ta1@(ValueRefine _ t1)) ta2@(ValueRefine _ t2) = do
      checkGeneralMatch r fm Covariant (SingleType $ JustTypeInstance t1)
                                       (SingleType $ JustTypeInstance t2) `reviseError`
        ("Cannot refine " ++ show ta1 ++ " from inherited " ++ show ta2)
      return ()
    checkConvert _ _ _ _ = return ()
    mergeFuncs r tm fm rs ds fs = do
      inheritValue <- fmap concat $ collectAllOrErrorM $ map (getRefinesFuncs tm) rs
      inheritType  <- fmap concat $ collectAllOrErrorM $ map (getDefinesFuncs tm) ds
      let inheritByName  = Map.fromListWith (++) $ map (\f -> (sfName f,[f])) $ inheritValue ++ inheritType
      let explicitByName = Map.fromListWith (++) $ map (\f -> (sfName f,[f])) fs
      let allNames = Set.toList $ Set.union (Map.keysSet inheritByName) (Map.keysSet explicitByName)
      collectAllOrErrorM $ map (mergeByName r fm inheritByName explicitByName) allNames
    getRefinesFuncs tm ra@(ValueRefine c (TypeInstance n ts)) = flip reviseError (show ra) $ do
      (_,t) <- getValueCategory tm (c,n)
      let ps = map vpParam $ getCategoryParams t
      let fs = getCategoryFunctions t
      paired <- processParamPairs alwaysPairParams (ParamSet ps) ts
      let assigned = Map.fromList paired
      collectAllOrErrorM (map (uncheckedSubFunction assigned) fs)
    getDefinesFuncs tm da@(ValueDefine c (DefinesInstance n ts)) = flip reviseError (show da) $  do
      (_,t) <- getInstanceCategory tm (c,n)
      let ps = map vpParam $ getCategoryParams t
      let fs = getCategoryFunctions t
      paired <- processParamPairs alwaysPairParams (ParamSet ps) ts
      let assigned = Map.fromList paired
      collectAllOrErrorM (map (uncheckedSubFunction assigned) fs)
    mergeByName r fm im em n =
      tryMerge r fm n (n `Map.lookup` im) (n `Map.lookup` em)
    -- Inherited without an override.
    tryMerge _ _ n (Just is) Nothing
      | length is == 1 = return $ head is
      | otherwise = compileError $ "Function " ++ show n ++ " is inherited " ++
                                    show (length is) ++ " times:\n---\n" ++
                                    intercalate "\n---\n" (map show is)
    -- Not inherited.
    tryMerge r fm n Nothing es = tryMerge r fm n (Just []) es
    -- Explicit override, possibly inherited.
    tryMerge r fm n (Just is) (Just es)
      | length es /= 1 = compileError $ "Function " ++ show n ++ " is declared " ++
                                        show (length es) ++ " times:\n---\n" ++
                                        intercalate "\n---\n" (map show es)
      | otherwise = do
        let ff@(ScopedFunction c n t s as rs ps fa ms) = head es
        mergeAllM $ map (checkMerge r fm ff) is
        return $ ScopedFunction c n t s as rs ps fa (ms ++ is)
        where
          checkMerge r fm f1 f2
            | sfScope f1 /= sfScope f2 =
              compileError $ "Cannot merge " ++ showScope (sfScope f2) ++ " with " ++
                             showScope (sfScope f1) ++ " in function merge:\n---\n" ++
                             show f2 ++ "\n  ->\n" ++ show f1
            | otherwise =
              flip reviseError ("In function merge:\n---\n" ++ show f2 ++
                                "\n  ->\n" ++ show f1 ++ "\n---\n") $ do
                f1' <- parsedToFunctionType f1
                f2' <- parsedToFunctionType f2
                checkFunctionConvert r fm f2' f1'

categoriesToTypeResolver :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> TypeResolver m
categoriesToTypeResolver tm =
  TypeResolver {
    trRefines = refines,
    trDefines = defines,
    trVariance = variance,
    trTypeFilters = typeFilters,
    trDefinesFilters = definesFilters,
    trConcrete = concrete
  } where
    refines (TypeInstance n1 ps1) n2
      | n1 == n2 = do
        (_,t) <- getValueCategory tm ([],n1)
        processParamPairs alwaysPairParams (ParamSet $ map vpParam $ getCategoryParams t) ps1
        return ps1
      | otherwise = do
        (_,t) <- getValueCategory tm ([],n1)
        let params = map vpParam $ getCategoryParams t
        assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams (ParamSet params) ps1
        let pa = Map.fromList $ map (\r -> (tiName r,tiParams r)) $ map vrType $ getCategoryRefines t
        ps2 <- case n2 `Map.lookup` pa of
                    (Just x) -> return x
                    _ -> compileError $ "Category " ++ show n1 ++ " does not refine " ++ show n2
        fmap ParamSet $ collectAllOrErrorM $ map (subAllParams assigned) $ psParams ps2
    defines (TypeInstance n1 ps1) n2 = do
      (_,t) <- getValueCategory tm ([],n1)
      let params = map vpParam $ getCategoryParams t
      assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams (ParamSet params) ps1
      let pa = Map.fromList $ map (\r -> (diName r,diParams r)) $ map vdType $ getCategoryDefines t
      ps2 <- case n2 `Map.lookup` pa of
                  (Just x) -> return x
                  _ -> compileError $ "Category " ++ show n1 ++ " does not define " ++ show n2
      fmap ParamSet $ collectAllOrErrorM $ map (subAllParams assigned) $ psParams ps2
    variance n = do
      (_,t) <- getCategory tm ([],n)
      return $ ParamSet $ map vpVariance $ getCategoryParams t
    typeFilters (TypeInstance n ps) = do
      (_,t) <- getValueCategory tm ([],n)
      checkFilters t ps
    definesFilters (DefinesInstance n ps) = do
      (_,t) <- getInstanceCategory tm ([],n)
      checkFilters t ps
    checkFilters t ps = do
      let params = map vpParam $ getCategoryParams t
      assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams (ParamSet params) ps
      fs <- collectAllOrErrorM $ map (subSingleFilter assigned . \f -> (pfParam f,pfFilter f))
                                     (getCategoryFilters t)
      let fa = Map.fromListWith (++) $ map (second (:[])) fs
      fmap ParamSet $ collectAllOrErrorM $ map (assignFilter fa) params
    subAllParams pa = uncheckedSubInstance (getValueForParam pa)
    subSingleFilter pa (n,(TypeFilter v t)) = do
      (SingleType t2) <- uncheckedSubInstance (getValueForParam pa) (SingleType t)
      return (n,(TypeFilter v t2))
    subSingleFilter pa (n,(DefinesFilter (DefinesInstance n2 ps))) = do
      ps2 <- collectAllOrErrorM $ map (uncheckedSubInstance $ getValueForParam pa) (psParams ps)
      return (n,(DefinesFilter (DefinesInstance n2 (ParamSet ps2))))
    assignFilter fa n =
      case n `Map.lookup` fa of
           (Just x) -> return x
           _ -> return []
    concrete n = do
      (_,t) <- getCategory tm ([],n)
      return (isValueConcrete t)

data FunctionName =
  FunctionName {
    fnName :: String
  } |
  BuiltinPresent |
  BuiltinReduce |
  BuiltinRequire |
  BuiltinStrong
  deriving (Eq,Ord)

instance Show FunctionName where
  show (FunctionName n) = n
  show BuiltinPresent = "present"
  show BuiltinReduce = "reduce"
  show BuiltinRequire = "require"
  show BuiltinStrong = "strong"

data ScopedFunction c =
  ScopedFunction {
    sfContext :: [c],
    sfName :: FunctionName,
    sfType :: CategoryName,
    sfScope :: SymbolScope,
    sfArgs :: ParamSet (PassedValue c),
    sfReturns :: ParamSet (PassedValue c),
    sfParams :: ParamSet (ValueParam c),
    sfFilters :: [ParamFilter c],
    sfMerges :: [ScopedFunction c]
  }

instance Show c => Show (ScopedFunction c) where
  show f = showFunctionInContext (showScope (sfScope f) ++ " ") "" f

showFunctionInContext :: Show c => String -> String -> ScopedFunction c -> String
showFunctionInContext s indent (ScopedFunction cs n t _ as rs ps fa ms) =
  indent ++ s ++ "/*" ++ show t ++ "*/ " ++ show n ++
  showParams (psParams ps) ++ " " ++ formatContext cs ++ "\n" ++
  concat (map (\v -> indent ++ formatValue v ++ "\n") fa) ++
  indent ++ "(" ++ intercalate "," (map (show . pvType) $ psParams as) ++ ") -> " ++
  "(" ++ intercalate "," (map (show . pvType) $ psParams rs) ++ ")" ++ showMerges (flatten ms)
  where
    showParams [] = ""
    showParams ps = "<" ++ intercalate "," (map (show . vpParam) ps) ++ ">"
    formatContext cs = "/*" ++ formatFullContext cs ++ "*/"
    formatValue v = "  " ++ show (pfParam v) ++ " " ++ show (pfFilter v) ++
                    " " ++ formatContext (pfContext v)
    flatten [] = Set.empty
    flatten ms = Set.unions $ (Set.fromList $ map sfType ms):(map (flatten . sfMerges) ms)
    showMerges ms
      | null (Set.toList ms) = " /*not merged*/"
      | otherwise = " /*merged from: " ++ intercalate ", " (map show $ Set.toList ms) ++ "*/"

data PassedValue c =
  PassedValue {
    pvContext :: [c],
    pvType :: ValueType
  }

instance Show c => Show (PassedValue c) where
  show (PassedValue c t) = show t ++ " [" ++ formatFullContext c ++ "]"

parsedToFunctionType :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  ScopedFunction c -> m FunctionType
parsedToFunctionType (ScopedFunction c n t _ as rs ps fa _) = do
  let as' = ParamSet $ map pvType $ psParams as
  let rs' = ParamSet $ map pvType $ psParams rs
  let ps' = ParamSet $ map vpParam $ psParams ps
  mergeAllM $ map checkFilter fa
  let fm = Map.fromListWith (++) $ map (\f -> (pfParam f,[pfFilter f])) fa
  let fa' = ParamSet $ map (getFilters fm) $ psParams ps'
  return $ FunctionType as' rs' ps' fa'
  where
    pa = Set.fromList $ map vpParam $ psParams ps
    checkFilter f =
      when (not $ (pfParam f) `Set.member` pa) $
      compileError $ "Filtered param " ++ show (pfParam f) ++
                     " is not defined for function " ++ show n ++
                     " [" ++ formatFullContext c ++ "]"
    getFilters fm n =
      case n `Map.lookup` fm of
           (Just fs) -> fs
           _ -> []

uncheckedSubFunction :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  Map.Map ParamName GeneralInstance -> ScopedFunction c -> m (ScopedFunction c)
uncheckedSubFunction pa ff@(ScopedFunction c n t s as rs ps fa ms) =
  flip reviseError ("In function:\n---\n" ++ show ff ++ "\n---\n") $ do
    let fixed = Map.fromList $ map (\n -> (n,SingleType $ JustParamName n)) $ map vpParam $ psParams ps
    let pa' = Map.union pa fixed
    as' <- fmap ParamSet $ collectAllOrErrorM $ map (subPassed pa') $ psParams as
    rs' <- fmap ParamSet $ collectAllOrErrorM $ map (subPassed pa') $ psParams rs
    fa' <- collectAllOrErrorM $ map (subFilter pa') fa
    ms' <- collectAllOrErrorM $ map (uncheckedSubFunction pa) ms
    return $ (ScopedFunction c n t s as' rs' ps fa' ms')
    where
      subPassed pa (PassedValue c t) = do
        t' <- uncheckedSubValueType (getValueForParam pa) t
        return $ PassedValue c t'
      subFilter pa (ParamFilter c n f) = do
        f' <- uncheckedSubFilter (getValueForParam pa) f
        return $ ParamFilter c n f'
