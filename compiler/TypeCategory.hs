{-# LANGUAGE Safe #-}

module TypeCategory (
  AnyCategory(..),
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
  checkInstanceDuplicates,
  checkParamVariances,
  declareAllTypes, -- TODO: Remove?
  flattenAllConnections,
  getCategoryContext,
  getCategoryDefines,
  getCategoryFilters,
  getCategoryName,
  getCategoryParams,
  getCategoryRefines,
  includeNewTypes,
  isInstanceInterface,
  isValueConcrete,
  isValueInterface,
  mergeCategoryFunctions,
  mergeCategoryInstances,
  parsedToFunctionType,
) where

import Control.Arrow (second)
import Control.Monad (when)
import Data.List (group,groupBy,intercalate,sort,sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Function
import TypeInstance
import TypesBase


newtype FunctionName =
  FunctionName {
    fnName :: String
  }
  deriving (Eq,Ord)

data AnyCategory c =
  ValueInterface {
    viContext :: [c],
    viName :: TypeName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c],
    viParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  } |
  InstanceInterface {
    iiContext :: [c],
    iiName :: TypeName,
    iiParams :: [ValueParam c],
    iiParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  } |
  ValueConcrete {
    vcContext :: [c],
    vcName :: TypeName,
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcDefines :: [ValueDefine c],
    vcParamFilter :: [ParamFilter c],
    viFunctions :: [ScopedFunction c]
  }
  deriving (Eq)

formatFullContext :: Show a => [a] -> String
formatFullContext cs = intercalate " -> " (map show cs)

instance Show c => Show (AnyCategory c) where
  show = format where
    format (ValueInterface cs n ps rs vs fs) =
      "@value interface " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "\n" ++
      concat (map (\r -> "  " ++ formatRefine r ++ "\n") rs) ++
      concat (map (\v -> "  " ++ formatValue v ++ "\n") vs) ++
      concat (map (\f -> "\n" ++ formatInterfaceFunc f ++ "\n") fs) ++
      "}\n"
    format (InstanceInterface cs n ps vs fs) =
      "@type interface " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++
      concat (map (\v -> "  " ++ formatValue v ++ "\n") vs) ++
      concat (map (\f -> "\n" ++ formatInterfaceFunc f ++ "\n") fs) ++
      "}\n"
    format (ValueConcrete cs n ps rs ds vs fs) =
      "concrete " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "\n" ++
      concat (map (\r -> "  " ++ formatRefine r ++ "\n") rs) ++
      concat (map (\d -> "  " ++ formatDefine d ++ "\n") ds) ++
      concat (map (\v -> "  " ++ formatValue v ++ "\n") vs) ++
      concat (map (\f -> "\n" ++ formatConcreteFunc f ++ "\n") fs) ++
      "}\n"
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
    formatConcreteFunc f = showFunctionInContext (showScope $ sfScope f) "  " f
    showScope CategoryScope = "@category "
    showScope TypeScope     = "@type "
    showScope ValueScope    = "@value "

getCategoryName :: AnyCategory c -> TypeName
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
  deriving (Eq)

instance Show c => Show (ValueRefine c) where
  show (ValueRefine c t) = show t ++ " [" ++ formatFullContext c ++ "]"

data ValueDefine c =
  ValueDefine {
    vdContext :: [c],
    vdType :: DefinesInstance
  }
  deriving (Eq)

instance Show c => Show (ValueDefine c) where
  show (ValueDefine c t) = show t ++ " [" ++ formatFullContext c ++ "]"

data ValueParam c =
  ValueParam {
    vpContext :: [c],
    vpParam :: ParamName,
    vpVariance :: Variance
  }
  deriving (Eq)

instance Show c => Show (ValueParam c) where
  show (ValueParam c t v) = show t ++ " (" ++ show v ++ ") [" ++ formatFullContext c ++ "]"

data ParamFilter c =
  ParamFilter {
    pfContext :: [c],
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }
  deriving (Eq)

instance Show c => Show (ParamFilter c) where
  show (ParamFilter c n f) = show n ++ " " ++ show f ++ " [" ++ formatFullContext c ++ "]"


type CategoryMap c = Map.Map TypeName (AnyCategory c)

getCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],TypeName) -> m ([c],AnyCategory c)
getCategory tm (c,n) =
  case n `Map.lookup` tm of
       (Just t) -> return (c,t)
       _ -> compileError $ "Type " ++ show n ++ context ++ " not found"
  where
    context
      | null c = ""
      | otherwise = " [" ++ formatFullContext c ++ "]"

getValueCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],TypeName) -> m ([c],AnyCategory c)
getValueCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueInterface t || isValueConcrete t
     then return (c2,t)
     else compileError $ "Category " ++ show n ++
                         " cannot be used as a value [" ++
                         formatFullContext c ++ "]"

getInstanceCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],TypeName) -> m ([c],AnyCategory c)
getInstanceCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isInstanceInterface t
     then return (c2,t)
     else compileError $ "Category " ++ show n ++
                         " cannot be used as a type interface [" ++
                         formatFullContext c ++ "]"


includeNewTypes :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
includeNewTypes tm0 ts = do
  checkConnectionCycles ts
  checkConnectedTypes tm0 ts
  checkParamVariances tm0 ts
  ts2 <- flattenAllConnections tm0 ts
  checkCategoryInstances tm0 ts2
  -- TODO: Will also need to merge/check functions once they are available.
  ts3 <- mergeCategoryInstances tm0 ts2
  checkInstanceDuplicates ts3
  ts4 <- mergeCategoryFunctions tm0 ts3
  declareAllTypes tm0 ts4

declareAllTypes :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
declareAllTypes tm0 = foldr (\t tm -> tm >>= update t) (return tm0) where
  update t tm =
    case getCategoryName t `Map.lookup` tm of
        (Just t2) -> compileError $ "Type " ++ show (getCategoryName t) ++
                                    " [" ++ formatFullContext (getCategoryContext t) ++
                                    "] has already been declared [" ++
                                    formatFullContext (getCategoryContext t2) ++ "]"
        _ -> return $ Map.insert (getCategoryName t) t tm

checkConnectedTypes :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectedTypes tm0 ts = do
  tm <- declareAllTypes tm0 ts
  mergeAll (map (checkSingle tm) ts)
  where
    checkSingle tm (ValueInterface c n _ rs _ _) = do
      let ts = map (\r -> (vrContext r,tiName $ vrType r)) rs
      is <- collectAllOrErrorM $ map (getCategory tm) ts
      mergeAll (map (valueRefinesInstanceError c n) is)
      mergeAll (map (valueRefinesConcreteError c n) is)
    checkSingle tm (ValueConcrete c n _ rs ds _ _) = do
      let ts1 = map (\r -> (vrContext r,tiName $ vrType r)) rs
      let ts2 = map (\d -> (vdContext d,diName $ vdType d)) ds
      is1 <- collectAllOrErrorM $ map (getCategory tm) ts1
      is2 <- collectAllOrErrorM $ map (getCategory tm) ts2
      mergeAll (map (concreteRefinesInstanceError c n) is1)
      mergeAll (map (concreteDefinesValueError c n) is2)
      mergeAll (map (concreteRefinesConcreteError c n) is1)
      mergeAll (map (concreteDefinesConcreteError c n) is2)
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
checkConnectionCycles ts = mergeAll (map (checker []) ts) where
  tm = Map.fromList $ zip (map getCategoryName ts) ts
  checker us (ValueInterface c n _ rs _ _) = do
    failIfCycle n c us
    let ts = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getValueCategory tm) ts
    mergeAll (map (checker (us ++ [n]) . snd) is)
  checker us (ValueConcrete c n _ rs _ _ _) = do
    failIfCycle n c us
    let ts = map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getValueCategory tm) ts
    mergeAll (map (checker (us ++ [n]) . snd) is)
  checker _ _ = return ()
  failIfCycle n c us =
    when (n `Set.member` (Set.fromList us)) $
      compileError $ "Category " ++ show n ++ " [" ++
                     formatFullContext c ++ "] refers back to itself: " ++
                     intercalate " -> " (map show (us ++ [n]))

flattenAllConnections :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
flattenAllConnections tm0 ts = do
  tm <- declareAllTypes tm0 ts
  collectAllOrErrorM $ map (updateSingle tm) ts
  where
    -- TODO: Also collect inherited functions.
    updateSingle tm (ValueInterface c n ps rs vs fs) = do
      rs2 <- collectAllOrErrorM $ map (getRefines tm) rs
      return $ ValueInterface c n ps (concat rs2) vs fs
    -- TODO: Also collect inherited functions.
    updateSingle tm (ValueConcrete c n ps rs ds vs fs) = do
      rs2 <- collectAllOrErrorM $ map (getRefines tm) rs
      return $ ValueConcrete c n ps (concat rs2) ds vs fs
    updateSingle _ t = return t
    getRefines tm ra@(ValueRefine c t@(TypeInstance n ps))
      | n `Map.member` tm0 = do
        pa <- assignParams tm c t
        (_,v) <- getValueCategory tm (c,n)
        -- Assume that tm0 has already been fully processed.
        (collectAllOrErrorM $ map (subAll c pa) (getCategoryRefines v)) >>= return . (ra:)
      | otherwise = do
        pa <- assignParams tm c t
        (_,v) <- getValueCategory tm (c,n)
        -- NOTE: Can't use mfix for this because that would require full
        -- evaluation before that same evaluation actually starts.
        -- Assumes that checkConnectedTypes already checked the types.
        rs <- collectAllOrErrorM $ map (getRefines tm) (getCategoryRefines v)
        (collectAllOrErrorM $ map (subAll c pa) (concat rs)) >>= return . (ra:)
    subAll c pa (ValueRefine c1 t1) = do
      (SingleType (JustTypeInstance t2)) <-
        uncheckedSubInstance (getValueForParam pa) (SingleType (JustTypeInstance t1))
      return $ ValueRefine (c ++ c1) t2
    assignParams tm c (TypeInstance n ps) = do
      (_,v) <- getValueCategory tm (c,n)
      -- TODO: From here down should be a top-level function.
      let ns = map vpParam $ getCategoryParams v
      paired <- processParamPairs alwaysPairParams (ParamSet ns) ps
      return $ Map.fromList paired

checkParamVariances :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkParamVariances tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = categoriesToTypeResolver tm
  mergeAll (map (checkCategory r) ts)
  where
    -- TODO: Also check function variances.
    checkCategory r (ValueInterface c n ps rs _ _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAll (map (checkRefine r vm) rs)
    -- TODO: Also check function variances.
    checkCategory r (ValueConcrete c n ps rs ds _ _) = do
      noDuplicates c n ps
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
      mergeAll (map (checkRefine r vm) rs)
      mergeAll (map (checkDefine r vm) ds)
    -- TODO: Also check function variances.
    checkCategory _ (InstanceInterface c n ps _ _) = noDuplicates c n ps
    noDuplicates c n ps = mergeAll (map checkCount $ group $ sort $ map vpParam ps) where
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
  mergeAll $ map (checkSingle r) ts
  where
    checkSingle r t = do
      let pa = Set.fromList $ map vpParam $ getCategoryParams t
      let fm = getFilterMap t
      let vm = Map.fromList $ map (\p -> (vpParam p,vpVariance p)) $ getCategoryParams t
      mergeAll $ map (checkFilterParam pa) (getCategoryFilters t)
      mergeAll $ map (checkRefine r fm) (getCategoryRefines t)
      mergeAll $ map (checkDefine r fm) (getCategoryDefines t)
      mergeAll $ map (checkFilter r fm) (getCategoryFilters t)
      mergeAll $ map (checkFunction r fm vm) (getCategoryFunctions t)
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
    checkFunction r fm vm f = flip reviseError (show f) $ do
      funcType <- parsedToFunctionType f
      validatateFunctionType r fm vm funcType

-- TODO: Maybe get rid of this and require an explicit override instead.
mergeCategoryInstances :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
mergeCategoryInstances tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = categoriesToTypeResolver tm
  collectAllOrErrorM $ map (updateSingle r) ts
  where
    updateSingle r t@(ValueInterface c n ps rs vs fs) = do
      let fm = getFilterMap t
      rs' <- mergeRefines r fm rs
      return $ ValueInterface c n ps rs' vs fs
    updateSingle r t@(ValueConcrete c n ps rs ds vs fs) = do
      let fm = getFilterMap t
      rs' <- mergeRefines r fm rs
      ds' <- mergeDefines r fm ds
      return $ ValueConcrete c n ps rs' ds' vs fs
    updateSingle _ t = return t

mergeCategoryFunctions :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
mergeCategoryFunctions tm0 ts = do
  tm <- declareAllTypes tm0 ts
  let r = categoriesToTypeResolver tm
  collectAllOrErrorM $ map (updateSingle r tm) ts
  where
    updateSingle r tm t@(ValueInterface c n ps rs vs fs) = do
      let fm = getFilterMap t
      fs' <- mergeFuncs r tm fm rs [] fs
      return $ ValueInterface c n ps rs vs fs'
    updateSingle r tm t@(ValueConcrete c n ps rs ds vs fs) = do
      let fm = getFilterMap t
      fs' <- mergeFuncs r tm fm rs ds fs
      return $ ValueConcrete c n ps rs ds vs fs'
    updateSingle _ _ t = return t
    mergeFuncs r tm fm rs ds fs = do
      -- TODO: This doesn't account for a parent that merges two or more
      -- functions from grandparents.
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
      -- TODO: Needs a better error message.
      (tryMerge r fm n (n `Map.lookup` im) (n `Map.lookup` em)) `reviseError` (show n)
    tryMerge _ _ n (Just fs) Nothing
      | length fs == 1 = return $ head fs
      -- TODO: Make this error show context.
      | otherwise = compileError $ "Function " ++ show n ++ " is inherited " ++
                                   show (length fs) ++ " times. " ++
                                   "An explicit override is required."
    tryMerge r fm n Nothing fs = tryMerge r fm n (Just []) fs
    tryMerge r fm n (Just fs1) (Just fs2)
      -- TODO: Make this error show context.
      | length fs2 /= 1 = compileError $ "Function " ++ show n ++ " is declared " ++
                                         show (length fs2) ++ " times. " ++
                                         "Only one declaration is allowed per function."
      | otherwise = do
        let ff@(ScopedFunction c n t s as rs ps fa ms) = head fs2
        mergeAll $ map (checkMerge r fm ff) fs1
        return $ ScopedFunction c n t s as rs ps fa (ms ++ fs1)
        where
          checkMerge r fm f1 f2 = flip reviseError (show f2 ++ "\n  ->\n" ++ show f1) $ do
            f1' <- parsedToFunctionType f1
            f2' <- parsedToFunctionType f2
            checkFunctionConvert r fm f2' f1'

checkInstanceDuplicates :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  [AnyCategory c] -> m ()
checkInstanceDuplicates ts = mergeAll $ map checkSingle ts where
  checkSingle t = do
    checkDuplicateRefines t $ getCategoryRefines t
    checkDuplicateDefines t $ getCategoryDefines t
  checkDuplicateRefines t rs = do
    let grouped = groupBy (\x y -> (tiName . vrType) x == (tiName . vrType) y) $
                  sortBy  (\x y -> (tiName . vrType) x `compare` (tiName . vrType) y) rs
    (mergeAll $ map (requireSingle (tiName . vrType)) grouped) `reviseError`
      ("In " ++ show (getCategoryName t) ++ " [" ++ formatFullContext (getCategoryContext t) ++ "]")
  checkDuplicateDefines t ds = do
    let grouped = groupBy (\x y -> (diName . vdType) x == (diName . vdType) y) $
                  sortBy  (\x y -> (diName . vdType) x `compare` (diName . vdType) y) ds
    (mergeAll $ map (requireSingle (diName . vdType)) grouped) `reviseError`
      ("In " ++ show (getCategoryName t) ++ " [" ++ formatFullContext (getCategoryContext t) ++ "]")
  requireSingle fn xs
    | length xs == 1 = return ()
    | otherwise =
      (compileError $ "Cannot merge instances of " ++ show (fn $ head xs)) `mergeNested`
        (mergeAll $ map (compileError . show) xs)

getFilterMap :: AnyCategory c -> ParamFilters
getFilterMap t = getFilters $ zip (Set.toList pa) (repeat []) where
  pa = Set.fromList $ map vpParam $ getCategoryParams t
  getFilters ps = let fs = map (\f -> (pfParam f,pfFilter f)) (getCategoryFilters t) in
                      Map.fromListWith (++) $ map (second (:[])) fs ++ ps

mergeObjects :: (MergeableM m, CompileErrorM m, Monad m) =>
  (a -> a -> m ()) -> [a] -> m [a]
mergeObjects f = return . merge [] where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    -- TODO: Should f just perform merging? In case we want to preserve info
    -- about what was merged, e.g., return m [(p,a)].
    checker x2 = f x2 x
    ys = if isCompileError $ mergeAny (map checker (cs ++ xs))
            then [x] -- x is not redundant => keep.
            else []  -- x is redundant => remove.

mergeRefines :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> [ValueRefine c] -> m [ValueRefine c]
mergeRefines r f = mergeObjects check where
  check (ValueRefine _ t1@(TypeInstance n1 _)) (ValueRefine _ t2@(TypeInstance n2 _))
    | n1 /= n2 = compileError $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise = do
      checkGeneralMatch r f Covariant (SingleType $ JustTypeInstance $ t1)
                                      (SingleType $ JustTypeInstance $ t2)
      return ()

mergeDefines :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> [ValueDefine c] -> m [ValueDefine c]
mergeDefines r f = mergeObjects check where
  check (ValueDefine _ t1@(DefinesInstance n1 ps1)) (ValueDefine _ t2@(DefinesInstance n2 ps2))
    | n1 /= n2 = compileError $ show t1 ++ " and " ++ show t2 ++ " are incompatible"
    | otherwise = do
      vs <- trVariance r n1
      let paired = zip3 (psParams vs) (psParams ps1) (psParams ps2)
      mergeAll $ map checkSingle paired
      return ()
      where
        checkSingle (v,t1,t2) = checkGeneralMatch r f v t1 t2

categoriesToTypeResolver :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> TypeResolver m ()
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
        return ((),ps1)
      | otherwise = do
        (_,t) <- getValueCategory tm ([],n1)
        let params = map vpParam $ getCategoryParams t
        assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams (ParamSet params) ps1
        let pa = Map.fromList $ map (\r -> (tiName r,tiParams r)) $ map vrType $ getCategoryRefines t
        ps2 <- case n2 `Map.lookup` pa of
                    (Just x) -> return x
                    _ -> compileError $ "Category " ++ show n1 ++ " does not refine " ++ show n2
        fmap ((,) () . ParamSet) $ collectAllOrErrorM $ map (subAllParams assigned) $ psParams ps2
    defines (TypeInstance n1 ps1) n2 = do
      (_,t) <- getValueCategory tm ([],n1)
      let params = map vpParam $ getCategoryParams t
      assigned <- fmap Map.fromList $ processParamPairs alwaysPairParams (ParamSet params) ps1
      let pa = Map.fromList $ map (\r -> (diName r,diParams r)) $ map vdType $ getCategoryDefines t
      ps2 <- case n2 `Map.lookup` pa of
                  (Just x) -> return x
                  _ -> compileError $ "Category " ++ show n1 ++ " does not define " ++ show n2
      fmap ((,) () . ParamSet) $ collectAllOrErrorM $ map (subAllParams assigned) $ psParams ps2
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
      fmap ((,) () . ParamSet) $ collectAllOrErrorM $ map (assignFilter fa) params
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

instance Show FunctionName where
  show (FunctionName n) = n

data ScopedFunction c =
  ScopedFunction {
    sfContext :: [c],
    sfName :: FunctionName,
    sfType :: TypeName,
    sfScope :: SymbolScope,
    sfArgs :: [PassedValue c],
    sfReturns :: [PassedValue c],
    sfParams :: [ValueParam c],
    sfFilters :: [ParamFilter c],
    sfMerges :: [ScopedFunction c]
  }
  deriving (Eq)

instance Show c => Show (ScopedFunction c) where
  show f = showFunctionInContext (showScope $ sfScope f) "" f
    where
      showScope CategoryScope = "@category "
      showScope TypeScope     = "@type "
      showScope ValueScope    = "@value "

showFunctionInContext :: Show c => String -> String -> ScopedFunction c -> String
showFunctionInContext s indent (ScopedFunction cs n t _ as rs ps fa ms) =
  indent ++ s ++ "/*" ++ show t ++ "*/ " ++ show n ++
  showParams ps ++ " " ++ formatContext cs ++ "\n" ++
  concat (map (\v -> indent ++ formatValue v ++ "\n") fa) ++
  indent ++ "(" ++ intercalate "," (map (show . pvType) as) ++ ") -> " ++
  "(" ++ intercalate "," (map (show . pvType) rs) ++ ")" ++ showMerges (flatten ms)
  where
    showParams [] = ""
    showParams ps = "<" ++ intercalate "," (map (show . vpParam) ps) ++ ">"
    formatContext cs = "/*" ++ formatFullContext cs ++ "*/"
    formatValue v = "  " ++ show (pfParam v) ++ " " ++ show (pfFilter v) ++
                    " " ++ formatContext (pfContext v)
    flatten [] = Set.empty
    flatten ms = Set.unions $ (Set.fromList $ map sfType ms):(map (flatten . sfMerges) ms)
    showMerges ms = " /*merged from: " ++ intercalate ", " (map show $ Set.toList ms) ++ "*/"

data PassedValue c =
  PassedValue {
    pvContext :: [c],
    pvType :: ValueType
  }
  deriving (Eq)

instance Show c => Show (PassedValue c) where
  show (PassedValue c t) = show t ++ " [" ++ formatFullContext c ++ "]"

parsedToFunctionType :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  ScopedFunction c -> m FunctionType
parsedToFunctionType (ScopedFunction c n t _ as rs ps fa _) = do
  let as' = ParamSet $ map pvType as
  let rs' = ParamSet $ map pvType rs
  let ps' = ParamSet $ map vpParam ps
  mergeAll $ map checkFilter fa
  let fm = Map.fromListWith (++) $ map (\f -> (pfParam f,[pfFilter f])) fa
  let fa' = ParamSet $ map (getFilters fm) $ psParams ps'
  return $ FunctionType as' rs' ps' fa'
  where
    pa = Set.fromList $ map vpParam ps
    checkFilter f =
      when (not $ (pfParam f) `Set.member` pa) $
      compileError $ "Filtered param " ++ show (pfParam f) ++ " [" ++
                     formatFullContext (pfContext f) ++
                     "] is not defined for function " ++ show n ++
                     " [" ++ formatFullContext c ++ "]"
    getFilters fm n =
      case n `Map.lookup` fm of
           (Just fs) -> fs
           _ -> []

uncheckedSubFunction :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  Map.Map ParamName GeneralInstance -> ScopedFunction c -> m (ScopedFunction c)
uncheckedSubFunction pa ff@(ScopedFunction c n t s as rs ps fa ms) =
  flip reviseError (show ff) $ do
    let fixed = Map.fromList $ map (\n -> (n,SingleType $ JustParamName n)) $ map vpParam ps
    let pa' = Map.union pa fixed
    as' <- collectAllOrErrorM $ map (subPassed pa') as
    rs' <- collectAllOrErrorM $ map (subPassed pa') rs
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
