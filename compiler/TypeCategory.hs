{-# LANGUAGE Safe #-}

module TypeCategory (
  AnyCategory(..),
  CategoryConnect(..),
  ParamValueFilter(..),
  ValueDefine(..),
  ValueParam(..),
  ValueRefine(..),
  checkConnectedTypes,
  checkConnectionCycles,
  checkParamVariances,
  flattenAllConnections,
) where

import Data.List (group,intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import TypeInstance
import TypesBase


data AnyCategory c =
  ValueInterface {
    viContext :: [c],
    viName :: TypeName,
    viParams :: [ValueParam c],
    viRefines :: [ValueRefine c]
  } |
  InstanceInterface {
    iiContext :: [c],
    iiName :: TypeName,
    iiParams :: [ValueParam c]
  } |
  ValueConcrete {
    vcContext :: [c],
    vcName :: TypeName,
    vcParams :: [ValueParam c],
    vcRefines :: [ValueRefine c],
    vcDefines :: [ValueDefine c],
    vcParamValue :: [ParamValueFilter c]
  }
  deriving (Eq)

formatFullContext :: Show a => [a] -> String
formatFullContext cs = intercalate " -> " (map show cs)

instance Show c => Show (AnyCategory c) where
  show = format where
    format (ValueInterface cs n ps rs) =
      "@value interface " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "\n" ++
      concat (map (\r -> "  " ++ formatRefine r ++ "\n") rs) ++ "}\n"
    format (InstanceInterface cs n ps) =
      "@type interface " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "}\n"
    format (ValueConcrete cs n ps rs ds vs) =
      "concrete " ++ show n ++ formatParams ps ++ " { " ++ formatContext cs ++ "\n" ++
      concat (map (\r -> "  " ++ formatRefine r ++ "\n") rs) ++
      concat (map (\d -> "  " ++ formatDefine d ++ "\n") ds) ++
      concat (map (\v -> "  " ++ formatValue v ++ "\n") vs) ++
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

getCategoryName :: AnyCategory c -> TypeName
getCategoryName (ValueInterface _ n _ _) = n
getCategoryName (InstanceInterface _ n _) = n
getCategoryName (ValueConcrete _ n _ _ _ _) = n

getCategoryContext :: AnyCategory c -> [c]
getCategoryContext (ValueInterface c _ _ _) = c
getCategoryContext (InstanceInterface c _ _) = c
getCategoryContext (ValueConcrete c _ _ _ _ _) = c

getCategoryParams :: AnyCategory c -> [ValueParam c]
getCategoryParams (ValueInterface _ _ ps _) = ps
getCategoryParams (InstanceInterface _ _ ps) = ps
getCategoryParams (ValueConcrete _ _ ps _ _ _) = ps

isValueInterface :: AnyCategory c -> Bool
isValueInterface (ValueInterface _ _ _ _) = True
isValueInterface _ = False

isInstanceInterface :: AnyCategory c -> Bool
isInstanceInterface (InstanceInterface _ _ _) = True
isInstanceInterface _ = False

isValueConcrete :: AnyCategory c -> Bool
isValueConcrete (ValueConcrete _ _ _ _ _ _) = True
isValueConcrete _ = False

data ValueRefine c =
  ValueRefine {
    vrContext :: [c],
    vrType :: TypeInstance
  }
  deriving (Eq,Show)

data ValueDefine c =
  ValueDefine {
    vdContext :: [c],
    vdType :: DefinesInstance
  }
  deriving (Eq)

data ValueParam c =
  ValueParam {
    vpContext :: [c],
    vpParam :: ParamName,
    vpVariance :: Variance
  }
  deriving (Eq,Show)

data ParamValueFilter c =
  ParamValueFilter {
    pfContext :: [c],
    pfParam :: ParamName,
    pfFilter :: TypeFilter
  }
  deriving (Eq,Show)


type CategoryMap c = Map.Map TypeName (AnyCategory c)

getCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],TypeName) -> m ([c],AnyCategory c)
getCategory tm (c,n) =
  case n `Map.lookup` tm of
       (Just t) -> return (c,t)
       _ -> compileError $ "type " ++ show n ++
                           " [" ++ formatFullContext c ++ "] not found"

getValueCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],TypeName) -> m ([c],AnyCategory c)
getValueCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isValueInterface t || isValueConcrete t
     then return (c2,t)
     else compileError $ "category " ++ show n ++
                         " cannot be used as a value [" ++
                         formatFullContext c ++ "]"

getInstanceCategory :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> ([c],TypeName) -> m ([c],AnyCategory c)
getInstanceCategory tm (c,n) = do
  (c2,t) <- getCategory tm (c,n)
  if isInstanceInterface t
     then return (c2,t)
     else compileError $ "category " ++ show n ++
                         " cannot be used as a type interface [" ++
                         formatFullContext c ++ "]"

declareAllTypes :: (Show c, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m (CategoryMap c)
declareAllTypes tm0 = foldr (\t tm -> tm >>= update t) (return tm0) where
  update t tm =
    case getCategoryName t `Map.lookup` tm of
        (Just t2) -> compileError $ "type " ++ show (getCategoryName t) ++
                                    " [" ++ formatFullContext (getCategoryContext t) ++
                                    "] has already been declared [" ++
                                    formatFullContext (getCategoryContext t2) ++ "]"
        _ -> return $ Map.insert (getCategoryName t) t tm

checkConnectedTypes :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkConnectedTypes tm0 ts = checked where
  checked = do
    tm <- declareAllTypes tm0 ts
    mergeAll $ map (checkSingle tm) ts
  checkSingle tm (ValueInterface c n _ rs) = do
    ts <- return $ map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getCategory tm) ts
    mergeAll $ map (valueRefinesInstanceError c n) is
    mergeAll $ map (valueRefinesConcreteError c n) is
  checkSingle tm (ValueConcrete c n _ rs ds _) = do
    ts1 <- return $ map (\r -> (vrContext r,tiName $ vrType r)) rs
    ts2 <- return $ map (\d -> (vdContext d,diName $ vdType d)) ds
    is1 <- collectAllOrErrorM $ map (getCategory tm) ts1
    is2 <- collectAllOrErrorM $ map (getCategory tm) ts2
    mergeAll $ map (concreteRefinesInstanceError c n) is1
    mergeAll $ map (concreteDefinesValueError c n) is2
    mergeAll $ map (concreteRefinesConcreteError c n) is1
    mergeAll $ map (concreteDefinesConcreteError c n) is2
  checkSingle _ _ = return ()
  valueRefinesInstanceError c n (c2,t)
    | isInstanceInterface t =
      compileError $ "value interface " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                     " cannot refine type interface " ++
                     show (iiName t) ++ " [" ++ formatFullContext c2 ++ "]"
    | otherwise = return ()
  valueRefinesConcreteError c n (c2,t)
    | isValueConcrete t =
      compileError $ "value interface " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                     " cannot refine concrete type " ++
                     show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]"
    | otherwise = return ()
  concreteRefinesInstanceError c n (c2,t)
    | isInstanceInterface t =
      compileError $ "concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                     " cannot refine instance interface " ++
                     show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]" ++
                     " => use defines instead"
    | otherwise = return ()
  concreteDefinesValueError c n (c2,t)
    | isValueInterface t =
      compileError $ "concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                     " cannot define value interface " ++
                     show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]" ++
                     " => use refines instead"
    | otherwise = return ()
  concreteRefinesConcreteError c n (c2,t)
    | isValueConcrete t =
      compileError $ "concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                     " cannot refine concrete type " ++
                     show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]"
    | otherwise = return ()
  concreteDefinesConcreteError c n (c2,t)
    | isValueConcrete t =
      compileError $ "concrete type " ++ show n ++ " [" ++ formatFullContext c ++ "]" ++
                     " cannot define concrete type " ++
                     show (getCategoryName t) ++ " [" ++ formatFullContext c2 ++ "]"
    | otherwise = return ()

checkConnectionCycles :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  [AnyCategory c] -> m ()
checkConnectionCycles ts = checked where
  checked = mergeAll $ map (checker []) ts
  tm = Map.fromList $ zip (map getCategoryName ts) ts
  checker us (ValueInterface c n _ rs) = do
    failIfCycle n c us
    ts <- return $ map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getValueCategory tm) ts
    mergeAll $ map (checker (us ++ [n]) . snd) is
  checker us (ValueConcrete c n _ rs _ _) = do
    failIfCycle n c us
    ts <- return $ map (\r -> (vrContext r,tiName $ vrType r)) rs
    is <- collectAllOrErrorM $ map (getValueCategory tm) ts
    mergeAll $ map (checker (us ++ [n]) . snd) is
  checker _ _ = return ()
  failIfCycle n c us =
    if n `Set.member` (Set.fromList us)
       then compileError $ "category " ++ show n ++ " [" ++
                           formatFullContext c ++ "] refers back to itself: " ++
                           intercalate " -> " (map show (us ++ [n]))
       else return ()

flattenAllConnections :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m [AnyCategory c]
flattenAllConnections tm0 ts = updated where
  updated = do
    tm <- declareAllTypes tm0 ts
    collectAllOrErrorM $ map (updateSingle tm) ts
  updateSingle tm (ValueInterface c n ps rs) = do
    rs2 <- collectAllOrErrorM $ map (getRefines tm) rs
    return $ ValueInterface c n ps (concat rs2)
  updateSingle tm (ValueConcrete c n ps rs ds vs) = do
    rs2 <- collectAllOrErrorM $ map (getRefines tm) rs
    return $ ValueConcrete c n ps (concat rs2) ds vs
  updateSingle _ t = return t
  getRefines tm ra@(ValueRefine c t@(TypeInstance n ps))
    | n `Map.member` tm0 = do
      pa <- assignParams tm c t
      (_,v) <- getValueCategory tm (c,n)
      -- Assume that tm0 has already been fully processed.
      (collectAllOrErrorM $ map (subAll c pa) (viRefines v)) >>= return . (ra:)
    | otherwise = do
      pa <- assignParams tm c t
      (_,v) <- getValueCategory tm (c,n)
      -- NOTE: Can't use mfix for this because that would require full
      -- evaluation before that same evaluation actually starts.
      -- Assumes that checkConnectedTypes already checked the types.
      rs <- collectAllOrErrorM $ map (getRefines tm) (viRefines v)
      (collectAllOrErrorM $ map (subAll c pa) (concat rs)) >>= return . (ra:)
  subAll c pa (ValueRefine c1 t1) = do
    (SingleType (JustTypeInstance t2)) <-
      uncheckedSubAllParams (getParam pa) (SingleType (JustTypeInstance t1))
    return $ ValueRefine (c ++ c1) t2
  getParam pa n =
    case n `Map.lookup` pa of
         (Just x) -> return x
         _ -> compileError $ "param " ++ show n ++ " does not exist"
  assignParams tm c (TypeInstance n ps) = do
    (_,v) <- getValueCategory tm (c,n)
    -- TODO: From here down should be a top-level function.
    ns <- return $ map vpParam $ viParams v
    paired <- checkParamsMatch alwaysPairParams (ParamSet ns) ps
    return $ Map.fromList paired

checkParamVariances :: (Show c, MergeableM m, CompileErrorM m, Monad m) =>
  CategoryMap c -> [AnyCategory c] -> m ()
checkParamVariances tm0 ts = updated where
  updated = do
    tm <- declareAllTypes tm0 ts
    mergeAll $ map (checkCategory tm) ts
  checkCategory tm (ValueInterface c n ps rs) = do
    vm <- return $ Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
    mergeAll $ map (checkRefine tm vm) rs
  checkCategory tm (ValueConcrete c n ps rs ds _) = do
    vm <- return $ Map.fromList $ map (\p -> (vpParam p,vpVariance p)) ps
    mergeAll $ map (checkRefine tm vm) rs
    mergeAll $ map (checkDefine tm vm) ds
  checkCategory _ _ = return ()
  getVariances tm c n = do
    (_,t) <- getValueCategory tm (c,n)
    return $ map vpVariance (getCategoryParams t)
  checkRefine tm vm (ValueRefine c t) =
    checkParam c (show t) tm vm Covariant (SingleType $ JustTypeInstance t)
  checkDefine tm vm (ValueDefine c t@(DefinesInstance n ps)) = do
    (_,t2) <- getInstanceCategory tm (c,n)
    vs <- return $ map vpVariance (getCategoryParams t2)
    paired <- checkParamsMatch alwaysPairParams (ParamSet vs) ps
    mergeAll $ map (\(v,p) -> checkParam c (show t) tm vm v p) paired
  checkParam c t tm vm v (SingleType (JustTypeInstance (TypeInstance n ps))) = do
    vs <- getVariances tm c n
    paired <- checkParamsMatch alwaysPairParams (ParamSet vs) ps
    mergeAll $ map (\(v2,p) -> checkParam c t tm vm (v `composeVariance` v2) p) paired
  checkParam c t tm vm v (TypeMerge MergeUnion ts) =
    mergeAll $ map (checkParam c t tm vm v) ts
  checkParam c t tm vm v (TypeMerge MergeIntersect ts) =
    mergeAll $ map (checkParam c t tm vm v) ts
  checkParam c t tm vm v (SingleType (JustParamName n)) =
    case n `Map.lookup` vm of
         Nothing -> compileError $ "param " ++ show n ++ " [" ++
                                   formatFullContext c ++ "] is undefined"
         (Just v0) -> if v0 `paramAllowsVariance` v
                         then return ()
                         else compileError $ "param " ++ show n ++
                                             " cannot be " ++ show v ++
                                             " in " ++ t ++
                                             " [" ++ formatFullContext c ++ "]"


newtype CategoryConnect a =
  CategoryConnect {
    ccMap :: Map.Map TypeName a
  }

type CategoryRefines = CategoryConnect [TypeInstance]
type CategoryParams = CategoryConnect (ParamSet ParamName)
type CategoryFilters = CategoryConnect (ParamSet [TypeFilter])

categoryLookup :: (CompileErrorM m, Monad m) => TypeName -> CategoryConnect a -> m a
categoryLookup n (CategoryConnect cs) = resolve $ n `Map.lookup` cs where
  resolve (Just x) = return x
  resolve _        = compileError $ "Category " ++ show n ++ " not found"

checkCategory :: (Mergeable b) => (TypeName -> a -> b) -> CategoryConnect a -> b
checkCategory f (CategoryConnect cs) = mergeAll $ map (\(k,v) -> f k v) (Map.toList cs)

uncheckedZipFilters ::
  ParamSet ParamName -> ParamSet [TypeFilter] -> Map.Map ParamName [TypeFilter]
uncheckedZipFilters (ParamSet pa) (ParamSet fa) =
  Map.fromList $ zip pa fa

mergeInstances :: (MergeableM m, Mergeable p, CompileErrorM m, Monad m) =>
  TypeResolver m p -> ParamFilters -> [TypeInstance] -> [TypeInstance]
mergeInstances r f gs = merge [] gs where
  merge cs [] = cs
  merge cs (x:xs) = merge (cs ++ ys) xs where
    checker x2 = checkGeneralMatch r f Covariant
                                   (SingleType $ JustTypeInstance x2)
                                   (SingleType $ JustTypeInstance x)
    ys = if isCompileError $ mergeAny $ map checker (cs ++ xs)
       then [x] -- x is not redundant => keep.
       else []  -- x is redundant => remove.

checkRefines :: (MergeableM m, CompileErrorM m, Monad m) =>
  TypeResolver m p -> CategoryParams -> CategoryFilters -> CategoryRefines -> m ()
checkRefines r ps fs = checkCategory checkAll where
  checkAll n gs = do
    ts <- collectAllOrErrorM $ map (getTypeName n) gs
    mergeAll $ map (checkGroup n) $ group ts
  getTypeName n ta@(TypeInstance t _) = do
    params <- n `categoryLookup` ps
    filters <- n `categoryLookup` fs
    mapped <- return $ uncheckedZipFilters params filters
    -- tfValidate r mapped ta
    return t
  checkGroup n (t:t2:ts) =
    compileError $ "Type " ++ show n ++ " has conflicting refinements of type " ++ show t
  checkGroup _ _ = return ()

uncheckedSubFilterParams :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> ParamSet [TypeFilter] ->
  m (ParamSet [TypeFilter])
uncheckedSubFilterParams replace pa = subbed where
  subbed = do
    updated <- collectAllOrErrorM $ map subAllFilters (psParams pa)
    return (ParamSet updated)
  subAllFilters ps = collectAllOrErrorM $ map subSingleFilter ps
  subSingleFilter (TypeFilter v t) = do
    (SingleType t2) <- uncheckedSubAllParams replace (SingleType t)
    return (TypeFilter v t2)

uncheckedSubAllParams :: (MergeableM m, CompileErrorM m, Monad m) =>
  (ParamName -> m GeneralInstance) -> GeneralInstance -> m GeneralInstance
uncheckedSubAllParams replace = subAll where
  subAll (TypeMerge MergeUnion ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (TypeMerge MergeUnion gs)
  subAll (TypeMerge MergeIntersect ts) = do
    gs <- collectAllOrErrorM $ map subAll ts
    return (TypeMerge MergeIntersect gs)
  subAll (SingleType t) = subInstance t
  subInstance (JustTypeInstance (TypeInstance n (ParamSet ts))) = do
    gs <- collectAllOrErrorM $ map subAll ts
    t2 <- return $ SingleType $ JustTypeInstance $ TypeInstance n (ParamSet gs)
    return (t2)
  subInstance (JustParamName n) = replace n
