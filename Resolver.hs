module Resolver (
  TypeCategoryGraph,
  TypeCategoryArg,
  createTypeCategoryGraph,
  -- For testing...
  resolveTypeCategoryInstance,
  checkConversion
) where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad (foldM, guard, join, liftM3)
import Control.Monad.Fix (fix)
import Data.List (group, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CompileInfo
import TypesBase
import Unresolved


data TypeCategoryName =
  TypeCategoryName {
    tcnName :: String
  }
  deriving (Eq, Ord)

instance Show TypeCategoryName where
  show (TypeCategoryName n) = n

tcnFromUTC :: UnresolvedTypeCategory -> TypeCategoryName
tcnFromUTC = TypeCategoryName . utcName

tcnFromUT :: UnresolvedType -> TypeCategoryName
tcnFromUT = TypeCategoryName . utTypeCategory


data TypeCategoryParamName =
  TypeCategoryParamName {
    tcpnName :: String
  }
  deriving (Eq, Ord)

instance Show TypeCategoryParamName where
  show (TypeCategoryParamName n) = n

tcpnFromUTP :: UnresolvedTypeParam -> TypeCategoryParamName
tcpnFromUTP = TypeCategoryParamName . utpName

tcpnFromUTA :: UnresolvedType -> TypeCategoryParamName
tcpnFromUTA = TypeCategoryParamName . utaName

tcpnFromUPF :: UnresolvedParamFilter -> TypeCategoryParamName
tcpnFromUPF (UnresolvedParamFilter  n _) = TypeCategoryParamName n
tcpnFromUPF (UnresolvedParamMissing n _) = TypeCategoryParamName n


data TypeFilter =
  TypeFilter {
    tfType :: TypeCategoryInstance
  }
  deriving (Eq, Ord)

instance Show TypeFilter where
  show (TypeFilter t) = show t


data TypeCategoryParam =
  TypeCategoryParam {
    tcpName :: TypeCategoryParamName,
    tcpVariance :: Variance,
    tcpMissing :: Missingness,
    tcpFilters :: Set.Set TypeFilter
  }
  deriving (Eq, Ord)

instance Show TypeCategoryParam where
  show (TypeCategoryParam n v m f) = (showName v) ++ (showFilters f) where
    showName Contravariant = "-" ++ show n ++ showMissing
    showName Covariant     = "+" ++ show n ++ showMissing
    showName _             = show n ++ showMissing
    showFilters fs
      | null (Set.toList fs) = ""
      | otherwise = " -> " ++ show (Set.toList fs)
    showMissing = case m of
                       AllowsMissing -> "?"
                       DisallowsMissing -> "!"
                       _ -> ""

data TypeCategoryInstance =
  TypeCategoryInstance {
    -- TODO: Is this needed? It's tedious to keep up to date. Maybe it can be
    -- used to prune the substitution list before traversing. A map might
    -- actually be easier to work with and update.
    tciFreeParams :: [TypeCategoryParam], -- TODO: Make this a set?
    tciClassName :: TypeCategoryName,
    tciArgs :: [TypeCategoryArg]
  }
  deriving (Eq, Ord)

instance Show TypeCategoryInstance where
  show (TypeCategoryInstance p n a) = show n ++ (showArgs a) ++ (showParams p) where
    showArgs [] = ""
    showArgs as = "<" ++ intercalate "," (map show as) ++ ">"
    showParams [] = ""
    showParams ps = " " ++ show ps


data TypeCategoryArg =
  TypeCategoryArgType {
    tcatInstance :: TypeCategoryInstance
  } |
  TypeCategoryArgParam {
    tcapParam :: TypeCategoryParam
  }
  deriving (Eq, Ord)

instance Show TypeCategoryArg where
  show (TypeCategoryArgType t)  = show t
  show (TypeCategoryArgParam p) = show p


data TypeCategoryGraph =
  TypeCategoryGraph {
    tcgParams :: TypeCategoryParamMap,
    tcgGraph :: TypeCategoryMap,
    tcgMissing :: TypeCategoryMissingMap,
    tcgRefines :: TypeCategoryRefinesMap
  }
  deriving (Eq)

instance Show TypeCategoryGraph where
  show (TypeCategoryGraph ps gs ms is) = showParams ++ "\n" ++
                                      showGraph ++ "\n" ++
                                      showRefines where
    showParams = "params {\n" ++ join params ++ "}"
    params = do
      (n,p) <- Map.toList ps
      ["  " ++ show n ++ " ~ " ++ show p ++ "\n"]
    showGraph = "typeclass graph {\n" ++ join simpleRefines ++ "}"
    simpleRefines = do
      (n,is) <- Map.toList gs
      ["  " ++ showWithMissing n ++ " -> [" ++
       intercalate "," (map showWithMissing $ Set.toList is) ++ "]\n"]
    showRefines = "typeclass refines {\n" ++ join fullRefines ++ "}"
    fullRefines = do
      (n,ts) <- Map.toList is
      t <- Set.toList ts
      ["  " ++ show n ++ " -> " ++ show t ++ "\n"]
    showWithMissing n = fromJust $ do
      missing <- return $ Map.findWithDefault DisallowsMissing n ms
      if missing == AllowsMissing
         then return (show n ++ "?")
         else return (show n)


type TypeCategoryParamMap = Map.Map TypeCategoryName [TypeCategoryParam]

type TypeCategoryMap = Map.Map TypeCategoryName (Set.Set TypeCategoryName)

type TypeCategoryRefinesMap = Map.Map TypeCategoryName (Set.Set TypeCategoryArg)

type TypeCategoryMissingMap = Map.Map TypeCategoryName Missingness


checkTypeCategoryCycles :: Map.Map String [String] -> (String, [String]) -> CompileInfo ()
checkTypeCategoryCycles m (n,is) = checked where
  checked = mergeAll $ map (checkRecursion n $ Set.fromList [n]) is
  checkRecursion n ts i = do
    if i `Set.member` ts
       then compileError $ "Refineance cycle found for type '" ++ n ++ "'"
       else return ()
    maybeRefined <- return $ i `Map.lookup` m
    refined <- if isJust maybeRefined
                   then return $ fromJust maybeRefined
                   else compileError $ "Type '" ++ i ++ "' not found, used in '" ++ n ++ "'"
    -- TODO: This is quadratic and might not scale well.
    mergeAll $ map (checkRecursion i $ i `Set.insert` ts) refined

-- TODO: Also check for *identical* refines (including params).
getTypeCategoryRefines :: UnresolvedTypeCategory -> CompileInfo (String, [UnresolvedType])
getTypeCategoryRefines u = refines >>= \ts -> return (name, ts) where
  name = utcName u
  refines = foldr collectRefines (return []) (utcRefines u)
  collectRefines (UnresolvedTypeArg p) _ =
    compileError $ "Type '" ++ name ++ "' cannot refine param '" ++ p ++ "'"
  collectRefines t c = do
    l <- c
    return $ t:l

checkDuplicates :: (Eq a, Show a) => [a] -> CompileInfo ()
checkDuplicates xs = mergeAll $ map check $ group xs where
  check (x:y:_) = compileError $ "Duplicate definition of '" ++ show x ++ "'"
  check _       = return ()

checkAllTypeCategoryCycles :: [UnresolvedTypeCategory] -> CompileInfo ()
checkAllTypeCategoryCycles us = do
  types <- collectAllOrErrorM $ map getTypeCategoryRefines us
  typeTuples <- return $ map (second (map utTypeCategory)) types
  typeMap <- return $ Map.fromList typeTuples
  mergeAll $ map (checkTypeCategoryCycles typeMap) typeTuples

checkMissingMatch :: Missingness -> Missingness -> CompileInfo ()
checkMissingMatch m1 m2 =
  if m1 `paramAllowsMissing` m2
      then return ()
      else compileError $ "Missingness " ++ show m1 ++ " does not allow " ++ show m2

updateTypeCategoryMap :: (TypeCategoryName, TypeCategoryName) -> TypeCategoryMap -> TypeCategoryMap
updateTypeCategoryMap (f, t)  m = updated where
  old = Map.findWithDefault Set.empty f m
  updated = Map.insert f (t `Set.insert` old) m

initialTypeCategoryParamMap :: [UnresolvedTypeCategory] -> CompileInfo TypeCategoryParamMap
initialTypeCategoryParamMap us = allParams where
  allParams = do
    checked <- collectAllOrErrorM $ map collectParams us
    return $ Map.fromList checked
  collectParams u = do
    checkDuplicates $ map tcpnFromUTP $ utcParams u
    return (tcnFromUTC u, map convertParam $ utcParams u)
  convertParam p = TypeCategoryParam {
    tcpName = tcpnFromUTP p,
    tcpVariance = utpVariance p,
    tcpMissing = UnspecifiedMissing,
    tcpFilters = Set.empty -- Resolved later on.
  }

-- TODO: Update this and updateTypeCategoryGraph so that an additional "module" of
-- unresolved type classes can be appended to the graph.
createTypeCategoryGraph :: [UnresolvedTypeCategory] -> CompileInfo TypeCategoryGraph
createTypeCategoryGraph us = do
  checkAllTypeCategoryCycles us
  checkDuplicates $ map utcName us
  edges <- return $ do
    u <- us
    t <- utcRefines u
    return (tcnFromUTC u, tcnFromUT t)
  defaultMap <- return $ Map.fromList $ zip (map tcnFromUTC us) (repeat Set.empty)
  initialParams <- initialTypeCategoryParamMap us
  missingMap <- return $ Map.fromList $ zip (map tcnFromUTC us) (map utcMissing us)
  initialGraph <- return TypeCategoryGraph {
      tcgParams = initialParams,
      tcgGraph = foldr updateTypeCategoryMap defaultMap edges,
      tcgMissing = missingMap,
      tcgRefines = Map.empty
    }
  updateTypeCategoryGraph initialGraph us

updateTypeCategoryGraph :: TypeCategoryGraph -> [UnresolvedTypeCategory] -> CompileInfo TypeCategoryGraph
updateTypeCategoryGraph g us = updated where
  oldParams = tcgParams g
  resolveFilters t fs = do
    oldParam <- return $ t `Map.lookup` oldParams
    if isJust oldParam
       then foldM singleFilter (fromJust oldParam) fs
       else compileError $ "Type '" ++ show t ++ "' not found"
  singleFilter fs f@(UnresolvedParamFilter _ t) = do
    filter <- tryTypeCategoryInstance True g Map.empty UnspecifiedMissing Set.empty t
    checkParam (tcpnFromUPF f) (snd filter)
    filterType <- getFilterType (fst filter)
    updateParamFilter (tcpnFromUPF f) filterType fs
  singleFilter fs f@(UnresolvedParamMissing _ m) = do
    updateParamMissing (tcpnFromUPF f) m fs
  -- Make sure that the filter isn't another param.
  getFilterType (TypeCategoryArgType t) = return t
  getFilterType _ = compileError $ "Filter must be a type, not a param"
  -- Ensures that the free params in the filter match what is expected.
  checkParam n [] = return ()
  checkParam n (x:[])
    | n == tcpName x = return ()
    | otherwise = compileError $ "Filter does not apply to param '" ++ show n ++ "'"
  checkParam _ _ = compileError $ "Too many free params for filter"
  -- Find the correct param (for the type class) and update the filter.
  updateParamFilter n a [] = compileError $ "Param '" ++ show n ++ "' not found"
  updateParamFilter n a (p:ps)
    | n /= tcpName p = do
      rest <- updateParamFilter n a ps
      return (p:rest)
    | otherwise = return (update:ps) where
      update = TypeCategoryParam {
          tcpName = tcpName p,
          tcpVariance = tcpVariance p,
          tcpMissing = tcpMissing p,
          tcpFilters = Set.insert (TypeFilter a) (tcpFilters p)
        }
  -- Find the correct param (for the type class) and update the missingness.
  updateParamMissing n m [] = compileError $ "Param '" ++ show n ++ "' not found"
  updateParamMissing n m (p:ps)
    | n /= tcpName p = do
      rest <- updateParamMissing n m ps
      return (p:rest)
    | (tcpMissing p) /= UnspecifiedMissing =
      compileError $ "Missingness for param '" ++ show n ++ "' was set more than once"
    | otherwise = return (update:ps) where
      update = TypeCategoryParam {
          tcpName = tcpName p,
          tcpVariance = tcpVariance p,
          tcpMissing = m,
          tcpFilters = tcpFilters p
        }
  -- Update the map with all filter updates for a single type class.
  updateParam us u = do
    realFilters <- resolveFilters (tcnFromUTC u) (utcFilters u)
    return $ Map.insert (tcnFromUTC u) realFilters us
  -- Update the inheritance graph.
  updateRefine g2 us u = do
    params <- return $ Map.findWithDefault [] (tcnFromUTC u) (tcgParams g2)
    extras <- return $ Map.fromList $ map (\p -> (tcpName p,p)) params
    refines <- foldM (getRefine g2 extras) [] (utcRefines u)
    return $ (tcnFromUTC u,Set.fromList refines):us
  getRefine g2 e is i = do
    -- Filters cannot be checked here because that depends on the graph having
    -- updated refines. Filters are validated later with validateParamFilters.
    refine <- tryTypeCategoryInstance False g2 e UnspecifiedMissing Set.empty i
    return (fst refine:is)
  -- The fully-updated graph.
  updated = do
    newParams <- foldM updateParam oldParams us
    partial <- return $ TypeCategoryGraph {
        tcgParams = newParams,
        tcgGraph = tcgGraph g,
        tcgMissing = tcgMissing g,
        tcgRefines = Map.empty
      }
    newRefines <- foldM (updateRefine partial) [] us
    full <- return $ TypeCategoryGraph {
        tcgParams = newParams,
        tcgGraph = tcgGraph g,
        tcgMissing = tcgMissing g,
        tcgRefines = Map.fromList newRefines
      }
    validateParamVariance full
    validateParamFilters full
    validateParamMissing full
    return full

validateParamVariance :: TypeCategoryGraph -> CompileInfo ()
validateParamVariance g = checked where
  checked = mergeAll $ map checkVariances (Map.toList $ tcgRefines g)
  checkVariances (t,is) = mergeAll $ map (checkVariance t) (Set.toList is)
  checkVariance t i = do
    -- Params available from the typeclass doing the inheriting.
    -- TODO: Missing value should be an error.
    params <- return $ Map.findWithDefault [] t (tcgParams g)
    required <- return $ Map.fromList $ mapParams params
    -- Params as used in inheritance.
    freeParams <- return $ mapParams $ tciFreeParams $ tcatInstance i
    mergeAll $ map (check (t,tcatInstance i) required) freeParams
  check (t,i) m (p,v1) = do
    v0 <- return $ p `Map.lookup` m
    if isJust v0
       then return ()
       else compileError $ "Param '" ++ show p ++ "' does not exist"
    if (fromJust v0) `paramAllowsVariance` v1
       then return ()
       else compileError $ "Param '" ++ show p ++ "' cannot be " ++
                              show v1 ++ " in '" ++ show t ++ "' -> '" ++ show i ++ "'"
  mapParams = map (\p -> (tcpName p,tcpVariance p))

validateParamFilters :: TypeCategoryGraph -> CompileInfo ()
validateParamFilters g = checked where
  checked = mergeAll $ map checkFilters (Map.toList $ tcgRefines g)
  checkFilters (t,is) = mergeAll $ map (checkFilter t) (Set.toList is)
  checkFilter t i = do
    -- Params available from the typeclass doing the inheriting.
    -- TODO: Missing value should be an error.
    params <- return $ Map.findWithDefault [] t (tcgParams g)
    required <- return $ Map.fromList $ mapParams params
    -- Params as used in inheritance.
    freeParams <- return $ mapParams $ tciFreeParams $ tcatInstance i
    message <- return $ " in '" ++ show t ++ "' -> '" ++ show (tcatInstance i) ++ "'"
    checkAllFilters g message required freeParams
  mapParams = map (\p -> (tcpName p,tcpFilters p))

validateParamMissing :: TypeCategoryGraph -> CompileInfo ()
validateParamMissing g = checked where
  checked = do
    mergeAll $ map checkRefineMissings (Map.toList $ tcgGraph g)
    mergeAll $ map checkParamMissings (Map.toList $ tcgRefines g)
  checkRefineMissings (t,is) = mergeAll $ map (checkRefineMissing t) (Set.toList is)
  checkRefineMissing t i = do
    -- TODO: Missing value should be an error.
    m1 <- return $ Map.findWithDefault DisallowsMissing i (tcgMissing g)
    m2 <- return $ Map.findWithDefault DisallowsMissing t (tcgMissing g)
    m1 `checkMissingMatch` m2
  checkParamMissings (t,is) = mergeAll $ map (checkParamMissing t) (Set.toList is)
  checkParamMissing t i = do
    -- Params available from the typeclass doing the inheriting.
    -- TODO: Missing value should be an error.
    params <- return $ Map.findWithDefault [] t (tcgParams g)
    required <- return $ Map.fromList $ mapParams params
    -- Params as used in inheritance.
    freeParams <- return $ mapParams $ tciFreeParams $ tcatInstance i
    checkAllMissings required freeParams
  mapParams = map (\p -> (tcpName p,tcpMissing p))

checkAllFilters :: TypeCategoryGraph -> String ->
                   Map.Map TypeCategoryParamName (Set.Set TypeFilter) ->
                   [(TypeCategoryParamName, (Set.Set TypeFilter))] -> CompileInfo ()
checkAllFilters g e m ps = mergeAll $ map checkFilter ps where
  checkFilter (p,fs) = do
    f0 <- return $ p `Map.lookup` m
    if isJust f0
      then return ()
      else compileError $ "Param '" ++ show p ++ "' does not exist"
    mergeAll $ map (findCompatFilter p (Set.toList $ fromJust f0)) (Set.toList fs)
  findCompatFilter p rs a = do
    mergeAny $
      (compileError $ "Param '" ++ show p ++ "' does not support required filter " ++ show a ++ e)
      : (map (check a) rs)
  check a r = do
    guessInstanceConversion g Covariant
      (TypeCategoryArgType $ tfType r) (TypeCategoryArgType $ tfType a)
    return ()

checkAllMissings :: Map.Map TypeCategoryParamName Missingness ->
                    [(TypeCategoryParamName, Missingness)] -> CompileInfo ()
checkAllMissings m ps = mergeAll $ map checkMissing ps where
  checkMissing (p,m1) = do
    m2 <- return $ p `Map.lookup` m
    if isJust m2
      -- The free param determines how the arg can be used.
      then m1 `checkMissingMatch` (fromJust m2)
      else compileError $ "Param '" ++ show p ++ "' does not exist"

flattenTypeCategoryParams :: [TypeCategoryParam] -> [TypeCategoryParam]
flattenTypeCategoryParams = map snd . Map.toList . foldr process Map.empty where
  process x m = update previous where
    name = tcpName x
    previous = Map.lookup name m
    update Nothing  = Map.insert name x m
    update (Just y) = flip (Map.insert name) m $
      TypeCategoryParam {
        tcpName = name,
        tcpVariance = (tcpVariance x) `composeVariance` (tcpVariance y),
        -- TODO: Check for conflicts here.
        tcpMissing = tcpMissing x,
        tcpFilters = (tcpFilters x) `Set.union` (tcpFilters y)
      }

-- NOTE: This only works because filters can only have one free param each.
renameFilters :: TypeCategoryParamName -> Set.Set TypeFilter -> Set.Set TypeFilter
renameFilters n fs = Set.fromList $ map renameFilter $ Set.toList fs where
  renameFilter f = TypeFilter {
      tfType = renameTypeInstance $ tfType f
    }
  renameTypeInstance t = TypeCategoryInstance {
      tciFreeParams = map renameParam $ tciFreeParams t,
      tciClassName = tciClassName t,
      tciArgs = map renameArg $ tciArgs t
    }
  renameParam p = TypeCategoryParam {
      tcpName = n,
      tcpVariance = tcpVariance p,
      tcpMissing = tcpMissing p,
      tcpFilters = renameFilters n $ tcpFilters p
    }
  renameArg (TypeCategoryArgType t) = TypeCategoryArgType {
      tcatInstance = renameTypeInstance t
    }
  renameArg (TypeCategoryArgParam p) = TypeCategoryArgParam {
      tcapParam = renameParam p
    }

tryTypeCategoryInstance :: Bool -> TypeCategoryGraph ->
                        Map.Map TypeCategoryParamName TypeCategoryParam ->
                        Missingness ->
                        Set.Set TypeFilter ->
                        UnresolvedType ->
                        CompileInfo (TypeCategoryArg, [TypeCategoryParam])
tryTypeCategoryInstance c g pm m fs a@(UnresolvedTypeArg _) = resolved where
  resolved = do
    if c
      then checkInstanceConversion g (TypeCategoryArgParam external) (TypeCategoryArgParam param) >> return ()
      else return ()
    return (arg, [param])
  -- TODO: Make it a failure for this to be not found?
  extras = fromJust $ (flip (<|>)) (Just Set.empty) $ do
    param <- (tcpnFromUTA a) `Map.lookup` pm
    return (tcpFilters param)
  -- TODO: Make it a failure for this to be not found?
  missing = fromJust $ (flip (<|>)) (Just UnspecifiedMissing) $ do
    param <- (tcpnFromUTA a) `Map.lookup` pm
    return (tcpMissing param)
  newFilters = renameFilters (tcpnFromUTA a) fs
  external = TypeCategoryParam {
      tcpName = tcpnFromUTA a,
      tcpVariance = Covariant,
      tcpMissing = missing,
      tcpFilters = extras
    }
  param = TypeCategoryParam {
      tcpName = tcpnFromUTA a,
      -- This variance is in the context of this particular TypeCategoryArg.
      tcpVariance = Covariant,
      tcpMissing = m,
      tcpFilters = newFilters `Set.union` extras
    }
  arg = TypeCategoryArgParam {
      tcapParam = param
    }
tryTypeCategoryInstance c g pm m fs u = result where
  name = utTypeCategory u
  properName = tcnFromUT u
  updateVariance v p = TypeCategoryParam {
      tcpName = tcpName p,
      tcpVariance = v `composeVariance` (tcpVariance p),
      tcpMissing = tcpMissing p,
      tcpFilters = tcpFilters p
    }
  checkSize ps us
    | length ps > length us = compileError $ "Too few args for type '" ++ name ++ "'"
    | length ps < length us = compileError $ "Too many args for type '" ++ name ++ "'"
    | otherwise = return ()
  collectTypeCategoryArgs ps us = do
    checkSize ps us
    aps <- collectAllOrErrorM $ map collectArgs (zip ps us)
    return $ (map fst aps,join $ map snd aps)
  collectArgs (p,u) = do
    (arg,newParams) <- tryTypeCategoryInstance c g pm (tcpMissing p) (tcpFilters p) u
    checkArgMissing p arg
    updatedParams <- return $ map (updateVariance $ tcpVariance p) newParams
    return (arg,updatedParams)
  checkArgMissing p (TypeCategoryArgType t) = do
    missing <- return $ Map.findWithDefault DisallowsMissing (tciClassName t) (tcgMissing g)
    (tcpMissing p) `checkMissingMatch` missing
  checkArgMissing p1 (TypeCategoryArgParam p2) =
    (tcpMissing p1) `checkMissingMatch` (tcpMissing p2)
  result = do
    typeParams <- return $ properName `Map.lookup` (tcgParams g)
    if isJust typeParams
      then return ()
      else compileError $ "Type class '" ++ name ++ "' not found"
    (args, params) <- collectTypeCategoryArgs (fromJust typeParams) (utParamArgs u)
    resolved <- return $ TypeCategoryInstance {
        tciFreeParams = flattenTypeCategoryParams params,
        tciClassName = properName,
        tciArgs = args
      }
    if c
       then checkInstanceFilters g resolved
       else return ()
    return (TypeCategoryArgType resolved,params)

checkTypeFilters :: TypeCategoryGraph -> [TypeCategoryParam] -> [TypeCategoryArg] -> CompileInfo ()
checkTypeFilters g ps as = mergeAll $ map check (zip ps as) where
  check (p,a) = mergeAll $ map (checkFilters (tcpName p,a) a) $
                  map (TypeCategoryArgType . tfType) $ Set.toList $ tcpFilters p
  checkFilters s a f = do
    -- Sub a into the filter, since we're also subbing on the left side. For
    -- example, with x = N<y> the check x -> T<x> becomes N<y> -> T<N<y>>.
    (subbed,_) <- uncheckedSubTypeCategoryArgs g (Map.fromList [s]) f
    guessInstanceConversion g Covariant a subbed
    return ()

checkInstanceFilters :: TypeCategoryGraph -> TypeCategoryInstance -> CompileInfo ()
checkInstanceFilters g (TypeCategoryInstance _ t as) = checked where
  allParams = tcgParams g
  checked = do
    params <- return $ t `Map.lookup` allParams
    if isJust params
       then checkTypeFilters g (fromJust params) as
       else compileError $ "Type '" ++ show t ++ "' does not exist"

uncheckedSubTypeCategoryArgs :: TypeCategoryGraph ->
                             Map.Map TypeCategoryParamName TypeCategoryArg ->
                             TypeCategoryArg ->
                             CompileInfo (TypeCategoryArg, [TypeCategoryParam])
uncheckedSubTypeCategoryArgs g ps = update (Map.toList ps) where
  update [] t = return (t,getFreeParams t)
  update ((p,a):ps) t@(TypeCategoryArgParam pt) =
    if (tcpName pt) /= p
       then update ps t
       else do
         checkMissing pt a
         return (a,getFreeParams a)
  update ps (TypeCategoryArgType t) = do
    prunedArgs <- return $ Map.fromList $ ps `pruneArgs` (tciFreeParams t)
    (newArgs,newFree) <- foldr (subAll prunedArgs) (return ([],[])) (tciArgs t)
    updatedParams <- return $ (tciFreeParams t) `pruneParams` (Map.fromList ps)
    -- Append newFree after pruning in case an update added back the same param.
    newParams <- return $ flattenTypeCategoryParams $ updatedParams ++ newFree
    updated <- return $ TypeCategoryArgType $ TypeCategoryInstance {
        tciFreeParams = newParams,
        tciClassName = tciClassName t,
        tciArgs = newArgs
      }
    return (updated,getFreeParams updated)
  checkMissing p1 (TypeCategoryArgParam p2) = (tcpMissing p1) `checkMissingMatch` (tcpMissing p2)
  checkMissing p (TypeCategoryArgType t) = do
    missing <- return $ Map.findWithDefault UnspecifiedMissing (tciClassName t) (tcgMissing g)
    (tcpMissing p) `checkMissingMatch` missing
  getFreeParams (TypeCategoryArgParam p) = [p]
  getFreeParams (TypeCategoryArgType t)  = tciFreeParams t
  subAll ps t ts = do
    (prevNew,prevFree) <- ts
    (new,freeParams) <- uncheckedSubTypeCategoryArgs g ps t
    return (new:prevNew,freeParams ++ prevFree)
  -- Remove provided args from free params.
  pruneParams os ps = do
    o <- os
    guard $ not $ (tcpName o) `Map.member` ps
    return o
  -- Remove unused args from those provided.
  pruneArgs [] _ = []
  pruneArgs ((p,a):ps) os
    | any ((p ==) . tcpName) os = (p,a) : (pruneArgs ps os)
    | otherwise = pruneArgs ps os

getTypeCategoryPaths :: TypeCategoryRefinesMap -> Variance -> TypeCategoryName -> TypeCategoryName -> [[TypeCategoryArg]]
getTypeCategoryPaths m Contravariant x y = getTypeCategoryPaths m Covariant y x
getTypeCategoryPaths m Covariant x y
  | x == y = [[]] -- Empty path, not a lack of paths.
  | otherwise = expanded where
    expanded = do
      z <- Set.toList $ Map.findWithDefault Set.empty x m
      ts <- getTypeCategoryPaths m Covariant (tciClassName $ tcatInstance z) y
      return (z:ts)
getTypeCategoryPaths _ _ x y
  | x == y = [[]] -- Empty path, not a lack of paths.
  | otherwise = []

-- TODO: use foldM here.
flattenPath :: TypeCategoryGraph -> TypeCategoryArg -> [TypeCategoryArg] -> CompileInfo TypeCategoryArg
flattenPath _ (TypeCategoryArgParam _)   _     = compileError $ "Cannot convert a param"
flattenPath _ t@(TypeCategoryArgType _) []     = return t
flattenPath g (TypeCategoryArgType x)   (y:ys) = flattened where
  allParams = tcgParams g
  flattened = do
    params <- return $ (tciClassName x) `Map.lookup` allParams
    if isJust params
       then return ()
       else compileError $ "Type class '" ++ show (tciClassName x) ++ "' not found"
    args <- return $ Map.fromList $ zip (map tcpName $ fromJust params) (tciArgs x)
    (subbed,_) <- uncheckedSubTypeCategoryArgs g args y
    flattenPath g subbed ys

guessInstanceConversion :: TypeCategoryGraph -> Variance -> TypeCategoryArg -> TypeCategoryArg -> CompileInfo TypeCategoryArg
guessInstanceConversion g Contravariant t1 t2 =
  guessInstanceConversion g Covariant t2 t1
guessInstanceConversion g _ t1@(TypeCategoryArgType _)  t2@(TypeCategoryArgParam _) =
  checkInstanceConversion g t1 t2 >>= return
guessInstanceConversion g _ t1@(TypeCategoryArgParam _) t2@(TypeCategoryArgType _)  =
  checkInstanceConversion g t1 t2 >>= return
guessInstanceConversion g _ t1@(TypeCategoryArgParam p1) t2@(TypeCategoryArgParam p2)
  | (tcpName p1) == (tcpName p2) = checkInstanceConversion g t1 t2 >>= return
  | otherwise = compileError $ "Mismatch between params"
guessInstanceConversion g v t1@(TypeCategoryArgType x) t2@(TypeCategoryArgType y) = checked where
  allParams = tcgParams g
  allRefines = tcgRefines g
  checked = do
    paths <- return $ getTypeCategoryPaths allRefines v (tciClassName x) (tciClassName y)
    collectOneOrErrorM$
      (compileError $ "No conversions found from '" ++ show t1 ++ "' to '" ++ show t2 ++ "'")
      : (map (checkConversion . flattenPath g t1) paths)
  checkConversion = (>>= flip (checkInstanceConversion g) t2)

checkInstanceConversion :: TypeCategoryGraph -> TypeCategoryArg -> TypeCategoryArg -> CompileInfo TypeCategoryArg
checkInstanceConversion g t1 t2 = checked where
  checked = do
    checkMissing t1 t2
    checkConversion t1 t2
  checkMissing (TypeCategoryArgType t) (TypeCategoryArgParam p) = do
    m1 <- return $ tcpMissing p
    m2 <- return $ Map.findWithDefault UnspecifiedMissing (tciClassName t) (tcgMissing g)
    m1 `checkMissingMatch` m2
  checkMissing (TypeCategoryArgParam p2) (TypeCategoryArgParam p1) =
    (tcpMissing p1) `checkMissingMatch` (tcpMissing p2)
  checkMissing _ _ = return ()
  checkConversion t1 t2@(TypeCategoryArgParam p2) = checked where
    checked = do
      mergeAll $ map fullCheck $ Set.toList $ tcpFilters p2
      -- TODO: Can't decide if this should be t1 if t1 is a param. t1 will have
      -- at least as strict of filters, but should that be applied to the result?
      -- Maybe the filters should always be limited to what is required by the
      -- typeclass, rather than the current restrictions on the param.
      return t2
    fullCheck f = do
      checkInstanceConversion g t1 (TypeCategoryArgType $ tfType f)
      return ()
  checkConversion t1@(TypeCategoryArgParam p) t2@(TypeCategoryArgType y) = checked where
    checked = collectOneOrErrorM$
      (compileError $ "No filter guarantees conversion: " ++ show (Set.toList $ tcpFilters p) ++ " vs " ++ show t2)
      : (map check (Set.toList $ tcpFilters p))
    check f = do
      -- Leave the filter untouched for the first attempt. This is required
      -- because inserting the param into its own filters in some cases actually
      -- *removes* nested filter information. (For example, if the param is
      -- inherited from a type class.)
      checked <- return $ guessInstanceConversion g Covariant (TypeCategoryArgType $ tfType f) t2
      if isCompileError checked
        then do
          -- Insert the full param into its own filters in case multiple filters
          -- are needed at once inside of any one of the required filters.
          (updated,_) <- uncheckedSubTypeCategoryArgs g (Map.fromList [(tcpName p,t1)]) (TypeCategoryArgType $ tfType f)
          -- NOTE: This returns the result of conversion!
          guessInstanceConversion g Covariant updated t2
        -- NOTE: This returns the result of conversion!
        else checked
  checkConversion (TypeCategoryArgType x) t@(TypeCategoryArgType y)
    | x == y = return t
    | tciClassName x /= tciClassName y =
      compileError $ "Typeclass mismatch: '" ++ show (tciClassName x) ++ "' != '" ++ show (tciClassName y) ++ "'"
    | length (tciArgs x) /= length (tciArgs y) =
      compileError $ "Arg count: '" ++ show (tciArgs x) ++ "' vs '" ++ show (tciArgs y) ++ "'"
    | otherwise = checked params where
      params = (tciClassName x) `Map.lookup` (tcgParams g)
      checked Nothing   = compileError $ "Type class '" ++ show (tciClassName x) ++ "' not found"
      checked (Just ps) = do
        updated <- collectAllOrErrorM $ map check (zip3 (map tcpVariance ps) (tciArgs x) (tciArgs y))
        return $ TypeCategoryArgType $ TypeCategoryInstance {
            -- TODO: This should be updated from the args.
            tciFreeParams = tciFreeParams y,
            tciClassName = tciClassName y,
            tciArgs = updated
          }
      check (v,x,y) = guessInstanceConversion g v x y

-- TODO: This is temporary.
resolveTypeCategoryInstance :: TypeCategoryGraph -> Maybe String -> UnresolvedType -> CompileInfo (TypeCategoryArg, [TypeCategoryParam])
resolveTypeCategoryInstance g Nothing  u = tryTypeCategoryInstance True g Map.empty UnspecifiedMissing Set.empty u
resolveTypeCategoryInstance g (Just t) u = do
  params <- return $ (TypeCategoryName t) `Map.lookup` (tcgParams g)
  if isJust params
     then return ()
     else compileError $ "Unknown type class '" ++ t ++ "'"
  mapped <- return $ Map.fromList $ map (\p -> (tcpName p,p)) (fromJust params)
  tryTypeCategoryInstance True g mapped UnspecifiedMissing Set.empty u

-- TODO: This is temporary.
checkConversion :: TypeCategoryGraph -> (TypeCategoryArg, [TypeCategoryParam]) -> (TypeCategoryArg, [TypeCategoryParam]) -> CompileInfo ()
checkConversion g x y = guessInstanceConversion g Covariant (fst x) (fst y) >> return ()
