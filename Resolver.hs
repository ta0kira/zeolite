module Resolver (
  TypeClassGraph,
  TypeClassArg,
  createTypeClassGraph,
  resolveTypeClassInstance,
  -- For testing...
  TypeClassName,
  checkConversion,
  checkAllFilters
) where

import Control.Arrow (second)
import Control.Monad (guard, join, liftM3)
import Control.Monad.Fix (fix)
import Data.Either (isLeft, isRight, partitionEithers)
import Data.List (group, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Unresolved
import Variance


data TypeClassName =
  TypeClassName {
    tcnName :: String
  }
  deriving (Eq, Ord)

instance Show TypeClassName where
  show (TypeClassName n) = n

tcnFromUTC :: UnresolvedTypeClass -> TypeClassName
tcnFromUTC = TypeClassName . utcName

tcnFromUT :: UnresolvedType -> TypeClassName
tcnFromUT = TypeClassName . utTypeClass


data TypeClassParamName =
  TypeClassParamName {
    tcpnName :: String
  }
  deriving (Eq, Ord)

instance Show TypeClassParamName where
  show (TypeClassParamName n) = n

tcpnFromUTP :: UnresolvedTypeParam -> TypeClassParamName
tcpnFromUTP = TypeClassParamName . utpName

tcpnFromUTA :: UnresolvedType -> TypeClassParamName
tcpnFromUTA = TypeClassParamName . utaName

tcpnFromUPF :: UnresolvedParamFilter -> TypeClassParamName
tcpnFromUPF = TypeClassParamName . upfName


data TypeFilter =
  TypeFilter {
    tfType :: TypeClassInstance
  }
  deriving (Eq, Ord)

instance Show TypeFilter where
  show (TypeFilter t) = show t


data TypeClassParam =
  TypeClassParam {
    tcpName :: TypeClassParamName,
    tcpVariance :: Variance,
    tcpFilters :: Set.Set TypeFilter
  }
  deriving (Eq, Ord)

instance Show TypeClassParam where
  show (TypeClassParam n v f) = (showName v) ++ (showFilters f) where
    showName Contravariant = "+" ++ show n
    showName Covariant     = show n ++ "+"
    showName _             = show n
    showFilters fs
      | null (Set.toList fs) = ""
      | otherwise = " -> " ++ show (Set.toList fs)


data TypeClassInstance =
  TypeClassInstance {
    -- TODO: Is this needed? It's tedious to keep up to date. Maybe it can be
    -- used to prune the substitution list before traversing. A map might
    -- actually be easier to work with and update.
    tciFreeParams :: [TypeClassParam], -- TODO: Make this a set?
    tciClassName :: TypeClassName,
    tciArgs :: [TypeClassArg]
  }
  deriving (Eq, Ord)

instance Show TypeClassInstance where
  show (TypeClassInstance p n a) = show n ++ (showArgs a) ++ (showParams p) where
    showArgs [] = ""
    showArgs as = "<" ++ intercalate "," (map show as) ++ ">"
    showParams [] = ""
    showParams ps = " " ++ show ps


data TypeClassArg =
  TypeClassArgType {
    tcatInstance :: TypeClassInstance
  } |
  TypeClassArgParam {
    tcapParam :: TypeClassParam
  }
  deriving (Eq, Ord)

instance Show TypeClassArg where
  show (TypeClassArgType t)  = show t
  show (TypeClassArgParam p) = show p


data TypeClassGraph =
  TypeClassGraph {
    tcgParams :: TypeClassParamMap,
    tcgGraph :: TypeClassMap,
    tcgInherits :: TypeClassInheritsMap
  }
  deriving (Eq)

instance Show TypeClassGraph where
  show (TypeClassGraph ps gs is) = showParams ++ "\n" ++
                                   showGraph ++ "\n" ++
                                   showInherits where
    showParams = "params {\n" ++ join params ++ "}"
    params = do
      (n,p) <- Map.toList ps
      ["  " ++ show n ++ " ~ " ++ show p ++ "\n"]
    showGraph = "typeclass graph {\n" ++ join simpleInherits ++ "}"
    simpleInherits = do
      (n,is) <- Map.toList gs
      ["  " ++ show n ++ " -> " ++ show (Set.toList is) ++ "\n"]
    showInherits = "typeclass inherits {\n" ++ join fullInherits ++ "}"
    fullInherits = do
      (n,ts) <- Map.toList is
      t <- Set.toList ts
      ["  " ++ show n ++ " -> " ++ show t ++ "\n"]


type TypeClassParamMap = Map.Map TypeClassName [TypeClassParam]

type TypeClassMap = Map.Map TypeClassName (Set.Set TypeClassName)

type TypeClassInheritsMap = Map.Map TypeClassName (Set.Set TypeClassArg)


requireAll :: [Either [e] a] -> Either [e] [a]
requireAll = result . partitionEithers where
  result ([],xs) = return xs
  result (es,_)  = Left $ join es

requireAll_ :: [Either [e] a] -> Either [e] ()
requireAll_ = (>> return ()) . requireAll

requireAny :: [Either [e] a] -> Either [e] a
requireAny = result . partitionEithers where
  result (_,x:_) = return x
  result (es,_)  = Left $ join es

requireAny_ :: [Either [e] a] -> Either [e] ()
requireAny_ = (>> return ()) . requireAny


checkTypeClassCycles :: Map.Map String [String] -> (String, [String]) -> Either [String] ()
checkTypeClassCycles m (n,is) = checked where
  checked = requireAll_ $ map (checkRecursion n $ Set.fromList [n]) is
  checkRecursion n ts i = do
    if i `Set.member` ts
       then Left ["Inheritance cycle found for type '" ++ n ++ "'"]
       else return ()
    maybeInherited <- return $ i `Map.lookup` m
    inherited <- if isJust maybeInherited
                    then return $ fromJust maybeInherited
                    else Left ["Type '" ++ i ++ "' not found, used in '" ++ n ++ "'"]
    -- TODO: This is quadratic and might not scale well.
    requireAll_ $ map (checkRecursion i $ i `Set.insert` ts) inherited

-- TODO: Also check for *identical* inherits (including params).
getTypeClassInherits :: UnresolvedTypeClass -> Either [String] (String, [UnresolvedType])
getTypeClassInherits u = inherits >>= \ts -> return (name, ts) where
  name = utcName u
  inherits = foldr collectInherits (return []) (utcInherits u)
  collectInherits (UnresolvedTypeArg p) _ =
    Left ["Type '" ++ name ++ "' cannot inherit param '" ++ p ++ "'"]
  collectInherits t c = do
    l <- c
    return $ t:l

checkDuplicates :: (Eq a, Show a) => [a] -> Either [String] ()
checkDuplicates xs = requireAll_ $ map check $ group xs where
  check (x:y:_) = Left ["Duplicate definition of '" ++ show x ++ "'"]
  check _       = return ()

checkAllTypeClassCycles :: [UnresolvedTypeClass] -> Either [String] ()
checkAllTypeClassCycles us = do
  types <- requireAll $ map getTypeClassInherits us
  typeTuples <- return $ map (second (map utTypeClass)) types
  typeMap <- return $ Map.fromList typeTuples
  requireAll_ $ map (checkTypeClassCycles typeMap) typeTuples

updateTypeClassMap :: (TypeClassName, TypeClassName) -> TypeClassMap -> TypeClassMap
updateTypeClassMap (f, t)  m = updated where
  old = Map.findWithDefault Set.empty f m
  updated = Map.insert f (t `Set.insert` old) m

initialTypeClassParamMap :: [UnresolvedTypeClass] -> Either [String] TypeClassParamMap
initialTypeClassParamMap us = allParams where
  allParams = do
    checked <- requireAll $ map collectParams us
    return $ Map.fromList checked
  collectParams u = do
    checkDuplicates $ map tcpnFromUTP $ utcParams u
    return (tcnFromUTC u, map convertParam $ utcParams u)
  convertParam p = TypeClassParam {
    tcpName = tcpnFromUTP p,
    tcpVariance = utpVariance p,
    tcpFilters = Set.empty -- Resolved later on.
  }

-- TODO: Update this and updateTypeClassGraph so that an additional "module" of
-- unresolved type classes can be appended to the graph.
createTypeClassGraph :: [UnresolvedTypeClass] -> Either [String] TypeClassGraph
createTypeClassGraph us = do
  checkAllTypeClassCycles us
  checkDuplicates $ map utcName us
  edges <- return $ do
    u <- us
    t <- utcInherits u
    return (tcnFromUTC u, tcnFromUT t)
  defaultMap <- return $ Map.fromList $ zip (map tcnFromUTC us) (repeat Set.empty)
  initialParams <- initialTypeClassParamMap us
  initialGraph <- return TypeClassGraph {
      tcgParams = initialParams,
      tcgGraph = foldr updateTypeClassMap defaultMap edges,
      tcgInherits = Map.empty
    }
  updateTypeClassGraph initialGraph us

updateTypeClassGraph :: TypeClassGraph -> [UnresolvedTypeClass] -> Either [String] TypeClassGraph
updateTypeClassGraph g us = updated where
  oldParams = tcgParams g
  resolveFilters t []     = do
    oldParam <- return $ t `Map.lookup` oldParams
    if isJust oldParam
       then return (fromJust oldParam)
       else Left ["Type '" ++ show t ++ "' not found"]
  resolveFilters t (f:fs) = do
    resolved <- resolveFilters t fs
    filter <- tryTypeClassInstance True g Map.empty Set.empty (upfType f)
    checkParam (tcpnFromUPF f) (snd filter)
    filterType <- getFilterType (fst filter)
    updateSingleParam (tcpnFromUPF f) filterType resolved
  -- Make sure that the filter isn't another param.
  getFilterType (TypeClassArgType t) = return t
  getFilterType _ = Left ["Filter must be a type, not a param"]
  -- Ensures that the free params in the filter match what is expected.
  checkParam n [] = return ()
  checkParam n (x:[])
    | n == tcpName x = return ()
    | otherwise = Left ["Filter does not apply to param '" ++ show n ++ "'"]
  checkParam _ _ = Left ["Too many free params for filter"]
  -- Find the correct param in the list (for the type class) and update it.
  updateSingleParam n a [] = Left ["Param '" ++ show n ++ "' not found"]
  updateSingleParam n a (p:ps)
    | n /= tcpName p = do
      rest <- updateSingleParam n a ps
      return (p:rest)
    | otherwise = return (update:ps) where
      update = TypeClassParam {
          tcpName = tcpName p,
          tcpVariance = tcpVariance p,
          tcpFilters = Set.insert (TypeFilter a) (tcpFilters p)
        }
  -- Update the map with all filter updates for a single type class.
  updateParams []     = return oldParams
  updateParams (u:us) = do
    updatedMap <- updateParams us
    realFilters <- resolveFilters (tcnFromUTC u) (utcFilters u)
    return $ Map.insert (tcnFromUTC u) realFilters updatedMap
  -- Update the inheritance graph.
  updateInherits g2 [] = return []
  updateInherits g2 (u:us) = do
    rest <- updateInherits g2 us
    params <- return $ Map.findWithDefault [] (tcnFromUTC u) (tcgParams g2)
    extras <- return $ Map.fromList $ map (\p -> (tcpName p, tcpFilters p)) params
    inherits <- getInherits g2 extras (utcInherits u)
    return $ (tcnFromUTC u,Set.fromList inherits):rest
  getInherits g2 e [] = return []
  getInherits g2 e (i:is) = do
    rest <- getInherits g2 e is
    -- Filters cannot be checked here because that depends on the graph having
    -- updated inherits. Filters are validated later with validateParamFilters.
    inherit <- tryTypeClassInstance False g2 e Set.empty i
    return (fst inherit:rest)
  -- The fully-updated graph.
  updated = do
    newParams <- updateParams us
    partial <- return $ TypeClassGraph {
        tcgParams = newParams,
        tcgGraph = tcgGraph g,
        tcgInherits = Map.empty
      }
    newInherits <- updateInherits partial us
    full <- return $ TypeClassGraph {
        tcgParams = newParams,
        tcgGraph = tcgGraph g,
        tcgInherits = Map.fromList newInherits
      }
    validateParamVariance full
    validateParamFilters full
    return full

validateParamVariance :: TypeClassGraph -> Either [String] ()
validateParamVariance g = checked where
  checked = requireAll_ $ map checkVariances (Map.toList $ tcgInherits g)
  checkVariances (t,is) = requireAll_ $ map (checkVariance t) (Set.toList is)
  checkVariance t i = do
    -- Params available from the typeclass doing the inheriting.
    -- TODO: Missing value should be an error.
    params <- return $ Map.findWithDefault [] t (tcgParams g)
    required <- return $ Map.fromList $ mapParams params
    -- Params as used in inheritance.
    freeParams <- return $ mapParams $ tciFreeParams $ tcatInstance i
    requireAll_ $ map (check (t,tcatInstance i) required) freeParams
  check (t,i) m (p,v1) = do
    v0 <- return $ p `Map.lookup` m
    if isJust v0
       then return ()
       else Left ["Param '" ++ show p ++ "' does not exist"]
    if (fromJust v0) `paramAllowsUsage` v1
       then return ()
       else Left ["Param '" ++ show p ++ "' cannot be " ++
                  show v1 ++ " in '" ++ show t ++ "' -> '" ++ show i ++ "'"]
  mapParams = map (\p -> (tcpName p,tcpVariance p))

-- TODO: Get rid of this duplication with validateParamVariance.
validateParamFilters :: TypeClassGraph -> Either [String] ()
validateParamFilters g = checked where
  checked = requireAll_ $ map checkFilters (Map.toList $ tcgInherits g)
  checkFilters (t,is) = requireAll_ $ map (checkFilter t) (Set.toList is)
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

checkAllFilters :: TypeClassGraph -> String ->
                   Map.Map TypeClassParamName (Set.Set TypeFilter) ->
                   [(TypeClassParamName, (Set.Set TypeFilter))] -> Either [String] ()
checkAllFilters g e m ps = requireAll_ $ map checkFilter ps where
  checkFilter (p,fs) = do
    f0 <- return $ p `Map.lookup` m
    if isJust f0
      then return ()
      else Left ["Param '" ++ show p ++ "' does not exist"]
    requireAll_ $ map (findCompatFilter p (Set.toList $ fromJust f0)) (Set.toList fs)
  findCompatFilter p rs a = do
    requireAny_ $
      (Left ["Param '" ++ show p ++ "' does not support required filter " ++ show a ++ e])
      : (map (check a) rs)
  check a r = guessInstanceConversion g Covariant
                (TypeClassArgType $ tfType r) (TypeClassArgType $ tfType a)

flattenTypeClassParams :: [TypeClassParam] -> [TypeClassParam]
flattenTypeClassParams = map snd . Map.toList . foldr process Map.empty where
  process x m = update previous where
    name = tcpName x
    previous = Map.lookup name m
    update Nothing  = Map.insert name x m
    update (Just y) = flip (Map.insert name) m $
      TypeClassParam {
        tcpName = name,
        tcpVariance = (tcpVariance x) `composeVariance` (tcpVariance y),
        tcpFilters = (tcpFilters x) `Set.union` (tcpFilters y)
      }

-- NOTE: This only works because filters can only have one free param each.
renameFilters :: TypeClassParamName -> Set.Set TypeFilter -> Set.Set TypeFilter
renameFilters n fs = Set.fromList $ map renameFilter $ Set.toList fs where
  renameFilter f = TypeFilter {
      tfType = renameTypeInstance $ tfType f
    }
  renameTypeInstance t = TypeClassInstance {
      tciFreeParams = map renameParam $ tciFreeParams t,
      tciClassName = tciClassName t,
      tciArgs = map renameArg $ tciArgs t
    }
  renameParam p = TypeClassParam {
      tcpName = n,
      tcpVariance = tcpVariance p,
      tcpFilters = renameFilters n $ tcpFilters p
    }
  renameArg (TypeClassArgType t) = TypeClassArgType {
      tcatInstance = renameTypeInstance t
    }
  renameArg (TypeClassArgParam p) = TypeClassArgParam {
      tcapParam = renameParam p
    }

tryTypeClassInstance :: Bool -> TypeClassGraph ->
                        Map.Map TypeClassParamName (Set.Set TypeFilter) ->
                        Set.Set TypeFilter ->
                        UnresolvedType ->
                        Either [String] (TypeClassArg, [TypeClassParam])
tryTypeClassInstance c g pm fs a@(UnresolvedTypeArg _) = resolved where
  resolved = do
    if c
      then checkInstanceConversion g (TypeClassArgParam external) (TypeClassArgParam param) >> return ()
      else return ()
    return (arg, [param])
  -- TODO: Make it a failure for this to be not found?
  extras = Map.findWithDefault Set.empty (tcpnFromUTA a) pm
  newFilters = renameFilters (tcpnFromUTA a) fs
  external = TypeClassParam {
      tcpName = tcpnFromUTA a,
      tcpVariance = IgnoreVariance,
      tcpFilters = extras
    }
  param = TypeClassParam {
      tcpName = tcpnFromUTA a,
      -- This variance is in the context of this particular TypeClassArg.
      tcpVariance = IgnoreVariance,
      tcpFilters = newFilters `Set.union` extras
    }
  arg = TypeClassArgParam {
      tcapParam = param
    }
tryTypeClassInstance c g pm fs u = result where
  name = utTypeClass u
  properName = tcnFromUT u
  updateVariance v p = TypeClassParam {
      tcpName = tcpName p,
      tcpVariance = v `composeVariance` (tcpVariance p),
      tcpFilters = tcpFilters p
    }
  checkSize ps us
    | length ps > length us = Left ["Too few args for type '" ++ name ++ "'"]
    | length ps < length us = Left ["Too many args for type '" ++ name ++ "'"]
    | otherwise = return ()
  collectTypeClassArgs ps us = do
    checkSize ps us
    aps <- requireAll $ map collectArgs (zip ps us)
    return $ (map fst aps,join $ map snd aps)
  collectArgs (p,u) = do
    (arg,newParams) <- tryTypeClassInstance c g pm (tcpFilters p) u
    updatedParams <- return $ map (updateVariance $ tcpVariance p) newParams
    return (arg,updatedParams)
  result = do
    typeParams <- return $ properName `Map.lookup` (tcgParams g)
    if isJust typeParams
      then return ()
      else Left ["Type class '" ++ name ++ "' not found"]
    (args, params) <- collectTypeClassArgs (fromJust typeParams) (utParamArgs u)
    resolved <- return $ TypeClassInstance {
        tciFreeParams = flattenTypeClassParams params,
        tciClassName = properName,
        tciArgs = args
      }
    if c
       then checkInstanceFilters g resolved
       else return ()
    return (TypeClassArgType resolved,params)

checkTypeFilters :: TypeClassGraph -> [TypeClassParam] -> [TypeClassArg] -> Either [String] ()
checkTypeFilters g ps as = requireAll_ $ map check (zip ps as) where
  check (p,a) = requireAll_ $ map (checkFilters (tcpName p,a) a) $
                  map (TypeClassArgType . tfType) $ Set.toList $ tcpFilters p
  checkFilters s a f = do
    -- Sub a into the filter, since we're also subbing on the left side. For
    -- example, with x = N<y> the check x -> T<x> becomes N<y> -> T<N<y>>.
    (subbed,_) <- uncheckedSubTypeClassArgs (Map.fromList [s]) f
    guessInstanceConversion g Covariant a subbed

checkInstanceFilters :: TypeClassGraph -> TypeClassInstance -> Either [String] ()
checkInstanceFilters g (TypeClassInstance _ t as) = checked where
  allParams = tcgParams g
  checked = do
    params <- return $ t `Map.lookup` allParams
    if isJust params
       then checkTypeFilters g (fromJust params) as
       else Left ["Type '" ++ show t ++ "' does not exist"]

uncheckedSubTypeClassArgs :: Map.Map TypeClassParamName TypeClassArg -> TypeClassArg -> Either [String] (TypeClassArg, [TypeClassParam])
uncheckedSubTypeClassArgs ps = update (Map.toList ps) where
  update [] t = return (t,getFreeParams t)
  update ((p,a):ps) t@(TypeClassArgParam pt) = do
    if (tcpName pt) == p
       then return (a,getFreeParams a)
       else update ps t
  update ps (TypeClassArgType t) = do
    prunedArgs <- return $ Map.fromList $ ps `pruneArgs` (tciFreeParams t)
    (newArgs,newFree) <- foldr (subAll prunedArgs) (return ([],[])) (tciArgs t)
    updatedParams <- return $ (tciFreeParams t) `pruneParams` (Map.fromList ps)
    -- Append newFree after pruning in case an update added back the same param.
    newParams <- return $ flattenTypeClassParams $ updatedParams ++ newFree
    updated <- return $ TypeClassArgType $ TypeClassInstance {
        tciFreeParams = newParams,
        tciClassName = tciClassName t,
        tciArgs = newArgs
      }
    return (updated,getFreeParams updated)
  getFreeParams (TypeClassArgParam p) = [p]
  getFreeParams (TypeClassArgType t)  = tciFreeParams t
  subAll ps t ts = do
    (prevNew,prevFree) <- ts
    (new,freeParams) <- uncheckedSubTypeClassArgs ps t
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

getTypeClassPaths :: TypeClassInheritsMap -> Variance -> TypeClassName -> TypeClassName -> [[TypeClassArg]]
getTypeClassPaths m Contravariant x y = getTypeClassPaths m Covariant y x
getTypeClassPaths m Covariant x y
  | x == y = [[]] -- Empty path, not a lack of paths.
  | otherwise = expanded where
    expanded = do
      z <- Set.toList $ Map.findWithDefault Set.empty x m
      ts <- getTypeClassPaths m Covariant (tciClassName $ tcatInstance z) y
      return (z:ts)
getTypeClassPaths _ _ x y
  | x == y = [[]] -- Empty path, not a lack of paths.
  | otherwise = []

flattenPath :: TypeClassGraph -> TypeClassArg -> [TypeClassArg] -> Either [String] TypeClassArg
flattenPath _ (TypeClassArgParam _)   _     = Left ["Cannot convert a param"]
flattenPath _ t@(TypeClassArgType _) []     = return t
flattenPath g (TypeClassArgType x)   (y:ys) = flattened where
  allParams = tcgParams g
  flattened = do
    params <- return $ (tciClassName x) `Map.lookup` allParams
    if isJust params
       then return ()
       else Left ["Type class '" ++ show (tciClassName x) ++ "' not found"]
    args <- return $ Map.fromList $ zip (map tcpName $ fromJust params) (tciArgs x)
    (subbed,_) <- uncheckedSubTypeClassArgs args y
    flattenPath g subbed ys

guessInstanceConversion :: TypeClassGraph -> Variance -> TypeClassArg -> TypeClassArg -> Either [String] TypeClassArg
guessInstanceConversion g Contravariant t1 t2 =
  guessInstanceConversion g Covariant t2 t1
guessInstanceConversion g _ t1@(TypeClassArgType _)  t2@(TypeClassArgParam _) =
  checkInstanceConversion g t1 t2 >>= return
guessInstanceConversion g _ t1@(TypeClassArgParam _) t2@(TypeClassArgType _)  =
  checkInstanceConversion g t1 t2 >>= return
guessInstanceConversion g _ t1@(TypeClassArgParam p1) t2@(TypeClassArgParam p2)
  | (tcpName p1) == (tcpName p2) = checkInstanceConversion g t1 t2 >>= return
  | otherwise = Left ["Mismatch between params"]
guessInstanceConversion g v t1@(TypeClassArgType x) t2@(TypeClassArgType y) = checked where
  allParams = tcgParams g
  allInherits = tcgInherits g
  checked = do
    paths <- return $ getTypeClassPaths allInherits v (tciClassName x) (tciClassName y)
    requireAny $
      (Left ["No conversions found from '" ++ show t1 ++ "' to '" ++ show t2 ++ "'"])
      : (map (checkConversion . flattenPath g t1) paths)
  checkConversion = (>>= flip (checkInstanceConversion g) t2)

checkInstanceConversion :: TypeClassGraph -> TypeClassArg -> TypeClassArg -> Either [String] TypeClassArg
checkInstanceConversion g t1 t2@(TypeClassArgParam p2) = checked where
  checked = do
    requireAll_ $ map fullCheck $ Set.toList $ tcpFilters p2
    -- TODO: Can't decide if this should be t1 if t1 is a param. t1 will have
    -- at least as strict of filters, but should that be applied to the result?
    -- Maybe the filters should always be limited to what is required by the
    -- typeclass, rather than the current restrictions on the param.
    return t2
  fullCheck f = checkInstanceConversion g t1 (TypeClassArgType $ tfType f)
checkInstanceConversion g t1@(TypeClassArgParam p) t2@(TypeClassArgType y) = checked where
  checked = requireAny $
    (Left ["No filter guarantees conversion: " ++ show (Set.toList $ tcpFilters p) ++ " vs " ++ show t2])
    : (map check (Set.toList $ tcpFilters p))
  check f = do
    -- Leave the filter untouched for the first attempt. This is required
    -- because inserting the param into its own filters in some cases actually
    -- *removes* nested filter information. (For example, if the param is
    -- inherited from a type class.)
    checked <- return $ guessInstanceConversion g Covariant (TypeClassArgType $ tfType f) t2
    if isRight checked
      -- NOTE: This returns the result of conversion!
      then checked
      else do
        -- Insert the full param into its own filters in case multiple filters
        -- are needed at once inside of any one of the required filters.
        (updated,_) <- uncheckedSubTypeClassArgs (Map.fromList [(tcpName p,t1)]) (TypeClassArgType $ tfType f)
        -- NOTE: This returns the result of conversion!
        guessInstanceConversion g Covariant updated t2
checkInstanceConversion g (TypeClassArgType x) t@(TypeClassArgType y)
  | x == y = return t
  | tciClassName x /= tciClassName y =
    Left ["Typeclass mismatch: '" ++ show (tciClassName x) ++ "' != '" ++ show (tciClassName y) ++ "'"]
  | length (tciArgs x) /= length (tciArgs y) =
    Left ["Arg count: '" ++ show (tciArgs x) ++ "' vs '" ++ show (tciArgs y) ++ "'"]
  | otherwise = checked params where
    params = (tciClassName x) `Map.lookup` (tcgParams g)
    checked Nothing   = Left ["Type class '" ++ show (tciClassName x) ++ "' not found"]
    checked (Just ps) = do
      updated <- requireAll $ map check (zip3 (map tcpVariance ps) (tciArgs x) (tciArgs y))
      return $ TypeClassArgType $ TypeClassInstance {
          -- TODO: This should be updated from the args.
          tciFreeParams = tciFreeParams y,
          tciClassName = tciClassName y,
          tciArgs = updated
        }
    check (v,x,y) = guessInstanceConversion g v x y

-- TODO: This is temporary.
resolveTypeClassInstance :: TypeClassGraph -> Maybe String -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
resolveTypeClassInstance g Nothing  u = tryTypeClassInstance True g Map.empty Set.empty u
resolveTypeClassInstance g (Just t) u = do
  params <- return $ (TypeClassName t) `Map.lookup` (tcgParams g)
  if isJust params
     then return ()
     else Left ["Unknown type class '" ++ t ++ "'"]
  mapped <- return $ Map.fromList $ map (\p -> (tcpName p,tcpFilters p)) (fromJust params)
  tryTypeClassInstance True g mapped Set.empty u

-- TODO: This is temporary.
checkConversion :: TypeClassGraph -> (TypeClassArg, [TypeClassParam]) -> (TypeClassArg, [TypeClassParam]) -> Either [String] ()
checkConversion g x y = guessInstanceConversion g Covariant (fst x) (fst y) >> return ()
