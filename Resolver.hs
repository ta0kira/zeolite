module Resolver (
  TypeClassGraph,
  TypeClassArg,
  createTypeClassGraph,
  resolveTypeClassInstance,
  -- For testing...
  TypeClassName,
  getTypeClassPaths,
  getTypePaths
) where

import Control.Arrow (second)
import Control.Monad (guard, join)
import Control.Monad.Fix (fix)
import Data.Either (partitionEithers)
import Data.List (intercalate)
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

-- TODO: Show tcgInherits.
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


-- TODO: Cool, but probably not needed.
composeGraph :: Ord a => [Map.Map a b -> (a, b)] -> Map.Map a b
composeGraph xs = fix (\m -> Map.fromList $ map ($m) xs)


checkTypeClassCycles :: Map.Map String [String] -> (String, [String]) -> [String]
checkTypeClassCycles m (n, is) = checked theError where
  theError = checkRecursion n (Set.fromList [n]) is
  checked (Left e) = [e]
  checked _        = []
  checkRecursion _ _  []     = Right ()
  checkRecursion n ts (i:is) = do
    maybeCycle <- return $ i `Set.member` ts
    if maybeCycle == True
       then Left $ "Inheritance cycle found for type '" ++ n ++ "'"
       else (Right ())
    maybeInherited <- return $ i `Map.lookup` m
    inherited <- if (isJust maybeInherited)
                    then (Right $ fromJust maybeInherited)
                    else Left $ "Type '" ++ i ++ "' not found, used in '" ++ n ++ "'"
    checkRecursion i (i `Set.insert` ts) inherited
    checkRecursion n ts is

-- TODO: Also check for *identical* inherits (including params).
getTypeClassInherits :: UnresolvedTypeClass -> Either String (String, [UnresolvedType])
getTypeClassInherits u = inherits >>= \ts -> return (name, ts) where
  name = utcName u
  inherits = foldr collectInherits (Right []) (utcInherits u)
  collectInherits (UnresolvedTypeArg p) _ =
    Left $ "Type '" ++ name ++ "' cannot inherit param '" ++ p ++ "'"
  collectInherits t c = do
    l <- c
    return $ t:l

-- TODO: Also check for duplicate type names!
getTypeClassCycleErrors :: [UnresolvedTypeClass] -> [String]
getTypeClassCycleErrors us = checked where
  (errors, types) = partitionEithers (map getTypeClassInherits us)
  typeTuples = map (second (map utTypeClass)) types
  typeMap = Map.fromList typeTuples
  checked = errors ++ (join $ map (checkTypeClassCycles typeMap) typeTuples)

updateTypeClassMap :: (TypeClassName, TypeClassName) -> TypeClassMap -> TypeClassMap
updateTypeClassMap (f, t)  m = updated where
  old = Map.findWithDefault Set.empty f m
  updated = Map.insert f (t `Set.insert` old) m

-- TODO: Also check for duplicate param names!
initialTypeClassParamMap :: [UnresolvedTypeClass] -> Either [String] TypeClassParamMap
initialTypeClassParamMap us = (Right params) where
  params = Map.fromList $ map collectParams us
  collectParams u = (tcnFromUTC u, map convertParam $ utcParams u)
  convertParam p = TypeClassParam {
    tcpName = tcpnFromUTP p,
    tcpVariance = utpVariance p,
    tcpFilters = Set.empty -- Resolved later on.
  }

createTypeClassGraph :: [UnresolvedTypeClass] -> Either [String] TypeClassGraph
createTypeClassGraph us = do
  errors <- return $ getTypeClassCycleErrors us
  if (null errors)
     then (Right ())
     else (Left errors)
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
  -- TODO: Validate inheritance w.r.t. filters and variance.
  updateTypeClassFilters initialGraph us

-- TODO: Rename this.
updateTypeClassFilters :: TypeClassGraph -> [UnresolvedTypeClass] -> Either [String] TypeClassGraph
updateTypeClassFilters g us = updated where
  oldParams = tcgParams g
  resolveFilters t []     = do
    oldParam <- return $ t `Map.lookup` oldParams
    if (isJust oldParam)
       then (Right $ fromJust oldParam)
       else Left ["Type '" ++ show t ++ "' not found"]
  resolveFilters t (f:fs) = do
    resolved <- resolveFilters t fs
    filter <- getTypeClassInstance g Set.empty (upfType f)
    checkParam (tcpnFromUPF f) (snd filter)
    filterType <- getFilterType (fst filter)
    updateSingleParam (tcpnFromUPF f) filterType resolved
  -- Make sure that the filter isn't another param.
  getFilterType (TypeClassArgType t) = return t
  getFilterType _ = Left ["Filter must be a type, not a param"]
  -- Ensures that the free params in the filter match what is expected.
  checkParam n [] = Right ()
  checkParam n (x:[])
    | n == tcpName x = Right ()
    | otherwise = Left ["Filter does not apply to param '" ++ show n ++ "'"]
  checkParam _ _ = Left ["Too many free params for filter"]
  -- Find the correct param in the list (for the type class) and update it.
  updateSingleParam n a [] = Left ["Param '" ++ show n ++ "' not found"]
  updateSingleParam n a (p:ps)
    | n /= tcpName p = do
      rest <- updateSingleParam n a ps
      return (p:rest)
    | otherwise = Right (update:ps) where
      update = TypeClassParam {
          tcpName = tcpName p,
          tcpVariance = tcpVariance p,
          tcpFilters = Set.insert (TypeFilter a) (tcpFilters p)
        }
  -- Update the map with all filter updates for a single type class.
  updateParams []     = Right oldParams
  updateParams (u:us) = do
    updatedMap <- updateParams us
    realFilters <- resolveFilters (tcnFromUTC u) (utcFilters u)
    return $ Map.insert (tcnFromUTC u) realFilters updatedMap
  -- Update the inheritance graph.
  updateInherits [] = return []
  updateInherits (u:us) = do
    rest <- updateInherits us
    inherits <- getInherits (utcInherits u)
    return $ (tcnFromUTC u,Set.fromList inherits):rest
  getInherits [] = return []
  getInherits (i:is) = do
    rest <- getInherits is
    inherit <- getTypeClassInstance g Set.empty i
    return (fst inherit:rest)
  -- The fully-updated graph.
  updated = do
    newParams <- updateParams us
    newInherits <- updateInherits us
    return $ TypeClassGraph {
        tcgParams = newParams,
        tcgGraph = tcgGraph g,
        tcgInherits = Map.fromList newInherits
      }

resolveTypeClassInstance :: TypeClassGraph -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
resolveTypeClassInstance g = getTypeClassInstance g Set.empty

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

getTypeClassInstance :: TypeClassGraph -> Set.Set TypeFilter -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
getTypeClassInstance g fs a@(UnresolvedTypeArg _) = (Right resolved) where
  resolved = (arg, [param])
  param = TypeClassParam {
      tcpName = tcpnFromUTA a,
      -- This variance is in the context of this particular TypeClassArg.
      tcpVariance = IgnoreVariance,
      tcpFilters = fs
    }
  arg = TypeClassArgParam {
      tcapParam = param
    }
getTypeClassInstance g fs u = result where
  name = utTypeClass u
  properName = tcnFromUT u
  updateVariance v p = TypeClassParam {
      tcpName = tcpName p,
      tcpVariance = v `composeVariance` (tcpVariance p),
      tcpFilters = tcpFilters p
    }
  collectTypeClassArgs []     []     = Right ([], [])
  collectTypeClassArgs (_:_)  []     = Left ["Too few args for type '" ++ name ++ "'"]
  collectTypeClassArgs []     (_:_)  = Left ["Too many args for type '" ++ name ++ "'"]
  collectTypeClassArgs (p:ps) (u:us) = do
    (args, params) <- collectTypeClassArgs ps us
    (arg, newParams) <- getTypeClassInstance g (tcpFilters p) u
    updatedParams <- return $ map (updateVariance $ tcpVariance p) newParams
    return (arg:args, updatedParams ++ params)
  result = do
    typeParams <- return $ properName `Map.lookup` (tcgParams g)
    if (isJust typeParams)
      then (Right ())
      else (Left ["Type class '" ++ name ++ "' not found"])
    (args, params) <- collectTypeClassArgs (fromJust typeParams) (utParamArgs u)
    -- TODO: Check that resolved can match all of the filters! (Or, just make
    -- that a second step after resolving the types.)
    resolved <- return $ TypeClassArgType {
        tcatInstance = TypeClassInstance {
          tciFreeParams = flattenTypeClassParams params,
          tciClassName = properName,
          tciArgs = args
        }
      }
    return (resolved, params)

uncheckedSubTypeClassArgs :: Map.Map TypeClassParamName TypeClassArg -> TypeClassArg -> Either [String] TypeClassArg
uncheckedSubTypeClassArgs ps = update (Map.toList ps) where
  update [] t = return t
  -- TODO: Also need to update free params based on a.
  update ((p,a):ps) t@(TypeClassArgParam pt) = do
    if (tcpName pt) == p
       then return a
       else update ps t
  update ps (TypeClassArgType t) = do
    prunedArgs <- return $ Map.fromList $ ps `pruneArgs` (tciFreeParams t)
    newArgs <- foldr (subAll prunedArgs) (Right []) (tciArgs t)
    return $ TypeClassArgType $ TypeClassInstance {
        -- TODO: Also need to update free params based recursion. (Specifically,
        -- accounting for free params in ps, and also variance updates, like is
        -- done in getTypeClassInstance.)
        tciFreeParams = (tciFreeParams t) `pruneParams` (Map.fromList ps),
        tciClassName = tciClassName t,
        tciArgs = newArgs
      }
  subAll ps t ts = do
    previous <- ts
    new <- uncheckedSubTypeClassArgs ps t
    return (new:previous)
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
  | x == y = return [] -- Empty path, not a lack of paths.
  | otherwise = expanded where
    expanded = do
      z <- Set.toList $ Map.findWithDefault Set.empty x m
      ts <- getTypeClassPaths m Covariant (tciClassName $ tcatInstance z) y
      return (z:ts)
getTypeClassPaths _ _ x y
  | x == y = return [] -- Empty path, not a lack of paths.
  | otherwise = []

checkStrictConversion :: TypeClassGraph -> Variance -> TypeClassArg -> TypeClassArg -> Either [String] TypeClassArg
checkStrictConversion _ _ (TypeClassArgType _) (TypeClassArgParam _) =
  Left ["Cannot convert param to instance"]
checkStrictConversion _ _ (TypeClassArgParam _) (TypeClassArgType _) =
  Left ["Cannot convert instance to param"]
checkStrictConversion _ _ (TypeClassArgParam p1) t@(TypeClassArgParam p2)
  -- TODO: Actually check the filters.
  | p1 == p2 = return t
  | otherwise = Left ["Mismatch between params"]
-- TODO: Implement this.
-- checkStrictConversion g v (TypeClassArgType t1) (TypeClassArgType t2) = checked where
--   allParams <- tcgParams g
--   allInherits <- tcgInherits g
--   checked = do
--     paths <- getTypeClassPaths (tcgGraph g) v (tciClassName t1) (tciClassName t2)
--     checkPaths paths
--   checkPaths = foldr firstPath (Left ["No paths found"]) . map (checkPath t1 . tail)
--   checkPathInherits
--   checkPath x [] = return x
--   checkPath x (y:ys) = do
--     params <- return $ (tciClassName x) `Map.lookup` allParams
--     template <- return $ y `Map.lookup` allInherits
--     if (isJust params)
--        then (Right ())
--        else Left "Type class not found"
--     args <- return $ Map.fromList $ zip (map tcpName $ fromJust params) (tciArgs x)
--     updated <- uncheckedSubTypeClassArgs args $
--       TypeClassArgType $ TypeClassInstance {
--           tciFreeParams
--           tciClassName = y,
--           tci
--         }

-- TODO: This is temporary.
getTypePaths :: TypeClassGraph -> (TypeClassArg,a) -> (TypeClassArg,a) -> [[TypeClassArg]]
getTypePaths g x y = getTypeClassPaths (tcgInherits g)
                                       Covariant
                                       (tciClassName $ tcatInstance $ fst x)
                                       (tciClassName $ tcatInstance $ fst y)
