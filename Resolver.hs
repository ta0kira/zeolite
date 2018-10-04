module Resolver (
  TypeClassGraph(..),
  createTypeClassGraph,
  resolveTypeClassInstance
) where

import Control.Arrow (second)
import Control.Monad (guard, join)
import Control.Monad.Fix (fix)
import Data.Either (partitionEithers)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Unresolved
import Variance


-- TODO: Put this in a separate file and don't expose the constructor.
data TypeClassName =
  TypeClassName {
    tcnName :: String
  }
  deriving (Eq, Ord, Read, Show)

-- TODO: Put this in a separate file and don't expose the constructor.
data TypeClassParamName =
  TypeClassParamName {
    tcpnName :: String
  }
  deriving (Eq, Ord, Read, Show)

data TypeFilter =
  TypeFilter {
    tfType :: TypeClassInstance
  }
  deriving (Eq, Read, Show)

data TypeClassInstance =
  TypeClassInstance {
    tciFreeParams :: [TypeClassParam],
    tciClassName :: TypeClassName,
    tciArgs :: [TypeClassArg]
  }
  deriving (Eq, Read, Show)

data TypeClassParam =
  TypeClassParam {
    tcpName :: TypeClassParamName,
    tcpVariance :: Variance,
    tcpFilters :: [TypeFilter]
  }
  deriving (Eq, Read, Show)

data TypeClassArg =
  TypeClassArgType {
    tcatInstance :: TypeClassInstance
  } |
  TypeClassArgParam {
    tcapParam :: TypeClassParam
  }
  deriving (Eq, Read, Show)

data TypeClassConverter =
  TypeClassConverter {
    tccFrom :: TypeClassName,
    tccTo :: TypeClassName,
    tccConvert :: TypeClassInstance -> TypeClassInstance
  }


type TypeClassParamMap = Map.Map TypeClassName [TypeClassParam]

type TypeClassMap = Map.Map TypeClassName (Set.Set TypeClassName)

data TypeClassGraph =
  TypeClassGraph {
    tcgParams :: TypeClassParamMap,
    tcgGraphContravariant :: TypeClassMap,
    tcgGraphInvariant :: TypeClassMap,
    tcgGraphCovariant :: TypeClassMap
  }
  deriving (Eq, Read, Show)


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

updateTypeClassMap :: Variance -> (TypeClassName, TypeClassName) -> TypeClassMap -> TypeClassMap
updateTypeClassMap Contravariant (f, t) m = updated where
  old = Map.findWithDefault Set.empty t m
  updated = Map.insert t (f `Set.insert` old) m
updateTypeClassMap Invariant    (f, _)  m = updated where
  old = Map.findWithDefault Set.empty f m
  updated = Map.insert f old m
updateTypeClassMap Covariant    (f, t)  m = updated where
  old = Map.findWithDefault Set.empty f m
  updated = Map.insert f (t `Set.insert` old) m
updateTypeClassMap _ _ m = m

-- TODO: Also check for duplicate param names!
initialTypeClassParamMap :: [UnresolvedTypeClass] -> Either [String] TypeClassParamMap
initialTypeClassParamMap us = (Right params) where
  params = Map.fromList $ map collectParams us
  collectParams u = (TypeClassName (utcName u), map convertParam $ utcParams u)
  convertParam p = TypeClassParam {
    tcpName = TypeClassParamName (utpName p),
    tcpVariance = utpVariance p,
    tcpFilters = [] -- Resolved later on.
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
    return (TypeClassName (utcName u), TypeClassName (utTypeClass t))
  defaultMap <- return $ Map.fromList $ zip (map (TypeClassName . utcName) us) (repeat Set.empty)
  initialParams <- initialTypeClassParamMap us
  initialGraph <- return TypeClassGraph {
      tcgParams = initialParams,
      tcgGraphContravariant = foldr (updateTypeClassMap Contravariant) defaultMap edges,
      tcgGraphInvariant     = foldr (updateTypeClassMap Invariant)     defaultMap edges,
      tcgGraphCovariant     = foldr (updateTypeClassMap Covariant)     defaultMap edges
    }
  -- TODO: Validate inheritance w.r.t. filters and variance.
  updateTypeClassFilters initialGraph us

updateTypeClassFilters :: TypeClassGraph -> [UnresolvedTypeClass] -> Either [String] TypeClassGraph
updateTypeClassFilters g us = updated where
  oldParams = tcgParams g
  resolveFilters t []     = do
    oldParam <- return $ t `Map.lookup` oldParams
    if (isJust oldParam)
       then (Right $ fromJust oldParam)
       else Left ["Type '" ++ tcnName t ++ "' not found"]
  resolveFilters t (f:fs) = do
    resolved <- resolveFilters t fs
    filter <- getTypeClassInstance g [] (upfType f)
    checkParam (upfName f) (snd filter)
    filterType <- getFilterType (fst filter)
    updateSingleParam (upfName f) filterType resolved
  -- Make sure that the filter isn't another param.
  getFilterType (TypeClassArgType t) = return t
  getFilterType _ = Left ["Filter must be a type, not a param"]
  -- Ensures that the free params in the filter match what is expected.
  checkParam n [] = Right ()
  checkParam n (x:[])
    | n == (tcpnName $ tcpName x) = Right ()
    | otherwise = Left ["Filter does not apply to param '" ++ n ++ "'"]
  checkParam _ _ = Left ["Too many free params for filter"]
  -- Find the correct param in the list (for the type class) and update it.
  updateSingleParam n a [] = Left ["Param '" ++ n ++ "' not found"]
  updateSingleParam n a (p:ps)
    | n /= tcpnName (tcpName p) = do
      rest <- updateSingleParam n a ps
      return (p:rest)
    | otherwise = Right (update:ps) where
      update = TypeClassParam {
          tcpName = tcpName p,
          tcpVariance = tcpVariance p,
          tcpFilters = (tcpFilters p) ++ [TypeFilter a]
        }
  -- Update the map with all filter updates for a single type class.
  updateParams []     = Right oldParams
  updateParams (u:us) = do
    updatedMap <- updateParams us
    realFilters <- resolveFilters (TypeClassName $ utcName u) (utcFilters u)
    return $ Map.insert (TypeClassName $ utcName u) realFilters updatedMap
  -- The fully-updated graph.
  updated = do
    newParams <- updateParams us
    return $ TypeClassGraph {
        tcgParams = newParams,
        tcgGraphContravariant = tcgGraphContravariant g,
        tcgGraphInvariant     = tcgGraphInvariant g,
        tcgGraphCovariant     = tcgGraphCovariant g
      }

resolveTypeClassInstance :: TypeClassGraph -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
resolveTypeClassInstance g = getTypeClassInstance g []

getTypeClassInstance :: TypeClassGraph -> [TypeFilter] -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
getTypeClassInstance g fs (UnresolvedTypeArg n) = (Right resolved) where
  resolved = (arg, [param])
  param = TypeClassParam {
      tcpName = TypeClassParamName n,
      -- This variance is in the context of this particular TypeClassArg.
      tcpVariance = IgnoreVariance,
      tcpFilters = fs
    }
  arg = TypeClassArgParam {
      tcapParam = param
    }
getTypeClassInstance g fs u = result where
  name = utTypeClass u
  properName = TypeClassName name
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
    -- TODO: Flatten all duplicate params and make sure that they have
    -- compatible variances.
    resolved <- return $ TypeClassArgType {
        tcatInstance = TypeClassInstance {
          tciFreeParams = params,
          tciClassName = properName,
          tciArgs = args
        }
      }
    return (resolved, params)
