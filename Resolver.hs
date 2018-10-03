module Resolver (
  createTypeClassGraph
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

data TypeClassName =
  TypeClassName {
    tcnName :: String
  }
  deriving (Eq, Ord, Read, Show)

type TypeClassMap = Map.Map TypeClassName (Set.Set TypeClassName)

data TypeClassGraph =
  TypeClassGraph {
    tcgGraphContravariant :: TypeClassMap,
    tcgGraphInvariant :: TypeClassMap,
    tcgGraphCovariant :: TypeClassMap
  }
  deriving (Eq, Read, Show)

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
  return TypeClassGraph {
      tcgGraphContravariant = foldr (updateTypeClassMap Contravariant) defaultMap edges,
      tcgGraphInvariant     = foldr (updateTypeClassMap Invariant)     defaultMap edges,
      tcgGraphCovariant     = foldr (updateTypeClassMap Covariant)     defaultMap edges
    }
