module Resolver (
  TypeClassGraph,
  TypeClassArg,
  createTypeClassGraph,
  resolveTypeClassInstance,
  -- For testing...
  TypeClassName,
  checkConversion
) where

import Control.Arrow (second)
import Control.Monad (guard, join, liftM3)
import Control.Monad.Fix (fix)
import Data.Either (isLeft, isRight, partitionEithers)
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
  updateTypeClassGraph initialGraph us

updateTypeClassGraph :: TypeClassGraph -> [UnresolvedTypeClass] -> Either [String] TypeClassGraph
updateTypeClassGraph g us = updated where
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
  updateInherits g2 [] = return []
  updateInherits g2 (u:us) = do
    rest <- updateInherits g2 us
    inherits <- getInherits g2 (utcInherits u)
    return $ (tcnFromUTC u,Set.fromList inherits):rest
  getInherits g2 [] = return []
  getInherits g2 (i:is) = do
    rest <- getInherits g2 is
    inherit <- getTypeClassInstance g2 Set.empty i
    return (fst inherit:rest)
  -- The fully-updated graph.
  updated = do
    newParams <- updateParams us
    partial <- return $ TypeClassGraph {
        tcgParams = newParams,
        tcgGraph = tcgGraph g,
        tcgInherits = Map.empty
      }
    -- TODO: Check variance and filters for inheritance.
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
validateParamVariance g = checkVariances (Map.toList $ tcgInherits g) where
  allParams = tcgParams g
  checkVariances [] = Right ()
  checkVariances ((t,is):ts) = do
    checkVariance t (Set.toList is)
    checkVariances ts
  checkVariance _ [] = Right ()
  checkVariance t (i:is) = do
    -- Params available from the typeclass doing the inheriting.
    -- TODO: Missing value should be an error.
    params <- return $ Map.findWithDefault [] t allParams
    required <- return $ Map.fromList $ mapParams params
    -- Params as used in inheritance.
    freeParams <- return $ mapParams $ tciFreeParams $ tcatInstance i
    checkAllVariances (t,tcatInstance i) required freeParams
    checkVariance t is
  checkAllVariances _ m [] = Right ()
  checkAllVariances (t,i) m ((p,v1):ps) = do
    v0 <- return $ p `Map.lookup` m
    if (isJust v0)
       then (return ())
       else Left ["Param '" ++ show p ++ "' does not exist"]
    if ((fromJust v0) `paramAllowsUsage` v1)
       then (return ())
       else Left ["Param '" ++ show p ++ "' cannot be " ++
                  show v1 ++ " in '" ++ show t ++ "' -> '" ++ show i ++ "'"]
  mapParams = map (\p -> (tcpName p,tcpVariance p))

-- TODO: Get rid of this duplication with validateParamVariance.
validateParamFilters :: TypeClassGraph -> Either [String] ()
validateParamFilters g = checkFilters (Map.toList $ tcgInherits g) where
  allParams = tcgParams g
  checkFilters [] = Right ()
  checkFilters ((t,is):ts) = do
    checkFilter t (Set.toList is)
    checkFilters ts
  checkFilter _ [] = Right ()
  checkFilter t (i:is) = do
    -- Params available from the typeclass doing the inheriting.
    -- TODO: Missing value should be an error.
    params <- return $ Map.findWithDefault [] t allParams
    required <- return $ Map.fromList $ mapParams params
    -- Params as used in inheritance.
    freeParams <- return $ mapParams $ tciFreeParams $ tcatInstance i
    checkAllFilters (t,tciClassName $ tcatInstance i) required freeParams
    checkFilter t is
  checkAllFilters _ m [] = Right ()
  checkAllFilters (t,i) m ((p,f1):ps) = checked where
    checked = do
      f0 <- return $ p `Map.lookup` m
      if (isJust f0)
        then (return ())
        else Left ["Param '" ++ show p ++ "' does not exist"]
      findCompatFilter g (Set.toList $ fromJust f0) $ Set.toList f1
    findCompatFilter _ _ [] = return ()
    findCompatFilter g rs (a:as) = checked where
      checked = do
        if (check a)
          then (return ())
          else Left ["Param '" ++ show p ++ "' does not support required filter " ++
                    show a ++ " in '" ++ show t ++ "' -> '" ++
                    show i ++ "'"]
        findCompatFilter g rs as
      check a = foldr (||) False $ do
        r <- rs
        -- NOTE: Filter conversion is always covariant.
        allowed <- return $ guessInstanceConversions g Covariant
                            (TypeClassArgType $ tfType r)
                            (TypeClassArgType $ tfType a)
        return $ isRight allowed
  mapParams = map (\p -> (tcpName p,tcpFilters p))

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

getTypeClassInstance :: TypeClassGraph -> Set.Set TypeFilter -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
getTypeClassInstance g fs a@(UnresolvedTypeArg _) = (Right resolved) where
  resolved = (arg, [param])
  param = TypeClassParam {
      tcpName = tcpnFromUTA a,
      -- This variance is in the context of this particular TypeClassArg.
      tcpVariance = IgnoreVariance,
      tcpFilters = renameFilters (tcpnFromUTA a) fs
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

-- TODO: Check filters somewhere in here.
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

flattenPath :: TypeClassGraph -> TypeClassArg -> [TypeClassArg] -> Either [String] TypeClassArg
flattenPath _ (TypeClassArgParam _)   _     = Left ["Cannot convert a param"]
flattenPath _ t@(TypeClassArgType _) []     = return t
flattenPath g (TypeClassArgType x)   (y:ys) = flattened where
  allParams = tcgParams g
  flattened = do
    params <- return $ (tciClassName x) `Map.lookup` allParams
    if (isJust params)
       then (Right ())
       else Left ["Type class '" ++ show (tciClassName x) ++ "' not found"]
    args <- return $ Map.fromList $ zip (map tcpName $ fromJust params) (tciArgs x)
    subbed <- uncheckedSubTypeClassArgs args y
    flattenPath g subbed ys

guessInstanceConversions :: TypeClassGraph -> Variance -> TypeClassArg -> TypeClassArg -> Either [String] [TypeClassArg]
guessInstanceConversions _ _ (TypeClassArgType _) (TypeClassArgParam _) =
  Left ["Cannot convert param to instance"]
guessInstanceConversions _ _ (TypeClassArgParam _) (TypeClassArgType _) =
  Left ["Cannot convert instance to param"]
guessInstanceConversions _ _ t@(TypeClassArgParam p1) (TypeClassArgParam p2)
  -- TODO: Actually check the filters.
  | (tcpName p1) == (tcpName p2) = return [t]
  | otherwise = Left ["Mismatch between params"]
guessInstanceConversions g v t1@(TypeClassArgType x) t2@(TypeClassArgType y) = checked where
  allParams = tcgParams g
  allInherits = tcgInherits g
  checked = do
    paths <- return $ getTypeClassPaths allInherits v (tciClassName x) (tciClassName y)
    remaining <- checkPaths $ map (checkConversion . flattenPath g t1) paths
    if (null remaining)
       then Left ["No conversions found"]
       else (return remaining)
  checkConversion t = do
    ok <- t
    checkInstanceConversion g ok t2
  checkPaths [] = return []
  checkPaths (p:ps)
    | isLeft p = checkPaths ps
    | otherwise = do
      ok <- p
      rest <- checkPaths ps
      return (ok:rest)

checkInstanceConversion :: TypeClassGraph -> TypeClassArg -> TypeClassArg -> Either [String] TypeClassArg
checkInstanceConversion _ _ (TypeClassArgParam _) =
  Left ["Cannot convert from param"]
checkInstanceConversion _ (TypeClassArgParam _) _ =
  Left ["Cannot convert to param"]
checkInstanceConversion g (TypeClassArgType x) t@(TypeClassArgType y)
  | tciClassName x /= tciClassName y =
    Left ["Typeclass mismatch: '" ++ show (tciClassName x) ++ "' != '" ++ show (tciClassName y) ++ "'"]
  | otherwise = checked typeClass where
    typeClass = (tciClassName x) `Map.lookup` (tcgParams g)
    checked Nothing   = Left ["Type class '" ++ show (tciClassName x) ++ "' not found"]
    checked (Just ps) = checkAll (zip3 (map tcpVariance ps) (tciArgs x) (tciArgs y))
    checkAll [] = return t
    checkAll ((v,x,y):ys) = do
      convs <- guessInstanceConversions g v x y
      if (null convs)
         then Left ["Conversion failed"]
         else (Right ())
      checkAll ys

-- TODO: This is temporary.
resolveTypeClassInstance :: TypeClassGraph -> UnresolvedType -> Either [String] (TypeClassArg, [TypeClassParam])
resolveTypeClassInstance g = getTypeClassInstance g Set.empty

-- TODO: This is temporary.
checkConversion :: Monad m => m (Either [String] TypeClassGraph) ->
                              m (Either [String] (TypeClassArg,a)) ->
                              m (Either [String] (TypeClassArg,a)) ->
                              m (Either [String] [TypeClassArg])
checkConversion = liftM3 check where
  check gg xx yy = do
    g <- gg
    (x,_) <- xx
    (y,_) <- yy
    guessInstanceConversions g Covariant x y
