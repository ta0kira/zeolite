module Types (
  TypeClass,
  TypeInstance,
  UnresolvedParamFilter(..),
  UnresolvedType(..),
  UnresolvedTypeClass(..),
  UnresolvedTypeParam(..),
  TypeResolver,
  newTypeResolver,
  getTypeClass,
  getTypeInstance,
) where

import Control.Monad.Fix (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Variance

data TypeClass =
  TypeClass {
    tcName :: TypeClassName,
    tcParams :: [ResolvedTypeParam],
    tcInherits :: [ResolvedType],
    tcFilters :: [ResolvedParamFilter]
  }
  deriving (Eq, Read, Show)

data TypeInstance =
  TypeInstance {
    tiTypeClass :: ResolvedTypeClass,
    tiParamArgs :: [ResolvedType]
    -- TODO: Extract required filters and type param args?
  }
  deriving (Eq, Read, Show)

data UnresolvedTypeClass =
  UnresolvedTypeClass {
    utcName :: String,
    utcParams :: [UnresolvedTypeParam],
    utcInherits :: [UnresolvedType],
    utcFilters :: [UnresolvedParamFilter]
  }
  deriving (Eq, Read, Show)

data UnresolvedType =
  UnresolvedType {
    utTypeClass :: String,
    utParamArgs :: [UnresolvedType]
  } |
  UnresolvedTypeArg {
    utName :: String
  }
  deriving (Eq, Read, Show)

data UnresolvedParamFilter =
  UnresolvedParamFilter {
    upfName :: String,
    upfType :: UnresolvedType
  }
  deriving (Eq, Read, Show)

data UnresolvedTypeParam =
  UnresolvedTypeParam {
    utpName :: String,
    utpVariance :: Variance
  }
  deriving (Eq, Read, Show)

-- Do not export!
data TypeClassName =
  TypeClassName {
    tcnName :: String
  }
  deriving (Eq, Ord, Read, Show)

-- Do not export!
data TypeParamName =
  TypeParamName {
    tpnName :: String
  }
  deriving (Eq, Ord, Read, Show)

data ResolvedTypeClass =
  ResolvedTypeClass {
    rtcName :: TypeClassName,
    rtcParams :: [ResolvedTypeParam],
    rtcInherits :: [ResolvedType]
  }
  deriving (Eq, Read, Show)

data ResolvedTypeClassFilters =
  ResolvedTypeClassFilters {
    rtcfName :: TypeClassName,
    rtcfFilters :: [ResolvedParamFilter]
  }
  deriving (Eq, Read, Show)

data ResolvedType =
  ResolvedType {
    rtTypeClass :: ResolvedTypeClass,
    rtParamArgs :: [ResolvedType]
  } |
  ResolvedTypeArg {
    rtName :: TypeParamName
  } |
  InvalidTypeClass {
    itcName :: String
  } |
  InvalidTypeParam {
    itpName :: String
  } |
  InvalidParameters {
    ipTypeClass :: String,
    ipRequired :: [ResolvedTypeParam]
  } |
  InvalidVariance {
    ivName :: String,
    ivRequired :: Variance,
    ivActual :: Variance
  }
  deriving (Eq, Read, Show)

data ResolvedParamFilter =
  ResolvedParamFilter {
    rpfName :: TypeParamName,
    rpfType :: ResolvedType
  } |
  InvalidFilterParam {
    ifpName :: String
  }
  deriving (Eq, Read, Show)

data ResolvedTypeParam =
  ResolvedTypeParam {
    rtpName :: TypeParamName,
    rtpVariance :: Variance
  }
  deriving (Eq, Read, Show)

data TypeResolver =
  TypeResolver {
    trTypeClasses :: Map.Map TypeClassName ResolvedTypeClass,
    trTypeClassFilters :: Map.Map TypeClassName ResolvedTypeClassFilters
  }
  deriving (Show)

resolveTypeClass :: Map.Map TypeClassName ResolvedTypeClass -> UnresolvedTypeClass -> (TypeClassName, ResolvedTypeClass)
resolveTypeClass m u = (name, resolved) where
  name = TypeClassName (utcName u)
  -- TODO: Need to make sure that there are not duplicates here. Maybe there
  -- should be a preliminary sanity-check of unresolved stuff first?
  params = map resolveTypeParam (utcParams u)
  paramsMap = Map.fromList $ map (\p -> (tpnName $ rtpName p, rtpVariance p)) params
  resolved =
    ResolvedTypeClass {
      rtcName = name,
      rtcParams = params,
      rtcInherits = map (\t -> resolveType m paramsMap (t, Covariant)) (utcInherits u)
    }

maybeZipParams :: [UnresolvedType] -> Variance -> [ResolvedTypeParam] -> Maybe [(UnresolvedType,Variance)]
maybeZipParams []     _ []     = Just []
maybeZipParams (x:xs) v (y:ys) = maybeZipParams xs v ys >>= Just . (:) (x,v `composeVariance` rtpVariance y)
maybeZipParams _      _ _      = Nothing

resolveType :: Map.Map TypeClassName ResolvedTypeClass -> Map.Map String Variance -> (UnresolvedType, Variance) -> ResolvedType
resolveType m pm ((UnresolvedType n t), v) = resolved typeClass paramPairs where
  typeClass = Map.lookup (TypeClassName n) m
  paramPairs = typeClass >>= maybeZipParams t v . rtcParams
  resolved Nothing   _         = InvalidTypeClass { itcName = n }
  resolved (Just tc) Nothing   =
    InvalidParameters {
      ipTypeClass = n,
      ipRequired = rtcParams tc
    }
  resolved (Just tc) (Just pp) =
    ResolvedType {
      rtTypeClass = tc,
      rtParamArgs = map (resolveType m pm) pp
    }
resolveType _ pm ((UnresolvedTypeArg n), v) = resolved param varianceAllowed where
  param = Map.lookup n pm
  varianceAllowed = param >>= Just . (`paramAllowsUsage` v)
  resolved Nothing  _            = InvalidTypeParam { itpName = n }
  resolved (Just p) (Just False) =
      InvalidVariance {
        ivName = n,
        ivRequired = p,
        ivActual = v
      }
  resolved (Just p) _            =
      ResolvedTypeArg {
        rtName = TypeParamName n
      }

resolveTypeParam :: UnresolvedTypeParam -> ResolvedTypeParam
resolveTypeParam u = ResolvedTypeParam {
    rtpName = TypeParamName $ utpName u,
    rtpVariance = utpVariance u
}

resolveTypeClassFilters :: Map.Map TypeClassName ResolvedTypeClass -> UnresolvedTypeClass -> (TypeClassName, ResolvedTypeClassFilters)
resolveTypeClassFilters m u = (name, resolved paramsSet) where
  name = TypeClassName (utcName u)
  params = fmap (map (tpnName . rtpName) . rtcParams) $ Map.lookup name m
  paramsSet = params >>= Just . Set.fromList
  resolved (Just ps) =
    ResolvedTypeClassFilters {
      rtcfName = name,
      rtcfFilters = map (resolveParamFilter m ps) (utcFilters u)
    }

resolveParamFilter :: Map.Map TypeClassName ResolvedTypeClass -> Set.Set String -> UnresolvedParamFilter -> ResolvedParamFilter
resolveParamFilter m ps (UnresolvedParamFilter n t)  = resolved param where
  param = Set.member n ps
  resolved False = InvalidFilterParam { ifpName = n }
  resolved _     =
    ResolvedParamFilter {
      rpfName = TypeParamName n,
      rpfType = resolveType m (Map.fromList [(n, IgnoreVariance)]) (t, Covariant)
    }

getTypeClass :: TypeResolver -> String -> Maybe TypeClass
getTypeClass t n = do
  typeClass <- (TypeClassName n) `Map.lookup` (trTypeClasses t)
  typeFilters <- (TypeClassName n) `Map.lookup` (trTypeClassFilters t)
  return TypeClass {
      tcName = rtcName typeClass,
      tcParams = rtcParams typeClass,
      tcInherits = rtcInherits typeClass,
      tcFilters = rtcfFilters typeFilters
    }

getTypeInstance :: TypeResolver -> [(String, Variance)] -> Variance -> UnresolvedType -> Maybe TypeInstance
getTypeInstance t pm v u = checkType resolved where
  resolved = resolveType (trTypeClasses t) (Map.fromList pm) (u, v)
  checkType t@(ResolvedType _ _) =
    Just $ TypeInstance {
      tiTypeClass = rtTypeClass t,
      tiParamArgs = rtParamArgs t
    }
  -- TODO: Return an error or something.
  checkType _ = Nothing

newTypeResolver :: [UnresolvedTypeClass] -> TypeResolver
newTypeResolver us = TypeResolver { trTypeClasses = types, trTypeClassFilters = filters } where
  types = fix (\t -> Map.fromList $ map (resolveTypeClass t) us)
  -- TODO: This might also need `fix` if filters are to be compared.
  -- TODO: Maybe if the individual resolver functions can return an error and
  -- short-circuit checking then we won't need a second pass for filters.
  filters = Map.fromList $ map (resolveTypeClassFilters types) us
