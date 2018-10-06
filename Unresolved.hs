module Unresolved (
  UnresolvedParamFilter(..),
  UnresolvedType(..),
  UnresolvedTypeClass(..),
  UnresolvedTypeParam(..),
) where

import Control.Monad.Fix (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Variance

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
    utaName :: String
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
