module Variance (
  Missingness(..),
  paramAllowsMissing,
  Variance(..),
  composeVariance,
  paramAllowsVariance,
) where

-- TODO: Move this.
data Missingness =
  AllowsMissing |
  DisallowsMissing |
  UnspecifiedMissing
  deriving (Eq, Ord, Read, Show)

paramAllowsMissing :: Missingness -> Missingness -> Bool
AllowsMissing      `paramAllowsMissing` _                = True
DisallowsMissing   `paramAllowsMissing` DisallowsMissing = True
UnspecifiedMissing `paramAllowsMissing` _                = True
paramAllowsMissing _ _ = False


data Variance =
  Contravariant |
  Invariant |
  Covariant |
  IgnoreVariance
  deriving (Eq, Ord, Read, Show)

composeVariance :: Variance -> Variance -> Variance
composeVariance IgnoreVariance x              = x
composeVariance x              IgnoreVariance = x
composeVariance Covariant      Covariant      = Covariant
composeVariance Contravariant  Contravariant  = Covariant
composeVariance Contravariant  Covariant      = Contravariant
composeVariance Covariant      Contravariant  = Contravariant
composeVariance _              _              = Invariant

paramAllowsVariance :: Variance -> Variance -> Bool
IgnoreVariance `paramAllowsVariance` _              = True
Covariant      `paramAllowsVariance` Covariant      = True
Contravariant  `paramAllowsVariance` Contravariant  = True
Invariant      `paramAllowsVariance` Covariant      = True
Invariant      `paramAllowsVariance` Invariant      = True
Invariant      `paramAllowsVariance` Contravariant  = True
paramAllowsVariance _ _ = False
