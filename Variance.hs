module Variance (
  Variance(..),
  composeVariance,
  paramAllowsUsage,
) where

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

paramAllowsUsage :: Variance -> Variance -> Bool
IgnoreVariance `paramAllowsUsage` _              = True
Covariant      `paramAllowsUsage` Covariant      = True
Contravariant  `paramAllowsUsage` Contravariant  = True
Invariant      `paramAllowsUsage` Covariant      = True
Invariant      `paramAllowsUsage` Contravariant  = True
paramAllowsUsage _ _ = False
