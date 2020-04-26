{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

{-# LANGUAGE Safe #-}

module Types.Variance (
  Variance(..),
  composeVariance,
  paramAllowsVariance,
) where


data Variance =
  Contravariant |
  Invariant |
  Covariant
  deriving (Eq,Ord)

instance Show Variance where
  show Contravariant = "contravariant"
  show Invariant     = "invariant"
  show Covariant     = "covariant"

composeVariance :: Variance -> Variance -> Variance
composeVariance Covariant      Covariant      = Covariant
composeVariance Contravariant  Contravariant  = Covariant
composeVariance Contravariant  Covariant      = Contravariant
composeVariance Covariant      Contravariant  = Contravariant
composeVariance _              _              = Invariant

paramAllowsVariance :: Variance -> Variance -> Bool
Covariant     `paramAllowsVariance` Covariant     = True
Contravariant `paramAllowsVariance` Contravariant = True
Invariant     `paramAllowsVariance` Covariant     = True
Invariant     `paramAllowsVariance` Invariant     = True
Invariant     `paramAllowsVariance` Contravariant = True
_             `paramAllowsVariance` _             = False
