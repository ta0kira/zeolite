{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

module Types.Pragma (
  CodeVisibility(..),
  MacroName(..),
  Pragma(..),
  TraceType(..),
  WithVisibility(..),
  getPragmaContext,
  hasCodeVisibility,
  isExprLookup,
  isModuleOnly,
  isNoTrace,
  isSourceContext,
  isTestsOnly,
  isTraceCreation,
  mapCodeVisibility,
  updateCodeVisibility,
) where

import qualified Data.Set as Set


data CodeVisibility = ModuleOnly | TestsOnly | FromDependency deriving (Eq,Ord,Show)

data WithVisibility a =
  WithVisibility {
    wvVisibility :: Set.Set CodeVisibility,
    wvData :: a
  }
  deriving (Show)

hasCodeVisibility :: CodeVisibility -> WithVisibility a -> Bool
hasCodeVisibility v = Set.member v . wvVisibility

mapCodeVisibility :: (a -> b) -> WithVisibility a -> WithVisibility b
mapCodeVisibility f (WithVisibility v x) = WithVisibility v (f x)

updateCodeVisibility :: (Set.Set CodeVisibility -> Set.Set CodeVisibility) ->
  WithVisibility a -> WithVisibility a
updateCodeVisibility f (WithVisibility v x) = WithVisibility (f v) x

data TraceType = NoTrace | TraceCreation deriving (Show)

newtype MacroName =
  MacroName {
    mnName :: String
  }
  deriving (Eq,Ord)

instance Show MacroName where
  show = mnName

data Pragma c =
  PragmaVisibility {
    pvContext :: [c],
    pvScopes :: CodeVisibility
  } |
  PragmaExprLookup {
    pelContext :: [c],
    pelName :: MacroName
  } |
  PragmaSourceContext {
    pscContext :: c
  } |
  PragmaTracing {
    ptContext :: [c],
    ptType :: TraceType
  } |
  -- This is mostly for testing purposes.
  PragmaComment {
    pcContext :: [c],
    pcComment :: String
  }
  deriving (Show)

getPragmaContext :: Pragma c -> [c]
getPragmaContext (PragmaVisibility c _)  = c
getPragmaContext (PragmaExprLookup c _)  = c
getPragmaContext (PragmaSourceContext c) = [c]
getPragmaContext (PragmaTracing c _)     = c
getPragmaContext (PragmaComment c _)     = c

isModuleOnly :: Pragma c -> Bool
isModuleOnly (PragmaVisibility _ ModuleOnly) = True
isModuleOnly _                               = False

isExprLookup :: Pragma c -> Bool
isExprLookup (PragmaExprLookup _ _) = True
isExprLookup _                      = False

isSourceContext :: Pragma c -> Bool
isSourceContext (PragmaSourceContext _) = True
isSourceContext _                       = False

isNoTrace :: Pragma c -> Bool
isNoTrace (PragmaTracing _ NoTrace) = True
isNoTrace _                         = False

isTraceCreation :: Pragma c -> Bool
isTraceCreation (PragmaTracing _ TraceCreation) = True
isTraceCreation _                               = False

isTestsOnly :: Pragma c -> Bool
isTestsOnly (PragmaVisibility _ TestsOnly) = True
isTestsOnly _                              = False
