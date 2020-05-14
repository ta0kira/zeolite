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
  Pragma(..),
  TraceType(..),
  getPragmaContext,
  isExprLookup,
  isModuleOnly,
  isNoTrace,
  isTestsOnly,
  isTraceCreation,
) where


data CodeVisibility = ModuleOnly | TestsOnly deriving (Show)

data TraceType = NoTrace | TraceCreation deriving (Show)

data Pragma c =
  PragmaVisibility {
    pvContext :: [c],
    pvScopes :: CodeVisibility
  } |
  PragmaExprLookup {
    pelContext :: [c],
    pelName :: String
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
getPragmaContext (PragmaVisibility c _) = c
getPragmaContext (PragmaExprLookup c _) = c
getPragmaContext (PragmaTracing c _)    = c
getPragmaContext (PragmaComment c _)    = c

isModuleOnly :: Pragma c -> Bool
isModuleOnly (PragmaVisibility _ ModuleOnly) = True
isModuleOnly _                               = False

isExprLookup :: Pragma c -> Bool
isExprLookup (PragmaExprLookup _ _) = True
isExprLookup _                      = False

isNoTrace :: Pragma c -> Bool
isNoTrace (PragmaTracing _ NoTrace) = True
isNoTrace _                         = False

isTraceCreation :: Pragma c -> Bool
isTraceCreation (PragmaTracing _ TraceCreation) = True
isTraceCreation _                               = False

isTestsOnly :: Pragma c -> Bool
isTestsOnly (PragmaVisibility _ TestsOnly) = True
isTestsOnly _                              = False
