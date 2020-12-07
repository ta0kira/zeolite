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

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module Base.CompilerMessage (
  CompilerMessage,
  compilerMessage,
  compilerMessages,
  prefixCompilerMessages,
  pushErrorScope,
  pushWarningScope,
) where

import Data.List (intercalate)
import Prelude hiding (foldr)

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif


data CompilerMessage =
  CompilerMessage {
    cmMessage :: String,
    cmNested :: [CompilerMessage]
  }
  deriving (Eq,Ord)

compilerMessage :: String -> CompilerMessage
compilerMessage = flip CompilerMessage []

compilerMessages :: [CompilerMessage] -> CompilerMessage
compilerMessages = CompilerMessage ""

instance Semigroup CompilerMessage where
  (CompilerMessage "" [])  <> e2                       = e2
  e1                       <> (CompilerMessage "" [])  = e1
  (CompilerMessage "" es1) <> (CompilerMessage "" es2) = CompilerMessage "" (es1 ++ es2)
  e1                       <> (CompilerMessage "" es2) = CompilerMessage "" ([e1] ++ es2)
  (CompilerMessage "" es1) <> e2                       = CompilerMessage "" (es1 ++ [e2])
  e1                       <> e2                       = CompilerMessage "" [e1,e2]

instance Monoid CompilerMessage where
  mempty = CompilerMessage "" []
  mappend = (<>)

instance Show CompilerMessage where
  show = format "" where
    format indent (CompilerMessage [] ms) =
      concat (map (format indent) ms)
    format indent (CompilerMessage m ms) =
      (doIndent indent m) ++ "\n" ++ concat (map (format $ indent ++ "  ") ms)
    doIndent indent s = intercalate "\n" $ map (indent ++) $ lines s

pushErrorScope :: String -> CompilerMessage -> CompilerMessage
pushErrorScope e2 ea@(CompilerMessage e ms)
  | null e    = CompilerMessage e2 ms
  | otherwise = CompilerMessage e2 [ea]

pushWarningScope :: String -> CompilerMessage -> CompilerMessage
pushWarningScope e2 ea
  | isEmpty ea = mempty  -- Skip the scope if there isn't already a warning.
  | otherwise  = pushErrorScope e2 ea

prefixCompilerMessages :: [String] -> CompilerMessage -> CompilerMessage
prefixCompilerMessages b (CompilerMessage e es) = CompilerMessage e (es ++ map (flip CompilerMessage []) b)

isEmpty :: CompilerMessage -> Bool
isEmpty (CompilerMessage "" ws) = all isEmpty ws
isEmpty _                       = False
