{-# LANGUAGE Safe #-}

module DefinedCategory (
  DefinedCategory(..),
  DefinedMember(..),
  isInitialized,
) where

import Procedure
import TypeCategory
import TypeInstance
import TypesBase


data DefinedCategory c =
  DefinedCategory {
    -- TODO: Add internal type params.
    dcContext :: [c],
    dcName :: CategoryName,
    dcMembers :: [DefinedMember c],
    dcProcedures :: [ExecutableProcedure c],
    dcFunctions :: [ScopedFunction c]
  }
  deriving (Show) -- TODO: Implement Show.

data DefinedMember c =
  DefinedMember {
    dmContext :: [c],
    dmScope :: SymbolScope,
    dmType :: ValueType,
    dmName :: VariableName,
    dmInit :: Maybe (Expression c)
  }
  deriving (Show) -- TODO: Implement Show.

isInitialized :: DefinedMember c -> Bool
isInitialized = check . dmInit where
  check Nothing = False
  check _       = True
