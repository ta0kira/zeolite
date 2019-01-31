{-# LANGUAGE Safe #-}

module Builtin (
  boolRequiredValue,
  builtinCategories,
  floatRequiredValue,
  intRequiredValue,
  stringRequiredValue,
) where


import qualified Data.Map as Map

import TypeCategory
import TypeInstance

builtinCategories :: CategoryMap c
builtinCategories = Map.fromList $ [
    (BuiltinBool,builtinBool),
    (BuiltinString,builtinString),
    (BuiltinInt,builtinInt),
    (BuiltinFloat,builtinFloat)
  ]

boolRequiredValue :: ValueType
boolRequiredValue = requiredSingleton BuiltinBool
stringRequiredValue :: ValueType
stringRequiredValue = requiredSingleton BuiltinString
intRequiredValue :: ValueType
intRequiredValue = requiredSingleton BuiltinInt
floatRequiredValue :: ValueType
floatRequiredValue = requiredSingleton BuiltinFloat

builtinBool :: AnyCategory c
builtinBool = ValueConcrete {
    vcContext = [],
    vcName = BuiltinBool,
    vcParams = [],
    vcRefines = [],
    vcDefines = [],
    vcParamFilter = [],
    viFunctions = []
  }

builtinString :: AnyCategory c
builtinString = ValueConcrete {
    vcContext = [],
    vcName = BuiltinString,
    vcParams = [],
    vcRefines = [],
    vcDefines = [],
    vcParamFilter = [],
    viFunctions = []
  }

builtinInt :: AnyCategory c
builtinInt = ValueConcrete {
    vcContext = [],
    vcName = BuiltinInt,
    vcParams = [],
    vcRefines = [],
    vcDefines = [],
    vcParamFilter = [],
    viFunctions = []
  }

builtinFloat :: AnyCategory c
builtinFloat = ValueConcrete {
    vcContext = [],
    vcName = BuiltinFloat,
    vcParams = [],
    vcRefines = [],
    vcDefines = [],
    vcParamFilter = [],
    viFunctions = []
  }
