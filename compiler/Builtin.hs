{- -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

module Builtin (
  boolRequiredValue,
  builtinCategories,
  emptyValue,
  floatRequiredValue,
  intRequiredValue,
  stringRequiredValue,
) where


import qualified Data.Map as Map

import TypeCategory
import TypeInstance
import TypesBase


builtinCategories :: CategoryMap c
builtinCategories = Map.fromList $ [
    (BuiltinBool,builtinBool),
    (BuiltinString,builtinString),
    (BuiltinInt,builtinInt),
    (BuiltinFloat,builtinFloat),
    (BuiltinLessThan,builtinLessThan),
    (BuiltinEquals,builtinEquals),
    (BuiltinFormatted,builtinFormatted)
  ]

boolRequiredValue :: ValueType
boolRequiredValue = requiredSingleton BuiltinBool
stringRequiredValue :: ValueType
stringRequiredValue = requiredSingleton BuiltinString
intRequiredValue :: ValueType
intRequiredValue = requiredSingleton BuiltinInt
floatRequiredValue :: ValueType
floatRequiredValue = requiredSingleton BuiltinFloat
emptyValue :: ValueType
emptyValue = ValueType OptionalValue $ TypeMerge MergeUnion []

builtinBool :: AnyCategory c
builtinBool = ValueConcrete {
    vcContext = [],
    vcName = BuiltinBool,
    vcParams = [],
    vcRefines = [ValueRefine [] $ TypeInstance BuiltinFormatted $ ParamSet []],
    vcDefines = [],
    vcParamFilter = [],
    vcFunctions = [functionFormatted]
  }

builtinString :: AnyCategory c
builtinString = ValueConcrete {
    vcContext = [],
    vcName = BuiltinString,
    vcParams = [],
    vcRefines = [ValueRefine [] $ TypeInstance BuiltinFormatted $ ParamSet []],
    vcDefines = [ValueDefine [] $ DefinesInstance BuiltinLessThan $ ParamSet [vtType stringRequiredValue],
                 ValueDefine [] $ DefinesInstance BuiltinEquals   $ ParamSet [vtType stringRequiredValue]],
    vcParamFilter = [],
    vcFunctions = [functionFormatted,
                   functionLessThan stringRequiredValue,
                   functionEquals stringRequiredValue]
  }

builtinInt :: AnyCategory c
builtinInt = ValueConcrete {
    vcContext = [],
    vcName = BuiltinInt,
    vcParams = [],
    vcRefines = [ValueRefine [] $ TypeInstance BuiltinFormatted $ ParamSet []],
    vcDefines = [ValueDefine [] $ DefinesInstance BuiltinLessThan $ ParamSet [vtType intRequiredValue],
                 ValueDefine [] $ DefinesInstance BuiltinEquals   $ ParamSet [vtType intRequiredValue]],
    vcParamFilter = [],
    vcFunctions = [functionFormatted,
                   functionLessThan intRequiredValue,
                   functionEquals intRequiredValue]
  }

builtinFloat :: AnyCategory c
builtinFloat = ValueConcrete {
    vcContext = [],
    vcName = BuiltinFloat,
    vcParams = [],
    vcRefines = [ValueRefine [] $ TypeInstance BuiltinFormatted $ ParamSet []],
    vcDefines = [ValueDefine [] $ DefinesInstance BuiltinLessThan $ ParamSet [vtType floatRequiredValue],
                 ValueDefine [] $ DefinesInstance BuiltinEquals   $ ParamSet [vtType floatRequiredValue]],
    vcParamFilter = [],
    vcFunctions = [functionFormatted,
                   functionLessThan floatRequiredValue,
                   functionEquals floatRequiredValue]
  }

functionLessThan t = ScopedFunction {
    sfContext = [],
    sfName = FunctionName "lessThan",
    sfType = BuiltinLessThan,
    sfScope = TypeScope,
    sfArgs = ParamSet [PassedValue [] t,PassedValue [] t],
    sfReturns = ParamSet [PassedValue [] boolRequiredValue],
    sfParams = ParamSet [],
    sfFilters = [],
    sfMerges = []
  }

builtinLessThan :: AnyCategory c
builtinLessThan = InstanceInterface {
    iiContext = [],
    iiName = BuiltinLessThan,
    iiParams = [ValueParam [] (ParamName "#x") Contravariant],
    iiParamFilter = [],
    iiFunctions = [functionLessThan (requiredParam $ ParamName "#x")]
  }

functionEquals t = ScopedFunction {
    sfContext = [],
    sfName = FunctionName "equals",
    sfType = BuiltinEquals,
    sfScope = TypeScope,
    sfArgs = ParamSet [PassedValue [] t,PassedValue [] t],
    sfReturns = ParamSet [PassedValue [] boolRequiredValue],
    sfParams = ParamSet [],
    sfFilters = [],
    sfMerges = []
  }

builtinEquals :: AnyCategory c
builtinEquals = InstanceInterface {
    iiContext = [],
    iiName = BuiltinEquals,
    iiParams = [ValueParam [] (ParamName "#x") Contravariant],
    iiParamFilter = [],
    iiFunctions = [functionEquals (requiredParam $ ParamName "#x")]
  }

functionFormatted = ScopedFunction {
    sfContext = [],
    sfName = FunctionName "formatted",
    sfType = BuiltinFormatted,
    sfScope = ValueScope,
    sfArgs = ParamSet [],
    sfReturns = ParamSet [PassedValue [] stringRequiredValue],
    sfParams = ParamSet [],
    sfFilters = [],
    sfMerges = []
  }

builtinFormatted :: AnyCategory c
builtinFormatted = ValueInterface {
    viContext = [],
    viName = BuiltinFormatted,
    viParams = [],
    viRefines = [],
    viParamFilter = [],
    viFunctions = [functionFormatted]
  }
