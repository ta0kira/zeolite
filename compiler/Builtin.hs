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
