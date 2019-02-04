{-# LANGUAGE Safe #-}

module TypeInstanceTest where

import Data.List
import Text.Parsec
import qualified Data.Map as Map

import CompileInfo
import ParseInstance
import ParserBase
import TestBase
import TypeInstance
import TypesBase


tests :: [IO (CompileInfo ())]
tests = [
    checkSimpleConvertSuccess
      "Type0"
      "Type0",
    checkSimpleConvertSuccess
      "Type3"
      "Type0",
    checkSimpleConvertFail
      "Type0"
      "Type3",
    checkSimpleConvertSuccess
      "Type1<Type0>"
      "Type0",
    checkSimpleConvertFail
      "Type0"
      "Type1<Type0>",
    checkSimpleConvertSuccess
      "Type2<Type0,Type0,Type0>"
      "Type2<Type0,Type0,Type0>",
    checkSimpleConvertSuccess
      "Type2<Type0,Type0,Type3>"
      "Type2<Type3,Type0,Type0>",
    checkSimpleConvertFail
      "Type2<Type3,Type0,Type3>"
      "Type2<Type0,Type0,Type0>",
    checkSimpleConvertFail
      "Type2<Type0,Type0,Type0>"
      "Type2<Type3,Type0,Type3>",
    checkSimpleConvertFail
      "Type2<Type0,Type3,Type0>"
      "Type2<Type0,Type0,Type0>",
    checkSimpleConvertFail
      "Type2<Type0,Type0,Type0>"
      "Type2<Type0,Type3,Type0>",
    checkSimpleConvertSuccess
      "Type3"
      "[Type0|Type3]",
    checkSimpleConvertSuccess
      "Type3"
      "[Type0|Type1<Type0>]",
    checkSimpleConvertSuccess
      "[Type3|Type1<Type0>]"
      "Type0",
    checkSimpleConvertSuccess
      "[Type0&Type3]"
      "Type3",
    checkSimpleConvertSuccess
      "[Type3|Type3]"
      "Type3",
    checkSimpleConvertSuccess
      "[Type1<Type0>&Type3]"
      "[Type1<Type0>|Type3]",
    checkSimpleConvertFail
      "[Type0|Type3]"
      "Type3",
    checkSimpleConvertFail
      "Type0"
      "[Type0&Type3]",
    checkSimpleConvertFail
      "[Type0|Type3]"
      "[Type0&Type3]",

    checkSimpleConvertSuccess
      "any"
      "any",
    checkSimpleConvertSuccess
      "Type0"
      "any",
    checkSimpleConvertFail
      "any"
      "Type0",
    checkSimpleConvertSuccess
      "all"
      "all",
    checkSimpleConvertFail
      "Type0"
      "all",
    checkSimpleConvertSuccess
      "all"
      "Type0",
    checkSimpleConvertSuccess
      "all"
      "any",
    checkSimpleConvertFail
      "any"
      "all",
    checkSimpleConvertFail
      "Type1<Type0>"
      "Type1<any>",
    checkSimpleConvertFail
      "Type1<all>"
      "Type1<Type0>",

    checkConvertSuccess
      [("#x",[])]
      "#x" "#x",
    checkConvertFail
      [("#x",[]),
       ("#y",[])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",["allows #x"])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",[])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["allows #x"])]
      "#x" "#y",

    -- NOTE: defines are not checked for instance conversions.
    checkConvertSuccess
      [("#x",["requires #y","defines Instance0"]),
       ("#y",["allows #x"])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",["allows #x","defines Instance0"])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires #y","defines Instance0"]),
       ("#y",["allows #x","defines Instance0"])]
      "#x" "#y",
    checkConvertFail
      [("#x",["defines Instance0"]),
       ("#y",["defines Instance0"])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires Type0","defines Instance0"])]
      "#x" "Type0",
    checkConvertSuccess
      [("#x",["allows Type0","defines Instance0"])]
      "Type0" "#x",

    checkConvertSuccess
      [("#x",["requires #z"]),
       ("#y",["allows #z"]),
       ("#z",[])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires #z"]),
       ("#y",[]),
       ("#z",["requires #y"])]
      "#x" "#y",
    -- NOTE: This is technically valid, but the checking mechanism doesn't do
    -- a full graph search, so the writer needs to be explicit about implied
    -- additional filters, e.g., "#x requires #y" => "#y allows #x".
    checkConvertFail
      [("#w",["allows #x"]),
       ("#x",[]),
       ("#y",[]),
       ("#z",["allows #w","requires #y"])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires Type3"]),
       ("#y",["allows Type0"])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",["requires Type3"])]
      "#x" "Type0",
    checkConvertSuccess
      [("#x",["allows #y"]),
       ("#y",["allows Type0"])]
      "Type3" "#x",

    checkConvertSuccess
      [("#x",[])]
      "Type2<Type0,Type0,#x>" "Type2<Type0,Type0,#x>",
    checkConvertFail
      [("#x",[]),
       ("#y",[])]
      "Type2<Type0,Type0,#x>" "Type2<Type0,Type0,#y>",
    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",["allows #x"])]
      "Type2<Type0,Type0,#x>" "Type2<Type0,Type0,#y>",
    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",[])]
      "Type2<Type0,Type0,#x>" "Type2<Type0,Type0,#y>",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["allows #x"])]
      "Type2<Type0,Type0,#x>" "Type2<Type0,Type0,#y>",

    checkConvertSuccess
      [("#x",[])]
      "Type2<#x,Type0,Type0>" "Type2<#x,Type0,Type0>",
    checkConvertFail
      [("#x",[]),
       ("#y",[])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",
    checkConvertFail
      [("#x",["requires #y"]),
       ("#y",["allows #x"])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",
    checkConvertFail
      [("#x",["requires #y"]),
       ("#y",[])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",
    checkConvertFail
      [("#x",[]),
       ("#y",["allows #x"])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",
    checkConvertSuccess
      [("#x",["allows #y"]),
       ("#y",["requires #x"])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",
    checkConvertSuccess
      [("#x",["allows #y"]),
       ("#y",[])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["requires #x"])]
      "Type2<#x,Type0,Type0>" "Type2<#y,Type0,Type0>",

    checkConvertFail
      [("#x",[])]
      "#x" "Type0",
    checkConvertSuccess
      [("#x",["requires Type0"])]
      "#x" "Type0",
    checkConvertSuccess
      [("#x",["requires Type3"])]
      "#x" "Type0",
    checkConvertFail
      [("#x",[])]
      "Type0" "#x",
    checkConvertSuccess
      [("#x",["allows Type0"])]
      "Type0" "#x",
    checkConvertSuccess
      [("#x",["allows Type0"])]
      "Type3" "#x",

    checkConvertFail
      [("#x",[])]
      "Type2<#x,Type0,Type0>" "Type2<Type0,Type0,Type0>",
    checkConvertSuccess
      [("#x",["allows Type0"])]
      "Type2<#x,Type0,Type0>" "Type2<Type0,Type0,Type0>",
    checkConvertSuccess
      [("#x",["allows Type0"])]
      "Type2<#x,Type0,Type0>" "Type2<Type3,Type0,Type0>",
    checkConvertFail
      [("#x",[])]
      "Type2<Type0,Type0,Type0>" "Type2<#x,Type0,Type0>",
    checkConvertSuccess
      [("#x",["requires Type0"])]
      "Type2<Type0,Type0,Type0>" "Type2<#x,Type0,Type0>",
    checkConvertSuccess
      [("#x",["requires Type3"])]
      "Type2<Type0,Type0,Type0>" "Type2<#x,Type0,Type0>",

    return $ checkTypeSuccess resolver
      []
      "Type4<Type0>",
    return $ checkTypeSuccess resolver
      [("#x",["allows Type0"])]
      "Type4<[#x&Type0]>",
    return $ checkTypeSuccess resolver
      [("#x",["allows Type0"])]
      "Type4<[#x|Type0]>",
    return $ checkTypeSuccess resolver
      [("#x",["allows Type0"])]
      "Type4<[#x|Type3]>",
    return $ checkTypeFail resolver
      []
      "Type5<#x>",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "Type5<#x>",

    checkConvertSuccess
      [("#x",["requires #y"]),
       ("#y",["requires #z"]),
       ("#z",[])]
      "#x" "#y",
    checkConvertSuccess
      [("#x",["allows #z"]),
       ("#y",["allows #x"]),
       ("#z",[])]
      "#x" "#y",

    checkSimpleConvertSuccess
      "any"
      "any",
    checkSimpleConvertSuccess
      "all"
      "all",
    checkSimpleConvertSuccess
      "all"
      "any",
    checkConvertSuccess
      [("#x",[]),
       ("#y",[])]
      "[#x&#y]" "[#x&#y]",
    checkConvertSuccess
      [("#x",[]),
       ("#y",[])]
      "[#x&#y]" "[#x|#y]",
    checkConvertSuccess
      [("#x",[]),
       ("#y",[])]
      "[#x|#y]" "[#x|#y]",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["defines Instance0"])]
      "[#x&#y]" "[#x|#y]",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["defines Instance0"])]
      "[#x&#y]" "#y",
    checkConvertFail
      [("#x",[]),
       ("#y",["defines Instance0"])]
      "[#x|#y]" "#y",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["requires Type3"])]
      "[#x&#y]" "[#x|#y]",
    checkConvertSuccess
      [("#x",[]),
       ("#y",["requires Type3"])]
      "[#x&#y]" "#y",
    checkConvertFail
      [("#x",[]),
       ("#y",["requires Type3"])]
      "[#x|#y]" "#y",
    checkConvertSuccess
      [("#x",[])]
      "all" "#x",
    checkConvertSuccess
      [("#x",["defines Instance0"])]
      "all" "#x",
    checkConvertSuccess
      [("#x",["requires Type3"])]
      "all" "#x",

    checkSimpleConvertSuccess
      "optional Type0"
      "optional Type0",
    checkSimpleConvertSuccess
      "weak Type0"
      "weak Type0",
    checkSimpleConvertSuccess
      "Type0"
      "optional Type0",
    checkSimpleConvertSuccess
      "Type0"
      "weak Type0",
    checkSimpleConvertSuccess
      "optional Type0"
      "weak Type0",
    checkSimpleConvertFail
      "optional Type0"
      "Type0",
    checkSimpleConvertFail
      "weak Type0"
      "Type0",
    checkSimpleConvertFail
      "weak Type0"
      "optional Type0",

    checkSimpleConvertSuccess
      "optional Type3"
      "optional Type0",
    checkSimpleConvertSuccess
      "weak Type3"
      "weak Type0",
    checkSimpleConvertSuccess
      "Type3"
      "optional Type0",
    checkSimpleConvertSuccess
      "Type3"
      "weak Type0",
    checkSimpleConvertSuccess
      "optional Type3"
      "weak Type0",

    checkSimpleConvertSuccess
      "any"
      "optional any",
    checkSimpleConvertSuccess
      "Type3"
      "optional any",
    checkSimpleConvertSuccess
      "optional all"
      "optional Type3",

    return $ checkTypeSuccess resolver
      [("#x",[])]
      "#x",
    return $ checkTypeFail resolver
      [("#x",[])]
      "Type1<#x>",
    return $ checkTypeFail resolver
      [("#x",["requires Type3"])]
      "Type1<#x>",
    return $ checkTypeFail resolver
      [("#x",["defines Instance0"])]
      "Type1<#x>",
    return $ checkTypeFail resolver
      []
      "Type1<all>",
    return $ checkTypeSuccess resolver
      [("#x",["requires Type3","defines Instance0"])]
      "Type1<#x>",
    return $ checkTypeSuccess resolver
      []
      "Type1<Type3>",
    return $ checkTypeFail resolver
      []
      "Type1<Type1<Type3>>",
    return $ checkTypeSuccess resolver
      []
      "Type2<Type0,Type0,Type0>",
    return $ checkTypeFail resolver
      []
      "Type2<all,Type0,Type0>",
    return $ checkTypeFail resolver
      []
      "Type2<any,Type0,Type0>",
    return $ checkTypeSuccess resolver
      []
      "Type4<any>",
    return $ checkTypeFail resolver
      []
      "Type4<all>",

    return $ checkTypeSuccess resolver
      [("#x",["defines Instance1<Type0>",
             "defines Instance1<#x>",
             "defines Instance1<Type3>"])]
      "Type2<#x,#x,#x>",
    return $ checkTypeFail resolver
      [("#x",["defines Instance1<#x>",
             "defines Instance1<Type3>"])]
      "Type2<#x,#x,#x>",
    return $ checkTypeFail resolver
      [("#x",["defines Instance1<Type0>",
             "defines Instance1<Type3>"])]
      "Type2<#x,#x,#x>",
    return $ checkTypeSuccess resolver
      [("#x",["defines Instance1<Type0>",
             "defines Instance1<#x>"])]
      "Type2<#x,#x,#x>",
    return $ checkTypeSuccess resolver
      [("#x",["allows Type0", -- Type0 -> #x implies Type3 -> #x
             "defines Instance1<#x>"])]
      "Type2<#x,#x,#x>",
    return $ checkTypeFail resolver
      [("#x",["allows Type3", -- Type3 -> #x doesn't imply Type0 -> #x
             "defines Instance1<#x>"])]
      "Type2<#x,#x,#x>",

    return $ checkTypeSuccess resolver
      []
      "Type4<Type0>",
    return $ checkTypeFail resolver
      []
      "Type5<#x>",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "Type5<#x>",

    return $ checkTypeSuccess resolver
      []
      "[Type4<Type0>|Type1<Type3>]",
    return $ checkTypeSuccess resolver
      []
      "[Type4<Type0>&Type1<Type3>]",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "[Type5<#x>|Type1<Type3>]",
    return $ checkTypeFail resolver
      [("#x",[])]
      "[Type5<#x>&Type1<Type3>]",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "[#x|Type1<Type3>]",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "[#x&Type1<Type3>]",
    return $ checkTypeFail resolver
      [("#x",[])]
      "[Type4<Type0>|Instance0]",
    return $ checkTypeFail resolver
      [("#x",[])]
      "[Type4<Type0>&Instance0]",

    return $ checkTypeSuccess resolver
      []
      "[[Type4<Type0>&Type1<Type3>]|Type1<Type3>]",
    return $ checkTypeSuccess resolver
      []
      "[[Type4<Type0>|Type1<Type3>]&Type1<Type3>]",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "[[Type4<Type0>&#x]|Type1<Type3>]",
    return $ checkTypeSuccess resolver
      [("#x",[])]
      "[[Type4<Type0>|#x]&Type1<Type3>]",

    return $ checkTypeFail resolver
      []
      "[Type0]",

    return $ checkDefinesFail resolver
      [("#x",[])]
      "Instance1<#x>",
    return $ checkDefinesSuccess resolver
      [("#x",["requires Type3"])]
      "Instance1<#x>",
    return $ checkDefinesFail resolver
      [("#x",["defines Instance1<#x>"])]
      "Instance1<#x>",
    return $ checkDefinesSuccess resolver
      []
      "Instance1<Type3>",
    return $ checkDefinesSuccess resolver
      []
      "Instance1<Type1<Type3>>"
  ]


type0 = CategoryName "Type0"
type1 = CategoryName "Type1"
type2 = CategoryName "Type2"
type3 = CategoryName "Type3"
type4 = CategoryName "Type4"
type5 = CategoryName "Type5"
instance0 = CategoryName "Instance0"
instance1 = CategoryName "Instance1"

variances :: Map.Map CategoryName InstanceVariances
variances = Map.fromList $ [
    (type0,ParamSet []), -- Type0<>
    (type1,ParamSet [Invariant]), -- Type1<x>
    (type2,ParamSet [Contravariant,Invariant,Covariant]), -- Type2<x|y|z>
    (type3,ParamSet []), -- Type3<>
    (type4,ParamSet [Invariant]), -- Type4<x>
    (type5,ParamSet [Invariant]), -- Type5<x>
    (instance0,ParamSet []), -- Instance0<>
    (instance1,ParamSet [Contravariant]) -- Instance1<x|>
  ]

refines :: Map.Map CategoryName (Map.Map CategoryName (InstanceParams -> InstanceParams))
refines = Map.fromList $ [
    (type0,Map.fromList $ []),
    (type1,Map.fromList $ [
        -- Type1<x> -> Type0
        (type0,\(ParamSet [_]) ->
               ParamSet [])
      ]),
    (type2,Map.fromList $ [
        -- Type2<x,y,z> -> Type0 (inherited from Type1)
        (type0,\(ParamSet [_,_,_]) ->
               ParamSet []),
        -- Type2<x,y,z> -> Type1<x>
        (type1,\(ParamSet [x,_,_]) ->
               ParamSet [x])
      ]),
    (type3,Map.fromList $ [
        -- Type3 -> Type0
        (type0,\(ParamSet []) ->
               ParamSet [])
      ]),
    (type4,Map.fromList $ []),
    (type5,Map.fromList $ [])
  ]

defines :: Map.Map CategoryName (Map.Map CategoryName (InstanceParams -> InstanceParams))
defines = Map.fromList $ [
    (type0,Map.fromList $ [
        -- Type0 defines Instance1<Type0>
        (instance1,\(ParamSet []) ->
                   ParamSet [forceParse "Type0"])
      ]),
    (type1,Map.fromList $ []),
    (type2,Map.fromList $ []),
    (type3,Map.fromList $ [
        -- Type3 defines Instance0
        (instance0,\(ParamSet []) ->
                   ParamSet [])
      ]),
    (type4,Map.fromList $ []),
    (type5,Map.fromList $ [])
  ]

typeFilters :: Map.Map CategoryName (InstanceParams -> InstanceFilters)
typeFilters = Map.fromList $ [
    (type0,\(ParamSet []) -> ParamSet []),
    (type1,\(ParamSet [_]) ->
           ParamSet [
             -- x requires Type0
             -- x defines Instance0
             [forceParse "requires Type0",forceParse "defines Instance0"]
           ]),
    (type2,\(ParamSet [_,y,_]) ->
           ParamSet [
             -- x defines Instance1<Type3>
             [forceParse $ "defines Instance1<Type3>"],
             -- y defines Instance1<y>
             [forceParse $ "defines Instance1<" ++ show y ++ ">"],
             -- z defines Instance1<Type0>
             [forceParse $ "defines Instance1<Type0>"]
           ]),
    (type3,\(ParamSet []) -> ParamSet []),
    (type4,\(ParamSet [_]) ->
           ParamSet [
             -- x allows Type0
             [forceParse "allows Type0"]
           ]),
    (type5,\(ParamSet [_]) -> ParamSet [[]])
  ]

definesFilters :: Map.Map CategoryName (InstanceParams -> InstanceFilters)
definesFilters = Map.fromList $ [
    (instance0,\(ParamSet []) -> ParamSet []),
    (instance1,\(ParamSet [_]) ->
           ParamSet [
             -- x requires Type0
             [forceParse "requires Type0"]
           ])
  ]


checkSimpleConvertSuccess = checkConvertSuccess []

checkSimpleConvertFail = checkConvertFail []

checkConvertSuccess pa x y = return checked where
  prefix = x ++ " -> " ++ y ++ " " ++ showParams pa
  checked = do
    ([t1,t2],pa2) <- parseTheTest pa [x,y]
    check $ checkValueTypeMatch resolver pa2 t1 t2
  check c
    | isCompileError c = compileError $ prefix ++ ":\n" ++ show (getCompileError c)
    | otherwise = return ()

checkConvertFail pa x y = return checked where
  prefix = x ++ " /> " ++ y ++ " " ++ showParams pa
  checked = do
    ([t1,t2],pa2) <- parseTheTest pa [x,y]
    check $ checkValueTypeMatch resolver pa2 t1 t2
  check c
    | isCompileError c = return ()
    | otherwise = compileError $ prefix ++ ": Expected failure\n"

resolver :: TypeResolver CompileInfo
resolver = TypeResolver {
    trRefines = getParams refines,
    trDefines = getParams defines,
    trVariance = mapLookup variances,
    trTypeFilters = getTypeFilters,
    trDefinesFilters = getDefinesFilters,
    -- Type5 is concrete, somewhat arbitrarily.
    trConcrete = \t -> return (t == type5)
  }

getParams ma (TypeInstance n1 ps1) n2 = do
  ra <- mapLookup ma n1
  f <- mapLookup ra n2
  return $ f ps1

getTypeFilters (TypeInstance n ps) = do
  f <- mapLookup typeFilters n
  return $ f ps

getDefinesFilters (DefinesInstance n ps) = do
  f <- mapLookup definesFilters n
  return $ f ps


mapLookup :: (Ord n, Show n, CompileErrorM m, Monad m) => Map.Map n a -> n -> m a
mapLookup ma n = resolve $ n `Map.lookup` ma where
  resolve (Just x) = return x
  resolve _        = compileError $ "Map key " ++ show n ++ " not found"
