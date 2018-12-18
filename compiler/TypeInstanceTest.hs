{-# LANGUAGE Safe #-}

module TypeInstanceTest where

import Data.List (intercalate)
import Text.Parsec
import qualified Data.Map as Map

import CompileInfo
import ParseInstance
import ParserBase
import TestBase
import TypeInstance
import TypesBase


main = runAllTests [
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
      "(Type0|Type3)",
    checkSimpleConvertSuccess
      "Type3"
      "(Type0|Type1<Type0>)",
    checkSimpleConvertSuccess
      "(Type3|Type1<Type0>)"
      "Type0",
    checkSimpleConvertSuccess
      "(Type0&Type3)"
      "Type3",
    checkSimpleConvertSuccess
      "(Type1<Type0>&Type3)"
      "(Type1<Type0>|Type3)",
    checkSimpleConvertFail
      "(Type0|Type3)"
      "Type3",
    checkSimpleConvertFail
      "Type0"
      "(Type0&Type3)",
    checkSimpleConvertFail
      "(Type0|Type3)"
      "(Type0&Type3)",

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

    checkConvertSuccess
      [("x",[])]
      "x" "x",
    checkConvertFail
      [("x",[]),
       ("y",[])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires y"]),
       ("y",["allows x"])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires y"]),
       ("y",[])]
      "x" "y",
    checkConvertSuccess
      [("x",[]),
       ("y",["allows x"])]
      "x" "y",

    -- NOTE: defines are not checked for instance conversions.
    checkConvertSuccess
      [("x",["requires y","defines Instance0"]),
       ("y",["allows x"])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires y"]),
       ("y",["allows x","defines Instance0"])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires y","defines Instance0"]),
       ("y",["allows x","defines Instance0"])]
      "x" "y",

    checkConvertSuccess
      [("x",["requires z"]),
       ("y",["allows z"]),
       ("z",[])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires z"]),
       ("y",[]),
       ("z",["requires y"])]
      "x" "y",
    -- NOTE: This is technically valid, but the checking mechanism doesn't do
    -- a full graph search, so the writer needs to be explicit about implied
    -- additional filters, e.g., "x requires y" => "y allows x".
    checkConvertFail
      [("w",["allows x"]),
       ("x",[]),
       ("y",[]),
       ("z",["allows w","requires y"])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires Type3"]),
       ("y",["allows Type0"])]
      "x" "y",
    checkConvertSuccess
      [("x",["requires y"]),
       ("y",["requires Type3"])]
      "x" "Type0",
    checkConvertSuccess
      [("x",["allows y"]),
       ("y",["allows Type0"])]
      "Type3" "x",

    checkConvertSuccess
      [("x",[])]
      "Type2<Type0,Type0,x>" "Type2<Type0,Type0,x>",
    checkConvertFail
      [("x",[]),
       ("y",[])]
      "Type2<Type0,Type0,x>" "Type2<Type0,Type0,y>",
    checkConvertSuccess
      [("x",["requires y"]),
       ("y",["allows x"])]
      "Type2<Type0,Type0,x>" "Type2<Type0,Type0,y>",
    checkConvertSuccess
      [("x",["requires y"]),
       ("y",[])]
      "Type2<Type0,Type0,x>" "Type2<Type0,Type0,y>",
    checkConvertSuccess
      [("x",[]),
       ("y",["allows x"])]
      "Type2<Type0,Type0,x>" "Type2<Type0,Type0,y>",

    checkConvertSuccess
      [("x",[])]
      "Type2<x,Type0,Type0>" "Type2<x,Type0,Type0>",
    checkConvertFail
      [("x",[]),
       ("y",[])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",
    checkConvertFail
      [("x",["requires y"]),
       ("y",["allows x"])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",
    checkConvertFail
      [("x",["requires y"]),
       ("y",[])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",
    checkConvertFail
      [("x",[]),
       ("y",["allows x"])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",
    checkConvertSuccess
      [("x",["allows y"]),
       ("y",["requires x"])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",
    checkConvertSuccess
      [("x",["allows y"]),
       ("y",[])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",
    checkConvertSuccess
      [("x",[]),
       ("y",["requires x"])]
      "Type2<x,Type0,Type0>" "Type2<y,Type0,Type0>",

    checkConvertFail
      [("x",[])]
      "x" "Type0",
    checkConvertSuccess
      [("x",["requires Type0"])]
      "x" "Type0",
    checkConvertSuccess
      [("x",["requires Type3"])]
      "x" "Type0",
    checkConvertFail
      [("x",[])]
      "Type0" "x",
    checkConvertSuccess
      [("x",["allows Type0"])]
      "Type0" "x",
    checkConvertSuccess
      [("x",["allows Type0"])]
      "Type3" "x",

    checkConvertFail
      [("x",[])]
      "Type2<x,Type0,Type0>" "Type2<Type0,Type0,Type0>",
    checkConvertSuccess
      [("x",["allows Type0"])]
      "Type2<x,Type0,Type0>" "Type2<Type0,Type0,Type0>",
    checkConvertSuccess
      [("x",["allows Type0"])]
      "Type2<x,Type0,Type0>" "Type2<Type3,Type0,Type0>",
    checkConvertFail
      [("x",[])]
      "Type2<Type0,Type0,Type0>" "Type2<x,Type0,Type0>",
    checkConvertSuccess
      [("x",["requires Type0"])]
      "Type2<Type0,Type0,Type0>" "Type2<x,Type0,Type0>",
    checkConvertSuccess
      [("x",["requires Type3"])]
      "Type2<Type0,Type0,Type0>" "Type2<x,Type0,Type0>",

    checkConvertSuccess
      [("x",["requires y"]),
       ("y",["requires z"]),
       ("z",[])]
      "x" "y",
    checkConvertSuccess
      [("x",["allows z"]),
       ("y",["allows x"]),
       ("z",[])]
      "x" "y",

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
      "optional Type3"
  ]


type0 = TypeName "Type0"
type1 = TypeName "Type1"
type2 = TypeName "Type2"
type3 = TypeName "Type3"
instance0 = TypeName "Instance0"
instance1 = TypeName "Instance1"

variances :: Map.Map TypeName InstanceVariances
variances = Map.fromList $ [
    (type0,ParamSet []), -- Type0<>
    (type1,ParamSet [Invariant]), -- Type1<x>
    (type2,ParamSet [Contravariant,Invariant,Covariant]), -- Type2<x|y|z>
    (type3,ParamSet []), -- Type3<>
    (instance0,ParamSet []), -- Instance0<>
    (instance1,ParamSet [Covariant]) -- Instance2<|x>
  ]

refines :: Map.Map TypeName (Map.Map TypeName (InstanceParams -> InstanceParams))
refines = Map.fromList $ [
    (type0,Map.fromList $ [
        -- Type0 defines Instance0
        (instance0,\(ParamSet []) ->
               ParamSet [])
      ]),
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
    (instance0,Map.fromList $ []),
    (instance1,Map.fromList $ [])
  ]


checkSimpleConvertSuccess = checkConvertSuccess []

checkSimpleConvertFail = checkConvertFail []

checkConvertSuccess pa x y = return checked where
  prefix = x ++ " -> " ++ y ++ " " ++ showParams pa
  checked = do
    (t1,t2,pa2) <- parseTheTest pa x y
    check $ checkValueTypeMatch resolver pa2 t1 t2
  check (Left es) = compileError $ prefix ++ ": " ++ show es
  check _ = return ()

checkConvertFail pa x y = return checked where
  prefix = x ++ " /> " ++ y ++ " " ++ showParams pa
  checked = do
    (t1,t2,pa2) <- parseTheTest pa x y
    check $ checkValueTypeMatch resolver pa2 t1 t2
  check (Right _) = compileError $ prefix ++ ": Expected failure"
  check _ = return ()

showParams pa = "[" ++ intercalate "," (concat $ map expand pa) ++ "]" where
  expand (n,ps) = map (\p -> n ++ " " ++ p) ps

parseTheTest :: [(String,[String])] -> String -> String ->
                CompileInfo (ValueType,ValueType,ParamFilters)
parseTheTest pa x y = parsed where
  parsed = do
    t1 <- parseObject x
    t2 <- parseObject y
    pa2 <- collectAllOrErrorM $ map parseFilters pa
    return (t1,t2,Map.fromList pa2)
  parseFilters (n,fs) = do
    fs2 <- collectAllOrErrorM $ map parseObject fs
    return (ParamName n,fs2)
  parseObject s = checked parsed where
    parsed = parse (between optionalSpace endOfDoc sourceParser) "(string)" s
    checked (Right t) = return t
    checked (Left e)  = compileError (show e)

resolver :: TypeResolver CompileInfo ()
resolver = TypeResolver {
    trRefines = getParams refines,
    -- NOTE: In these tests, we just treat value and instance types the same for
    -- the purposes of lookup.
    trDefines = getParams refines,
    trVariance = mapLookup variances,
    trFilters = undefined
  }

getParams ma (TypeInstance n1 ps1) n2 = do
  ra <- mapLookup ma n1
  f <- mapLookup ra n2
  return ((),f ps1)


mapLookup :: (Ord n, Show n, CompileErrorM m, Monad m) => Map.Map n a -> n -> m a
mapLookup ma n = resolve $ n `Map.lookup` ma where
  resolve (Just x) = return x
  resolve _        = compileError $ "Map key " ++ show n ++ " not found"
