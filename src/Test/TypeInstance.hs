{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

module Test.TypeInstance (tests) where

import Control.Monad (when)
import qualified Data.Map as Map

import Base.CompilerError
import Base.GeneralType
import Base.Positional
import Base.TrackedErrors
import Base.MergeTree
import Base.Mergeable
import Parser.TypeInstance ()
import Test.Common
import Types.TypeInstance
import Types.Variance


tests :: [IO (TrackedErrors ())]
tests = [
    checkParseSuccess
      "Type0"
      (singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional [])),
    checkParseSuccess
      "Type0<Type1,Type2>"
      (singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type2") (Positional [])
        ])),
    checkParseSuccess
      "#x"
      (singleType $ JustParamName False $ ParamName "#x"),
    checkParseSuccess
      "#self"
      (singleType $ JustParamName False $ ParamSelf),
    checkParseSuccess
      "Type0<#self>"
      (singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional [
          singleType $ JustParamName False $ ParamSelf
        ])),
    checkParseFail "x",
    checkParseFail "",

    checkParseSuccess
      "[Type0&Type0]"
      (singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional [])),
    checkParseSuccess
      "[Type0|Type0]"
      (singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional [])),
    checkParseSuccess "all" minBound,
    checkParseSuccess "any" maxBound,
    checkParseFail "[Type0]",
    checkParseFail "[]",

    checkParseSuccess
      "[Type1&Type0&Type1]"
      (mergeAll [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional [])
        ]),
    checkParseSuccess
      "[Type1|Type0|Type1]"
      (mergeAny [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional [])
        ]),
    checkParseFail "[Type0&Type1|Type2]",
    checkParseSuccess
      "[Type0<#x>&#x]"
      (mergeAll [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional [
              singleType $ JustParamName False $ ParamName "#x"
            ]),
          singleType $ JustParamName False $ ParamName "#x"
        ]),
    checkParseFail "[Type0&]",
    checkParseFail "[Type0|]",
    checkParseFail "[Type0 Type1]",

    checkParseSuccess
      "[Type0&[Type1&Type3]&Type2]"
      (mergeAll [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type2") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type3") (Positional [])
        ]),
    checkParseSuccess
      "[Type0|[Type1|Type3]|Type2]"
      (mergeAny [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type2") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type3") (Positional [])
        ]),
    checkParseSuccess
      "[Type0&[Type1|Type3]&Type2]"
      (mergeAll [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type2") (Positional []),
          mergeAny [
              singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional []),
              singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type3") (Positional [])
            ]
        ]),
    checkParseSuccess
      "[Type0|[Type1&Type3]|Type2]"
      (mergeAny [
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type0") (Positional []),
          singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type2") (Positional []),
          mergeAll [
              singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type1") (Positional []),
              singleType $ JustTypeInstance $ TypeInstance (CategoryName "Type3") (Positional [])
            ]
        ]),

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

    -- Make sure that both sides are separately expanded with checks from
    -- intersection to union.
    checkSimpleConvertSuccess
      "[Type1<Type0>&Type4<Type0>]"
      "[[Type1<Type0>&Type4<Type0>]|Type5<Type0>]",
    checkSimpleConvertSuccess
      "[[Type1<Type0>|Type4<Type0>]&Type5<Type0>]"
      "[Type1<Type0>|Type4<Type0>]",

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
    checkSimpleConvertFail
      "Type1<all>"
      "Type1<any>",
    checkSimpleConvertFail
      "Type1<any>"
      "Type1<all>",
    checkSimpleConvertSuccess
      "Type1<any>"
      "Type1<any>",
    checkSimpleConvertSuccess
      "Type1<all>"
      "Type1<all>",

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

    return $ checkTypeSuccess Resolver
      []
      "Type4<Type0>",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type4<[#x&Type0]>",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type4<[#x|Type0]>",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type4<[#x|Type3]>",
    return $ checkTypeFail Resolver
      []
      "Type5<#x>",
    return $ checkTypeSuccess Resolver
      ["#x"]
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

    return $ checkTypeSuccess Resolver
      ["#x"]
      "#x",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type1<#x>",
    return $ checkTypeSuccess Resolver
      []
      "Type1<all>",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type1<#x>",
    return $ checkTypeSuccess Resolver
      []
      "Type1<Type3>",
    return $ checkTypeSuccess Resolver
      []
      "Type1<Type1<Type3>>",
    return $ checkTypeSuccess Resolver
      []
      "Type2<Type0,Type0,Type0>",
    return $ checkTypeSuccess Resolver
      []
      "Type2<all,Type0,Type0>",
    return $ checkTypeSuccess Resolver
      []
      "Type2<any,Type0,Type0>",
    return $ checkTypeSuccess Resolver
      []
      "Type4<any>",
    return $ checkTypeSuccess Resolver
      []
      "Type4<all>",

    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type2<#x,#x,#x>",

    return $ checkTypeSuccess Resolver
      []
      "Type4<Type0>",
    return $ checkTypeFail Resolver
      []
      "Type5<#x>",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "Type5<#x>",

    return $ checkTypeSuccess Resolver
      []
      "[Type4<Type0>|Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      []
      "[Type4<Type0>&Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "[Type5<#x>|Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "[Type5<#x>&Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "[#x|Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "[#x&Type1<Type3>]",
    return $ checkTypeFail Resolver
      ["#x"]
      "[Type4<Type0>|Instance0]",
    return $ checkTypeFail Resolver
      ["#x"]
      "[Type4<Type0>&Instance0]",

    return $ checkTypeSuccess Resolver
      []
      "[[Type4<Type0>&Type1<Type3>]|Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      []
      "[[Type4<Type0>|Type1<Type3>]&Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "[[Type4<Type0>&#x]|Type1<Type3>]",
    return $ checkTypeSuccess Resolver
      ["#x"]
      "[[Type4<Type0>|#x]&Type1<Type3>]",

    return $ checkDefinesSuccess Resolver
      ["#x"]
      "Instance1<#x>",
    return $ checkDefinesFail Resolver
      []
      "Instance1<#x>",
    return $ checkDefinesSuccess Resolver
      []
      "Instance1<Type3>",
    return $ checkDefinesSuccess Resolver
      []
      "Instance1<Type1<Type3>>",

    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Type1<Type0>" "Type1<#x>"
      (mergeLeaf ("#x","Type0",Invariant)),
    checkInferenceFail
      [("#x",[])] ["#x"]
      "Type1<Type3>" "Type4<#x>",

    checkInferenceSuccess
      [("#x",[])] ["#x"]
      -- Invariant should not be split for non-merged types.
      "Type1<Type1<Type1<Type0>>>" "Type1<Type1<Type1<#x>>>"
      (mergeLeaf ("#x","Type0",Invariant)),

    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Instance1<Type1<Type3>>" "Instance1<#x>"
      (mergeLeaf ("#x","Type1<Type3>",Contravariant)),
    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Instance1<Type1<Type3>>" "Instance1<Type1<#x>>"
      (mergeLeaf ("#x","Type3",Invariant)),

    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Type2<Type3,Type0,Type3>" "Type2<#x,Type0,#x>"
      (mergeAll [mergeLeaf ("#x","Type3",Contravariant),
                 mergeLeaf ("#x","Type3",Covariant)]),
    checkInferenceSuccess
      [("#x",[]),("#y",[])] ["#x"]
      "Type2<Type3,#y,Type3>" "Type2<#x,#y,#x>"
      (mergeAll [mergeLeaf ("#x","Type3",Contravariant),
                 mergeLeaf ("#x","Type3",Covariant)]),
    checkInferenceFail
      [("#x",[]),("#y",[])] ["#x"]
      "Type2<Type3,Type0,Type3>" "Type2<#x,#y,#x>",

    checkInferenceSuccess
      [("#x",[]),("#y",[])] ["#x"]
      "Type2<Type3,#y,Type0>" "Type1<#x>"
      (mergeLeaf ("#x","Type3",Invariant)),

    checkInferenceSuccess
      [("#x",[]),("#y",[])] ["#x"]
      "Instance1<#y>" "Instance1<#x>"
      (mergeLeaf ("#x","#y",Contravariant)),

    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Instance1<Instance0>" "Instance1<[#x&Type0]>"
      (mergeLeaf ("#x","Instance0",Contravariant)),
    checkInferenceFail
      [("#x",[])] ["#x"]
      "Instance1<Instance0>" "Instance1<[#x|Type0]>",
    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Instance1<Type1<Type0>>" "Instance1<[Type0&Type1<#x>]>"
      (mergeLeaf ("#x","Type0",Invariant)),
    checkInferenceSuccess
      [("#x",[])] ["#x"]
      "Instance1<Type1<Type0>>" "Instance1<[#x&Type1<#x>]>"
      (mergeAny [mergeLeaf ("#x","Type0",Invariant),
                 mergeLeaf ("#x","Type1<Type0>",Contravariant)]),

    checkInferenceSuccess
      [("#x",[]),("#y",["allows #x"])] ["#x"]
      "Type0" "#y"  -- The filter for #y influences the guess for #x.
      (mergeLeaf ("#x","Type0",Covariant))
  ]


type0 :: CategoryName
type0 = CategoryName "Type0"

type1 :: CategoryName
type1 = CategoryName "Type1"

type2 :: CategoryName
type2 = CategoryName "Type2"

type3 :: CategoryName
type3 = CategoryName "Type3"

type4 :: CategoryName
type4 = CategoryName "Type4"

type5 :: CategoryName
type5 = CategoryName "Type5"

instance0 :: CategoryName
instance0 = CategoryName "Instance0"

instance1 :: CategoryName
instance1 = CategoryName "Instance1"

variances :: Map.Map CategoryName InstanceVariances
variances = Map.fromList $ [
    (type0,Positional []), -- Type0<>
    (type1,Positional [Invariant]), -- Type1<x>
    (type2,Positional [Contravariant,Invariant,Covariant]), -- Type2<x|y|z>
    (type3,Positional []), -- Type3<>
    (type4,Positional [Invariant]), -- Type4<x>
    (type5,Positional [Invariant]), -- Type5<x>
    (instance0,Positional []), -- Instance0<>
    (instance1,Positional [Contravariant]) -- Instance1<x|>
  ]

refines :: Map.Map CategoryName (Map.Map CategoryName (InstanceParams -> InstanceParams))
refines = Map.fromList $ [
    (type0,Map.fromList $ []),
    (type1,Map.fromList $ [
        -- Type1<x> -> Type0
        (type0,\(Positional [_]) ->
               Positional [])
      ]),
    (type2,Map.fromList $ [
        -- Type2<x,y,z> -> Type0 (inherited from Type1)
        (type0,\(Positional [_,_,_]) ->
               Positional []),
        -- Type2<x,y,z> -> Type1<x>
        (type1,\(Positional [x,_,_]) ->
               Positional [x])
      ]),
    (type3,Map.fromList $ [
        -- Type3 -> Type0
        (type0,\(Positional []) ->
               Positional [])
      ]),
    (type4,Map.fromList $ []),
    (type5,Map.fromList $ [])
  ]

defines :: Map.Map CategoryName (Map.Map CategoryName (InstanceParams -> InstanceParams))
defines = Map.fromList $ [
    (type0,Map.fromList $ [
        -- Type0 defines Instance1<Type0>
        (instance1,\(Positional []) ->
                   Positional [forceParse "Type0"])
      ]),
    (type1,Map.fromList $ []),
    (type2,Map.fromList $ []),
    (type3,Map.fromList $ [
        -- Type3 defines Instance0
        (instance0,\(Positional []) ->
                   Positional [])
      ]),
    (type4,Map.fromList $ []),
    (type5,Map.fromList $ [])
  ]

typeFilters :: Map.Map CategoryName (InstanceParams -> InstanceFilters)
typeFilters = Map.fromList $ [
    (type0,\(Positional []) -> Positional []),
    (type1,\(Positional [_]) ->
           Positional [
             -- x requires Type0
             -- x defines Instance0
             [forceParse "requires Type0",forceParse "defines Instance0"]
           ]),
    (type2,\(Positional [_,y,_]) ->
           Positional [
             -- x defines Instance1<Type3>
             [forceParse $ "defines Instance1<Type3>"],
             -- y defines Instance1<y>
             [forceParse $ "defines Instance1<" ++ show y ++ ">"],
             -- z defines Instance1<Type0>
             [forceParse $ "defines Instance1<Type0>"]
           ]),
    (type3,\(Positional []) -> Positional []),
    (type4,\(Positional [_]) ->
           Positional [
             -- x allows Type0
             [forceParse "allows Type0"]
           ]),
    (type5,\(Positional [_]) -> Positional [[]])
  ]

definesFilters :: Map.Map CategoryName (InstanceParams -> InstanceFilters)
definesFilters = Map.fromList $ [
    (instance0,\(Positional []) -> Positional []),
    (instance1,\(Positional [_]) ->
           Positional [
             -- x requires Type0
             [forceParse "requires Type0"]
           ])
  ]

checkParseSuccess :: String -> GeneralInstance -> IO (TrackedErrors ())
checkParseSuccess x y = return $ do
  t <- readSingle "(string)" x <!! ("When parsing " ++ show x)
  when (t /= y) $ compilerErrorM $ "Expected " ++ show x ++ " to parse as " ++ show y

checkParseFail :: String -> IO (TrackedErrors ())
checkParseFail x = return $ do
  let t = readSingle "(string)" x :: TrackedErrors GeneralInstance
  when (not $ isCompilerError t) $
    compilerErrorM $ "Expected failure to parse " ++ show x ++
                    " but got " ++ show (getCompilerSuccess t)

checkSimpleConvertSuccess :: String -> String -> IO (TrackedErrors ())
checkSimpleConvertSuccess = checkConvertSuccess []

checkSimpleConvertFail :: String -> String -> IO (TrackedErrors ())
checkSimpleConvertFail = checkConvertFail []

checkConvertSuccess :: [(String, [String])] -> String -> String -> IO (TrackedErrors ())
checkConvertSuccess pa x y = return checked where
  prefix = x ++ " -> " ++ y ++ " " ++ showFilters pa
  checked = do
    ([t1,t2],pa2) <- parseTestWithFilters pa [x,y]
    check $ checkValueAssignment Resolver pa2 t1 t2
  check c
    | isCompilerError c = compilerErrorM $ prefix ++ ":\n" ++ show (getCompilerError c)
    | otherwise = return ()

checkInferenceSuccess :: [(String, [String])] -> [String] -> String ->
  String -> MergeTree (String,String,Variance) -> IO (TrackedErrors ())
checkInferenceSuccess pa is x y gs = checkInferenceCommon check pa is x y gs where
  prefix = x ++ " -> " ++ y ++ " " ++ showFilters pa
  check gs2 c
    | isCompilerError c = compilerErrorM $ prefix ++ ":\n" ++ show (getCompilerError c)
    | otherwise        = getCompilerSuccess c `checkEquals` gs2

checkInferenceFail :: [(String, [String])] -> [String] -> String ->
  String -> IO (TrackedErrors ())
checkInferenceFail pa is x y = checkInferenceCommon check pa is x y (mergeAll []) where
  prefix = x ++ " -> " ++ y ++ " " ++ showFilters pa
  check _ c
    | isCompilerError c = return ()
    | otherwise = compilerErrorM $ prefix ++ ": Expected failure\n"

checkInferenceCommon ::
  (MergeTree InferredTypeGuess -> TrackedErrors (MergeTree InferredTypeGuess) -> TrackedErrors ()) ->
  [(String, [String])] -> [String] -> String -> String ->
  MergeTree (String,String,Variance) -> IO (TrackedErrors ())
checkInferenceCommon check pa is x y gs = return $ checked <!! context where
  context = "With params = " ++ show pa ++ ", pair = (" ++ show x ++ "," ++ show y ++ ")"
  checked = do
    ([t1,t2],pa2) <- parseTestWithFilters pa [x,y]
    ia2 <- mapCompilerM readInferred is
    gs' <- sequence $ fmap parseGuess gs
    let iaMap = Map.fromList ia2
    -- TODO: Merge duplication with Test.TypeCategory.
    pa3 <- fmap Map.fromList $ mapCompilerM (filterSub iaMap) $ Map.toList pa2
    t2' <- uncheckedSubInstance (weakLookup iaMap) t2
    check gs' $ checkGeneralMatch Resolver pa3 Covariant t1 t2'
  readInferred p = do
    p' <- readSingle "(string)" p
    return (p',singleType $ JustInferredType p')
  parseGuess (p,t,v) = do
    p' <- readSingle "(string)" p
    t' <- readSingle "(string)" t
    return $ InferredTypeGuess p' t' v
  weakLookup tm n =
    case n `Map.lookup` tm of
         Just t  -> return t
         Nothing -> return $ singleType $ JustParamName True n
  filterSub im (k,fs) = do
    fs' <- mapCompilerM (uncheckedSubFilter (weakLookup im)) fs
    return (k,fs')

checkConvertFail :: [(String, [String])] -> String -> String -> IO (TrackedErrors ())
checkConvertFail pa x y = return checked where
  prefix = x ++ " /> " ++ y ++ " " ++ showFilters pa
  checked = do
    ([t1,t2],pa2) <- parseTestWithFilters pa [x,y]
    check $ checkValueAssignment Resolver pa2 t1 t2
  check :: TrackedErrors a -> TrackedErrors ()
  check c
    | isCompilerError c = return ()
    | otherwise = compilerErrorM $ prefix ++ ": Expected failure\n"

data Resolver = Resolver

instance TypeResolver Resolver where
  trRefines _ = getParams refines
  trDefines _ = getParams defines
  trVariance _ = mapLookup variances
  trTypeFilters _ = getTypeFilters
  trDefinesFilters _ = getDefinesFilters
  -- Type5 is concrete, somewhat arbitrarily.
  trConcrete _ = \t -> return (t == type5)

getParams :: CollectErrorsM m =>
  Map.Map CategoryName (Map.Map CategoryName (InstanceParams -> InstanceParams))
  -> TypeInstance -> CategoryName -> m InstanceParams
getParams ma (TypeInstance n1 ps1) n2 = do
  ra <- mapLookup ma n1 <?? "In lookup of category " ++ show n1
  f <- mapLookup ra n2 <?? "In lookup of parent " ++ show n2 ++ " of " ++ show n1
  return $ f ps1

getTypeFilters :: CollectErrorsM m => TypeInstance -> m InstanceFilters
getTypeFilters (TypeInstance n ps) = "In type filters lookup" ??> do
  f <- mapLookup typeFilters n
  return $ f ps

getDefinesFilters :: CollectErrorsM m => DefinesInstance -> m InstanceFilters
getDefinesFilters (DefinesInstance n ps) = "In defines filters lookup" ??> do
  f <- mapLookup definesFilters n
  return $ f ps

mapLookup :: (Ord n, Show n, CollectErrorsM m) => Map.Map n a -> n -> m a
mapLookup ma n = resolve $ n `Map.lookup` ma where
  resolve (Just x) = return x
  resolve _        = compilerErrorM $ "Map key " ++ show n ++ " not found"
