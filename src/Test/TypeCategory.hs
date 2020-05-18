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

{-# LANGUAGE Safe #-}

module Test.TypeCategory (tests) where

import Control.Arrow
import System.FilePath
import Text.Parsec
import qualified Data.Map as Map

import Base.CompileError
import Base.Mergeable
import Compilation.CompileInfo
import Parser.TypeCategory ()
import Test.Common
import Types.Builtin
import Types.Positional
import Types.TypeCategory
import Types.TypeInstance
import Types.Variance


tests :: [IO (CompileInfo ())]
tests = [
    checkSingleParseSuccess ("testfiles" </> "value_interface.0rx"),
    checkSingleParseSuccess ("testfiles" </> "type_interface.0rx"),
    checkSingleParseSuccess ("testfiles" </> "concrete.0rx"),

    checkShortParseSuccess "concrete Type<#x> {}",
    checkShortParseSuccess "concrete Type {}",
    checkShortParseFail "concrete Type<T> {}",
    checkShortParseFail "concrete Type<optional> {}",
    checkShortParseFail "concrete Type<optional T> {}",
    checkShortParseFail "concrete Type<T<#x>> {}",
    checkShortParseSuccess "concrete Type { refines T }",
    checkShortParseFail "concrete Type { refines #x }",
    checkShortParseSuccess "concrete Type { defines T }",
    checkShortParseFail "concrete Type { defines #x }",
    checkShortParseFail "concrete Type { refines optional }",
    checkShortParseFail "concrete Type { refines optional T }",
    checkShortParseSuccess "concrete Type<#x|#y> { #x requires #y }",
    checkShortParseSuccess "concrete Type<#x|#y> { #x allows #y }",
    checkShortParseSuccess "concrete Type<#x|#y> { #x defines T }",
    checkShortParseFail "concrete Type<#x|#y> { #x defines #y }",

    checkShortParseSuccess "@type interface Type<#x> {}",
    checkShortParseSuccess "@type interface Type {}",
    checkShortParseFail "@type interface Type { refines T }",
    checkShortParseFail "@type interface Type { defines T }",
    checkShortParseSuccess "@type interface Type<#x> { #x allows T }",

    checkShortParseSuccess "@value interface Type<#x> {}",
    checkShortParseSuccess "@value interface Type {}",
    checkShortParseSuccess "@value interface Type { refines T }",
    checkShortParseFail "@value interface Type { defines T }",
    checkShortParseSuccess "@value interface Type<#x> { #x allows T }",

    checkOperationSuccess ("testfiles" </> "value_refines_value.0rx") (checkConnectedTypes defaultCategories),
    checkOperationFail ("testfiles" </> "value_refines_instance.0rx") (checkConnectedTypes defaultCategories),
    checkOperationFail ("testfiles" </> "value_refines_concrete.0rx") (checkConnectedTypes defaultCategories),

    checkOperationSuccess ("testfiles" </> "concrete_refines_value.0rx") (checkConnectedTypes defaultCategories),
    checkOperationFail ("testfiles" </> "concrete_refines_instance.0rx") (checkConnectedTypes defaultCategories),
    checkOperationFail ("testfiles" </> "concrete_refines_concrete.0rx") (checkConnectedTypes defaultCategories),
    checkOperationSuccess ("testfiles" </> "concrete_defines_instance.0rx") (checkConnectedTypes defaultCategories),
    checkOperationFail ("testfiles" </> "concrete_defines_value.0rx") (checkConnectedTypes defaultCategories),
    checkOperationFail ("testfiles" </> "concrete_defines_concrete.0rx") (checkConnectedTypes defaultCategories),

    checkOperationSuccess
      ("testfiles" </> "concrete_refines_value.0rx")
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent2",InstanceInterface [] NoNamespace (CategoryName "Parent2") [] [] [])
        ]),
    checkOperationFail
      ("testfiles" </> "concrete_refines_value.0rx")
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",InstanceInterface [] NoNamespace (CategoryName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      ("testfiles" </> "partial.0rx")
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",ValueInterface [] NoNamespace (CategoryName "Parent") [] [] [] [])
        ]),
    checkOperationFail
      ("testfiles" </> "partial.0rx")
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",InstanceInterface [] NoNamespace (CategoryName "Parent") [] [] [])
        ]),
    checkOperationFail
      ("testfiles" </> "partial.0rx")
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",ValueConcrete [] NoNamespace (CategoryName "Parent") [] [] [] [] [])
        ]),

    checkOperationSuccess ("testfiles" </> "value_refines_value.0rx") (checkConnectionCycles Map.empty),
    checkOperationSuccess ("testfiles" </> "concrete_refines_value.0rx") (checkConnectionCycles Map.empty),
    checkOperationSuccess ("testfiles" </> "concrete_defines_instance.0rx") (checkConnectionCycles Map.empty),
    checkOperationFail ("testfiles" </> "value_cycle.0rx") (checkConnectionCycles Map.empty),

    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        map (show . getCategoryName) ts2 `containsPaired` [
            "Object2","Object3","Object1","Type","Parent","Child"
          ]),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2),

    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        scrapeAllRefines ts3 `containsExactly` [
            ("Object1","Object3<#y>"),
            ("Object1","Object2"),
            ("Object3","Object2"),
            ("Parent","Object1<#x,Object3<Object2>>"),
            ("Parent","Object3<Object3<Object2>>"),
            ("Parent","Object2"),
            ("Child","Parent<Child>"),
            ("Child","Object1<Child,Object3<Object2>>"),
            ("Child","Object3<Object3<Object2>>"),
            ("Child","Object2")
          ]
        scrapeAllDefines ts3 `containsExactly` [
            ("Child","Type<Child>")
          ]),

    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        existing  <- return $ Map.fromList [
            (CategoryName "Parent2",InstanceInterface [] NoNamespace (CategoryName "Parent2") [] [] [])
          ]
        ts2 <- topoSortCategories existing ts
        flattenAllConnections existing ts2),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        existing  <- return $ Map.fromList [
            (CategoryName "Parent",InstanceInterface [] NoNamespace (CategoryName "Parent") [] [] [])
          ]
        topoSortCategories existing ts),

    checkOperationSuccess
      ("testfiles" </> "partial.0rx")
      (\ts -> do
        existing  <- return $ Map.fromList [
            (CategoryName "Parent",
            ValueInterface [] NoNamespace (CategoryName "Parent") []
                           [ValueRefine [] $ TypeInstance (CategoryName "Object1") (Positional []),
                           ValueRefine [] $ TypeInstance (CategoryName "Object2") (Positional [])] [] []),
            -- NOTE: Object1 deliberately excluded here so that we catch
            -- unnecessary recursion in existing categories.
            (CategoryName "Object2",
            ValueInterface [] NoNamespace (CategoryName "Object2") [] [] [] [])
          ]
        ts2 <- topoSortCategories existing ts
        ts3 <- flattenAllConnections existing ts2
        scrapeAllRefines ts3 `containsExactly` [
            ("Child","Parent"),
            ("Child","Object1"),
            ("Child","Object2")
          ]),

    checkOperationSuccess ("testfiles" </> "valid_variances.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "contravariant_refines_covariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "contravariant_refines_invariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "covariant_refines_contravariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "covariant_refines_invariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "contravariant_defines_covariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "contravariant_defines_invariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "covariant_defines_contravariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "covariant_defines_invariant.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "concrete_duplicate_param.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "type_duplicate_param.0rx") (checkParamVariances defaultCategories),
    checkOperationFail ("testfiles" </> "value_duplicate_param.0rx") (checkParamVariances defaultCategories),

    checkOperationSuccess
      ("testfiles" </> "concrete_refines_value.0rx")
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent2",InstanceInterface [] NoNamespace (CategoryName "Parent2") [] [] [])
        ]),
    checkOperationFail
      ("testfiles" </> "concrete_refines_value.0rx")
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",InstanceInterface [] NoNamespace (CategoryName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      ("testfiles" </> "partial_params.0rx")
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",
           ValueInterface [] NoNamespace (CategoryName "Parent")
                          [ValueParam [] (ParamName "#w") Contravariant,
                           ValueParam [] (ParamName "#z") Covariant] [] [] [])
      ]),
    checkOperationFail
      ("testfiles" </> "partial_params.0rx")
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",
           ValueInterface [] NoNamespace (CategoryName "Parent")
                          [ValueParam [] (ParamName "#w") Invariant,
                           ValueParam [] (ParamName "#z") Covariant] [] [] [])
      ]),
    checkOperationFail
      ("testfiles" </> "partial_params.0rx")
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",
           ValueInterface [] NoNamespace (CategoryName "Parent")
                          [ValueParam [] (ParamName "#w") Contravariant,
                           ValueParam [] (ParamName "#z") Invariant] [] [] [])
      ]),

    checkOperationSuccess
      ("testfiles" </> "concrete.0rx")
      (\ts -> do
        rs <- getTypeRefines ts "Type<#a,#b,#c,#d,#e,#f>" "Type"
        rs `containsPaired` ["#a","#b","#c","#d","#e","#f"]
        ),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        rs <- getTypeRefines ts3 "Object1<#a,#b>" "Object1"
        rs `containsPaired` ["#a","#b"]),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        rs <- getTypeRefines ts3 "Object1<#a,#b>" "Object3"
        rs `containsPaired` ["#b"]),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        rs <- getTypeRefines ts3 "Undefined<#a,#b>" "Undefined"
        rs `containsPaired` ["#a","#b"]),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        rs <- getTypeRefines ts3 "Object1<#a>" "Object1"
        rs `containsPaired` ["#a"]),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        rs <- getTypeRefines ts3 "Parent<#t>" "Object1"
        rs `containsPaired` ["#t","Object3<Object2>"]),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        getTypeRefines ts3 "Parent<#t>" "Child"),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        getTypeRefines ts3 "Child" "Type"),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        getTypeRefines ts3 "Child" "Missing"),

    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        rs <- getTypeDefines ts "Child" "Type"
        rs `containsPaired` ["Child"]),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        getTypeDefines ts "Child" "Parent"),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        getTypeDefines ts "Child" "Missing"),

    checkOperationSuccess
      ("testfiles" </> "concrete.0rx")
      (\ts -> do
        vs <- getTypeVariance ts "Type"
        vs `containsPaired` [Contravariant,Contravariant,
                             Invariant,Invariant,
                             Covariant,Covariant]),
    checkOperationFail
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        getTypeVariance ts "Missing"),

    checkOperationSuccess
      ("testfiles" </> "concrete.0rx")
      (\ts -> do
        rs <- getTypeFilters ts "Type<#a,#b,#c,#d,#e,#f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<#a>"],
            ["defines Equals<#c>"],
            [],
            [],
            []
          ]),
    checkOperationSuccess
      ("testfiles" </> "concrete.0rx")
      (\ts -> do
        rs <- getTypeFilters ts "Type<Type<#t>,#b,Type3<#x>,#d,#e,#f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<#t>>"],
            ["defines Equals<Type3<#x>>"],
            [],
            [],
            []
          ]),

    checkOperationSuccess
      ("testfiles" </> "value_interface.0rx")
      (\ts -> do
        rs <- getTypeFilters ts "Type<#a,#b,#c,#d,#e,#f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<#a>"],
            ["defines Equals<#c>"],
            [],
            [],
            []
          ]),
    checkOperationSuccess
      ("testfiles" </> "value_interface.0rx")
      (\ts -> do
        rs <- getTypeFilters ts "Type<Type<#t>,#b,Type3<#x>,#d,#e,#f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<#t>>"],
            ["defines Equals<Type3<#x>>"],
            [],
            [],
            []
          ]),

    checkOperationSuccess
      ("testfiles" </> "type_interface.0rx")
      (\ts -> do
        rs <- getTypeDefinesFilters ts "Type<#a,#b,#c,#d,#e,#f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<#a>"],
            ["defines Equals<#c>"],
            [],
            [],
            []
          ]),
    checkOperationSuccess
      ("testfiles" </> "type_interface.0rx")
      (\ts -> do
        rs <- getTypeDefinesFilters ts "Type<Type<#t>,#b,Type3<#x>,#d,#e,#f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<#t>>"],
            ["defines Equals<Type3<#x>>"],
            [],
            [],
            []
          ]),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeSuccess r [] "Child"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeSuccess r [] "[Child|Child]"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeSuccess r [] "[Child&Child]"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeSuccess r [] "Object2"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeSuccess r [] "[Object2|Object2]"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeSuccess r [] "[Object2&Object2]"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeFail r [] "Type<Child>"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeFail r [] "[Type<Child>|Type<Child>]"),
    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ta <- flattenAllConnections defaultCategories ts2 >>= declareAllTypes defaultCategories
        let r = CategoryResolver ta
        checkTypeFail r [] "[Type<Child>&Type<Child>]"),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r [] "Value0<Value1,Value2>"),
    checkOperationFail
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r [] "Value0<Value1,Value1>"),
    checkOperationSuccess
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r [] "Value0<Value3,Value2>"),
    checkOperationFail
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r
          [("#x",[]),("#y",[])]
          "Value0<#x,#y>"),
    checkOperationSuccess
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r
          [("#x",["allows #y","requires Function<#x,#y>"]),
           ("#y",["requires #x","defines Equals<#y>"])]
          "Value0<#x,#y>"),
    checkOperationSuccess
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r
          [("#x",["allows Value2","requires Function<#x,Value2>"])]
          "Value0<#x,Value2>"),
    checkOperationFail
      ("testfiles" </> "filters.0rx")
      (\ts -> do
        ts2 <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts2 >>= declareAllTypes Map.empty
        let r = CategoryResolver ta
        checkTypeSuccess r
          [("#x",["allows Value2","requires Function<#x,Value2>"]),
           ("#y",["requires #x","defines Equals<#y>"])]
          "Value0<#x,#y>"),

    checkOperationSuccess
      ("testfiles" </> "concrete_instances.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationFail
      ("testfiles" </> "concrete_missing_define.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationFail
      ("testfiles" </> "concrete_missing_refine.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationSuccess
      ("testfiles" </> "value_instances.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationFail
      ("testfiles" </> "value_missing_define.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationFail
      ("testfiles" </> "value_missing_refine.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationSuccess
      ("testfiles" </> "type_instances.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationFail
      ("testfiles" </> "type_missing_define.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationFail
      ("testfiles" </> "type_missing_refine.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),
    checkOperationSuccess
      ("testfiles" </> "requires_concrete.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        checkCategoryInstances defaultCategories ts3),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      ("testfiles" </> "merged.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        ts3 <- flattenAllConnections defaultCategories ts2
        tm <- declareAllTypes defaultCategories ts3
        rs <- getRefines tm "Test"
        rs `containsExactly` ["Value0","Value1","Value2","Value3",
                              "Value4<Value1,Value1>","Inherit1","Inherit2"]),

    checkOperationSuccess
      ("testfiles" </> "merged.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "duplicate_refine.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "duplicate_define.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "refine_wrong_direction.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "inherit_incompatible.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "merge_incompatible.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),

    checkOperationSuccess
      ("testfiles" </> "flatten.0rx")
      (\ts -> do
        let tm0 = Map.fromList [
                    (CategoryName "Parent2",InstanceInterface [] NoNamespace (CategoryName "Parent2") [] [] [])
                  ]
        tm <- includeNewTypes tm0 ts
        rs <- getRefines tm "Child"
        rs `containsExactly` ["Parent<Child>","Object2",
                              "Object1<Child,Object3<Object2>>",
                              "Object3<Object3<Object2>>"]),

    checkOperationSuccess
      ("testfiles" </> "category_function_param_match.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_param_clash.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_duplicate_param.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_filter_param.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_allows_type.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_allows_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_requires_type.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_requires_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_defines_type.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_defines_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_arg.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_bad_return.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "weak_arg.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "weak_return.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),

    checkOperationSuccess
      ("testfiles" </> "function_filters_satisfied.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_requires_missed.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_allows_missed.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "function_defines_missed.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),

    checkOperationSuccess
      ("testfiles" </> "valid_function_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_value_arg_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_value_return_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_type_arg_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_type_return_variance.0rx")
      (\ts -> checkCategoryInstances defaultCategories ts),

    checkOperationSuccess
      ("testfiles" </> "valid_filter_variance.0rx")
      (\ts -> checkParamVariances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_allows_variance_right.0rx")
      (\ts -> checkParamVariances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_defines_variance_right.0rx")
      (\ts -> checkParamVariances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_requires_variance_right.0rx")
      (\ts -> checkParamVariances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_allows_variance_left.0rx")
      (\ts -> checkParamVariances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_defines_variance_left.0rx")
      (\ts -> checkParamVariances defaultCategories ts),
    checkOperationFail
      ("testfiles" </> "bad_requires_variance_left.0rx")
      (\ts -> checkParamVariances defaultCategories ts),

    checkOperationFail
      ("testfiles" </> "conflicting_declaration.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "conflicting_inherited.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "successful_merge.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "merge_with_refine.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "failed_merge.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "ambiguous_merge_inherit.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "merge_different_scopes.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),

    checkOperationSuccess
      ("testfiles" </> "successful_merge_params.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "failed_merge_params.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "preserve_merged.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationFail
      ("testfiles" </> "conflict_in_preserved.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ()),
    checkOperationSuccess
      ("testfiles" </> "resolved_in_preserved.0rx")
      (\ts -> do
        ts2 <- topoSortCategories defaultCategories ts
        flattenAllConnections defaultCategories ts2 >> return ())
  ]

getRefines :: Map.Map CategoryName (AnyCategory c) -> String -> CompileInfo [String]
getRefines tm n =
  case (CategoryName n) `Map.lookup` tm of
       (Just t) -> return $ map (show . vrType) (getCategoryRefines t)
       _ -> compileErrorM $ "Type " ++ n ++ " not found"

getDefines ::  Map.Map CategoryName (AnyCategory c) -> String -> CompileInfo [String]
getDefines tm n =
  case (CategoryName n) `Map.lookup` tm of
       (Just t) -> return $ map (show . vdType) (getCategoryDefines t)
       _ -> compileErrorM $ "Type " ++ n ++ " not found"

getTypeRefines :: Show c => [AnyCategory c] -> String -> String -> CompileInfo [String]
getTypeRefines ts s n = do
  ta <- declareAllTypes defaultCategories ts
  let r = CategoryResolver ta
  t <- readSingle "(string)" s
  Positional rs <- trRefines r t (CategoryName n)
  return $ map show rs

getTypeDefines :: Show c => [AnyCategory c] -> String -> String -> CompileInfo [String]
getTypeDefines ts s n = do
  ta <- declareAllTypes defaultCategories ts
  let r = CategoryResolver ta
  t <- readSingle "(string)" s
  Positional ds <- trDefines r t (CategoryName n)
  return $ map show ds

getTypeVariance :: Show c => [AnyCategory c] -> String -> CompileInfo [Variance]
getTypeVariance ts n = do
  ta <- declareAllTypes defaultCategories ts
  let r = CategoryResolver ta
  (Positional vs) <- trVariance r (CategoryName n)
  return vs

getTypeFilters :: Show c => [AnyCategory c] -> String -> CompileInfo [[String]]
getTypeFilters ts s = do
  ta <- declareAllTypes defaultCategories ts
  let r = CategoryResolver ta
  t <- readSingle "(string)" s
  Positional vs <- trTypeFilters r t
  return $ map (map show) vs

getTypeDefinesFilters :: Show c => [AnyCategory c] -> String -> CompileInfo [[String]]
getTypeDefinesFilters ts s = do
  ta <- declareAllTypes defaultCategories ts
  let r = CategoryResolver ta
  t <- readSingle "(string)" s
  Positional vs <- trDefinesFilters r t
  return $ map (map show) vs

scrapeAllRefines :: [AnyCategory c] -> [(String, String)]
scrapeAllRefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueInterface _ _ n _ rs _ _) = map ((,) n . vrType) rs
  scrapeSingle (ValueConcrete _ _ n _ rs _ _ _) = map ((,) n . vrType) rs
  scrapeSingle _ = []

scrapeAllDefines :: [AnyCategory c] -> [(String, String)]
scrapeAllDefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueConcrete _ _ n _ _ ds _ _) = map ((,) n . vdType) ds
  scrapeSingle _ = []

checkPaired :: Show a => (a -> a -> CompileInfo ()) -> [a] -> [a] -> CompileInfo ()
checkPaired f actual expected
  | length actual /= length expected =
    compileErrorM $ "Different item counts: " ++ show actual ++ " (actual) vs. " ++
                   show expected ++ " (expected)"
  | otherwise = mergeAllM $ map check (zip3 actual expected ([1..] :: [Int])) where
    check (a,e,n) = f a e `reviseErrorM` ("Item " ++ show n ++ " mismatch")

containsPaired :: (Eq a, Show a) => [a] -> [a] -> CompileInfo ()
containsPaired = checkPaired checkSingle where
  checkSingle a e
    | a == e = return ()
    | otherwise = compileErrorM $ show a ++ " (actual) vs. " ++ show e ++ " (expected)"

checkOperationSuccess :: String -> ([AnyCategory SourcePos] -> CompileInfo a) -> IO (CompileInfo ())
checkOperationSuccess f o = do
  contents <- loadFile f
  let parsed = readMulti f contents :: CompileInfo [AnyCategory SourcePos]
  return $ check (parsed >>= o >> return ())
  where
    check = flip reviseErrorM ("Check " ++ f ++ ":")

checkOperationFail :: String -> ([AnyCategory SourcePos] -> CompileInfo a) -> IO (CompileInfo ())
checkOperationFail f o = do
  contents <- loadFile f
  let parsed = readMulti f contents :: CompileInfo [AnyCategory SourcePos]
  return $ check (parsed >>= o >> return ())
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileErrorM $ "Check " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkSingleParseSuccess :: String -> IO (CompileInfo ())
checkSingleParseSuccess f = do
  contents <- loadFile f
  let parsed = readSingle f contents :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = compileErrorM $ "Parse " ++ f ++ ":\n" ++ show (getCompileError c)
      | otherwise = return ()

checkSingleParseFail :: String -> IO (CompileInfo ())
checkSingleParseFail f = do
  contents <- loadFile f
  let parsed = readSingle f contents :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileErrorM $ "Parse " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkShortParseSuccess :: String -> IO (CompileInfo ())
checkShortParseSuccess s = do
  let parsed = readSingle "(string)" s :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = compileErrorM $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkShortParseFail :: String -> IO (CompileInfo ())
checkShortParseFail s = do
  let parsed = readSingle "(string)" s :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileErrorM $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"
