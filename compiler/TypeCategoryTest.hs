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

module TypeCategoryTest where

import Control.Arrow
import Control.Monad
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import qualified Data.Set as Set

import Builtin
import CompileInfo
import ParseCategory
import ParserBase
import TestBase
import TypeCategory
import TypeInstance
import TypesBase


tests :: [IO (CompileInfo ())]
tests = [
    checkSingleParseSuccess "testfiles/value_interface.0rx",
    checkSingleParseSuccess "testfiles/type_interface.0rx",
    checkSingleParseSuccess "testfiles/concrete.0rx",

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

    checkOperationSuccess "testfiles/value_refines_value.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/value_refines_instance.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/value_refines_concrete.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/builtin_clash.0rx" (checkConnectedTypes builtinCategories),

    checkOperationSuccess "testfiles/concrete_refines_value.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/concrete_refines_instance.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/concrete_refines_concrete.0rx" (checkConnectedTypes builtinCategories),
    checkOperationSuccess "testfiles/concrete_defines_instance.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/concrete_defines_value.0rx" (checkConnectedTypes builtinCategories),
    checkOperationFail "testfiles/concrete_defines_concrete.0rx" (checkConnectedTypes builtinCategories),

    checkOperationSuccess
      "testfiles/concrete_refines_value.0rx"
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent2",InstanceInterface [] (CategoryName "Parent2") [] [] [])
        ]),
    checkOperationFail
      "testfiles/concrete_refines_value.0rx"
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",InstanceInterface [] (CategoryName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      "testfiles/partial.0rx"
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",ValueInterface [] (CategoryName "Parent") [] [] [] [])
        ]),
    checkOperationFail
      "testfiles/partial.0rx"
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",InstanceInterface [] (CategoryName "Parent") [] [] [])
        ]),
    checkOperationFail
      "testfiles/partial.0rx"
      (checkConnectedTypes $ Map.fromList [
          (CategoryName "Parent",ValueConcrete [] (CategoryName "Parent") [] [] [] [] [])
        ]),

    checkOperationSuccess "testfiles/value_refines_value.0rx" (checkConnectionCycles Map.empty),
    checkOperationSuccess "testfiles/concrete_refines_value.0rx" (checkConnectionCycles Map.empty),
    checkOperationSuccess "testfiles/concrete_defines_instance.0rx" (checkConnectionCycles Map.empty),
    checkOperationFail "testfiles/value_cycle.0rx" (checkConnectionCycles Map.empty),

    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        map (show . getCategoryName) ts `containsPaired` [
            "Type","Object2","Object3","Object1","Parent","Child"
          ]),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts),

    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        scrapeAllRefines ts `containsExactly` [
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
        scrapeAllDefines ts `containsExactly` [
            ("Child","Type<Child>")
          ]),

    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        existing  <- return $ Map.fromList [
            (CategoryName "Parent2",InstanceInterface [] (CategoryName "Parent2") [] [] [])
          ]
        ts <- topoSortCategories existing ts
        flattenAllConnections existing ts),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        existing  <- return $ Map.fromList [
            (CategoryName "Parent",InstanceInterface [] (CategoryName "Parent") [] [] [])
          ]
        topoSortCategories existing ts),

    checkOperationSuccess
      "testfiles/partial.0rx"
      (\ts -> do
        existing  <- return $ Map.fromList [
            (CategoryName "Parent",
            ValueInterface [] (CategoryName "Parent") []
                           [ValueRefine [] $ TypeInstance (CategoryName "Object1") (ParamSet []),
                           ValueRefine [] $ TypeInstance (CategoryName "Object2") (ParamSet [])] [] []),
            -- NOTE: Object1 deliberately excluded here so that we catch
            -- unnecessary recursion in existing categories.
            (CategoryName "Object2",
            ValueInterface [] (CategoryName "Object2") [] [] [] [])
          ]
        ts <- topoSortCategories existing ts
        ts <- flattenAllConnections existing ts
        scrapeAllRefines ts `containsExactly` [
            ("Child","Parent"),
            ("Child","Object1"),
            ("Child","Object2")
          ]),

    checkOperationSuccess "testfiles/valid_variances.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/contravariant_refines_covariant.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/contravariant_refines_invariant.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/covariant_refines_contravariant.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/covariant_refines_invariant.0rx" (checkParamVariances builtinCategories),
    checkOperationSuccess "testfiles/contravariant_defines_covariant.0rx" (checkParamVariances builtinCategories),
    checkOperationSuccess "testfiles/contravariant_defines_invariant.0rx" (checkParamVariances builtinCategories),
    checkOperationSuccess "testfiles/covariant_defines_contravariant.0rx" (checkParamVariances builtinCategories),
    checkOperationSuccess "testfiles/covariant_defines_invariant.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/concrete_duplicate_param.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/type_duplicate_param.0rx" (checkParamVariances builtinCategories),
    checkOperationFail "testfiles/value_duplicate_param.0rx" (checkParamVariances builtinCategories),

    checkOperationSuccess
      "testfiles/concrete_refines_value.0rx"
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent2",InstanceInterface [] (CategoryName "Parent2") [] [] [])
        ]),
    checkOperationFail
      "testfiles/concrete_refines_value.0rx"
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",InstanceInterface [] (CategoryName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      "testfiles/partial_params.0rx"
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",
           ValueInterface [] (CategoryName "Parent")
                          [ValueParam [] (ParamName "#w") Contravariant,
                           ValueParam [] (ParamName "#z") Covariant] [] [] [])
      ]),
    checkOperationFail
      "testfiles/partial_params.0rx"
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",
           ValueInterface [] (CategoryName "Parent")
                          [ValueParam [] (ParamName "#w") Invariant,
                           ValueParam [] (ParamName "#z") Covariant] [] [] [])
      ]),
    checkOperationFail
      "testfiles/partial_params.0rx"
      (checkParamVariances $ Map.fromList [
          (CategoryName "Parent",
           ValueInterface [] (CategoryName "Parent")
                          [ValueParam [] (ParamName "#w") Contravariant,
                           ValueParam [] (ParamName "#z") Invariant] [] [] [])
      ]),

    checkOperationSuccess
      "testfiles/concrete.0rx"
      (\ts -> do
        rs <- getTypeRefines ts "Type<#a,#b,#c,#d,#e,#f>" "Type"
        rs `containsPaired` ["#a","#b","#c","#d","#e","#f"]
        ),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        rs <- getTypeRefines ts "Object1<#a,#b>" "Object1"
        rs `containsPaired` ["#a","#b"]),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        rs <- getTypeRefines ts "Object1<#a,#b>" "Object3"
        rs `containsPaired` ["#b"]),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        rs <- getTypeRefines ts "Undefined<#a,#b>" "Undefined"
        rs `containsPaired` ["#a","#b"]),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        rs <- getTypeRefines ts "Object1<#a>" "Object1"
        rs `containsPaired` ["#a"]),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        rs <- getTypeRefines ts "Parent<#t>" "Object1"
        rs `containsPaired` ["#t","Object3<Object2>"]),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        getTypeRefines ts "Parent<#t>" "Child"),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        getTypeRefines ts "Child" "Type"),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        getTypeRefines ts "Child" "Missing"),

    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        rs <- getTypeDefines ts "Child" "Type"
        rs `containsPaired` ["Child"]),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        getTypeDefines ts "Child" "Parent"),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        getTypeDefines ts "Child" "Missing"),

    checkOperationSuccess
      "testfiles/concrete.0rx"
      (\ts -> do
        vs <- getTypeVariance ts "Type"
        vs `containsPaired` [Contravariant,Contravariant,
                             Invariant,Invariant,
                             Covariant,Covariant]),
    checkOperationFail
      "testfiles/flatten.0rx"
      (\ts -> do
        getTypeVariance ts "Missing"),

    checkOperationSuccess
      "testfiles/concrete.0rx"
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
      "testfiles/concrete.0rx"
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
      "testfiles/value_interface.0rx"
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
      "testfiles/value_interface.0rx"
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
      "testfiles/type_interface.0rx"
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
      "testfiles/type_interface.0rx"
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
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Child"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "[Child|Child]"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "[Child&Child]"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Object2"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "[Object2|Object2]"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "[Object2&Object2]"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "Type<Child>"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "[Type<Child>|Type<Child>]"),
    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ta <- flattenAllConnections builtinCategories ts >>= declareAllTypes builtinCategories
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "[Type<Child>&Type<Child>]"),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Value0<Value1,Value2>"),
    checkOperationFail
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Value0<Value1,Value1>"),
    checkOperationSuccess
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Value0<Value3,Value2>"),
    checkOperationFail
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("#x",[]),("#y",[])]
          "Value0<#x,#y>"),
    checkOperationSuccess
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("#x",["allows #y","requires Function<#x,#y>"]),
           ("#y",["requires #x","defines Equals<#y>"])]
          "Value0<#x,#y>"),
    checkOperationSuccess
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("#x",["allows Value2","requires Function<#x,Value2>"])]
          "Value0<#x,Value2>"),
    checkOperationFail
      "testfiles/filters.0rx"
      (\ts -> do
        ts <- topoSortCategories Map.empty ts
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("#x",["allows Value2","requires Function<#x,Value2>"]),
           ("#y",["requires #x","defines Equals<#y>"])]
          "Value0<#x,#y>"),

    checkOperationSuccess
      "testfiles/concrete_instances.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/concrete_missing_define.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/concrete_missing_refine.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationSuccess
      "testfiles/value_instances.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/value_missing_define.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/value_missing_refine.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationSuccess
      "testfiles/type_instances.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/type_missing_define.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/type_missing_refine.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),
    checkOperationSuccess
      "testfiles/requires_concrete.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        checkCategoryInstances builtinCategories ts),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      "testfiles/merged.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        ts <- flattenAllConnections builtinCategories ts
        tm <- declareAllTypes builtinCategories ts
        rs <- getRefines tm "Test"
        rs `containsExactly` ["Value0","Value1","Value2","Value3",
                              "Value4<Value1,Value1>","Inherit1","Inherit2"]),

    checkOperationSuccess
      "testfiles/merged.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/duplicate_refine.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/duplicate_define.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/refine_wrong_direction.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/inherit_incompatible.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationSuccess
      "testfiles/merge_incompatible.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),

    checkOperationSuccess
      "testfiles/flatten.0rx"
      (\ts -> do
        let tm0 = Map.fromList [
                    (CategoryName "Parent2",InstanceInterface [] (CategoryName "Parent2") [] [] [])
                  ]
        tm <- includeNewTypes tm0 ts
        rs <- getRefines tm "Child"
        rs `containsExactly` ["Parent<Child>","Object2",
                              "Object1<Child,Object3<Object2>>",
                              "Object3<Object3<Object2>>"]),

    checkOperationSuccess
      "testfiles/category_function_param_match.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_param_clash.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_duplicate_param.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_filter_param.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_allows_type.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_allows_variance.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_requires_type.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_requires_variance.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_defines_type.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_defines_variance.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_arg.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_bad_return.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/weak_arg.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/weak_return.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),

    checkOperationSuccess
      "testfiles/function_filters_satisfied.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_requires_missed.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_allows_missed.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/function_defines_missed.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),

    checkOperationSuccess
      "testfiles/valid_function_variance.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/bad_arg_variance.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),
    checkOperationFail
      "testfiles/bad_return_variance.0rx"
      (\ts -> checkCategoryInstances builtinCategories ts),

    checkOperationFail
      "testfiles/conflicting_declaration.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/conflicting_inherited.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationSuccess
      "testfiles/successful_merge.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/failed_merge.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/merge_different_scopes.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),

    checkOperationSuccess
      "testfiles/successful_merge_params.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/failed_merge_params.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationSuccess
      "testfiles/preserve_merged.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationFail
      "testfiles/conflict_in_preserved.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ()),
    checkOperationSuccess
      "testfiles/resolved_in_preserved.0rx"
      (\ts -> do
        ts <- topoSortCategories builtinCategories ts
        flattenAllConnections builtinCategories ts
        return ())
  ]


getRefines tm n =
  case (CategoryName n) `Map.lookup` tm of
       (Just t) -> return $ map (show . vrType) (getCategoryRefines t)
       _ -> compileError $ "Type " ++ n ++ " not found"

getDefines tm n =
  case (CategoryName n) `Map.lookup` tm of
       (Just t) -> return $ map (show . vdType) (getCategoryDefines t)
       _ -> compileError $ "Type " ++ n ++ " not found"

getTypeRefines ts s n = do
  ta <- declareAllTypes builtinCategories ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ParamSet rs <- trRefines r t (CategoryName n)
  return $ map show rs

getTypeDefines ts s n = do
  ta <- declareAllTypes builtinCategories ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ParamSet ds <- trDefines r t (CategoryName n)
  return $ map show ds

getTypeVariance ts n = do
  ta <- declareAllTypes builtinCategories ts
  let r = categoriesToTypeResolver ta
  (ParamSet vs) <- trVariance r (CategoryName n)
  return vs

getTypeFilters ts s = do
  ta <- declareAllTypes builtinCategories ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ParamSet vs <- trTypeFilters r t
  return $ map (map show) vs

getTypeDefinesFilters ts s = do
  ta <- declareAllTypes builtinCategories ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ParamSet vs <- trDefinesFilters r t
  return $ map (map show) vs

scrapeAllRefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueInterface _ n _ rs _ _) = map ((,) n . vrType) rs
  scrapeSingle (ValueConcrete _ n _ rs _ _ _) = map ((,) n . vrType) rs
  scrapeSingle _ = []

scrapeAllDefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueConcrete _ n _ _ ds _ _) = map ((,) n . vdType) ds
  scrapeSingle _ = []


containsExactly actual expected = do
  containsNoDuplicates actual
  containsAtLeast actual expected
  containsAtMost actual expected

containsNoDuplicates expected =
  (mergeAllM $ map checkSingle $ group $ sort expected) `reviseError` (show expected)
  where
    checkSingle xa@(x:_:_) =
      compileError $ "Item " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkSingle _ = return ()

containsAtLeast actual expected =
  (mergeAllM $ map (checkInActual $ Set.fromList actual) expected) `reviseError`
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInActual va v =
      if v `Set.member` va
         then return ()
         else compileError $ "Item " ++ show v ++ " was expected but not present"

containsAtMost actual expected =
  (mergeAllM $ map (checkInExpected $ Set.fromList expected) actual) `reviseError`
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInExpected va v =
      if v `Set.member` va
         then return ()
         else compileError $ "Item " ++ show v ++ " is unexpected"

checkPaired :: (Show a, CompileErrorM m, MergeableM m, Monad m) =>
  (a -> a -> m ()) -> [a] -> [a] -> m ()
checkPaired f actual expected
  | length actual /= length expected =
    compileError $ "Different item counts: " ++ show actual ++ " (actual) vs. " ++
                   show expected ++ " (expected)"
  | otherwise = mergeAllM $ map check (zip3 actual expected [1..]) where
    check (a,e,n) = f a e `reviseError` ("Item " ++ show n ++ " mismatch")

containsPaired :: (Eq a, Show a, CompileErrorM m, MergeableM m, Monad m) =>
  [a] -> [a] -> m ()
containsPaired = checkPaired checkSingle where
  checkSingle a e
    | a == e = return ()
    | otherwise = compileError $ show a ++ " (actual) vs. " ++ show e ++ " (expected)"

checkOperationSuccess f o = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [AnyCategory SourcePos]
  return $ check (parsed >>= o >> return ())
  where
    check = flip reviseError ("Check " ++ f ++ ":")

checkOperationFail f o = do
  contents <- readFile f
  let parsed = readMulti f contents :: CompileInfo [AnyCategory SourcePos]
  return $ check (parsed >>= o >> return ())
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Check " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkSingleParseSuccess f = do
  contents <- readFile f
  let parsed = readSingle f contents :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = compileError $ "Parse " ++ f ++ ":\n" ++ show (getCompileError c)
      | otherwise = return ()

checkSingleParseFail f = do
  contents <- readFile f
  let parsed = readSingle f contents :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"

checkShortParseSuccess s = do
  let parsed = readSingle "(string)" s :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = compileError $ "Parse '" ++ s ++ "':\n" ++ show (getCompileError c)
      | otherwise = return ()

checkShortParseFail s = do
  let parsed = readSingle "(string)" s :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check c
      | isCompileError c = return ()
      | otherwise = compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++
                                   show (getCompileSuccess c) ++ "\n"
