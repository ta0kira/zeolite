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

import CompileInfo
import ParseCategory
import ParserBase
import TestBase
import TypeCategory
import TypeInstance
import TypesBase


main = runAllTests [
    checkSingleParseSuccess "testfiles/value_interface.txt",
    checkSingleParseSuccess "testfiles/type_interface.txt",
    checkSingleParseSuccess "testfiles/concrete.txt",

    checkShortParseSuccess "concrete Type<x> {}",
    checkShortParseSuccess "concrete Type {}",
    checkShortParseFail "concrete Type<T> {}",
    checkShortParseFail "concrete Type<optional> {}",
    checkShortParseFail "concrete Type<optional T> {}",
    checkShortParseFail "concrete Type<T<x>> {}",
    checkShortParseSuccess "concrete Type { refines T }",
    checkShortParseFail "concrete Type { refines x }",
    checkShortParseSuccess "concrete Type { defines T }",
    checkShortParseFail "concrete Type { defines x }",
    checkShortParseFail "concrete Type { refines optional }",
    checkShortParseFail "concrete Type { refines optional T }",
    checkShortParseSuccess "concrete Type<x|y> { x requires y }",
    checkShortParseSuccess "concrete Type<x|y> { x allows y }",
    checkShortParseSuccess "concrete Type<x|y> { x defines T }",
    checkShortParseFail "concrete Type<x|y> { x defines y }",

    checkShortParseSuccess "@type interface Type<x> {}",
    checkShortParseSuccess "@type interface Type {}",
    checkShortParseFail "@type interface Type { refines T }",
    checkShortParseFail "@type interface Type { defines T }",
    checkShortParseSuccess "@type interface Type<x> { x allows T }",

    checkShortParseSuccess "@value interface Type<x> {}",
    checkShortParseSuccess "@value interface Type {}",
    checkShortParseSuccess "@value interface Type { refines T }",
    checkShortParseFail "@value interface Type { defines T }",
    checkShortParseSuccess "@value interface Type<x> { x allows T }",

    checkOperationSuccess "testfiles/value_refines_value.txt" (checkConnectedTypes Map.empty),
    checkOperationFail "testfiles/value_refines_instance.txt" (checkConnectedTypes Map.empty),
    checkOperationFail "testfiles/value_refines_concrete.txt" (checkConnectedTypes Map.empty),

    checkOperationSuccess "testfiles/concrete_refines_value.txt" (checkConnectedTypes Map.empty),
    checkOperationFail "testfiles/concrete_refines_instance.txt" (checkConnectedTypes Map.empty),
    checkOperationFail "testfiles/concrete_refines_concrete.txt" (checkConnectedTypes Map.empty),
    checkOperationSuccess "testfiles/concrete_defines_instance.txt" (checkConnectedTypes Map.empty),
    checkOperationFail "testfiles/concrete_defines_value.txt" (checkConnectedTypes Map.empty),
    checkOperationFail "testfiles/concrete_defines_concrete.txt" (checkConnectedTypes Map.empty),

    checkOperationSuccess
      "testfiles/concrete_refines_value.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [] [] [])
        ]),
    checkOperationFail
      "testfiles/concrete_refines_value.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      "testfiles/partial.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",ValueInterface [] (TypeName "Parent") [] [] [] [])
        ]),
    checkOperationFail
      "testfiles/partial.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [] [] [])
        ]),
    checkOperationFail
      "testfiles/partial.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",ValueConcrete [] (TypeName "Parent") [] [] [] [] [])
        ]),

    checkOperationSuccess "testfiles/value_refines_value.txt" checkConnectionCycles,
    checkOperationSuccess "testfiles/concrete_refines_value.txt" checkConnectionCycles,
    checkOperationSuccess "testfiles/concrete_defines_instance.txt" checkConnectionCycles,
    checkOperationFail "testfiles/value_cycle.txt" checkConnectionCycles,

    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        scrapeAllRefines ts2 `containsExactly` [
            ("Object1","Object3<y>"),
            ("Object1","Object2"),
            ("Object3","Object2"),
            ("Parent","Object1<x,Object3<Object2>>"),
            ("Parent","Object3<Object3<Object2>>"),
            ("Parent","Object2"),
            ("Child","Parent<Child>"),
            ("Child","Object1<Child,Object3<Object2>>"),
            ("Child","Object3<Object3<Object2>>"),
            ("Child","Object2")
          ]
        scrapeAllDefines ts2 `containsExactly` [
            ("Child","Type<Child>")
          ]),

    checkOperationSuccess
      "testfiles/flatten.txt"
      (flattenAllConnections $ Map.fromList [
          (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [] [] [])
        ]),
    checkOperationFail
      "testfiles/flatten.txt"
      (flattenAllConnections $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      "testfiles/partial.txt"
      (\ts -> do
        ts2 <- (flip flattenAllConnections ts)
                 (Map.fromList [
                   (TypeName "Parent",
                    ValueInterface [] (TypeName "Parent") []
                                   [ValueRefine [] $ TypeInstance (TypeName "Object1") (ParamSet []),
                                    ValueRefine [] $ TypeInstance (TypeName "Object2") (ParamSet [])] [] []),
                   -- NOTE: Object1 deliberately excluded here so that we catch
                   -- unnecessary recursion in existing categories.
                   (TypeName "Object2",
                    ValueInterface [] (TypeName "Object2") [] [] [] [])
                 ])
        scrapeAllRefines ts2 `containsExactly` [
            ("Child","Parent"),
            ("Child","Object1"),
            ("Child","Object2")
          ]),

    checkOperationSuccess "testfiles/valid_variances.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/contravariant_refines_covariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/contravariant_refines_invariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/covariant_refines_contravariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/covariant_refines_invariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/contravariant_defines_covariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/contravariant_defines_invariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/covariant_defines_contravariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/covariant_defines_invariant.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/concrete_duplicate_param.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/type_duplicate_param.txt" (checkParamVariances Map.empty),
    checkOperationFail "testfiles/value_duplicate_param.txt" (checkParamVariances Map.empty),

    checkOperationSuccess
      "testfiles/concrete_refines_value.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [] [] [])
        ]),
    checkOperationFail
      "testfiles/concrete_refines_value.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [] [] [])
        ]),

    checkOperationSuccess
      "testfiles/partial_params.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",
           ValueInterface [] (TypeName "Parent")
                          [ValueParam [] (ParamName "w") Contravariant,
                           ValueParam [] (ParamName "z") Covariant] [] [] [])
      ]),
    checkOperationFail
      "testfiles/partial_params.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",
           ValueInterface [] (TypeName "Parent")
                          [ValueParam [] (ParamName "w") Invariant,
                           ValueParam [] (ParamName "z") Covariant] [] [] [])
      ]),
    checkOperationFail
      "testfiles/partial_params.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",
           ValueInterface [] (TypeName "Parent")
                          [ValueParam [] (ParamName "w") Contravariant,
                           ValueParam [] (ParamName "z") Invariant] [] [] [])
      ]),

    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        rs <- getTypeRefines ts "Type<a,b,c,d,e,f>" "Type"
        rs `containsPaired` ["a","b","c","d","e","f"]
        ),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getTypeRefines ts2 "Object1<a,b>" "Object1"
        rs `containsPaired` ["a","b"]),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getTypeRefines ts2 "Object1<a,b>" "Object3"
        rs `containsPaired` ["b"]),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getTypeRefines ts2 "Undefined<a,b>" "Undefined"
        rs `containsPaired` ["a","b"]),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getTypeRefines ts2 "Object1<a>" "Object1"
        rs `containsPaired` ["a"]),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getTypeRefines ts2 "Parent<t>" "Object1"
        rs `containsPaired` ["t","Object3<Object2>"]),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        getTypeRefines ts2 "Parent<t>" "Child"),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        getTypeRefines ts2 "Child" "Type"),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        getTypeRefines ts2 "Child" "Missing"),

    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        rs <- getTypeDefines ts "Child" "Type"
        rs `containsPaired` ["Child"]),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        getTypeDefines ts "Child" "Parent"),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        getTypeDefines ts "Child" "Missing"),

    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        vs <- getTypeVariance ts "Type"
        vs `containsPaired` [Contravariant,Contravariant,
                             Invariant,Invariant,
                             Covariant,Covariant]),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        getTypeVariance ts "Missing"),

    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        rs <- getTypeFilters ts "Type<a,b,c,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<a>"],
            ["defines Equals<c>"],
            [],
            [],
            []
          ]),
    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        rs <- getTypeFilters ts "Type<Type<t>,b,Type3<x>,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<t>>"],
            ["defines Equals<Type3<x>>"],
            [],
            [],
            []
          ]),

    checkOperationSuccess
      "testfiles/value_interface.txt"
      (\ts -> do
        rs <- getTypeFilters ts "Type<a,b,c,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<a>"],
            ["defines Equals<c>"],
            [],
            [],
            []
          ]),
    checkOperationSuccess
      "testfiles/value_interface.txt"
      (\ts -> do
        rs <- getTypeFilters ts "Type<Type<t>,b,Type3<x>,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<t>>"],
            ["defines Equals<Type3<x>>"],
            [],
            [],
            []
          ]),

    checkOperationSuccess
      "testfiles/type_interface.txt"
      (\ts -> do
        rs <- getTypeDefinesFilters ts "Type<a,b,c,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<a>"],
            ["defines Equals<c>"],
            [],
            [],
            []
          ]),
    checkOperationSuccess
      "testfiles/type_interface.txt"
      (\ts -> do
        rs <- getTypeDefinesFilters ts "Type<Type<t>,b,Type3<x>,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<t>>"],
            ["defines Equals<Type3<x>>"],
            [],
            [],
            []
          ]),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Child"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "(Child|Child)"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "(Child&Child)"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Object2"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "(Object2|Object2)"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "(Object2&Object2)"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "Type<Child>"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "(Type<Child>|Type<Child>)"),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeFail r [] "(Type<Child>&Type<Child>)"),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Value0<Value1,Value2>"),
    checkOperationFail
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Value0<Value1,Value1>"),
    checkOperationSuccess
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r [] "Value0<Value3,Value2>"),
    checkOperationFail
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("x",[]),("y",[])]
          "Value0<x,y>"),
    checkOperationSuccess
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("x",["allows y","requires Function<x,y>"]),
           ("y",["requires x","defines Equals<y>"])]
          "Value0<x,y>"),
    checkOperationSuccess
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("x",["allows Value2","requires Function<x,Value2>"])]
          "Value0<x,Value2>"),
    checkOperationFail
      "testfiles/filters.txt"
      (\ts -> do
        ta <- flattenAllConnections Map.empty ts >>= declareAllTypes Map.empty
        let r = categoriesToTypeResolver ta
        checkTypeSuccess r
          [("x",["allows Value2","requires Function<x,Value2>"]),
           ("y",["requires x","defines Equals<y>"])]
          "Value0<x,y>"),

    checkOperationSuccess
      "testfiles/concrete_instances.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/concrete_missing_define.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/concrete_missing_refine.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationSuccess
      "testfiles/value_instances.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/value_missing_define.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/value_missing_refine.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationSuccess
      "testfiles/type_instances.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/type_missing_define.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/type_missing_refine.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),
    checkOperationFail
      "testfiles/requires_concrete.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        checkCategoryInstances Map.empty ts2),

    -- TODO: Clean these tests up.
    checkOperationSuccess
      "testfiles/merged.txt"
      (\ts -> do
        ts2 <- mergeCategoryInstances Map.empty ts
        tm <- declareAllTypes Map.empty ts2
        rs <- getRefines tm "Test"
        rs `containsExactly` ["Value0","Value1","Value2","Value3",
                              "Value4<Value1,Value1>","Value4<Value3,Value1>"]),
    checkOperationSuccess
      "testfiles/merged.txt"
      (\ts -> do
        ts2 <- mergeCategoryInstances Map.empty ts
        tm <- declareAllTypes Map.empty ts2
        ds <- getDefines tm "Test"
        ds `containsExactly` ["Type0<Value1,Value1>","Type0<Value3,Value1>"]),

    checkOperationFail
      "testfiles/merged.txt"
      (\ts -> do
        ts2 <- mergeCategoryInstances Map.empty ts
        checkInstanceDuplicates ts2),
    checkOperationSuccess
      "testfiles/no_duplicates.txt"
      (\ts -> do
        ts2 <- mergeCategoryInstances Map.empty ts
        checkInstanceDuplicates ts2),
    checkOperationFail
      "testfiles/duplicate_refine.txt"
      (\ts -> do
        ts2 <- mergeCategoryInstances Map.empty ts
        checkInstanceDuplicates ts2),
    checkOperationFail
      "testfiles/duplicate_define.txt"
      (\ts -> do
        ts2 <- mergeCategoryInstances Map.empty ts
        checkInstanceDuplicates ts2),

    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        let tm0 = Map.fromList [
                    (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [] [] [])
                  ]
        tm <- includeNewTypes tm0 ts
        rs <- getRefines tm "Child"
        rs `containsExactly` ["Parent<Child>","Object2",
                              "Object1<Child,Object3<Object2>>",
                              "Object3<Object3<Object2>>"]),
    checkOperationSuccess
      "testfiles/no_duplicates.txt"
      (\ts -> do
        tm <- includeNewTypes Map.empty ts
        rs <- getRefines tm "Object2"
        rs `containsExactly` ["Value0","Value1","Value2<Value1>"]),
    checkOperationSuccess
      "testfiles/no_duplicates.txt"
      (\ts -> do
        tm <- includeNewTypes Map.empty ts
        rs <- getDefines tm "Object"
        rs `containsExactly` ["Type0<Value1>"]),

    checkOperationFail
      "testfiles/function_param_clash.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_duplicate_param.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_bad_filter.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_bad_allows.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_bad_requires.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_bad_defines.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_bad_arg.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_bad_return.txt"
      (\ts -> checkCategoryInstances Map.empty ts),

    checkOperationSuccess
      "testfiles/function_filters_satisfied.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_requires_missed.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_allows_missed.txt"
      (\ts -> checkCategoryInstances Map.empty ts),
    checkOperationFail
      "testfiles/function_defines_missed.txt"
      (\ts -> checkCategoryInstances Map.empty ts)
  ]


getRefines tm n =
  case (TypeName n) `Map.lookup` tm of
       (Just t) -> return $ map (show . vrType) (getCategoryRefines t)
       _ -> compileError $ "Type " ++ n ++ " not found"

getDefines tm n =
  case (TypeName n) `Map.lookup` tm of
       (Just t) -> return $ map (show . vdType) (getCategoryDefines t)
       _ -> compileError $ "Type " ++ n ++ " not found"

getTypeRefines ts s n = do
  ta <- declareAllTypes Map.empty ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet rs) <- trRefines r t (TypeName n)
  return $ map show rs

getTypeDefines ts s n = do
  ta <- declareAllTypes Map.empty ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet ds) <- trDefines r t (TypeName n)
  return $ map show ds

getTypeVariance ts n = do
  ta <- declareAllTypes Map.empty ts
  let r = categoriesToTypeResolver ta
  (ParamSet vs) <- trVariance r (TypeName n)
  return vs

getTypeFilters ts s = do
  ta <- declareAllTypes Map.empty ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet vs) <- trTypeFilters r t
  return $ map (map show) vs

getTypeDefinesFilters ts s = do
  ta <- declareAllTypes Map.empty ts
  let r = categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet vs) <- trDefinesFilters r t
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
  (mergeAll $ map checkSingle $ group $ sort expected) `reviseError` (show expected)
  where
    checkSingle xa@(x:_:_) =
      compileError $ "Item " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
    checkSingle _ = return ()

containsAtLeast actual expected =
  (mergeAll $ map (checkInActual $ Set.fromList actual) expected) `reviseError`
        (show actual ++ " (actual) vs. " ++ show expected ++ " (expected)")
  where
    checkInActual va v =
      if v `Set.member` va
         then return ()
         else compileError $ "Item " ++ show v ++ " was expected but not present"

containsAtMost actual expected =
  (mergeAll $ map (checkInExpected $ Set.fromList expected) actual) `reviseError`
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
  | otherwise = mergeAll $ map check (zip3 actual expected [1..]) where
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
    check (Right x) =
      compileError $ "Check " ++ f ++ ": Expected failure but got\n" ++ show x ++ "\n"
    check _ = return ()

checkSingleParseSuccess f = do
  contents <- readFile f
  let parsed = readSingle f contents :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check (Left es) = compileError $ "Parse " ++ f ++ ":\n" ++ show es
    check _ = return ()

checkSingleParseFail f = do
  contents <- readFile f
  let parsed = readSingle f contents :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check (Right t) =
      compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t ++ "\n"
    check _ = return ()

checkShortParseSuccess s = do
  let parsed = readSingle "(string)" s :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check (Left es) = compileError $ "Parse '" ++ s ++ "':\n" ++ show es
    check _ = return ()

checkShortParseFail s = do
  let parsed = readSingle "(string)" s :: CompileInfo (AnyCategory SourcePos)
  return $ check parsed
  where
    check (Right t) =
      compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++ show t ++ "\n"
    check _ = return ()
