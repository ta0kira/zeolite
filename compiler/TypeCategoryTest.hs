{-# LANGUAGE Safe #-}

module TypeCategoryTest where

import Control.Arrow
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
    checkShortParseFail "@type interface Type<x> { x allows T }",

    checkShortParseSuccess "@value interface Type<x> {}",
    checkShortParseSuccess "@value interface Type {}",
    checkShortParseSuccess "@value interface Type { refines T }",
    checkShortParseFail "@value interface Type { defines T }",
    checkShortParseFail "@value interface Type<x> { x allows T }",

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
          (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [])
        ]),
    checkOperationFail
      "testfiles/concrete_refines_value.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [])
        ]),

    checkOperationSuccess
      "testfiles/partial.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",ValueInterface [] (TypeName "Parent") [] [])
        ]),
    checkOperationFail
      "testfiles/partial.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [])
        ]),
    checkOperationFail
      "testfiles/partial.txt"
      (checkConnectedTypes $ Map.fromList [
          (TypeName "Parent",ValueConcrete [] (TypeName "Parent") [] [] [] [])
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
          (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [])
        ]),
    checkOperationFail
      "testfiles/flatten.txt"
      (flattenAllConnections $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [])
        ]),

    checkOperationSuccess
      "testfiles/partial.txt"
      (\ts -> do
        ts2 <- (flip flattenAllConnections ts)
                 (Map.fromList [
                   (TypeName "Parent",
                    ValueInterface [] (TypeName "Parent") []
                                   [ValueRefine [] $ TypeInstance (TypeName "Object1") (ParamSet []),
                                    ValueRefine [] $ TypeInstance (TypeName "Object2") (ParamSet [])]),
                   -- NOTE: Object1 deliberately excluded here so that we catch
                   -- unnecessary recursion in existing categories.
                   (TypeName "Object2",
                    ValueInterface [] (TypeName "Object2") [] [])
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
          (TypeName "Parent2",InstanceInterface [] (TypeName "Parent2") [])
        ]),
    checkOperationFail
      "testfiles/concrete_refines_value.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",InstanceInterface [] (TypeName "Parent") [])
        ]),

    checkOperationSuccess
      "testfiles/partial_params.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",
           ValueInterface [] (TypeName "Parent")
                          [ValueParam [] (ParamName "w") Contravariant,
                           ValueParam [] (ParamName "z") Covariant] [])
      ]),
    checkOperationFail
      "testfiles/partial_params.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",
           ValueInterface [] (TypeName "Parent")
                          [ValueParam [] (ParamName "w") Invariant,
                           ValueParam [] (ParamName "z") Covariant] [])
      ]),
    checkOperationFail
      "testfiles/partial_params.txt"
      (checkParamVariances $ Map.fromList [
          (TypeName "Parent",
           ValueInterface [] (TypeName "Parent")
                          [ValueParam [] (ParamName "w") Contravariant,
                           ValueParam [] (ParamName "z") Invariant] [])
      ]),

    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        rs <- getRefines ts "Type<a,b,c,d,e,f>" "Type"
        rs `containsPaired` ["a","b","c","d","e","f"]
        ),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getRefines ts2 "Object1<a,b>" "Object1"
        rs `containsPaired` ["a","b"]
        ),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getRefines ts2 "Object1<a,b>" "Object3"
        rs `containsPaired` ["b"]
        ),
    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        rs <- getRefines ts2 "Parent<t>" "Object1"
        rs `containsPaired` ["t","Object3<Object2>"]
        ),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        getRefines ts2 "Parent<t>" "Child"
        ),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        getRefines ts2 "Child" "Type"
        ),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        ts2 <- flattenAllConnections Map.empty ts
        getRefines ts2 "Child" "Missing"
        ),

    checkOperationSuccess
      "testfiles/flatten.txt"
      (\ts -> do
        rs <- getDefines ts "Child" "Type"
        rs `containsPaired` ["Child"]
        ),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        getDefines ts "Child" "Parent"
        ),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        getDefines ts "Child" "Missing"
        ),

    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        vs <- getVariance ts "Type"
        vs `containsPaired` [Contravariant,Contravariant,
                             Invariant,Invariant,
                             Covariant,Covariant]
        ),
    checkOperationFail
      "testfiles/flatten.txt"
      (\ts -> do
        getVariance ts "Missing"
        ),

    checkOperationSuccess
      "testfiles/concrete.txt"
      (\ts -> do
        rs <- getFilters ts "Type<a,b,c,d,e,f>"
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
        rs <- getFilters ts "Type<Type<t>,b,Type3<x>,d,e,f>"
        checkPaired containsExactly rs [
            ["allows Parent"],
            ["requires Type2<Type<t>>"],
            ["defines Equals<Type3<x>>"],
            [],
            [],
            []
          ])
  ]


getRefines ts s n = do
  ta <- declareAllTypes Map.empty ts
  r <- return $ categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet rs) <- trRefines r t (TypeName n)
  return $ map show rs

getDefines ts s n = do
  ta <- declareAllTypes Map.empty ts
  r <- return $ categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet ds) <- trDefines r t (TypeName n)
  return $ map show ds

getVariance ts n = do
  ta <- declareAllTypes Map.empty ts
  r <- return $ categoriesToTypeResolver ta
  (ParamSet vs) <- trVariance r (TypeName n)
  return vs

getFilters ts s = do
  ta <- declareAllTypes Map.empty ts
  r <- return $ categoriesToTypeResolver ta
  t <- readSingle "(string)" s
  ((),ParamSet vs) <- trFilters r t
  return $ map (map show) vs

scrapeAllRefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueInterface _ n _ rs) = map ((,) n . vrType) rs
  scrapeSingle (ValueConcrete _ n _ rs _ _) = map ((,) n . vrType) rs
  scrapeSingle _ = []

scrapeAllDefines = map (show *** show) . concat . map scrapeSingle where
  scrapeSingle (ValueConcrete _ n _ _ ds _) = map ((,) n . vdType) ds
  scrapeSingle _ = []


containsExactly actual expected = do
  containsNoDuplicates actual
  containsAtLeast actual expected
  containsAtMost actual expected

containsNoDuplicates expected = checked where
  checked = mergeAll $ map checkSingle $ group $ sort expected
  checkSingle xa@(x:_:_) =
    compileError $ "Item " ++ show x ++ " occurs " ++ show (length xa) ++ " times"
  checkSingle _ = return ()

containsAtLeast actual expected = checked where
  checked = mergeAll $ map (checkInActual $ Set.fromList actual) expected
  checkInActual va v =
    if v `Set.member` va
       then return ()
       else compileError $ "Item " ++ show v ++ " was expected but not present "

containsAtMost actual expected = checked where
  checked = mergeAll $ map (checkInExpected $ Set.fromList expected) actual
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

checkOperationSuccess f o = checked where
  checked = do
    contents <- readFile f
    parsed <- return $ readMulti f contents :: IO (CompileInfo [AnyCategory SourcePos])
    return $ check (parsed >>= o)
  check (Left es) = compileError $ "Check " ++ f ++ ": " ++ show es
  check _ = return ()

checkOperationFail f o = checked where
  checked = do
    contents <- readFile f
    parsed <- return $ readMulti f contents :: IO (CompileInfo [AnyCategory SourcePos])
    return $ check (parsed >>= o)
  check (Right x) = compileError $ "Check " ++ f ++ ": Expected failure but got\n" ++ show x
  check _ = return ()

checkSingleParseSuccess f = checked where
  checked = do
    contents <- readFile f
    parsed <- return $ readSingle f contents :: IO (CompileInfo (AnyCategory SourcePos))
    return $ check parsed
  check (Left es) = compileError $ "Parse " ++ f ++ ": " ++ show es
  check _ = return ()

checkSingleParseFail f = checked where
  checked = do
    contents <- readFile f
    parsed <- return $ readSingle f contents :: IO (CompileInfo (AnyCategory SourcePos))
    return $ check parsed
  check (Right t) = compileError $ "Parse " ++ f ++ ": Expected failure but got\n" ++ show t
  check _ = return ()

checkShortParseSuccess s = checked where
  checked = do
    parsed <- return $ readSingle "(string)" s :: IO (CompileInfo (AnyCategory SourcePos))
    return $ check parsed
  check (Left es) = compileError $ "Parse '" ++ s ++ "': " ++ show es
  check _ = return ()

checkShortParseFail s = checked where
  checked = do
    parsed <- return $ readSingle "(string)" s :: IO (CompileInfo (AnyCategory SourcePos))
    return $ check parsed
  check (Right t) = compileError $ "Parse '" ++ s ++ "': Expected failure but got\n" ++ show t
  check _ = return ()

readSingle :: ParseFromSource a => String -> String -> CompileInfo a
readSingle f s = parsed where
  parsed = unwrap $ parse (between optionalSpace endOfDoc sourceParser) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

readMulti :: ParseFromSource a => String -> String -> CompileInfo [a]
readMulti f s = parsed where
  parsed =
    unwrap $ parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t
