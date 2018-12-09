{-# LANGUAGE Safe #-}

module TypeInstanceTest where

import Control.Monad
import Data.Either
import System.IO
import Text.Parsec
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Map as Map

import CompileInfo
import ParserBase
import TypeInstance
import TypesBase


testCases :: [IO (CompileInfo ())]
testCases = [
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
      "(Type0&Type3)"
      "(Type0|Type3)",
    checkSimpleConvertFail
      "(Type0|Type3)"
      "Type3",
    checkSimpleConvertFail
      "Type0"
      "(Type0&Type3)",
    checkSimpleConvertFail
      "(Type0|Type3)"
      "(Type0&Type3)"
  ]

main = do
  results <- sequence testCases
  (es,ps) <- return $ partitionEithers $ zipWith numberError [1..] results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e ++ "\n")) es
  hPutStr stderr $ show (length ps) ++ " tests passed\n"


type0 = TypeName "Type0"
type1 = TypeName "Type1"
type2 = TypeName "Type2"
type3 = TypeName "Type3"

variances :: Map.Map TypeName InstanceVariances
variances = Map.fromList $ [
    (type0,ParamSet []), -- Type0<>
    (type1,ParamSet [Invariant]), -- Type1<x>
    (type2,ParamSet [Contravariant,Invariant,Covariant]), -- Type2<x|y|z>
    (type3,ParamSet []) -- Type3<>
  ]

refines :: Map.Map TypeName (Map.Map TypeName (InstanceParams -> InstanceParams))
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
      ])
  ]


checkSimpleConvertSuccess = checkConvertSuccess Map.empty

checkSimpleConvertFail = checkConvertFail Map.empty

checkConvertSuccess ps x y = return $ checked $ tryConvert ps x y where
  checked (Left es) = compileError $ x ++ " -> " ++ y ++ ": " ++ show es
  checked _ = return ()

checkConvertFail ps x y = return $ checked $ tryConvert ps x y where
  checked (Right _) = compileError $ x ++ " /> " ++ y ++ ": Expected failure"
  checked _ = return ()

tryConvert ps x y = do
    t1 <- getInstance x
    t2 <- getInstance y
    checkGeneralMatch resolver Map.empty Covariant t1 t2

resolver :: TypeResolver CompileInfo ()
resolver = TypeResolver {
    trFind = getParams refines,
    trVariance = mapLookup variances,
    tfValidate = undefined, -- TODO: Define this with an error.
    trParams = undefined -- TODO: Define this with an error.
  }

getParams ma t2 t1 ps = do
  ra <- mapLookup ma t1
  f <- mapLookup ra t2
  return ((),f ps)


numberError :: a -> Either b c -> Either (a,b) c
numberError n (Left e)  = Left (n,e)
numberError _ (Right x) = Right x -- Not the same Either!

mapLookup :: (Ord n, Show n, CompileErrorM m, Monad m) => Map.Map n a -> n -> m a
mapLookup ma n = resolve $ n `Map.lookup` ma where
  resolve (Just x) = return x
  resolve _        = compileError $ "Map key " ++ show n ++ " not found"

getInstance :: (CompileErrorM m, Monad m) => String -> m GeneralInstance
getInstance s = checked parsed where
  checked (Right t) = return t
  checked (Left e)  = compileError (show e)
  parsed = parse sourceParser "(string)" s
