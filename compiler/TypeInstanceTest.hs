{-# LANGUAGE Safe #-}

module TypeInstanceTest where

import Control.Monad
import Data.Either
import System.IO
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Map as Map

import CompileInfo
import TypeInstance
import TypesBase


testCases :: [IO (CompileInfo ())]
testCases = [
    checkSimpleConvertSuccess "Type0 -> Type0"
      (SingleType $ getType0)
      (SingleType $ getType0),
    checkSimpleConvertSuccess "Type3 -> Type0"
      (SingleType $ getType3)
      (SingleType $ getType0),
    checkSimpleConvertFail "Type0 -> Type3"
      (SingleType $ getType0)
      (SingleType $ getType3),
    checkSimpleConvertSuccess "Type1<Type0> -> Type0"
      (SingleType $ getType1 (SingleType getType0))
      (SingleType $ getType0),
    checkSimpleConvertFail "Type0 -> Type1<Type0>"
      (SingleType $ getType0)
      (SingleType $ getType1 (SingleType getType0)),
    checkSimpleConvertSuccess "Type2<Type0,Type0,Type0> -> Type2<Type0,Type0,Type0>"
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType0))
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType0)),
    checkSimpleConvertSuccess "Type2<Type0,Type0,Type3> -> Type2<Type3,Type0,Type0>"
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType3))
      (SingleType $ getType2 (SingleType getType3) (SingleType getType0) (SingleType getType0)),
    checkSimpleConvertFail "Type2<Type3,Type0,Type3> -> Type2<Type0,Type0,Type0>"
      (SingleType $ getType2 (SingleType getType3) (SingleType getType0) (SingleType getType3))
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType0)),
    checkSimpleConvertFail "Type2<Type0,Type0,Type0> -> Type2<Type3,Type0,Type3>"
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType0))
      (SingleType $ getType2 (SingleType getType3) (SingleType getType0) (SingleType getType3)),
    checkSimpleConvertFail "Type2<Type0,Type3,Type0> -> Type2<Type0,Type0,Type0>"
      (SingleType $ getType2 (SingleType getType0) (SingleType getType3) (SingleType getType0))
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType0)),
    checkSimpleConvertFail "Type2<Type0,Type0,Type0> -> Type2<Type0,Type3,Type0>"
      (SingleType $ getType2 (SingleType getType0) (SingleType getType0) (SingleType getType0))
      (SingleType $ getType2 (SingleType getType0) (SingleType getType3) (SingleType getType0)),
    checkSimpleConvertSuccess "Type3 -> (Type0|Type3)"
      (SingleType $ getType3)
      (TypeMerge MergeUnion [SingleType getType0,SingleType getType3]),
    checkSimpleConvertSuccess "Type3 -> (Type0|Type1<Type0>)"
      (SingleType $ getType3)
      (TypeMerge MergeUnion [SingleType getType0,SingleType (getType1 (SingleType getType0))]),
    checkSimpleConvertSuccess "(Type3|Type1<Type0>) -> Type0"
      (TypeMerge MergeUnion [SingleType getType3,SingleType (getType1 (SingleType getType0))])
      (SingleType $ getType0),
    checkSimpleConvertSuccess "(Type0&Type3) -> Type3"
      (TypeMerge MergeIntersect [SingleType getType0,SingleType getType3])
      (SingleType $ getType3),
    checkSimpleConvertSuccess "(Type0&Type3) -> (Type0|Type3)"
      (TypeMerge MergeIntersect [SingleType getType0,SingleType getType3])
      (TypeMerge MergeUnion [SingleType getType0,SingleType getType3]),
    checkSimpleConvertFail "(Type0|Type3) -> Type3"
      (TypeMerge MergeUnion [SingleType getType0,SingleType getType3])
      (SingleType $ getType3),
    checkSimpleConvertFail "Type0 -> (Type0&Type3)"
      (SingleType $ getType0)
      (TypeMerge MergeIntersect [SingleType getType0,SingleType getType3]),
    checkSimpleConvertFail "(Type0|Type3) -> (Type0&Type3)"
      (TypeMerge MergeUnion [SingleType getType0,SingleType getType3])
      (TypeMerge MergeIntersect [SingleType getType0,SingleType getType3])
  ]

main = do
  results <- sequence testCases
  (es,ps) <- return $ partitionEithers $ zipWith numberError [1..] results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e ++ "\n")) es
  hPutStr stderr $ show (length ps) ++ " tests passed\n"


type0 = TypeName "Type0" -- <>
type1 = TypeName "Type1" -- <x>
type2 = TypeName "Type2" -- <x|y|z>
type3 = TypeName "Type3" -- <>

getType0 = JustTypeInstance (TypeInstance type0 (ParamSet []))
getType1 x = JustTypeInstance (TypeInstance type1 (ParamSet [x]))
getType2 x y z = JustTypeInstance (TypeInstance type2 (ParamSet [x,y,z]))
getType3 = JustTypeInstance (TypeInstance type3 (ParamSet []))

variances :: Map.Map TypeName InstanceVariances
variances = Map.fromList $ [
    (type0,ParamSet []),
    (type1,ParamSet [Invariant]),
    (type2,ParamSet [Contravariant,Invariant,Covariant]),
    (type3,ParamSet [])
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


checkSimpleConvertSuccess m x y = return $ checked result where
  result = checkGeneralMatch resolver Map.empty Covariant x y
  checked (Left es) = compileError $ m ++ ": " ++ show es
  checked _ = return ()

checkSimpleConvertFail m x y = return $ checked result where
  result = checkGeneralMatch resolver Map.empty Covariant x y
  checked (Right _) = compileError $ m ++ ": Expected failure"
  checked _ = return ()

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
