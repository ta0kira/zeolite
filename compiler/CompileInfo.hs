{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module CompileInfo (
  CompileInfo(..),
  CompileMessage(..),
) where

import Data.Either (isLeft,partitionEithers)
import Data.List (intercalate)

import TypesBase (CompileErrorM(..),Mergeable(..),MergeableM(..))


data CompileMessage =
  CompileMessage {
    cmMessage :: String,
    ccNested :: [CompileMessage]
  }

instance Show CompileMessage where
  show = format "" where
    format indent (CompileMessage [] ms) =
      concat (map (format indent) ms)
    format indent (CompileMessage m ms) =
      (doIndent indent m) ++ "\n" ++ concat (map (format $ indent ++ "  ") ms)
    doIndent indent s = intercalate "\n" $ map (indent ++) $ lines s

type CompileInfo = Either CompileMessage

instance CompileErrorM CompileInfo where
  compileErrorM = Left . flip CompileMessage []
  isCompileErrorM = isLeft
  collectAllOrErrorM = result . splitErrorsAndData where
    result ([],xs) = return xs
    result (es,_) = Left $ CompileMessage "" es
  collectOneOrErrorM = result . splitErrorsAndData where
    result (_,x:_) = return x
    result ([],_)  = compileErrorM "No choices found"
    result (es,_)  = Left $ CompileMessage "" es
  reviseErrorM x@(Right _) _ = x
  reviseErrorM x@(Left (CompileMessage [] ms)) s = Left $ CompileMessage s ms
  reviseErrorM x@(Left e) s = Left $ CompileMessage s [e]

instance MergeableM CompileInfo where
  mergeAnyM = result . splitErrorsAndData where
    result (_,xs@(x:_)) = return $ mergeAny xs
    result ([],_)       = compileErrorM "No choices found"
    result (es,_)       = Left $ CompileMessage "" es
  mergeAllM = result . splitErrorsAndData where
    result ([],xs) = return $ mergeAll xs
    result (es,_)  = Left $ CompileMessage "" es
  (Right x)  `mergeNestedM` (Right y)  = return $ x `mergeNested` y
  e@(Left _) `mergeNestedM` (Right _)  = e
  (Right _)  `mergeNestedM` e@(Left _) = e
  (Left e1)  `mergeNestedM` (Left e2)  = Left $ e1 `nestMessages` e2

nestMessages (CompileMessage m1 ms1) (CompileMessage [] ms2) =
  CompileMessage m1 (ms1 ++ ms2)
nestMessages (CompileMessage [] ms1) (CompileMessage m2 ms2) =
  CompileMessage m2 (ms1 ++ ms2)
nestMessages (CompileMessage m1 ms1) ma@(CompileMessage _ _) =
  CompileMessage m1 (ms1 ++ [ma])

splitErrorsAndData :: Foldable f => f (Either a b) -> ([a],[b])
splitErrorsAndData = partitionEithers . foldr (:) []
