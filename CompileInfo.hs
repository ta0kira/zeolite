{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module CompileInfo (
  CompileInfo(..),
  CompileMessage(..),
) where

import Data.Either (isLeft,partitionEithers)

import TypesBase (CompileErrorM(..),Mergeable(..))


data CompileMessage =
  CompileMessage {
    cmMessage :: String
  } |
  CompileNested {
    cnNested :: [CompileMessage]
  }

instance Show CompileMessage where
  show (CompileMessage m) = m
  show (CompileNested ms) = show ms

type CompileInfo = Either CompileMessage

instance CompileErrorM CompileInfo where
  compileErrorM = Left . CompileMessage
  isCompileErrorM = isLeft
  collectAllOrErrorM = result . splitErrorsAndData where
    result ([],xs) = return xs
    result (e:_,_) = Left e  -- Take first error.
  collectOneOrErrorM = result . splitErrorsAndData where
    result (_,x:_) = return x
    result ([],_)  = compileErrorM "No values in the empty set"
    result (es,_)  = Left $ joinMessages es  -- Take all errors.

instance Mergeable () where
  mergeAny = const ()
  mergeAll = const ()

instance Mergeable Bool where
  mergeAny = any id
  mergeAll = all id

instance Mergeable a => Mergeable (CompileInfo a) where
  mergeAny = result . splitErrorsAndData where
    result (_,xs@(x:_)) = return $ mergeAny xs
    result ([],_)       = compileErrorM "No values in the empty set"
    result (es,_)       = Left $ joinMessages es  -- Take all errors.
  mergeAll = result . splitErrorsAndData where
    result ([],xs) = return $ mergeAll xs
    result (e:_,_) = Left e  -- Take first error.
  (Right x)  `mergeNested` (Right y)  = return $ x `mergeNested` y
  e@(Left _) `mergeNested` (Right _)  = e
  (Right _)  `mergeNested` e@(Left _) = e
  (Left e1)  `mergeNested` (Left e2)  = Left $ e1 `nestMessages` e2

joinMessages = foldr joinPair (CompileNested []) where
  joinPair m                     (CompileNested [])    = m
  joinPair (CompileNested [])    m                     = m
  joinPair m@(CompileMessage _)  (CompileNested ms)    = CompileNested (m:ms)
  joinPair (CompileNested ms)    m@(CompileMessage _)  = CompileNested (ms ++ [m])
  joinPair m1@(CompileMessage _) m2@(CompileMessage _) = CompileNested [m1,m2]
  joinPair (CompileNested ms1)   (CompileNested ms2)   = CompileNested (ms1 ++ ms2)

nestMessages m@(CompileMessage _) m2 = CompileNested (m:[m2])
nestMessages (CompileNested ms)   m2 = CompileNested (ms ++ [m2])

splitErrorsAndData :: Foldable f => f (Either a b) -> ([a],[b])
splitErrorsAndData = partitionEithers . foldr (:) []
