{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module CompileInfo (
  CompileInfo(..),
  CompileMessage(..),
) where

import Data.Either (isLeft,partitionEithers)
import Control.Monad (join)

import TypesBase (CompileErrorM(..),Mergeable(..))


data CompileMessage =
  CompileMessage {
    cmMessage :: String
  } |
  CompileNested {
    cnNested :: [CompileMessage]
  }
  deriving (Show)

joinMessages = foldr joinPair (CompileNested []) where
  joinPair m                     (CompileNested [])    = m
  joinPair (CompileNested [])    m                     = m
  joinPair m@(CompileMessage _)  (CompileNested ms)    = CompileNested (m:ms)
  joinPair (CompileNested ms)    m@(CompileMessage _)  = CompileNested (ms ++ [m])
  joinPair m1@(CompileMessage _) m2@(CompileMessage _) = CompileNested [m1,m2]
  joinPair (CompileNested ms1)   (CompileNested ms2)   = CompileNested (ms1 ++ ms2)

nestMessages m@(CompileMessage _) m2 = CompileNested (m:[m2])
nestMessages (CompileNested ms)   m2 = CompileNested (ms ++ [m2])

type CompileInfo = Either CompileMessage

instance CompileErrorM CompileInfo where
  compileErrorM e = Left $ CompileMessage e
  isCompileErrorM = isLeft
  collectOrErrorM = result . partitionEithers . foldr (:) [] where
    result ([],xs) = return xs
    result (es,_)  = Left $ joinMessages es  -- Take all errors.

instance Mergeable a => Mergeable (CompileInfo a) where
  mergeDefault = return mergeDefault
  mergeAny = result . partitionEithers . foldr (:) [] where
    result (_,xs@(x:_)) = return $ mergeAny xs
    result ([],_)       = compileErrorM "No successes in the empty set"
    result (es,_)       = Left $ joinMessages es  -- Take all errors.
  mergeAll = result . partitionEithers . foldr (:) [] where
    result ([],xs) = return $ mergeAll xs
    result (e:_,_) = Left e  -- Take first error.
  (Right x)  `mergeNested` (Right y)  = return $ x `mergeNested` y
  e@(Left _) `mergeNested` (Right _)  = e
  (Right _)  `mergeNested` e@(Left _) = e
  (Left e1)  `mergeNested` (Left e2)  = Left $ e1 `nestMessages` e2

instance Mergeable () where
  mergeDefault = ()
  mergeAny = const ()
  mergeAll = const ()
  () `mergeNested` () = ()
