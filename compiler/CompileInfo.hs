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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module CompileInfo (
  CompileInfo,
  CompileMessage,
  getCompileError,
  getCompileSuccess,
  getCompileWarnings,
) where

import Data.List (intercalate)

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

import TypesBase (CompileErrorM(..),Mergeable(..),MergeableM(..))


data CompileMessage =
  CompileMessage {
    cmMessage :: String,
    cmNested :: [CompileMessage]
  }

instance Show CompileMessage where
  show = format "" where
    format indent (CompileMessage [] ms) =
      concat (map (format indent) ms)
    format indent (CompileMessage m ms) =
      (doIndent indent m) ++ "\n" ++ concat (map (format $ indent ++ "  ") ms)
    doIndent indent s = intercalate "\n" $ map (indent ++) $ lines s

data CompileInfo a =
  CompileFail {
    cfWarnings :: [String],
    cfErrors :: CompileMessage
  } |
  CompileSuccess {
    csWarnings :: [String],
    csData :: a
  }

isCompileError (CompileFail _ _) = True
isCompileError _                 = False

getCompileError   = cfErrors
getCompileSuccess = csData
getCompileWarnings (CompileFail w _)    = w
getCompileWarnings (CompileSuccess w _) = w


instance Functor CompileInfo where
  fmap f (CompileFail w e)    = CompileFail w e -- Not the same a.
  fmap f (CompileSuccess w d) = CompileSuccess w (f d)

instance Applicative CompileInfo where
  pure = CompileSuccess []
  (CompileFail w e) <*> _ = CompileFail w e -- Not the same a.
  i <*> (CompileFail w e) = CompileFail (getCompileWarnings i ++ w) e -- Not the same a.
  (CompileSuccess w1 f) <*> (CompileSuccess w2 d) = CompileSuccess (w1 ++ w2) (f d)

instance Monad CompileInfo where
  (CompileFail w e)    >>= _ = CompileFail w e -- Not the same a.
  (CompileSuccess w d) >>= f = prependWarning w $ f d
  return = CompileSuccess []

prependWarning w (CompileSuccess w2 d) = CompileSuccess (w ++ w2) d
prependWarning w (CompileFail w2 e)    = CompileFail (w ++ w2) e

instance CompileErrorM CompileInfo where
  compileErrorM = CompileFail [] . flip CompileMessage []
  isCompileErrorM = isCompileError
  collectAllOrErrorM = result . splitErrorsAndData where
    result ([],xs,ws) = CompileSuccess ws xs
    result (es,_,ws) = CompileFail ws $ CompileMessage "" es
  collectOneOrErrorM = result . splitErrorsAndData where
    result (_,x:_,ws) = CompileSuccess ws x
    result ([],_,ws)  = CompileFail ws $ CompileMessage "No choices found" []
    result (es,_,ws)  = CompileFail ws $ CompileMessage "" es
  reviseErrorM x@(CompileSuccess _ _) _ = x
  reviseErrorM x@(CompileFail w (CompileMessage [] ms)) s = CompileFail w $ CompileMessage s ms
  reviseErrorM x@(CompileFail w e) s = CompileFail w $ CompileMessage s [e]
  compileWarningM w = CompileSuccess [w] ()

instance MergeableM CompileInfo where
  mergeAnyM = result . splitErrorsAndData where
    result (_,xs@(x:_),ws) = CompileSuccess ws $ mergeAny xs
    result ([],_,ws)       = CompileFail ws $ CompileMessage "No choices found" []
    result (es,_,ws)       = CompileFail ws $ CompileMessage "" es
  mergeAllM = result . splitErrorsAndData where
    result ([],xs,ws) = CompileSuccess ws $ mergeAll xs
    result (es,_,ws)  = CompileFail ws $ CompileMessage "" es
  (CompileSuccess w1 x) `mergeNestedM` (CompileSuccess w2 y) = CompileSuccess (w1 ++ w2) $ x `mergeNested` y
  (CompileFail w1 e)    `mergeNestedM` (CompileSuccess w2 _) = CompileFail (w1 ++ w2) e
  (CompileSuccess w1 _) `mergeNestedM` (CompileFail w2 e)    = CompileFail (w1 ++ w2) e
  (CompileFail w1 e1)   `mergeNestedM` (CompileFail w2 e2)   = CompileFail (w1 ++ w2) $ e1 `nestMessages` e2

#if MIN_VERSION_base(4,9,0)
instance MonadFail CompileInfo where
  fail = compileErrorM
#endif

nestMessages (CompileMessage m1 ms1) (CompileMessage [] ms2) =
  CompileMessage m1 (ms1 ++ ms2)
nestMessages (CompileMessage [] ms1) (CompileMessage m2 ms2) =
  CompileMessage m2 (ms1 ++ ms2)
nestMessages (CompileMessage m1 ms1) ma@(CompileMessage _ _) =
  CompileMessage m1 (ms1 ++ [ma])

splitErrorsAndData :: Foldable f => f (CompileInfo a) -> ([CompileMessage],[a],[String])
splitErrorsAndData = foldr partition ([],[],[]) where
  partition (CompileFail w e)    (es,ds,ws) = (e:es,ds,w++ws)
  partition (CompileSuccess w d) (es,ds,ws) = (es,d:ds,w++ws)
