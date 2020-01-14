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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module CompileInfo (
  CompileInfo,
  CompileMessage,
  getCompileError,
  getCompileSuccess,
) where

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

data CompileInfo a =
  CompileFail {
    ceErrors :: CompileMessage
  } |
  CompileSuccess {
    csData :: a
  }

isCompileError (CompileFail _) = True
isCompileError _                = False

getCompileError   = ceErrors
getCompileSuccess = csData

instance Functor CompileInfo where
  fmap f (CompileFail e)    = (CompileFail e) -- Not the same a.
  fmap f (CompileSuccess d) = CompileSuccess (f d)

instance Applicative CompileInfo where
  pure = CompileSuccess
  (CompileFail e) <*> _ = (CompileFail e) -- Not the same a.
  _ <*> (CompileFail e) = (CompileFail e) -- Not the same a.
  (CompileSuccess f) <*> (CompileSuccess d) = CompileSuccess (f d)

instance Monad CompileInfo where
  (CompileFail e)    >>= _ = (CompileFail e) -- Not the same a.
  (CompileSuccess d) >>= f = f d
  return = CompileSuccess

instance CompileErrorM CompileInfo where
  compileErrorM = CompileFail . flip CompileMessage []
  isCompileErrorM = isCompileError
  collectAllOrErrorM = result . splitErrorsAndData where
    result ([],xs) = return xs
    result (es,_) = CompileFail $ CompileMessage "" es
  collectOneOrErrorM = result . splitErrorsAndData where
    result (_,x:_) = return x
    result ([],_)  = compileErrorM "No choices found"
    result (es,_)  = CompileFail $ CompileMessage "" es
  reviseErrorM x@(CompileSuccess _) _ = x
  reviseErrorM x@(CompileFail (CompileMessage [] ms)) s = CompileFail $ CompileMessage s ms
  reviseErrorM x@(CompileFail e) s = CompileFail $ CompileMessage s [e]

instance MergeableM CompileInfo where
  mergeAnyM = result . splitErrorsAndData where
    result (_,xs@(x:_)) = return $ mergeAny xs
    result ([],_)       = compileErrorM "No choices found"
    result (es,_)       = CompileFail $ CompileMessage "" es
  mergeAllM = result . splitErrorsAndData where
    result ([],xs) = return $ mergeAll xs
    result (es,_)  = CompileFail $ CompileMessage "" es
  (CompileSuccess x) `mergeNestedM` (CompileSuccess y) = return $ x `mergeNested` y
  e@(CompileFail _)  `mergeNestedM` (CompileSuccess _) = e
  (CompileSuccess _) `mergeNestedM` e@(CompileFail _)  = e
  (CompileFail e1)   `mergeNestedM` (CompileFail e2)   = CompileFail $ e1 `nestMessages` e2

nestMessages (CompileMessage m1 ms1) (CompileMessage [] ms2) =
  CompileMessage m1 (ms1 ++ ms2)
nestMessages (CompileMessage [] ms1) (CompileMessage m2 ms2) =
  CompileMessage m2 (ms1 ++ ms2)
nestMessages (CompileMessage m1 ms1) ma@(CompileMessage _ _) =
  CompileMessage m1 (ms1 ++ [ma])

splitErrorsAndData :: Foldable f => f (CompileInfo a) -> ([CompileMessage],[a])
splitErrorsAndData = foldr partition ([],[]) where
  partition (CompileFail e)    (es,ds) = (e:es,ds)
  partition (CompileSuccess d) (es,ds) = (es,d:ds)
