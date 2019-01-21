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
  CompileError {
    ceErrors :: CompileMessage
  } |
  CompileSuccess {
    csData :: a
  }

isCompileError (CompileError _) = True
isCompileError _                = False

getCompileError   = ceErrors
getCompileSuccess = csData

instance Functor CompileInfo where
  fmap f (CompileError e)   = (CompileError e) -- Not the same a.
  fmap f (CompileSuccess d) = CompileSuccess (f d)

instance Applicative CompileInfo where
  pure = CompileSuccess
  (CompileError e) <*> _ = (CompileError e) -- Not the same a.
  _ <*> (CompileError e) = (CompileError e) -- Not the same a.
  (CompileSuccess f) <*> (CompileSuccess d) = CompileSuccess (f d)

instance Monad CompileInfo where
  (CompileError e)   >>= _ = (CompileError e) -- Not the same a.
  (CompileSuccess d) >>= f = f d
  return = CompileSuccess

instance CompileErrorM CompileInfo where
  compileErrorM = CompileError . flip CompileMessage []
  isCompileErrorM = isCompileError
  collectAllOrErrorM = result . splitErrorsAndData where
    result ([],xs) = return xs
    result (es,_) = CompileError $ CompileMessage "" es
  collectOneOrErrorM = result . splitErrorsAndData where
    result (_,x:_) = return x
    result ([],_)  = compileErrorM "No choices found"
    result (es,_)  = CompileError $ CompileMessage "" es
  reviseErrorM x@(CompileSuccess _) _ = x
  reviseErrorM x@(CompileError (CompileMessage [] ms)) s = CompileError $ CompileMessage s ms
  reviseErrorM x@(CompileError e) s = CompileError $ CompileMessage s [e]

instance MergeableM CompileInfo where
  mergeAnyM = result . splitErrorsAndData where
    result (_,xs@(x:_)) = return $ mergeAny xs
    result ([],_)       = compileErrorM "No choices found"
    result (es,_)       = CompileError $ CompileMessage "" es
  mergeAllM = result . splitErrorsAndData where
    result ([],xs) = return $ mergeAll xs
    result (es,_)  = CompileError $ CompileMessage "" es
  (CompileSuccess x) `mergeNestedM` (CompileSuccess y) = return $ x `mergeNested` y
  e@(CompileError _) `mergeNestedM` (CompileSuccess _) = e
  (CompileSuccess _) `mergeNestedM` e@(CompileError _) = e
  (CompileError e1)  `mergeNestedM` (CompileError e2)  = CompileError $ e1 `nestMessages` e2

nestMessages (CompileMessage m1 ms1) (CompileMessage [] ms2) =
  CompileMessage m1 (ms1 ++ ms2)
nestMessages (CompileMessage [] ms1) (CompileMessage m2 ms2) =
  CompileMessage m2 (ms1 ++ ms2)
nestMessages (CompileMessage m1 ms1) ma@(CompileMessage _ _) =
  CompileMessage m1 (ms1 ++ [ma])

splitErrorsAndData :: Foldable f => f (CompileInfo a) -> ([CompileMessage],[a])
splitErrorsAndData = foldr partition ([],[]) where
  partition (CompileError e)   (es,ds) = (e:es,ds)
  partition (CompileSuccess d) (es,ds) = (es,d:ds)
