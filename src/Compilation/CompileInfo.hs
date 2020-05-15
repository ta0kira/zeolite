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

module Compilation.CompileInfo (
  CompileInfo,
  CompileInfoT,
  CompileMessage,
  getCompileError,
  getCompileErrorT,
  getCompileSuccess,
  getCompileSuccessT,
  getCompileWarnings,
  getCompileWarningsT,
  isCompileError,
) where

import Control.Applicative
import Control.Monad.Trans
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.List (intercalate)
import Prelude hiding (concat,foldr)

#if MIN_VERSION_base(4,8,0)
#else
import Data.Foldable
#endif

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ()
#elif MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

import Base.CompileError
import Base.Mergeable


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

data CompileInfoState a =
  CompileFail {
    cfWarnings :: [String],
    cfErrors :: CompileMessage
  } |
  CompileSuccess {
    csWarnings :: [String],
    csData :: a
  }

data CompileInfoT m a =
  CompileInfoT {
    citState :: m (CompileInfoState a)
  }

type CompileInfo a = CompileInfoT Identity a

getCompileErrorT :: Monad m => CompileInfoT m a -> m CompileMessage
getCompileErrorT = fmap cfErrors . citState

getCompileSuccessT :: Monad m => CompileInfoT m a -> m a
getCompileSuccessT = fmap csData . citState

getCompileWarningsT :: Monad m => CompileInfoT m a -> m [String]
getCompileWarningsT = fmap getWarnings . citState

isCompileError :: CompileInfo a -> Bool
isCompileError x =
  case runIdentity (citState x) of
       CompileFail _ _ -> True
       _               -> False

getCompileError :: CompileInfo a -> CompileMessage
getCompileError = runIdentity . getCompileErrorT

getCompileSuccess :: CompileInfo a -> a
getCompileSuccess = runIdentity . getCompileSuccessT

getCompileWarnings :: CompileInfo a -> [String]
getCompileWarnings = runIdentity . getCompileWarningsT

instance (Functor m, Monad m) => Functor (CompileInfoT m) where
  fmap f x = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e    -> return $ CompileFail w e -- Not the same a.
         CompileSuccess w d -> return $ CompileSuccess w (f d)

instance (Applicative m, Monad m) => Applicative (CompileInfoT m) where
  pure = CompileInfoT .return . CompileSuccess []
  f <*> x = CompileInfoT $ do
    f' <- citState f
    x' <- citState x
    case (f',x') of
         (CompileFail w e,     _)                   -> return $ CompileFail w e -- Not the same a.
         (i,                   CompileFail w e)     -> return $ CompileFail (getWarnings i ++ w) e -- Not the same a.
         (CompileSuccess w1 f2,CompileSuccess w2 d) -> return $ CompileSuccess (w1 ++ w2) (f2 d)

instance Monad m => Monad (CompileInfoT m) where
  x >>= f = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e    -> return $ CompileFail w e -- Not the same a.
         CompileSuccess w d -> do
           d2 <- citState $ f d
           return $ prependWarning w d2
  return = pure

#if MIN_VERSION_base(4,9,0)
instance Monad m => MonadFail (CompileInfoT m) where
  fail = compileErrorM
#endif

instance MonadTrans CompileInfoT where
  lift = CompileInfoT . fmap (CompileSuccess [])

instance Monad m => CompileErrorM (CompileInfoT m) where
  compileErrorM e = CompileInfoT (return $ CompileFail [] $ CompileMessage e [])
  collectAllOrErrorM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result ([],xs2,ws) = CompileSuccess ws xs2
      result (es,_,ws)   = CompileFail ws $ CompileMessage "" es
  collectOneOrErrorM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result (_,x:_,ws) = CompileSuccess ws x
      result ([],_,ws)  = CompileFail ws $ CompileMessage "" []
      result (es,_,ws)  = CompileFail ws $ CompileMessage "" es
  reviseErrorM x e2 = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w (CompileMessage [] ms) -> return $ CompileFail w $ CompileMessage e2 ms
         CompileFail w e                      -> return $ CompileFail w $ CompileMessage e2 [e]
         x2                                   -> return x2
  compileWarningM w = CompileInfoT (return $ CompileSuccess [w] ())

instance Monad m => MergeableM (CompileInfoT m) where
  mergeAnyM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result (_,xs2@(_:_),ws) = CompileSuccess ws $ mergeAny xs2
      result ([],_,ws)        = CompileFail ws $ CompileMessage "" []
      result (es,_,ws)        = CompileFail ws $ CompileMessage "" es
  mergeAllM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result ([],xs2,ws) = CompileSuccess ws $ mergeAll xs2
      result (es,_,ws)   = CompileFail ws $ CompileMessage "" es
  mergeNestedM x y = CompileInfoT $ do
    x' <- citState x
    y' <- citState y
    case (x',y') of
         (CompileSuccess w1 x2,CompileSuccess w2 y2) -> return $ CompileSuccess (w1 ++ w2) $ x2 `mergeNested` y2
         (CompileFail w1 e,    CompileSuccess w2 _)  -> return $ CompileFail (w1 ++ w2) e
         (CompileSuccess w1 _, CompileFail w2 e)     -> return $ CompileFail (w1 ++ w2) e
         (CompileFail w1 e1,   CompileFail w2 e2)    -> return $ CompileFail (w1 ++ w2) $ e1 `nestMessages` e2

getWarnings :: CompileInfoState a -> [String]
getWarnings (CompileFail w _)    = w
getWarnings (CompileSuccess w _) = w

prependWarning :: [String] -> CompileInfoState a -> CompileInfoState a
prependWarning w (CompileSuccess w2 d) = CompileSuccess (w ++ w2) d
prependWarning w (CompileFail w2 e)    = CompileFail (w ++ w2) e

nestMessages :: CompileMessage -> CompileMessage -> CompileMessage
nestMessages (CompileMessage m1 ms1) (CompileMessage [] ms2) =
  CompileMessage m1 (ms1 ++ ms2)
nestMessages (CompileMessage [] ms1) (CompileMessage m2 ms2) =
  CompileMessage m2 (ms1 ++ ms2)
nestMessages (CompileMessage m1 ms1) ma@(CompileMessage _ _) =
  CompileMessage m1 (ms1 ++ [ma])

splitErrorsAndData :: Foldable f => f (CompileInfoState a) -> ([CompileMessage],[a],[String])
splitErrorsAndData = foldr partition ([],[],[]) where
  partition (CompileFail w e)    (es,ds,ws) = (e:es,ds,w++ws)
  partition (CompileSuccess w d) (es,ds,ws) = (es,d:ds,w++ws)
