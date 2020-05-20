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

module Base.CompileInfo (
  CompileInfo,
  CompileInfoIO,
  CompileMessage,
  fromCompileInfo,
  getCompileError,
  getCompileErrorT,
  getCompileSuccess,
  getCompileSuccessT,
  getCompileWarnings,
  getCompileWarningsT,
  isCompileError,
  isCompileErrorT,
  toCompileInfo,
  tryCompileInfoIO,
) where

import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.IO.Class ()
import Control.Monad.Trans
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.List (intercalate)
import Prelude hiding (concat,foldr)
import System.Exit
import System.IO

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ()
#elif MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

import Base.CompileError
import Base.Mergeable


type CompileInfo a = CompileInfoT Identity a

type CompileInfoIO a = CompileInfoT IO a

data CompileInfoT m a =
  CompileInfoT {
    citState :: m (CompileInfoState a)
  }

getCompileError :: CompileInfo a -> CompileMessage
getCompileError = runIdentity . getCompileErrorT

getCompileSuccess :: CompileInfo a -> a
getCompileSuccess = runIdentity . getCompileSuccessT

getCompileWarnings :: CompileInfo a -> [String]
getCompileWarnings = runIdentity . getCompileWarningsT

isCompileError :: CompileInfo a -> Bool
isCompileError = runIdentity . isCompileErrorT

getCompileErrorT :: Monad m => CompileInfoT m a -> m CompileMessage
getCompileErrorT = fmap cfErrors . citState

getCompileSuccessT :: Monad m => CompileInfoT m a -> m a
getCompileSuccessT = fmap csData . citState

getCompileWarningsT :: Monad m => CompileInfoT m a -> m [String]
getCompileWarningsT = fmap getWarnings . citState

isCompileErrorT :: Monad m => CompileInfoT m a -> m Bool
isCompileErrorT x = do
  x' <- citState x
  case x' of
       CompileFail _ _ -> return True
       _               -> return False

fromCompileInfo :: Monad m => CompileInfo a -> CompileInfoT m a
fromCompileInfo x = runIdentity $ do
  x' <- citState x
  return $ CompileInfoT $ return x'

toCompileInfo :: Monad m => CompileInfoT m a -> m (CompileInfo a)
toCompileInfo x = do
  x' <- citState x
  return $ CompileInfoT $ return x'

tryCompileInfoIO :: String -> CompileInfoIO a -> IO a
tryCompileInfoIO message x = do
  x' <- toCompileInfo $ x `reviseErrorM` message
  if isCompileError x'
     then do
       hPutStr stderr $ concat $ map (++ "\n") (getCompileWarnings x')
       hPutStr stderr $ show $ getCompileError x'
       exitFailure
     else do
       hPutStr stderr $ concat $ map (++ "\n") (getCompileWarnings x')
       return $ getCompileSuccess x'

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
    csBackground :: [String],
    csData :: a
  }

instance (Functor m, Monad m) => Functor (CompileInfoT m) where
  fmap f x = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e      -> return $ CompileFail w e -- Not the same a.
         CompileSuccess w b d -> return $ CompileSuccess w b (f d)

instance (Applicative m, Monad m) => Applicative (CompileInfoT m) where
  pure = CompileInfoT .return . CompileSuccess [] []
  f <*> x = CompileInfoT $ do
    f' <- citState f
    x' <- citState x
    case (f',x') of
         (CompileFail w e,_) ->
           return $ CompileFail w e -- Not the same a.
         (i,CompileFail w e) ->
           return $ CompileFail (getWarnings i ++ w) (addBackground (getBackground i) e)
         (CompileSuccess w1 b1 f2,CompileSuccess w2 b2 d) ->
           return $ CompileSuccess (w1 ++ w2) (b2 ++ b1) (f2 d)

instance Monad m => Monad (CompileInfoT m) where
  x >>= f = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e -> return $ CompileFail w e -- Not the same a.
         CompileSuccess w b d -> do
           d2 <- citState $ f d
           return $ includeBackground b $ prependWarning w d2
  return = pure

#if MIN_VERSION_base(4,9,0)
instance Monad m => MonadFail (CompileInfoT m) where
  fail = compileErrorM
#endif

instance MonadTrans CompileInfoT where
  lift = CompileInfoT . fmap (CompileSuccess [] [])

instance MonadIO m => MonadIO (CompileInfoT m) where
  liftIO = lift . liftIO

instance Monad m => CompileErrorM (CompileInfoT m) where
  compileErrorM e = CompileInfoT (return $ CompileFail [] $ CompileMessage e [])
  collectAllOrErrorM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result ([],xs2,ws) = CompileSuccess ws [] xs2
      result (es,_,ws)   = CompileFail ws $ CompileMessage "" es
  collectOneOrErrorM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result (_,x:_,ws) = CompileSuccess ws [] x
      result ([],_,ws)  = CompileFail ws $ CompileMessage "" []
      result (es,_,ws)  = CompileFail ws $ CompileMessage "" es
  reviseErrorM x e2 = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w (CompileMessage [] ms) -> return $ CompileFail w $ CompileMessage e2 ms
         CompileFail w e                      -> return $ CompileFail w $ CompileMessage e2 [e]
         x2                                   -> return x2
  compileWarningM w = CompileInfoT (return $ CompileSuccess [w] [] ())
  compileBackgroundM b = CompileInfoT (return $ CompileSuccess [] [b] ())
  resetBackgroundM x = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileSuccess w _ d -> return $ CompileSuccess w [] d
         x2                   -> return x2

instance Monad m => MergeableM (CompileInfoT m) where
  mergeAnyM xs = CompileInfoT $ do
    xs' <- sequence $ map citState $ foldr (:) [] xs
    return $ result $ splitErrorsAndData xs' where
      result ([],[],ws) = CompileFail ws $ CompileMessage "" []
      result (es,[],ws) = CompileFail ws $ CompileMessage "" es
      result (_,xs2,ws) = CompileSuccess ws [] (mergeAny xs2)
  mergeAllM = collectAllOrErrorM >=> return . mergeAll

getWarnings :: CompileInfoState a -> [String]
getWarnings (CompileFail w _)      = w
getWarnings (CompileSuccess w _ _) = w

prependWarning :: [String] -> CompileInfoState a -> CompileInfoState a
prependWarning w (CompileSuccess w2 b d) = CompileSuccess (w ++ w2) b d
prependWarning w (CompileFail w2 e)      = CompileFail (w ++ w2) e

getBackground :: CompileInfoState a -> [String]
getBackground (CompileFail _ _)      = []
getBackground (CompileSuccess _ b _) = b

includeBackground :: [String] -> CompileInfoState a -> CompileInfoState a
includeBackground b  (CompileFail w e)       = CompileFail w (addBackground b e)
includeBackground b1 (CompileSuccess w b2 d) = CompileSuccess w (b2 ++ b1) d

addBackground :: [String] -> CompileMessage -> CompileMessage
addBackground b (CompileMessage e es) = CompileMessage e (es ++ map (flip CompileMessage []) b)

splitErrorsAndData :: Foldable f => f (CompileInfoState a) -> ([CompileMessage],[a],[String])
splitErrorsAndData = foldr partition ([],[],[]) where
  partition (CompileFail w e)      (es,ds,ws) = (e:es,ds,w++ws)
  partition (CompileSuccess w _ d) (es,ds,ws) = (es,d:ds,w++ws)
