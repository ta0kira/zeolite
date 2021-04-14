{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

module Base.CompilerError (
  CollectErrorsM(..),
  ErrorContextM(..),
  ErrorContextT(..),
  (<??),
  (??>),
  (<!!),
  (!!>),
  collectAllM_,
  collectFirstM_,
  emptyErrorM,
  errorFromIO,
  isCompilerError,
  isCompilerErrorM,
  isCompilerSuccess,
  isCompilerSuccessM,
  mapCompilerM,
  mapCompilerM_,
  mapErrorsM,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT,mapStateT)
import Data.Functor.Identity
import System.IO.Error (catchIOError)

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ()
#elif MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif


-- For some GHC versions, pattern-matching failures require MonadFail.
#if MIN_VERSION_base(4,9,0)
class (Monad m, MonadFail m) => ErrorContextM m where
#else
class Monad m => ErrorContextM m where
#endif
  compilerErrorM :: String -> m a
  withContextM :: m a -> String -> m a
  withContextM c _ = c
  summarizeErrorsM :: m a -> String -> m a
  summarizeErrorsM e _ = e
  compilerWarningM :: String -> m ()
  compilerWarningM _ = return ()
  compilerBackgroundM :: String -> m ()
  compilerBackgroundM _ = return ()
  resetBackgroundM :: m a -> m a
  resetBackgroundM = id

class ErrorContextM m => CollectErrorsM m where
  collectAllM :: Foldable f => f (m a) -> m [a]
  collectAnyM :: Foldable f => f (m a) -> m [a]
  collectFirstM :: Foldable f => f (m a) -> m a

class MonadTrans t => ErrorContextT t where
  isCompilerErrorT :: (Monad m, ErrorContextM (t m)) => t m a -> m Bool
  isCompilerSuccessT :: (Monad m, ErrorContextM (t m)) => t m a -> m Bool
  isCompilerSuccessT = fmap not . isCompilerErrorT
  ifElseSuccessT :: (Monad m, ErrorContextM (t m)) => t m a -> m () -> m () -> t m a

(<??) :: ErrorContextM m => m a -> String -> m a
(<??) = withContextM
infixl 1 <??

(??>) :: ErrorContextM m => String -> m a -> m a
(??>) = flip withContextM
infixr 1 ??>

(<!!) :: ErrorContextM m => m a -> String -> m a
(<!!) = summarizeErrorsM
infixl 1 <!!

(!!>) :: ErrorContextM m => String -> m a -> m a
(!!>) = flip summarizeErrorsM
infixr 1 !!>

collectAllM_ :: (Foldable f, CollectErrorsM m) => f (m a) -> m ()
collectAllM_ = fmap (const ()) . collectAllM

collectFirstM_ :: (Foldable f, CollectErrorsM m) => f (m a) -> m ()
collectFirstM_ = fmap (const ()) . collectFirstM

mapCompilerM :: CollectErrorsM m => (a -> m b) -> [a] -> m [b]
mapCompilerM f = collectAllM . map f

mapCompilerM_ :: CollectErrorsM m => (a -> m b) -> [a] -> m ()
mapCompilerM_ f = collectAllM_ . map f

isCompilerError :: (ErrorContextT t, ErrorContextM (t Identity)) => t Identity a -> Bool
isCompilerError = runIdentity . isCompilerErrorT

isCompilerSuccess :: (ErrorContextT t, ErrorContextM (t Identity)) => t Identity a -> Bool
isCompilerSuccess = runIdentity . isCompilerSuccessT

isCompilerErrorM :: CollectErrorsM m => m a -> m Bool
isCompilerErrorM x = collectFirstM [x >> return False,return True]

isCompilerSuccessM :: CollectErrorsM m => m a -> m Bool
isCompilerSuccessM x = collectFirstM [x >> return True,return False]

mapErrorsM :: CollectErrorsM m => [String] -> m a
mapErrorsM es = mapCompilerM_ compilerErrorM es >> emptyErrorM

emptyErrorM :: CollectErrorsM m => m a
emptyErrorM = compilerErrorM ""

errorFromIO :: (MonadIO m, ErrorContextM m) => IO a -> m a
errorFromIO x = do
  x' <- liftIO $ fmap Right x `catchIOError` (return . Left . show)
  case x' of
       (Right x2) -> return x2
       (Left e)   -> compilerErrorM e

instance ErrorContextM m => ErrorContextM (StateT a m) where
  compilerErrorM      = lift . compilerErrorM
  withContextM        = flip $ mapStateT . (??>)
  summarizeErrorsM    = flip $ mapStateT . (!!>)
  compilerWarningM    = lift . compilerWarningM
  compilerBackgroundM = lift . compilerBackgroundM
  resetBackgroundM    = mapStateT resetBackgroundM
