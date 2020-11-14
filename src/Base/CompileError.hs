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

module Base.CompileError (
  CompileErrorM(..),
  (<??),
  (??>),
  (<!!),
  (!!>),
  collectAllM_,
  collectFirstM_,
  errorFromIO,
  isCompileErrorM,
  isCompileSuccessM,
  mapErrorsM,
  mapErrorsM_,
) where

import Control.Monad.IO.Class
import System.IO.Error (catchIOError)

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ()
#elif MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif


-- For some GHC versions, pattern-matching failures require MonadFail.
#if MIN_VERSION_base(4,9,0)
class (Monad m, MonadFail m) => CompileErrorM m where
#else
class Monad m => CompileErrorM m where
#endif
  compileErrorM :: String -> m a
  collectAllM :: Foldable f => f (m a) -> m [a]
  collectAnyM :: Foldable f => f (m a) -> m [a]
  collectFirstM :: Foldable f => f (m a) -> m a
  withContextM :: m a -> String -> m a
  withContextM c _ = c
  summarizeErrorsM :: m a -> String -> m a
  summarizeErrorsM e _ = e
  compileWarningM :: String -> m ()
  compileWarningM _ = return ()
  compileBackgroundM :: String -> m ()
  compileBackgroundM _ = return ()
  resetBackgroundM :: m a -> m a
  resetBackgroundM = id

(<??) :: CompileErrorM m => m a -> String -> m a
(<??) = withContextM

(??>) :: CompileErrorM m => String -> m a -> m a
(??>) = flip withContextM

(<!!) :: CompileErrorM m => m a -> String -> m a
(<!!) = summarizeErrorsM

(!!>) :: CompileErrorM m => String -> m a -> m a
(!!>) = flip summarizeErrorsM

collectAllM_ :: (Foldable f, CompileErrorM m) => f (m a) -> m ()
collectAllM_ = fmap (const ()) . collectAllM

collectFirstM_ :: (Foldable f, CompileErrorM m) => f (m a) -> m ()
collectFirstM_ = fmap (const ()) . collectFirstM

mapErrorsM :: CompileErrorM m => (a -> m b) -> [a] -> m [b]
mapErrorsM f = collectAllM . map f

mapErrorsM_ :: CompileErrorM m => (a -> m b) -> [a] -> m ()
mapErrorsM_ f = collectAllM_ . map f

isCompileErrorM :: CompileErrorM m => m a -> m Bool
isCompileErrorM x = collectFirstM [x >> return False,return True]

isCompileSuccessM :: CompileErrorM m => m a -> m Bool
isCompileSuccessM x = collectFirstM [x >> return True,return False]

errorFromIO :: (MonadIO m, CompileErrorM m) => IO a -> m a
errorFromIO x = do
  x' <- liftIO $ fmap Right x `catchIOError` (return . Left . show)
  case x' of
       (Right x2) -> return x2
       (Left e)   -> compileErrorM e
