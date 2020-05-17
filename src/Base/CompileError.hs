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
  mapErrorsM,
  mapErrorsM_,
) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Foldable
#endif

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
  collectAllOrErrorM :: Foldable f => f (m a) -> m [a]
  collectOneOrErrorM :: Foldable f => f (m a) -> m a
  reviseErrorM :: m a -> String -> m a
  reviseErrorM e _ = e
  compileWarningM :: String -> m ()

mapErrorsM :: CompileErrorM m => (a -> m b) -> [a] -> m [b]
mapErrorsM f = collectAllOrErrorM . map f

mapErrorsM_ :: CompileErrorM m => (a -> m b) -> [a] -> m ()
mapErrorsM_ f xs = mapErrorsM f xs >> return ()
