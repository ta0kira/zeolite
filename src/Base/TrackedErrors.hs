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

module Base.TrackedErrors (
  TrackedErrors,
  TrackedErrorsIO,
  TrackedErrorsT,
  asCompilerError,
  asCompilerWarnings,
  fromTrackedErrors,
  getCompilerError,
  getCompilerErrorT,
  getCompilerSuccess,
  getCompilerSuccessT,
  getCompilerWarnings,
  getCompilerWarningsT,
  toTrackedErrors,
  tryTrackedErrorsIO,
) where

import Control.Applicative
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

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

import Base.CompilerError
import Base.CompilerMessage


type TrackedErrors = TrackedErrorsT Identity

type TrackedErrorsIO = TrackedErrorsT IO

data TrackedErrorsT m a =
  TrackedErrorsT {
    tetState :: m (TrackedErrorsState a)
  }

getCompilerError :: TrackedErrors a -> CompilerMessage
getCompilerError = runIdentity . getCompilerErrorT

getCompilerSuccess :: TrackedErrors a -> a
getCompilerSuccess = runIdentity . getCompilerSuccessT

getCompilerWarnings :: TrackedErrors a -> CompilerMessage
getCompilerWarnings = runIdentity . getCompilerWarningsT

getCompilerErrorT :: Monad m => TrackedErrorsT m a -> m CompilerMessage
getCompilerErrorT = fmap cfErrors . tetState

getCompilerSuccessT :: Monad m => TrackedErrorsT m a -> m a
getCompilerSuccessT = fmap csData . tetState

getCompilerWarningsT :: Monad m => TrackedErrorsT m a -> m CompilerMessage
getCompilerWarningsT = fmap getWarnings . tetState

fromTrackedErrors :: Monad m => TrackedErrors a -> TrackedErrorsT m a
fromTrackedErrors x = runIdentity $ do
  x' <- tetState x
  return $ TrackedErrorsT $ return x'

asCompilerWarnings :: Monad m => TrackedErrors a -> TrackedErrorsT m ()
asCompilerWarnings x = runIdentity $ do
  x' <- tetState x
  return $ TrackedErrorsT $ return $
    case x' of
         (CompilerFail ws es)      -> CompilerSuccess (ws <> es) [] ()
         (CompilerSuccess ws bs _) -> CompilerSuccess ws bs ()

asCompilerError :: Monad m => TrackedErrors a -> TrackedErrorsT m ()
asCompilerError x = runIdentity $ do
  x' <- tetState x
  return $ TrackedErrorsT $ return $
    case x' of
         (CompilerSuccess ws bs _) -> includeBackground bs $ CompilerFail mempty ws
         (CompilerFail ws es)      -> CompilerFail ws es

toTrackedErrors :: Monad m => TrackedErrorsT m a -> m (TrackedErrors a)
toTrackedErrors x = do
  x' <- tetState x
  return $ TrackedErrorsT $ return x'

tryTrackedErrorsIO :: String -> String -> TrackedErrorsIO a -> IO a
tryTrackedErrorsIO warn err x = do
  x' <- toTrackedErrors x
  let w = getCompilerWarnings $ x' <?? warn
  let e = getCompilerError    $ x' <?? err
  if isCompilerError x'
     then do
       hPutStr stderr $ show w
       hPutStr stderr $ show e
       exitFailure
     else do
       hPutStr stderr $ show w
       return $ getCompilerSuccess x'

data TrackedErrorsState a =
  CompilerFail {
    cfWarnings :: CompilerMessage,
    cfErrors :: CompilerMessage
  } |
  CompilerSuccess {
    csWarnings :: CompilerMessage,
    csBackground :: [String],
    csData :: a
  }

instance Show a => Show (TrackedErrorsState a) where
  show = format where
    format (CompilerFail w e) = intercalate "\n" $ errors ++ warnings where
      errors   = showAs "Errors:"   $ lines $ show e
      warnings = showAs "Warnings:" $ lines $ show w
    format (CompilerSuccess w b x) = intercalate "\n" $ content ++ warnings ++ background where
      content    = [show x]
      warnings   = showAs "Warnings:" $ lines $ show w
      background = showAs "Background:" b
    showAs m = (m:) . map ("  " ++)

instance Show a => Show (TrackedErrors a) where
  show = show . runIdentity . tetState

instance (Functor m, Monad m) => Functor (TrackedErrorsT m) where
  fmap f x = TrackedErrorsT $ do
    x' <- tetState x
    case x' of
         CompilerFail w e      -> return $ CompilerFail w e -- Not the same a.
         CompilerSuccess w b d -> return $ CompilerSuccess w b (f d)

instance (Applicative m, Monad m) => Applicative (TrackedErrorsT m) where
  pure = TrackedErrorsT .return . CompilerSuccess mempty []
  f <*> x = TrackedErrorsT $ do
    f' <- tetState f
    x' <- tetState x
    case (f',x') of
         (CompilerFail w e,_) ->
           return $ CompilerFail w e -- Not the same a.
         (i,CompilerFail w e) ->
           return $ CompilerFail (getWarnings i <> w) (prefixCompilerMessages (getBackground i) e)
         (CompilerSuccess w1 b1 f2,CompilerSuccess w2 b2 d) ->
           return $ CompilerSuccess (w1 <> w2) (b1 ++ b2) (f2 d)

instance Monad m => Monad (TrackedErrorsT m) where
  x >>= f = TrackedErrorsT $ do
    x' <- tetState x
    case x' of
         CompilerFail w e -> return $ CompilerFail w e -- Not the same a.
         CompilerSuccess w b d -> do
           d2 <- tetState $ f d
           return $ includeBackground b $ includeWarnings w d2
  return = pure

#if MIN_VERSION_base(4,9,0)
instance Monad m => MonadFail (TrackedErrorsT m) where
  fail = compilerErrorM
#endif

instance MonadTrans TrackedErrorsT where
  lift = TrackedErrorsT . fmap (CompilerSuccess mempty [])

instance MonadIO m => MonadIO (TrackedErrorsT m) where
  liftIO = lift . liftIO

instance Monad m => ErrorContextM (TrackedErrorsT m) where
  compilerErrorM e = TrackedErrorsT $ return $ CompilerFail mempty $ compilerMessage e
  withContextM x c = TrackedErrorsT $ do
    x' <- tetState x
    case x' of
         CompilerFail w e        -> return $ CompilerFail (pushWarningScope c w) (pushErrorScope c e)
         CompilerSuccess w bs x2 -> return $ CompilerSuccess (pushWarningScope c w) bs x2
  summarizeErrorsM x e2 = TrackedErrorsT $ do
    x' <- tetState x
    case x' of
         CompilerFail w e -> return $ CompilerFail w (pushErrorScope e2 e)
         x2 -> return x2
  compilerWarningM w = TrackedErrorsT (return $ CompilerSuccess (compilerMessage w) [] ())
  compilerBackgroundM b = TrackedErrorsT (return $ CompilerSuccess mempty [b] ())
  resetBackgroundM x = TrackedErrorsT $ do
    x' <- tetState x
    case x' of
         CompilerSuccess w _ d -> return $ CompilerSuccess w [] d
         x2                   -> return x2

instance Monad m => CollectErrorsM (TrackedErrorsT m) where
  collectAllM = combineResults (select . splitErrorsAndData) where
    select ([],xs2,bs,ws) = CompilerSuccess (compilerMessages ws) bs xs2
    select (es,_,bs,ws)   = CompilerFail (compilerMessages ws) $ prefixCompilerMessages bs $ compilerMessages es
  collectAnyM = combineResults (select . splitErrorsAndData) where
    select (_,xs2,bs,ws) = CompilerSuccess (compilerMessages ws) bs xs2
  collectFirstM = combineResults (select . splitErrorsAndData) where
    select (_,x:_,bs,ws) = CompilerSuccess (compilerMessages ws) bs x
    select (es,_,bs,ws)  = CompilerFail (compilerMessages ws) $ prefixCompilerMessages bs $ compilerMessages es

instance ErrorContextT TrackedErrorsT where
  isCompilerErrorT x = do
    x' <- tetState x
    case x' of
         CompilerFail _ _ -> return True
         _                -> return False
  ifElseSuccessT x success failure = TrackedErrorsT $ do
    x' <- tetState x
    case x' of
         CompilerSuccess _ _ _ -> success
         _                     -> failure
    return x'

combineResults :: (Monad m, Foldable f) =>
  ([TrackedErrorsState a] -> TrackedErrorsState b) -> f (TrackedErrorsT m a) -> TrackedErrorsT m b
combineResults f = TrackedErrorsT . fmap f . sequence . map tetState . foldr (:) []

getWarnings :: TrackedErrorsState a -> CompilerMessage
getWarnings (CompilerFail w _)      = w
getWarnings (CompilerSuccess w _ _) = w

includeWarnings :: CompilerMessage -> TrackedErrorsState a -> TrackedErrorsState a
includeWarnings = update where
  update w (CompilerSuccess w2 b d) = CompilerSuccess (w <> w2) b d
  update w (CompilerFail w2 e)      = CompilerFail (w <> w2) e

getBackground :: TrackedErrorsState a -> [String]
getBackground (CompilerSuccess _ b _) = b
getBackground _                      = []

includeBackground :: [String] -> TrackedErrorsState a -> TrackedErrorsState a
includeBackground b  (CompilerFail w e)       = CompilerFail w (prefixCompilerMessages b e)
includeBackground b1 (CompilerSuccess w b2 d) = CompilerSuccess w (b1 ++ b2) d

splitErrorsAndData :: Foldable f => f (TrackedErrorsState a) -> ([CompilerMessage],[a],[String],[CompilerMessage])
splitErrorsAndData = foldr partition ([],[],[],[]) where
  partition (CompilerFail w e)      (es,ds,bs,ws) = (e:es,ds,bs,w:ws)
  partition (CompilerSuccess w b d) (es,ds,bs,ws) = (es,d:ds,b++bs,w:ws)
