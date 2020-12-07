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
  CompileMessage,
  asCompilerError,
  asCompileWarnings,
  fromTrackedErrors,
  getCompilerError,
  getCompilerErrorT,
  getCompilerSuccess,
  getCompilerSuccessT,
  getCompileWarnings,
  getCompileWarningsT,
  isCompilerError,
  isCompilerErrorT,
  isEmptyCompileMessage,
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

import Base.CompilerError


type TrackedErrors = TrackedErrorsT Identity

type TrackedErrorsIO = TrackedErrorsT IO

data TrackedErrorsT m a =
  TrackedErrorsT {
    citState :: m (TrackedErrorsState a)
  }

getCompilerError :: TrackedErrors a -> CompileMessage
getCompilerError = runIdentity . getCompilerErrorT

getCompilerSuccess :: TrackedErrors a -> a
getCompilerSuccess = runIdentity . getCompilerSuccessT

getCompileWarnings :: TrackedErrors a -> CompileMessage
getCompileWarnings = runIdentity . getCompileWarningsT

isCompilerError :: TrackedErrors a -> Bool
isCompilerError = runIdentity . isCompilerErrorT

getCompilerErrorT :: Monad m => TrackedErrorsT m a -> m CompileMessage
getCompilerErrorT = fmap cfErrors . citState

getCompilerSuccessT :: Monad m => TrackedErrorsT m a -> m a
getCompilerSuccessT = fmap csData . citState

getCompileWarningsT :: Monad m => TrackedErrorsT m a -> m CompileMessage
getCompileWarningsT = fmap getWarnings . citState

isCompilerErrorT :: Monad m => TrackedErrorsT m a -> m Bool
isCompilerErrorT x = do
  x' <- citState x
  case x' of
       CompileFail _ _ -> return True
       _               -> return False

isEmptyCompileMessage :: CompileMessage -> Bool
isEmptyCompileMessage (CompileMessage "" ws) = all isEmptyCompileMessage ws
isEmptyCompileMessage _                      = False

fromTrackedErrors :: Monad m => TrackedErrors a -> TrackedErrorsT m a
fromTrackedErrors x = runIdentity $ do
  x' <- citState x
  return $ TrackedErrorsT $ return x'

asCompileWarnings :: Monad m => TrackedErrors a -> TrackedErrorsT m ()
asCompileWarnings x = runIdentity $ do
  x' <- citState x
  return $ TrackedErrorsT $ return $
    case x' of
         (CompileFail ws es)      -> CompilerSuccess (ws `mergeMessages` es) [] ()
         (CompilerSuccess ws bs _) -> CompilerSuccess ws bs ()

asCompilerError :: Monad m => TrackedErrors a -> TrackedErrorsT m ()
asCompilerError x = runIdentity $ do
  x' <- citState x
  return $ TrackedErrorsT $ return $
    case x' of
         (CompilerSuccess ws bs _) -> includeBackground bs $ CompileFail emptyMessage ws
         (CompileFail ws es)      -> CompileFail ws es

toTrackedErrors :: Monad m => TrackedErrorsT m a -> m (TrackedErrors a)
toTrackedErrors x = do
  x' <- citState x
  return $ TrackedErrorsT $ return x'

tryTrackedErrorsIO :: String -> String -> TrackedErrorsIO a -> IO a
tryTrackedErrorsIO warn err x = do
  x' <- toTrackedErrors x
  let w = getCompileWarnings $ x' <?? warn
  let e = getCompilerError    $ x' <?? err
  if isCompilerError x'
     then do
       hPutStr stderr $ show w
       hPutStr stderr $ show e
       exitFailure
     else do
       hPutStr stderr $ show w
       return $ getCompilerSuccess x'

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

data TrackedErrorsState a =
  CompileFail {
    cfWarnings :: CompileMessage,
    cfErrors :: CompileMessage
  } |
  CompilerSuccess {
    csWarnings :: CompileMessage,
    csBackground :: [String],
    csData :: a
  }

instance Show a => Show (TrackedErrorsState a) where
  show = format where
    format (CompileFail w e) = intercalate "\n" $ errors ++ warnings where
      errors   = showAs "Errors:"   $ lines $ show e
      warnings = showAs "Warnings:" $ lines $ show w
    format (CompilerSuccess w b x) = intercalate "\n" $ content ++ warnings ++ background where
      content    = [show x]
      warnings   = showAs "Warnings:" $ lines $ show w
      background = showAs "Background:" b
    showAs m = (m:) . map ("  " ++)

instance Show a => Show (TrackedErrors a) where
  show = show . runIdentity . citState

instance (Functor m, Monad m) => Functor (TrackedErrorsT m) where
  fmap f x = TrackedErrorsT $ do
    x' <- citState x
    case x' of
         CompileFail w e      -> return $ CompileFail w e -- Not the same a.
         CompilerSuccess w b d -> return $ CompilerSuccess w b (f d)

instance (Applicative m, Monad m) => Applicative (TrackedErrorsT m) where
  pure = TrackedErrorsT .return . CompilerSuccess emptyMessage []
  f <*> x = TrackedErrorsT $ do
    f' <- citState f
    x' <- citState x
    case (f',x') of
         (CompileFail w e,_) ->
           return $ CompileFail w e -- Not the same a.
         (i,CompileFail w e) ->
           return $ CompileFail (getWarnings i `mergeMessages` w) (addBackground (getBackground i) e)
         (CompilerSuccess w1 b1 f2,CompilerSuccess w2 b2 d) ->
           return $ CompilerSuccess (w1 `mergeMessages` w2) (b1 ++ b2) (f2 d)

instance Monad m => Monad (TrackedErrorsT m) where
  x >>= f = TrackedErrorsT $ do
    x' <- citState x
    case x' of
         CompileFail w e -> return $ CompileFail w e -- Not the same a.
         CompilerSuccess w b d -> do
           d2 <- citState $ f d
           return $ includeBackground b $ includeWarnings w d2
  return = pure

#if MIN_VERSION_base(4,9,0)
instance Monad m => MonadFail (TrackedErrorsT m) where
  fail = compilerErrorM
#endif

instance MonadTrans TrackedErrorsT where
  lift = TrackedErrorsT . fmap (CompilerSuccess emptyMessage [])

instance MonadIO m => MonadIO (TrackedErrorsT m) where
  liftIO = lift . liftIO

instance Monad m => ErrorContextM (TrackedErrorsT m) where
  compilerErrorM e = TrackedErrorsT $ return $ CompileFail emptyMessage $ CompileMessage e []
  withContextM x c = TrackedErrorsT $ do
    x' <- citState x
    case x' of
         CompileFail w e        -> return $ CompileFail (pushWarningScope c w) (pushErrorScope c e)
         CompilerSuccess w bs x2 -> return $ CompilerSuccess (pushWarningScope c w) bs x2
  summarizeErrorsM x e2 = TrackedErrorsT $ do
    x' <- citState x
    case x' of
         CompileFail w e -> return $ CompileFail w (pushErrorScope e2 e)
         x2 -> return x2
  compilerWarningM w = TrackedErrorsT (return $ CompilerSuccess (CompileMessage w []) [] ())
  compilerBackgroundM b = TrackedErrorsT (return $ CompilerSuccess emptyMessage [b] ())
  resetBackgroundM x = TrackedErrorsT $ do
    x' <- citState x
    case x' of
         CompilerSuccess w _ d -> return $ CompilerSuccess w [] d
         x2                   -> return x2

instance Monad m => CollectErrorsM (TrackedErrorsT m) where
  collectAllM = combineResults (select . splitErrorsAndData) where
    select ([],xs2,bs,ws) = CompilerSuccess (CompileMessage "" ws) bs xs2
    select (es,_,bs,ws)   = CompileFail (CompileMessage "" ws) $ addBackground bs $ CompileMessage "" es
  collectAnyM = combineResults (select . splitErrorsAndData) where
    select (_,xs2,bs,ws) = CompilerSuccess (CompileMessage "" ws) bs xs2
  collectFirstM = combineResults (select . splitErrorsAndData) where
    select (_,x:_,bs,ws) = CompilerSuccess (CompileMessage "" ws) bs x
    select (es,_,bs,ws)  = CompileFail (CompileMessage "" ws) $ addBackground bs $ CompileMessage "" es

combineResults :: (Monad m, Foldable f) =>
  ([TrackedErrorsState a] -> TrackedErrorsState b) -> f (TrackedErrorsT m a) -> TrackedErrorsT m b
combineResults f = TrackedErrorsT . fmap f . sequence . map citState . foldr (:) []

emptyMessage :: CompileMessage
emptyMessage = CompileMessage "" []

pushErrorScope :: String -> CompileMessage -> CompileMessage
pushErrorScope e2 ea@(CompileMessage e ms)
  | null e            = CompileMessage e2 ms
  | otherwise         = CompileMessage e2 [ea]

pushWarningScope :: String -> CompileMessage -> CompileMessage
pushWarningScope e2 ea
  | isEmptyCompileMessage ea = emptyMessage  -- Skip the scope if there isn't already a warning.
  | otherwise                = pushErrorScope e2 ea

mergeMessages :: CompileMessage -> CompileMessage -> CompileMessage
mergeMessages (CompileMessage "" []) e2                       = e2
mergeMessages e1                     (CompileMessage "" [])   = e1
mergeMessages (CompileMessage "" es1) (CompileMessage "" es2) = CompileMessage "" (es1 ++ es2)
mergeMessages e1                      (CompileMessage "" es2) = CompileMessage "" ([e1] ++ es2)
mergeMessages (CompileMessage "" es1) e2                      = CompileMessage "" (es1 ++ [e2])
mergeMessages e1                      e2                      = CompileMessage "" [e1,e2]

addBackground :: [String] -> CompileMessage -> CompileMessage
addBackground b (CompileMessage e es) = CompileMessage e (es ++ map (flip CompileMessage []) b)

getWarnings :: TrackedErrorsState a -> CompileMessage
getWarnings (CompileFail w _)      = w
getWarnings (CompilerSuccess w _ _) = w

includeWarnings :: CompileMessage -> TrackedErrorsState a -> TrackedErrorsState a
includeWarnings = update where
  update w (CompilerSuccess w2 b d) = CompilerSuccess (w `mergeMessages` w2) b d
  update w (CompileFail w2 e)      = CompileFail (w `mergeMessages` w2) e

getBackground :: TrackedErrorsState a -> [String]
getBackground (CompilerSuccess _ b _) = b
getBackground _                      = []

includeBackground :: [String] -> TrackedErrorsState a -> TrackedErrorsState a
includeBackground b  (CompileFail w e)       = CompileFail w (addBackground b e)
includeBackground b1 (CompilerSuccess w b2 d) = CompilerSuccess w (b1 ++ b2) d

splitErrorsAndData :: Foldable f => f (TrackedErrorsState a) -> ([CompileMessage],[a],[String],[CompileMessage])
splitErrorsAndData = foldr partition ([],[],[],[]) where
  partition (CompileFail w e)      (es,ds,bs,ws) = (e:es,ds,bs,w:ws)
  partition (CompilerSuccess w b d) (es,ds,bs,ws) = (es,d:ds,b++bs,w:ws)
