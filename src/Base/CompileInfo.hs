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
  asCompileError,
  asCompileWarnings,
  fromCompileInfo,
  getCompileError,
  getCompileErrorT,
  getCompileSuccess,
  getCompileSuccessT,
  getCompileWarnings,
  getCompileWarningsT,
  isCompileError,
  isCompileErrorT,
  isEmptyCompileMessage,
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

getCompileWarnings :: CompileInfo a -> CompileMessage
getCompileWarnings = runIdentity . getCompileWarningsT

isCompileError :: CompileInfo a -> Bool
isCompileError = runIdentity . isCompileErrorT

getCompileErrorT :: Monad m => CompileInfoT m a -> m CompileMessage
getCompileErrorT = fmap cfErrors . citState

getCompileSuccessT :: Monad m => CompileInfoT m a -> m a
getCompileSuccessT = fmap csData . citState

getCompileWarningsT :: Monad m => CompileInfoT m a -> m CompileMessage
getCompileWarningsT = fmap getWarnings . citState

isCompileErrorT :: Monad m => CompileInfoT m a -> m Bool
isCompileErrorT x = do
  x' <- citState x
  case x' of
       CompileFail _ _ -> return True
       _               -> return False

isEmptyCompileMessage :: CompileMessage -> Bool
isEmptyCompileMessage (CompileMessage "" ws) = all isEmptyCompileMessage ws
isEmptyCompileMessage _                      = False

fromCompileInfo :: Monad m => CompileInfo a -> CompileInfoT m a
fromCompileInfo x = runIdentity $ do
  x' <- citState x
  return $ CompileInfoT $ return x'

asCompileWarnings :: Monad m => CompileInfo a -> CompileInfoT m ()
asCompileWarnings x = runIdentity $ do
  x' <- citState x
  return $ CompileInfoT $ return $
    case x' of
         (CompileFail ws es)      -> CompileSuccess (ws `mergeMessages` es) [] ()
         (CompileSuccess ws bs _) -> CompileSuccess ws bs ()

asCompileError :: Monad m => CompileInfo a -> CompileInfoT m ()
asCompileError x = runIdentity $ do
  x' <- citState x
  return $ CompileInfoT $ return $
    case x' of
         (CompileSuccess ws bs _) -> includeBackground bs $ CompileFail emptyMessage ws
         (CompileFail ws es)      -> CompileFail ws es

toCompileInfo :: Monad m => CompileInfoT m a -> m (CompileInfo a)
toCompileInfo x = do
  x' <- citState x
  return $ CompileInfoT $ return x'

tryCompileInfoIO :: String -> String -> CompileInfoIO a -> IO a
tryCompileInfoIO warn err x = do
  x' <- toCompileInfo x
  let w = getCompileWarnings $ x' <?? warn
  let e = getCompileError    $ x' <?? err
  if isCompileError x'
     then do
       hPutStr stderr $ show w
       hPutStr stderr $ show e
       exitFailure
     else do
       hPutStr stderr $ show w
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
    cfWarnings :: CompileMessage,
    cfErrors :: CompileMessage
  } |
  CompileSuccess {
    csWarnings :: CompileMessage,
    csBackground :: [String],
    csData :: a
  }

instance Show a => Show (CompileInfoState a) where
  show = format where
    format (CompileFail w e) = intercalate "\n" $ errors ++ warnings where
      errors   = showAs "Errors:"   $ lines $ show e
      warnings = showAs "Warnings:" $ lines $ show w
    format (CompileSuccess w b x) = intercalate "\n" $ content ++ warnings ++ background where
      content    = [show x]
      warnings   = showAs "Warnings:" $ lines $ show w
      background = showAs "Background:" b
    showAs m = (m:) . map ("  " ++)

instance Show a => Show (CompileInfo a) where
  show = show . runIdentity . citState

instance (Functor m, Monad m) => Functor (CompileInfoT m) where
  fmap f x = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e      -> return $ CompileFail w e -- Not the same a.
         CompileSuccess w b d -> return $ CompileSuccess w b (f d)

instance (Applicative m, Monad m) => Applicative (CompileInfoT m) where
  pure = CompileInfoT .return . CompileSuccess emptyMessage []
  f <*> x = CompileInfoT $ do
    f' <- citState f
    x' <- citState x
    case (f',x') of
         (CompileFail w e,_) ->
           return $ CompileFail w e -- Not the same a.
         (i,CompileFail w e) ->
           return $ CompileFail (getWarnings i `mergeMessages` w) (addBackground (getBackground i) e)
         (CompileSuccess w1 b1 f2,CompileSuccess w2 b2 d) ->
           return $ CompileSuccess (w1 `mergeMessages` w2) (b1 ++ b2) (f2 d)

instance Monad m => Monad (CompileInfoT m) where
  x >>= f = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e -> return $ CompileFail w e -- Not the same a.
         CompileSuccess w b d -> do
           d2 <- citState $ f d
           return $ includeBackground b $ includeWarnings w d2
  return = pure

#if MIN_VERSION_base(4,9,0)
instance Monad m => MonadFail (CompileInfoT m) where
  fail = compileErrorM
#endif

instance MonadTrans CompileInfoT where
  lift = CompileInfoT . fmap (CompileSuccess emptyMessage [])

instance MonadIO m => MonadIO (CompileInfoT m) where
  liftIO = lift . liftIO

instance Monad m => CompileErrorM (CompileInfoT m) where
  compileErrorM e = CompileInfoT $ return $ CompileFail emptyMessage $ CompileMessage e []
  collectAllM = combineResults (select . splitErrorsAndData) where
      select ([],xs2,bs,ws) = CompileSuccess (CompileMessage "" ws) bs xs2
      select (es,_,bs,ws)   = CompileFail (CompileMessage "" ws) $ addBackground bs $ CompileMessage "" es
  collectFirstM = combineResults (select . splitErrorsAndData) where
      select (_,x:_,bs,ws) = CompileSuccess (CompileMessage "" ws) bs x
      select (es,_,bs,ws)  = CompileFail (CompileMessage "" ws) $ addBackground bs $ CompileMessage "" es
  withContextM x e2 = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileFail w e        -> return $ CompileFail (pushWarningScope e2 w) (pushErrorScope e2 e)
         CompileSuccess w bs x2 -> return $ CompileSuccess (pushWarningScope e2 w) bs x2
  compileWarningM w = CompileInfoT (return $ CompileSuccess (CompileMessage w []) [] ())
  compileBackgroundM b = CompileInfoT (return $ CompileSuccess emptyMessage [b] ())
  resetBackgroundM x = CompileInfoT $ do
    x' <- citState x
    case x' of
         CompileSuccess w _ d -> return $ CompileSuccess w [] d
         x2                   -> return x2

instance Monad m => MergeableM (CompileInfoT m) where
  mergeAnyM = combineResults (select . splitErrorsAndData) where
      select (es,[],bs,ws) = CompileFail (CompileMessage "" ws) $ addBackground bs $ CompileMessage "" es
      select (_,xs2,bs,ws) = CompileSuccess (CompileMessage "" ws) bs (mergeAny xs2)
  mergeAllM = collectAllM >=> return . mergeAll

combineResults :: (Monad m, Foldable f) =>
  ([CompileInfoState a] -> CompileInfoState b) -> f (CompileInfoT m a) -> CompileInfoT m b
combineResults f = CompileInfoT . fmap f . sequence . map citState . foldr (:) []

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

getWarnings :: CompileInfoState a -> CompileMessage
getWarnings (CompileFail w _)      = w
getWarnings (CompileSuccess w _ _) = w

includeWarnings :: CompileMessage -> CompileInfoState a -> CompileInfoState a
includeWarnings = update where
  update w (CompileSuccess w2 b d) = CompileSuccess (w `mergeMessages` w2) b d
  update w (CompileFail w2 e)      = CompileFail (w `mergeMessages` w2) e

getBackground :: CompileInfoState a -> [String]
getBackground (CompileSuccess _ b _) = b
getBackground _                      = []

includeBackground :: [String] -> CompileInfoState a -> CompileInfoState a
includeBackground b  (CompileFail w e)       = CompileFail w (addBackground b e)
includeBackground b1 (CompileSuccess w b2 d) = CompileSuccess w (b1 ++ b2) d

splitErrorsAndData :: Foldable f => f (CompileInfoState a) -> ([CompileMessage],[a],[String],[CompileMessage])
splitErrorsAndData = foldr partition ([],[],[],[]) where
  partition (CompileFail w e)      (es,ds,bs,ws) = (e:es,ds,bs,w:ws)
  partition (CompileSuccess w b d) (es,ds,bs,ws) = (es,d:ds,b++bs,w:ws)
