{- -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Cli.Programs (
  CompilerBackend(..),
  CxxCommand(..),
  TestCommand(..),
  TestCommandResult(..),
  VersionHash(..),
  parallelProcess,
) where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Either (partitionEithers)

import Base.CompilerError


class CompilerBackend b where
  type AsyncWait b :: *
  syncCxxCommand   :: (MonadIO m, CollectErrorsM m) => b -> CxxCommand -> m FilePath
  asyncCxxCommand   :: (MonadIO m, CollectErrorsM m) => b -> CxxCommand -> m (AsyncWait b)
  waitCxxCommand :: (MonadIO m, CollectErrorsM m) => b -> AsyncWait b -> m (Either (AsyncWait b) FilePath)
  runTestCommand  :: (MonadIO m, CollectErrorsM m) => b -> TestCommand -> m TestCommandResult
  getCompilerHash :: (MonadIO m, CollectErrorsM m) => b -> m VersionHash

newtype VersionHash = VersionHash String deriving (Eq)

instance Show VersionHash where
  show (VersionHash h) = h

data CxxCommand =
  CompileToObject {
    ctoSource :: FilePath,
    ctoPath :: FilePath,
    ctoMacros :: [(String,Maybe String)],
    ctoPaths :: [FilePath],
    ctoExtra :: Bool
  } |
  CompileToShared {
    ctsSources :: [FilePath],
    ctsOutput :: FilePath,
    ctsLinkFlags :: [String]
  } |
  CompileToBinary {
    ctbMain :: FilePath,
    ctbSources :: [FilePath],
    ctbMacros :: [(String,Maybe String)],
    ctbOutput :: FilePath,
    ctbPaths :: [FilePath],
    ctbLinkFlags :: [String]
  }
  deriving (Show)

data TestCommand =
  TestCommand {
    tcBinary :: FilePath,
    tcPath :: FilePath,
    tcArgs :: [String]
  }
  deriving (Show)

data TestCommandResult =
  TestCommandResult {
    tcrSuccess :: Bool,
    tcrOutput :: [String],
    tcrError :: [String]
  }
  deriving (Show)

parallelProcess :: (CompilerBackend b, MonadIO m, CollectErrorsM m) =>
  b -> Int -> [(m (AsyncWait b),a)] -> m [(FilePath,a)]
parallelProcess b n xs | n < 1 = parallelProcess b 1 xs
parallelProcess b n xs = do
  now <- mapCompilerM start $ take n xs
  let later = drop n xs
  recursive now later where
    start (process,extra) = do
      process' <- process
      return (process',extra)
    wait (process,extra) = do
      process' <- waitCxxCommand b process
      case process' of
           Left process2 -> return $ Left (process2,extra)
           Right path -> return $ Right (path,extra)
    recursive [] _ = return []
    recursive now later = do
      tried <- mapCompilerM wait now
      let (running,done) = partitionEithers tried
      let k = length done
      when (k == 0) $ liftIO $ threadDelay 10000  -- Sleep 10ms to control rate.
      new <- mapCompilerM start $ take k later
      following <- recursive (running ++ new) (drop k later)
      return $ done ++ following
