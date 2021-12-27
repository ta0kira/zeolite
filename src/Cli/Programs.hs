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
  waitCxxCommand :: (MonadIO m, CollectErrorsM m) => b -> AsyncWait b -> m (Either (AsyncWait b) (FilePath,CxxCommand))
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
  b -> Int -> [m (AsyncWait b)] -> m [(FilePath,CxxCommand)]
parallelProcess b n xs = do
  now <- collectAllM $ take n xs  -- This starts execution!
  let later = drop n xs
  recursive now later where
    recursive [] _ = return []
    recursive now later = do
      tried <- mapCompilerM (waitCxxCommand b) now
      let (wait,done) = partitionEithers tried
      let k = length done
      when (k == 0) $ liftIO $ threadDelay 1000  -- Sleep 1ms to control rate.
      new <- collectAllM $ take k later  -- This starts execution!
      following <- recursive (wait ++ new) (drop k later)
      return $ done ++ following
