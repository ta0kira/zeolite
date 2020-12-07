{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

module Module.Paths (
  PathIOHandler(..),
  fixPath,
  fixPaths,
) where

import Control.Monad.IO.Class
import Data.List (nub,isSuffixOf)
import System.FilePath

import Base.CompileError


class PathIOHandler r where
  resolveModule     :: (MonadIO m, CollectErrorsM m) => r -> FilePath -> FilePath -> m FilePath
  isSystemModule    :: (MonadIO m, CollectErrorsM m) => r -> FilePath -> FilePath -> m Bool
  resolveBaseModule :: (MonadIO m, CollectErrorsM m) => r -> m FilePath
  isBaseModule      :: (MonadIO m, CollectErrorsM m) => r -> FilePath -> m Bool
  zipWithContents   :: (MonadIO m, CollectErrorsM m) => r -> FilePath -> [FilePath] -> m [(FilePath,String)]

fixPath :: FilePath -> FilePath
fixPath = foldl (</>) "" . process [] . map dropSlash . splitPath where
  dropSlash "/" = "/"
  dropSlash d
    | isSuffixOf "/" d = reverse $ tail $ reverse d
    | otherwise        = d
  process rs        (".":ds)  = process rs ds
  process ("..":rs) ("..":ds) = process ("..":"..":rs) ds
  process ("/":[])  ("..":ds) = process ("/":[]) ds
  process (_:rs)    ("..":ds) = process rs ds
  process rs        (d:ds)    = process (d:rs) ds
  process rs        _         = reverse rs

fixPaths :: [FilePath] -> [FilePath]
fixPaths = nub . map fixPath
