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

module Config.LoadConfig (
  localConfigPath,
  loadConfig,
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import System.Directory

import Base.CompilerError
import Config.LocalConfig
import Config.ParseConfig ()
import Module.ParseMetadata (autoReadConfig)

import Paths_zeolite_lang (getDataFileName)


loadConfig :: (MonadIO m, ErrorContextM m) => m (Resolver,Backend)
loadConfig = do
  configFile <- liftIO localConfigPath
  isFile <- liftIO $ doesFileExist configFile
  when (not isFile) $ compilerErrorM "Zeolite has not been configured. Please run zeolite-setup."
  configString <- liftIO $ readFile configFile
  lc <- autoReadConfig configFile configString <!! "Zeolite configuration is corrupt. Please rerun zeolite-setup."
  pathsFile   <- liftIO $ globalPathsPath
  pathsExists <- liftIO $ doesFileExist pathsFile
  paths <- if pathsExists
              then liftIO $ readFile pathsFile >>= return . lines
              else return []
  return (addPaths (lcResolver lc) paths,lcBackend lc)

localConfigPath :: IO FilePath
localConfigPath = getDataFileName localConfigFilename >>= canonicalizePath

localConfigFilename :: FilePath
localConfigFilename = ".local-config"

globalPathsFilename :: FilePath
globalPathsFilename = "global-paths"

globalPathsPath :: IO FilePath
globalPathsPath = getDataFileName globalPathsFilename >>= canonicalizePath

addPaths :: Resolver -> [FilePath] -> Resolver
addPaths (SimpleResolver ls ps) ps2 = SimpleResolver ls (ps ++ ps2)
