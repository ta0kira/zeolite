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

module Config.ParseConfig (
) where

import Control.Applicative.Permutations

import Config.CompilerConfig
import Module.ParseMetadata
import Parser.Common


instance ConfigFormat Backend where
  readConfig = do
    sepAfter (string_ "unix_backend")
    structOpen
    o <- runPermutation $ UnixBackend
      <$> parseRequired "cxx_binary:"    parseQuoted
      <*> parseRequired "compile_flags:" (parseList parseQuoted)
      <*> parseRequired "library_flags:" (parseList parseQuoted)
      <*> parseRequired "binary_flags:"  (parseList parseQuoted)
      <*> parseRequired "ar_binary:"     parseQuoted
    structClose
    return o
  writeConfig (UnixBackend cb cf lf bf ar) = do
    return $ [
        "unix_backend {",
        indent $ "cxx_binary: " ++ show cb,
        indent "compile_flags: ["
      ] ++ (indents . indents) (map show cf) ++ [
        indent "]",
        indent "library_flags: ["
      ] ++ (indents . indents) (map show lf) ++ [
        indent "]",
        indent "binary_flags: ["
      ] ++ (indents . indents) (map show bf) ++ [
        indent "]",
        indent $ "ar_binary: " ++ show ar,
        "}"
      ]

instance ConfigFormat Resolver where
  readConfig = do
    sepAfter (string_ "simple_resolver")
    structOpen
    o <- runPermutation $ SimpleResolver
      <$> parseRequired "system_allowed:" (parseList parseQuoted)
      <*> parseRequired "extra_paths:"    (parseList parseQuoted)
    structClose
    return o
  writeConfig (SimpleResolver ss es) = do
    return $ [
        "simple_resolver {",
        indent "system_allowed: ["
      ] ++ (indents . indents) (map show ss) ++ [
        indent "]",
        indent "extra_paths: ["
      ] ++ (indents . indents) (map show es) ++ [
        indent "]",
        "}"
      ]

instance ConfigFormat LocalConfig where
  readConfig = runPermutation $ LocalConfig
    <$> parseRequired "backend:"  readConfig
    <*> parseRequired "resolver:" readConfig
  writeConfig (LocalConfig b r) = do
    b' <- writeConfig b
    r' <- writeConfig r
    return $ ("backend: " `prependFirst` b') ++ ("resolver:" `prependFirst` r')
