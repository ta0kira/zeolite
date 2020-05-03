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

module Cli.ParseMetadata (
  ConfigFormat,
  autoReadConfig,
  autoWriteConfig,
) where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String

import Base.CompileError
import Cli.CompileMetadata
import Cli.CompileOptions
import Parser.Common
import Parser.TypeCategory ()
import Parser.TypeInstance ()
import Text.Regex.TDFA -- Not safe!
import Types.TypeCategory (FunctionName)
import Types.TypeInstance (CategoryName)


class ConfigFormat a where
  readConfig :: Parser a
  writeConfig :: CompileErrorM m => a -> m [String]

autoReadConfig :: (ConfigFormat a, CompileErrorM m) => String -> String -> m a
autoReadConfig f s  = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc readConfig) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

autoWriteConfig ::  (ConfigFormat a, CompileErrorM m) => a -> m String
autoWriteConfig = fmap unlines . writeConfig

structOpen :: Parser ()
structOpen = sepAfter (string_ "{")

structClose :: Parser ()
structClose = sepAfter (string_ "}")

indents :: [String] -> [String]
indents = map indent

indent :: String -> String
indent = ("  " ++)

prependFirst :: String -> [String] -> [String]
prependFirst s0 (s:ss) = (s0 ++ s):ss
prependFirst s0 _      = [s0]

validateCategoryName :: CompileErrorM m => String -> m ()
validateCategoryName c =
    when (not $ c =~ "^[A-Z][A-Za-z0-9]*$") $
      compileError $ "Invalid category name: \"" ++ c ++ "\""

parseCategoryName :: Parser String
parseCategoryName = fmap show (sourceParser :: Parser CategoryName)

validateFunctionName :: CompileErrorM m => String -> m ()
validateFunctionName f =
    when (not $ f =~ "^[a-z][A-Za-z0-9]*$") $
      compileError $ "Invalid function name: \"" ++ f ++ "\""

parseFunctionName :: Parser String
parseFunctionName = fmap show (sourceParser :: Parser FunctionName)

validateHash :: CompileErrorM m => String -> m ()
validateHash h =
    when (not $ h =~ "^[A-Za-z0-9]+$") $
      compileError $ "Version hash must be a hex string: \"" ++ h ++ "\""

parseHash :: Parser String
parseHash = labeled "version hash" $ sepAfter (many1 hexDigit)

validateNamespace :: CompileErrorM m => String -> m ()
validateNamespace ns =
    when (not $ ns =~ "^[A-Za-z][A-Za-z0-9_]*$") $
      compileError $ "Invalid category namespace: \"" ++ ns ++ "\""

parseNamespace :: Parser String
parseNamespace = labeled "namespace" $ do
    b <- lower
    e <- sepAfter $ many (alphaNum <|> char '_')
    return (b:e)

parseQuoted :: Parser String
parseQuoted = labeled "quoted string" $ do
  string_ "\""
  ss <- manyTill stringChar (string_ "\"")
  optionalSpace
  return ss

parseList :: Parser a -> Parser [a]
parseList p = labeled "list" $ do
  sepAfter (string_ "[")
  xs <- manyTill (sepAfter p) (string_ "]")
  optionalSpace
  return xs

instance ConfigFormat CompileMetadata where
  readConfig = do
    sepAfter (string_ "version_hash:")
    h <- parseHash
    sepAfter (string_ "path:")
    p <- parseQuoted
    sepAfter (string_ "namespace:")
    ns <- parseNamespace
    sepAfter (string_ "public_deps:")
    is <- parseList parseQuoted
    sepAfter (string_ "private_deps:")
    is2 <- parseList parseQuoted
    sepAfter (string_ "extra:")
    es <- parseList readConfig
    sepAfter (string_ "categories:")
    cs <- parseList parseCategoryName
    sepAfter (string_ "subdirs:")
    ds <- parseList parseQuoted
    sepAfter (string_ "public_files:")
    ps <- parseList parseQuoted
    sepAfter (string_ "private_files:")
    xs <- parseList parseQuoted
    sepAfter (string_ "test_files:")
    ts <- parseList parseQuoted
    sepAfter (string_ "hxx_files:")
    hxx <- parseList parseQuoted
    sepAfter (string_ "cxx_files:")
    cxx <- parseList parseQuoted
    sepAfter (string_ "object_files:")
    os <- parseList readConfig
    return (CompileMetadata h p ns is is2 es cs ds ps xs ts hxx cxx os)
  writeConfig m = do
    validateHash (cmVersionHash m)
    validateNamespace (cmNamespace m)
    _ <- collectAllOrErrorM $ map validateCategoryName (cmCategories m)
    extra   <- fmap concat $ collectAllOrErrorM $ map writeConfig $ cmExtraRequires m
    objects <- fmap concat $ collectAllOrErrorM $ map writeConfig $ cmObjectFiles m
    return $ [
        "version_hash: " ++ (cmVersionHash m),
        "path: " ++ (show $ cmPath m),
        "namespace: " ++ (cmNamespace m),
        "public_deps: ["
      ] ++ indents (map show $ cmPublicDeps m) ++ [
        "]",
        "private_deps: ["
      ] ++ indents (map show $ cmPrivateDeps m) ++ [
        "]",
        "extra: ["
      ] ++ indents extra ++ [
        "]",
        "categories: ["
      ] ++ indents (cmCategories m) ++ [
        "]",
        "subdirs: ["
      ] ++ indents (map show $ cmSubdirs m) ++ [
        "]",
        "public_files: ["
      ] ++ indents (map show $ cmPublicFiles m) ++ [
        "]",
        "private_files: ["
      ] ++ indents (map show $ cmPrivateFiles m) ++ [
        "]",
        "test_files: ["
      ] ++ indents (map show $ cmTestFiles m) ++ [
        "]",
        "hxx_files: ["
      ] ++ indents (map show $ cmHxxFiles m) ++ [
        "]",
        "cxx_files: ["
      ] ++ indents (map show $ cmCxxFiles m) ++ [
        "]",
        "object_files: ["
      ] ++ indents objects ++ [
        "]"
      ]

instance ConfigFormat ObjectFile where
  readConfig = category <|> other where
    category = do
      sepAfter (string_ "category_object")
      structOpen
      sepAfter (string_ "category:")
      c <- readConfig
      sepAfter (string_ "requires:")
      rs <- parseList readConfig
      sepAfter (string_ "files:")
      fs <- parseList parseQuoted
      structClose
      return (CategoryObjectFile c rs fs)
    other = do
      sepAfter (string_ "other_object")
      structOpen
      sepAfter (string_ "file:")
      f <- parseQuoted
      structClose
      return (OtherObjectFile f)
  writeConfig (CategoryObjectFile c rs fs) = do
    category <- writeConfig c
    requires <- fmap concat $ collectAllOrErrorM $ map writeConfig rs
    return $ [
        "category_object {"
      ] ++ indents ("category: " `prependFirst` category) ++ [
        indent "requires: ["
      ] ++ (indents . indents) requires ++ [
        indent "]",
        indent "files: ["
      ] ++ (indents . indents) (map show fs) ++ [
        indent "]",
        "}"
      ]
  writeConfig (OtherObjectFile f) = do
    return $ [
        "other_object {",
        indent ("file: " ++ show f),
        "}"
      ]

instance ConfigFormat CategoryIdentifier where
  readConfig = category <|> unresolved where
    category = do
      sepAfter (string_ "category")
      structOpen
      sepAfter (string_ "name:")
      c <- parseCategoryName
      sepAfter (string_ "namespace:")
      ns <- parseNamespace
      sepAfter (string_ "path:")
      p <- parseQuoted
      structClose
      return (CategoryIdentifier p c ns)
    unresolved = do
      sepAfter (string_ "unresolved")
      structOpen
      sepAfter (string_ "name:")
      c <- parseCategoryName
      structClose
      return (UnresolvedCategory c)
  writeConfig (CategoryIdentifier p c ns) = do
    validateCategoryName c
    validateNamespace ns
    return $ [
        "category {",
        indent $ "name: " ++ c,
        indent $ "namespace: " ++ ns,
        indent $ "path: " ++ show p,
        "}"
      ]
  writeConfig (UnresolvedCategory c) = do
    validateCategoryName c
    return $ ["unresolved { " ++ "name: " ++ c ++ " " ++ "}"]

instance ConfigFormat ModuleConfig where
  readConfig = do
      sepAfter (string_ "root:")
      p <- parseQuoted
      sepAfter (string_ "path:")
      d <- parseQuoted
      sepAfter (string_ "public_deps:")
      is <- parseList parseQuoted
      sepAfter (string_ "private_deps:")
      is2 <- parseList parseQuoted
      sepAfter (string_ "extra_files:")
      es <- parseList parseQuoted
      sepAfter (string_ "extra_paths:")
      ep <- parseList parseQuoted
      sepAfter (string_ "always_include:")
      ec <- parseList parseCategoryName
      sepAfter (string_ "mode:")
      m <- readConfig
      o <- output <|> return ""
      return ((ModuleConfig p d is is2 es ep ec m o)) where
        output = do
          sepAfter (string_ "output:")
          parseQuoted
  writeConfig m = do
    _ <- collectAllOrErrorM $ map validateCategoryName (rmExtraRequires m)
    mode <- writeConfig (rmMode m)
    return $ [
        "root: " ++ show (rmRoot m),
        "path: " ++ show (rmPath m),
        "public_deps: ["
      ] ++ indents (map show $ rmPublicDeps m) ++ [
        "]",
        "private_deps: ["
      ] ++ indents (map show $ rmPrivateDeps m) ++ [
        "]",
        "extra_files: ["
      ] ++ indents (map show $ rmExtraFiles m) ++ [
        "]",
        "extra_paths: ["
      ] ++ indents (map show $ rmExtraPaths m) ++ [
        "]",
        "always_include: ["
      ] ++ indents (rmExtraRequires m) ++ [
        "]"
      ] ++ "mode: " `prependFirst` mode ++ output where
      output = if null (rmOutputName m)
                  then []
                  else ["output: " ++ show (rmOutputName m)]

instance ConfigFormat CompileMode where
  readConfig = labeled "compile mode" $ binary <|> incremental where
    binary = do
      sepAfter (string_ "binary")
      structOpen
      sepAfter (string_ "category:")
      c <- parseCategoryName
      sepAfter (string_ "function:")
      f <- parseFunctionName
      structClose
      return (CompileBinary c f)
    incremental = do
      sepAfter (string_ "incremental")
      return CompileIncremental
  writeConfig (CompileBinary c f) = do
    validateCategoryName c
    validateFunctionName f
    return $ [
        "binary { ",
        indent ("category: " ++ c),
        indent ("function: " ++ f),
        "}"
      ]
  writeConfig CompileIncremental = return ["incremental"]
  writeConfig _ = compileError "Invalid compile mode"
