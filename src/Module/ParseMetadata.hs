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

module Module.ParseMetadata (
  ConfigFormat,
  autoReadConfig,
  autoWriteConfig,
) where

import Control.Applicative.Permutations
import Control.Monad (when)

import Base.CompilerError
import Cli.CompileOptions
import Cli.Programs (VersionHash(..))
import Module.CompileMetadata
import Parser.Common
import Parser.Procedure ()
import Parser.TextParser
import Parser.TypeCategory ()
import Parser.TypeInstance ()
import Text.Regex.TDFA
import Types.Procedure (Expression,MacroName)
import Types.TypeCategory (FunctionName(..),Namespace(..))
import Types.TypeInstance (CategoryName(..))


class ConfigFormat a where
  readConfig :: TextParser a
  writeConfig :: CollectErrorsM m => a -> m [String]

autoReadConfig :: (ConfigFormat a, ErrorContextM m) => String -> String -> m a
autoReadConfig f s = runTextParser (between optionalSpace endOfDoc readConfig) f s

autoWriteConfig ::  (ConfigFormat a, CollectErrorsM m) => a -> m String
autoWriteConfig = fmap unlines . writeConfig

structOpen :: TextParser ()
structOpen = sepAfter (string_ "{")

structClose :: TextParser ()
structClose = sepAfter (string_ "}")

indents :: [String] -> [String]
indents = map indent

indent :: String -> String
indent = ("  " ++)

prependFirst :: String -> [String] -> [String]
prependFirst s0 (s:ss) = (s0 ++ s):ss
prependFirst s0 _      = [s0]

validateCategoryName :: ErrorContextM m => CategoryName -> m ()
validateCategoryName c =
    when (not $ show c =~ "^[A-Z][A-Za-z0-9]*$") $
      compilerErrorM $ "Invalid category name: \"" ++ show c ++ "\""

validateFunctionName :: ErrorContextM m => FunctionName -> m ()
validateFunctionName f =
    when (not $ show f =~ "^[a-z][A-Za-z0-9]*$") $
      compilerErrorM $ "Invalid function name: \"" ++ show f ++ "\""

validateHash :: ErrorContextM m => VersionHash -> m ()
validateHash h =
    when (not $ show h =~ "^[A-Za-z0-9]+$") $
      compilerErrorM $ "Version hash must be a hex string: \"" ++ show h ++ "\""

parseHash :: TextParser VersionHash
parseHash = labeled "version hash" $ sepAfter (fmap VersionHash $ some hexDigitChar)

maybeShowNamespace :: ErrorContextM m => String -> Namespace -> m [String]
maybeShowNamespace l (StaticNamespace ns) = do
  when (not $ ns =~ "^[A-Za-z][A-Za-z0-9_]*$") $
    compilerErrorM $ "Invalid category namespace: \"" ++ ns ++ "\""
  return [l ++ " " ++ ns]
maybeShowNamespace _ _ = return []

parseNamespace :: TextParser Namespace
parseNamespace = labeled "namespace" $ do
  b <- lowerChar
  e <- sepAfter $ many (alphaNumChar <|> char '_')
  return $ StaticNamespace (b:e)

parseQuoted :: TextParser String
parseQuoted = labeled "quoted string" $ do
  string_ "\""
  ss <- manyTill stringChar (string_ "\"")
  optionalSpace
  return ss

parseList :: TextParser a -> TextParser [a]
parseList p = labeled "list" $ do
  sepAfter (string_ "[")
  xs <- manyTill (sepAfter p) (string_ "]")
  optionalSpace
  return xs

parseOptional :: String -> a -> TextParser a -> Permutation (TextParser) a
parseOptional l def p = toPermutationWithDefault def $ do
    try $ sepAfter (string_ l)
    p

parseRequired :: String -> TextParser a -> Permutation (TextParser) a
parseRequired l p = toPermutation $ do
    try $ sepAfter (string_ l)
    p

instance ConfigFormat CompileMetadata where
  readConfig = runPermutation $ CompileMetadata
    <$> parseRequired "version_hash:"       parseHash
    <*> parseRequired "path:"               parseQuoted
    <*> parseOptional "public_namespace:"   NoNamespace parseNamespace
    <*> parseOptional "private_namespace:"  NoNamespace parseNamespace
    <*> parseRequired "public_deps:"        (parseList parseQuoted)
    <*> parseRequired "private_deps:"       (parseList parseQuoted)
    <*> parseRequired "public_categories:"  (parseList sourceParser)
    <*> parseRequired "private_categories:" (parseList sourceParser)
    <*> parseRequired "public_subdirs:"     (parseList parseQuoted)
    <*> parseRequired "private_subdirs:"    (parseList parseQuoted)
    <*> parseRequired "public_files:"       (parseList parseQuoted)
    <*> parseRequired "private_files:"      (parseList parseQuoted)
    <*> parseRequired "test_files:"         (parseList parseQuoted)
    <*> parseRequired "hxx_files:"          (parseList parseQuoted)
    <*> parseRequired "cxx_files:"          (parseList parseQuoted)
    <*> parseRequired "binaries:"           (parseList parseQuoted)
    <*> parseRequired "link_flags:"         (parseList parseQuoted)
    <*> parseRequired "object_files:"       (parseList readConfig)
  writeConfig (CompileMetadata h p ns1 ns2 is is2 cs1 cs2 ds1 ds2 ps xs ts hxx cxx bs lf os) = do
    validateHash h
    ns1' <- maybeShowNamespace "public_namespace:"  ns1
    ns2' <- maybeShowNamespace "private_namespace:" ns2
    mapCompilerM_ validateCategoryName cs1
    mapCompilerM_ validateCategoryName cs2
    os' <- fmap concat $ mapCompilerM writeConfig os
    return $ [
        "version_hash: " ++ show h,
        "path: " ++ show p
      ] ++ ns1' ++ ns2' ++ [
        "public_deps: ["
      ] ++ indents (map show is) ++ [
        "]",
        "private_deps: ["
      ] ++ indents (map show is2) ++ [
        "]",
        "public_categories: ["
      ] ++ indents (map show cs1) ++ [
        "]",
        "private_categories: ["
      ] ++ indents (map show cs2) ++ [
        "]",
        "public_subdirs: ["
      ] ++ indents (map show ds1) ++ [
        "]",
        "private_subdirs: ["
      ] ++ indents (map show ds2) ++ [
        "]",
        "public_files: ["
      ] ++ indents (map show ps) ++ [
        "]",
        "private_files: ["
      ] ++ indents (map show xs) ++ [
        "]",
        "test_files: ["
      ] ++ indents (map show ts) ++ [
        "]",
        "hxx_files: ["
      ] ++ indents (map show hxx) ++ [
        "]",
        "cxx_files: ["
      ] ++ indents (map show cxx) ++ [
        "]",
        "binaries: ["
      ] ++ indents (map show bs) ++ [
        "]",
        "link_flags: ["
      ] ++ indents (map show lf) ++ [
        "]",
        "object_files: ["
      ] ++ indents os' ++ [
        "]"
      ]

instance ConfigFormat ObjectFile where
  readConfig = category <|> other where
    category = do
      sepAfter (string_ "category_object")
      structOpen
      o <- runPermutation $ CategoryObjectFile
        <$> parseRequired "category:" readConfig
        <*> parseRequired "requires:" (parseList readConfig)
        <*> parseRequired "files:"    (parseList parseQuoted)
      structClose
      return o
    other = do
      sepAfter (string_ "other_object")
      structOpen
      f <- runPermutation $ OtherObjectFile
        <$> parseRequired "file:" parseQuoted
      structClose
      return f
  writeConfig (CategoryObjectFile c rs fs) = do
    category <- writeConfig c
    requires <- fmap concat $ mapCompilerM writeConfig rs
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
      i <- runPermutation $ CategoryIdentifier
        <$> parseRequired "path:"      parseQuoted
        <*> parseRequired "name:"      sourceParser
        <*> parseOptional "namespace:" NoNamespace parseNamespace
      structClose
      return i
    unresolved = do
      sepAfter (string_ "unresolved")
      structOpen
      c <- runPermutation $ UnresolvedCategory
        <$> parseRequired "name:" sourceParser
      structClose
      return c
  writeConfig (CategoryIdentifier p c ns) = do
    validateCategoryName c
    namespace <- maybeShowNamespace "namespace:" ns
    return $ [
        "category {",
        indent $ "name: " ++ show c
      ] ++ indents namespace ++ [
        indent $ "path: " ++ show p,
        "}"
      ]
  writeConfig (UnresolvedCategory c) = do
    validateCategoryName c
    return $ ["unresolved { " ++ "name: " ++ show c ++ " " ++ "}"]

instance ConfigFormat ModuleConfig where
  readConfig = runPermutation $ ModuleConfig
    <$> parseOptional "root:"           "" parseQuoted
    <*> parseRequired "path:"              parseQuoted
    <*> parseOptional "expression_map:" [] (parseList parseExprMacro)
    <*> parseOptional "public_deps:"    [] (parseList parseQuoted)
    <*> parseOptional "private_deps:"   [] (parseList parseQuoted)
    <*> parseOptional "extra_files:"    [] (parseList readConfig)
    <*> parseOptional "include_paths:"  [] (parseList parseQuoted)
    <*> parseRequired "mode:"              readConfig
  writeConfig (ModuleConfig p d em is is2 es ep m) = do
    es' <- fmap concat $ mapCompilerM writeConfig es
    m' <- writeConfig m
    when (not $ null em) $ compilerErrorM "Only empty expression maps are allowed when writing"
    return $ [
        "root: " ++ show p,
        "path: " ++ show d,
        "expression_map: [",
        -- NOTE: expression_map isn't output because that would require making
        -- all Expression serializable.
        "]",
        "public_deps: ["
      ] ++ indents (map show is) ++ [
        "]",
        "private_deps: ["
      ] ++ indents (map show is2) ++ [
        "]",
        "extra_files: ["
      ] ++ indents es' ++ [
        "]",
        "include_paths: ["
      ] ++ indents (map show ep) ++ [
        "]"
      ] ++ "mode: " `prependFirst` m'

instance ConfigFormat ExtraSource where
  readConfig = category <|> other where
    category = do
      sepAfter (string_ "category_source")
      structOpen
      s <- runPermutation $ CategorySource
        <$> parseRequired "source:"        parseQuoted
        <*> parseOptional "categories:" [] (parseList sourceParser)
        <*> parseOptional "requires:"   [] (parseList sourceParser)
      structClose
      return s
    other = do
      f <- parseQuoted
      return (OtherSource f)
  writeConfig (CategorySource f cs ds) = do
    mapCompilerM_ validateCategoryName cs
    mapCompilerM_ validateCategoryName ds
    return $ [
        "category_source {",
        indent ("source: " ++ show f),
        indent "categories: ["
      ] ++ (indents . indents . map show) cs ++ [
        indent "]",
        indent "requires: ["
      ] ++ (indents . indents . map show) ds ++ [
        indent "]",
        "}"
      ]
  writeConfig (OtherSource f) = return [show f]

instance ConfigFormat CompileMode where
  readConfig = labeled "compile mode" $ binary <|> incremental where
    binary = do
      sepAfter (string_ "binary")
      structOpen
      b <- runPermutation $ CompileBinary
        <$> parseRequired "category:"      sourceParser
        <*> parseRequired "function:"      sourceParser
        <*> parseOptional "output:"     "" parseQuoted
        <*> parseOptional "link_flags:" [] (parseList parseQuoted)
      structClose
      return b
    incremental = do
      sepAfter (string_ "incremental")
      structOpen
      lf <- runPermutation $ CompileIncremental
        <$> parseOptional "link_flags:" [] (parseList parseQuoted)
      structClose
      return lf
  writeConfig (CompileBinary c f o lf) = do
    validateCategoryName c
    validateFunctionName f
    return $ [
        "binary {",
        indent ("category: " ++ show c),
        indent ("function: " ++ show f),
        indent ("output: " ++ show o),
        indent ("link_flags: [")
      ] ++ (indents . indents) (map show lf) ++ [
        indent "]",
        "}"
      ]
  writeConfig (CompileIncremental lf) = do
    return $ [
        "incremental {",
        indent ("link_flags: [")
      ] ++ (indents . indents) (map show lf) ++ [
        indent "]",
        "}"
      ]
  writeConfig _ = compilerErrorM "Invalid compile mode"

parseExprMacro :: TextParser (MacroName,Expression SourceContext)
parseExprMacro = do
  sepAfter (string_ "expression_macro")
  structOpen
  e <- runPermutation $ (,)
    <$> parseRequired "name:"       sourceParser
    <*> parseRequired "expression:" sourceParser
  structClose
  return e
