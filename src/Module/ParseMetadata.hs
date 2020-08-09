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

module Module.ParseMetadata (
  ConfigFormat,
  autoReadConfig,
  autoWriteConfig,
) where

import Control.Monad (when)
import Text.Parsec
import Text.Parsec.String

import Base.CompileError
import Cli.CompileOptions
import Cli.Programs (VersionHash(..))
import Module.CompileMetadata
import Parser.Common
import Parser.Procedure ()
import Parser.Pragma (parseMacroName)
import Parser.TypeCategory ()
import Parser.TypeInstance ()
import Text.Regex.TDFA -- Not safe!
import Types.Procedure (Expression)
import Types.TypeCategory (FunctionName(..),Namespace(..))
import Types.TypeInstance (CategoryName(..))


class ConfigFormat a where
  readConfig :: Parser a
  writeConfig :: CompileErrorM m => a -> m [String]

autoReadConfig :: (ConfigFormat a, CompileErrorM m) => String -> String -> m a
autoReadConfig f s  = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc readConfig) f s
  unwrap (Left e)  = compileErrorM (show e)
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

validateCategoryName :: CompileErrorM m => CategoryName -> m ()
validateCategoryName c =
    when (not $ show c =~ "^[A-Z][A-Za-z0-9]*$") $
      compileErrorM $ "Invalid category name: \"" ++ show c ++ "\""

parseCategoryName :: Parser CategoryName
parseCategoryName = sourceParser :: Parser CategoryName

validateFunctionName :: CompileErrorM m => FunctionName -> m ()
validateFunctionName f =
    when (not $ show f =~ "^[a-z][A-Za-z0-9]*$") $
      compileErrorM $ "Invalid function name: \"" ++ show f ++ "\""

parseFunctionName :: Parser FunctionName
parseFunctionName = sourceParser :: Parser FunctionName

validateHash :: CompileErrorM m => VersionHash -> m ()
validateHash h =
    when (not $ show h =~ "^[A-Za-z0-9]+$") $
      compileErrorM $ "Version hash must be a hex string: \"" ++ show h ++ "\""

parseHash :: Parser VersionHash
parseHash = labeled "version hash" $ sepAfter (fmap VersionHash $ many1 hexDigit)

maybeShowNamespace :: CompileErrorM m => String -> Namespace -> m [String]
maybeShowNamespace l (StaticNamespace ns) = do
  when (not $ ns =~ "^[A-Za-z][A-Za-z0-9_]*$") $
    compileErrorM $ "Invalid category namespace: \"" ++ ns ++ "\""
  return [l ++ " " ++ ns]
maybeShowNamespace _ _ = return []

parseNamespace :: Parser Namespace
parseNamespace = labeled "namespace" $ do
  b <- lower
  e <- sepAfter $ many (alphaNum <|> char '_')
  return $ StaticNamespace (b:e)

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

parseOptional :: String -> a -> Parser a -> Parser a
parseOptional l def p = parseRequired l p <|> return def

parseRequired :: String -> Parser a -> Parser a
parseRequired l p = do
    try $ sepAfter (string_ l)
    p

instance ConfigFormat CompileMetadata where
  readConfig = do
    h   <- parseRequired "version_hash:"       parseHash
    p   <- parseRequired "path:"               parseQuoted
    ns1 <- parseOptional "public_namespace:"   NoNamespace parseNamespace
    ns2 <- parseOptional "private_namespace:"  NoNamespace parseNamespace
    is  <- parseRequired "public_deps:"        (parseList parseQuoted)
    is2 <- parseRequired "private_deps:"       (parseList parseQuoted)
    cs1 <- parseRequired "public_categories:"  (parseList parseCategoryName)
    cs2 <- parseRequired "private_categories:" (parseList parseCategoryName)
    ds1 <- parseRequired "public_subdirs:"     (parseList parseQuoted)
    ds2 <- parseRequired "private_subdirs:"    (parseList parseQuoted)
    ps  <- parseRequired "public_files:"       (parseList parseQuoted)
    xs  <- parseRequired "private_files:"      (parseList parseQuoted)
    ts  <- parseRequired "test_files:"         (parseList parseQuoted)
    hxx <- parseRequired "hxx_files:"          (parseList parseQuoted)
    cxx <- parseRequired "cxx_files:"          (parseList parseQuoted)
    bs  <- parseRequired "binaries:"           (parseList parseQuoted)
    lf  <- parseRequired "link_flags:"         (parseList parseQuoted)
    os  <- parseRequired "object_files:"       (parseList readConfig)
    return (CompileMetadata h p ns1 ns2 is is2 cs1 cs2 ds1 ds2 ps xs ts hxx cxx bs lf os)
  writeConfig m = do
    validateHash (cmVersionHash m)
    ns1 <- maybeShowNamespace "public_namespace:"  (cmPublicNamespace m)
    ns2 <- maybeShowNamespace "private_namespace:" (cmPrivateNamespace m)
    _ <- mapErrorsM validateCategoryName (cmPublicCategories m)
    _ <- mapErrorsM validateCategoryName (cmPrivateCategories m)
    objects <- fmap concat $ mapErrorsM writeConfig $ cmObjectFiles m
    return $ [
        "version_hash: " ++ (show $ cmVersionHash m),
        "path: " ++ (show $ cmPath m)
      ] ++ ns1 ++ ns2 ++ [
        "public_deps: ["
      ] ++ indents (map show $ cmPublicDeps m) ++ [
        "]",
        "private_deps: ["
      ] ++ indents (map show $ cmPrivateDeps m) ++ [
        "]",
        "public_categories: ["
      ] ++ indents (map show $ cmPublicCategories m) ++ [
        "]",
        "private_categories: ["
      ] ++ indents (map show $ cmPrivateCategories m) ++ [
        "]",
        "public_subdirs: ["
      ] ++ indents (map show $ cmPublicSubdirs m) ++ [
        "]",
        "private_subdirs: ["
      ] ++ indents (map show $ cmPrivateSubdirs m) ++ [
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
        "binaries: ["
      ] ++ indents (map show $ cmBinaries m) ++ [
        "]",
        "link_flags: ["
      ] ++ indents (map show $ cmLinkFlags m) ++ [
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
      c <-  parseRequired "category:" readConfig
      rs <- parseRequired "requires:" (parseList readConfig)
      fs <- parseRequired "files:"    (parseList parseQuoted)
      structClose
      return (CategoryObjectFile c rs fs)
    other = do
      sepAfter (string_ "other_object")
      structOpen
      f <- parseRequired "file:" parseQuoted
      structClose
      return (OtherObjectFile f)
  writeConfig (CategoryObjectFile c rs fs) = do
    category <- writeConfig c
    requires <- fmap concat $ mapErrorsM writeConfig rs
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
      c <-  parseRequired "name:"      parseCategoryName
      ns <- parseOptional "namespace:" NoNamespace parseNamespace
      p <-  parseRequired "path:"      parseQuoted
      structClose
      return (CategoryIdentifier p c ns)
    unresolved = do
      sepAfter (string_ "unresolved")
      structOpen
      c <- parseRequired "name:" parseCategoryName
      structClose
      return (UnresolvedCategory c)
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
  readConfig = do
      p   <- parseOptional "root:"           "" parseQuoted
      d   <- parseRequired "path:"              parseQuoted
      em  <- parseOptional "expression_map:" [] (parseList parseExprMacro)
      is  <- parseOptional "public_deps:"    [] (parseList parseQuoted)
      is2 <- parseOptional "private_deps:"   [] (parseList parseQuoted)
      es  <- parseOptional "extra_files:"    [] (parseList readConfig)
      ep  <- parseOptional "include_paths:"  [] (parseList parseQuoted)
      m   <- parseRequired "mode:"              readConfig
      return (ModuleConfig p d em is is2 es ep m)
  writeConfig m = do
    extra    <- fmap concat $ mapErrorsM writeConfig $ rmExtraFiles m
    mode <- writeConfig (rmMode m)
    when (not $ null $ rmExprMap m) $ compileErrorM "Only empty expression maps are allowed when writing"
    return $ [
        "root: " ++ show (rmRoot m),
        "path: " ++ show (rmPath m),
        "expression_map: [",
        -- NOTE: expression_map isn't output because that would require making
        -- all Expression serializable.
        "]",
        "public_deps: ["
      ] ++ indents (map show $ rmPublicDeps m) ++ [
        "]",
        "private_deps: ["
      ] ++ indents (map show $ rmPrivateDeps m) ++ [
        "]",
        "extra_files: ["
      ] ++ indents extra ++ [
        "]",
        "include_paths: ["
      ] ++ indents (map show $ rmExtraPaths m) ++ [
        "]"
      ] ++ "mode: " `prependFirst` mode

instance ConfigFormat ExtraSource where
  readConfig = category <|> other where
    category = do
      sepAfter (string_ "category_source")
      structOpen
      f <-  parseRequired "source:"        parseQuoted
      cs <- parseOptional "categories:" [] (parseList parseCategoryName)
      ds <- parseOptional "requires:"   [] (parseList parseCategoryName)
      structClose
      return (CategorySource f cs ds)
    other = do
      f <- parseQuoted
      return (OtherSource f)
  writeConfig (CategorySource f cs ds) = do
    _ <- mapErrorsM validateCategoryName cs
    _ <- mapErrorsM validateCategoryName ds
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
      c <-  parseRequired "category:"      parseCategoryName
      f <-  parseRequired "function:"      parseFunctionName
      o <-  parseOptional "output:"     "" parseQuoted
      lf <- parseOptional "link_flags:" [] (parseList parseQuoted)
      structClose
      return (CompileBinary c f o lf)
    incremental = do
      sepAfter (string_ "incremental")
      structOpen
      lf <- parseOptional "link_flags:" [] (parseList parseQuoted)
      structClose
      return (CompileIncremental lf)
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
  writeConfig CompileUnspecified = writeConfig (CompileIncremental [])
  writeConfig _ = compileErrorM "Invalid compile mode"

parseExprMacro :: Parser (String,Expression SourcePos)
parseExprMacro = do
  sepAfter (string_ "expression_macro")
  structOpen
  n <- parseRequired "name:"       parseMacroName
  e <- parseRequired "expression:" sourceParser
  structClose
  return (n,e)
