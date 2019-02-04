{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseInstance (
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

import ParserBase
import TypeInstance
import TypesBase


instance ParseFromSource GeneralInstance where
  sourceParser = try all <|> try any <|> intersectOrUnion <|> single where
    all = labeled "all" $ do
      kwAll
      return $ TypeMerge MergeUnion []
    any = labeled "any" $ do
      kwAny
      return $ TypeMerge MergeIntersect []
    intersectOrUnion = try intersect <|> union
    intersect = labeled "intersection" $ do
      ts <- between (sepAfter $ string "[")
                    (sepAfter $ string "]")
                    (sepBy1 (labeled "type" $ sourceParser) (sepAfter $ string "&"))
      return $ TypeMerge MergeIntersect ts
    union = labeled "union" $ do
      ts <- between (sepAfter $ string "[")
                    (sepAfter $ string "]")
                    (sepBy1 (labeled "type" $ sourceParser) (sepAfter $ string "|"))
      return $ TypeMerge MergeUnion ts
    single = do
      t <- sourceParser
      return $ SingleType t

instance ParseFromSource ValueType where
  sourceParser = do
    r <- getWeak <|> getOptional <|> getRequired
    t <- sourceParser
    return $ ValueType r t
    where
      getWeak = labeled "weak" $ do
        try kwWeak
        return WeakValue
      getOptional = labeled "optional" $ do
        try kwOptional
        return OptionalValue
      getRequired = return RequiredValue

instance ParseFromSource CategoryName where
  sourceParser = labeled "type name" $ do
    noKeywords
    b <- upper
    e <- sepAfter $ many alphaNum
    return $ box (b:e)
    where
      box n
        | n == "Bool"      = BuiltinBool
        | n == "String"    = BuiltinString
        | n == "Int"       = BuiltinInt
        | n == "Float"     = BuiltinFloat
        | n == "LessThan"  = BuiltinLessThan
        | n == "Equals"    = BuiltinEquals
        | n == "Formatted" = BuiltinFormatted
        | otherwise = CategoryName n

instance ParseFromSource ParamName where
  sourceParser = labeled "param name" $ do
    noKeywords
    char '#'
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ ParamName ('#':b:e)

instance ParseFromSource TypeInstance where
  sourceParser = labeled "type" $ do
    n <- sourceParser
    as <- labeled "type args" $ try args <|> return []
    return $ TypeInstance n (ParamSet as)
    where
      args = between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy sourceParser (sepAfter $ string ","))

instance ParseFromSource DefinesInstance where
  sourceParser = labeled "type" $ do
    n <- sourceParser
    as <- labeled "type args" $ try args <|> return []
    return $ DefinesInstance n (ParamSet as)
    where
      args = between (sepAfter $ string "<")
                     (sepAfter $ string ">")
                     (sepBy sourceParser (sepAfter $ string ","))

instance ParseFromSource TypeInstanceOrParam where
  sourceParser = try param <|> inst where
    param = labeled "param" $ do
      n <- sourceParser
      return $ JustParamName n
    inst = labeled "type" $ do
      t <- sourceParser
      return $ JustTypeInstance t

instance ParseFromSource TypeFilter where
  sourceParser = requires <|> allows <|> defines where
    requires = labeled "requires filter" $ do
      try kwRequires
      t <- sourceParser
      return $ TypeFilter FilterRequires t
    allows = labeled "allows filter" $ do
      try kwAllows
      t <- sourceParser
      return $ TypeFilter FilterAllows t
    defines = labeled "defines filter" $ do
      try kwDefines
      t <- sourceParser
      return $ DefinesFilter t
