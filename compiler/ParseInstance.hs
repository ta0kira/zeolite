{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module ParseInstance () where

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

import ParserBase
import TypeInstance
import TypesBase


instance ParseFromSource GeneralInstance where
  sourceParser = try all <|> try any <|> try intersect <|> try union <|> single where
    all = labeled "all" $ do
      keyword "all"
      return $ TypeMerge MergeUnion []
    any = labeled "any" $ do
      keyword "any"
      return $ TypeMerge MergeIntersect []
    single = do
      t <- sourceParser
      return $ SingleType t
    intersect = labeled "intersection" $ do
      ts <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string "&"))
      return $ TypeMerge MergeIntersect ts
    union = labeled "union" $ do
      ts <- between (sepAfter $ string "(")
                    (sepAfter $ string ")")
                    (sepBy sourceParser (sepAfter $ string "|"))
      return $ TypeMerge MergeUnion ts

instance ParseFromSource ValueType where
  sourceParser = value where
    value = do
      r <- try getWeak <|> try getOptional <|> getRequired
      t <- sourceParser
      return $ ValueType r t
    getWeak = labeled "weak" $ do
      keyword "weak"
      return WeakValue
    getOptional = labeled "optional" $ do
      keyword "optional"
      return OptionalValue
    getRequired = return RequiredValue

instance ParseFromSource TypeName where
  sourceParser = labeled "type name" $ do
    noKeywords
    b <- upper
    e <- sepAfter $ many alphaNum
    return $ TypeName (b:e)

instance ParseFromSource ParamName where
  sourceParser = labeled "param name" $ do
    noKeywords
    b <- lower
    e <- sepAfter $ many alphaNum
    return $ ParamName (b:e)

instance ParseFromSource TypeInstance where
  sourceParser = parsed where
    args = between (sepAfter $ string "<")
                   (sepAfter $ string ">")
                   (sepBy sourceParser (sepAfter $ string ","))
    parsed = do
      n <- sourceParser
      as <- labeled "type args" $ try args <|> return []
      return $ TypeInstance n (ParamSet as)

instance ParseFromSource TypeInstanceOrParam where
  sourceParser = try param <|> inst where
    param = labeled "param" $ do
      n <- sourceParser
      return $ JustParamName n
    inst = labeled "type" $ do
      t <- sourceParser
      return $ JustTypeInstance t

instance ParseFromSource TypeFilter where
  sourceParser = try requires <|> allows where
    requires = labeled "requires filter" $ do
      keyword "requires"
      t <- sourceParser
      return $ TypeFilter Covariant t
    allows = labeled "allows filter" $ do
      keyword "allows"
      t <- sourceParser
      return $ TypeFilter Contravariant t
