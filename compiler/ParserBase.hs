{-# LANGUAGE Safe #-}

module ParserBase (
  ParseFromSource(..),
) where

import Text.Parsec.String


class ParseFromSource a where
  sourceParser :: Parser a
