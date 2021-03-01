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

module Test.Pragma (tests) where

import Base.TrackedErrors
import Parser.Common
import Parser.Pragma
import Parser.TextParser
import Test.Common


tests :: [IO (TrackedErrors ())]
tests = [
    checkParseMatch "$Comment[ \"this is a pragma with args\" ]$" (fmap (:[]) pragmaComment)
      (\e -> case e of
                  [PragmaComment _ "this is a pragma with args"] -> True
                  _ -> False),

    checkParseError "$Comment$" "requires arguments" pragmaComment
  ]

data PragmaComment c =
  PragmaComment {
    pcContext :: [c],
    pcComment :: String
  }
  deriving (Show)

pragmaComment :: TextParser (PragmaComment SourceContext)
pragmaComment = autoPragma "Comment" $ Right parseAt where
  parseAt c = do
    string_ "\""
    ss <- manyTill stringChar (string_ "\"")
    optionalSpace
    return $ PragmaComment [c] ss
