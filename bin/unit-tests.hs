{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

import Base.CompileError
import Test.Common
import qualified Test.DefinedCategory as TestDefinedCategory
import qualified Test.IntegrationTest as TestIntegrationTest
import qualified Test.ParseMetadata   as TestParseMetadata
import qualified Test.Parser          as TestParser
import qualified Test.Pragma          as TestPragma
import qualified Test.Procedure       as TestProcedure
import qualified Test.SourceFile      as TestSourceFile
import qualified Test.TypeCategory    as TestTypeCategory
import qualified Test.TypeInstance    as TestTypeInstance


main :: IO ()
main = runAllTests $ concat [
    labelWith "DefinedCategory" TestDefinedCategory.tests,
    labelWith "IntegrationTest" TestIntegrationTest.tests,
    labelWith "ParseMetadata"   TestParseMetadata.tests,
    labelWith "Parser"          TestParser.tests,
    labelWith "Pragma"          TestPragma.tests,
    labelWith "Procedure"       TestProcedure.tests,
    labelWith "SourceFile"      TestSourceFile.tests,
    labelWith "TypeCategory"    TestTypeCategory.tests,
    labelWith "TypeInstance"    TestTypeInstance.tests
  ]

labelWith :: CompileErrorM m => String -> [IO (m ())] -> [IO (m ())]
labelWith s ts = map (\(n,t) -> fmap (`reviseError` ("In " ++ s ++ " (#" ++ show n ++ "):")) t) (zip ([1..] :: [Int]) ts)
