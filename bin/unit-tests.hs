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
import qualified Test.DefinedCategory as DefinedCategoryTest
import qualified Test.IntegrationTest as IntegrationTestTest
import qualified Test.ParseMetadata   as ParseMetadataTest
import qualified Test.Parser          as ParserTest
import qualified Test.Procedure       as ProcedureTest
import qualified Test.TypeCategory    as TypeCategoryTest
import qualified Test.TypeInstance    as TypeInstanceTest


main :: IO ()
main = runAllTests $ concat [
    labelWith "DefinedCategoryTest" DefinedCategoryTest.tests,
    labelWith "TypeInstanceTest"    TypeInstanceTest.tests,
    labelWith "ParseMetadataTest"   ParseMetadataTest.tests,
    labelWith "ParserTest"          ParserTest.tests,
    labelWith "TypeCategoryTest"    TypeCategoryTest.tests,
    labelWith "ProcedureTest"       ProcedureTest.tests,
    labelWith "IntegrationTestTest" IntegrationTestTest.tests
  ]

labelWith :: CompileErrorM m => String -> [IO (m ())] -> [IO (m ())]
labelWith s ts = map (\(n,t) -> fmap (`reviseError` ("In " ++ s ++ " (#" ++ show n ++ "):")) t) (zip ([1..] :: [Int]) ts)
