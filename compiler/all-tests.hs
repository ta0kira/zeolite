{- -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

{-# LANGUAGE Safe #-}

import CompileInfo
import TestBase
import TypesBase
import qualified DefinitionTest as DefinitionTest
import qualified TypeInstanceTest as TypeInstanceTest
import qualified TypeCategoryTest as TypeCategoryTest
import qualified ProcedureTest as ProcedureTest

main = runAllTests $ concat [
    labelWith "DefinitionTest" DefinitionTest.tests,
    labelWith "TypeInstanceTest" TypeInstanceTest.tests,
    labelWith "TypeCategoryTest" TypeCategoryTest.tests,
    labelWith "ProcedureTest"    ProcedureTest.tests
  ]

labelWith s ts = map (\(n,t) -> fmap (`reviseError` ("In " ++ s ++ " (#" ++ show n ++ "):")) t) (zip [1..] ts)
