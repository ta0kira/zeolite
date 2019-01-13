{-# LANGUAGE Safe #-}

module AllTests where

import CompileInfo
import TestBase
import TypesBase
import qualified TypeInstanceTest as TypeInstanceTest
import qualified TypeCategoryTest as TypeCategoryTest
import qualified ProcedureTest as ProcedureTest

main = runAllTests $ concat [
    labelWith "TypeInstanceTest" TypeInstanceTest.tests,
    labelWith "TypeCategoryTest" TypeCategoryTest.tests,
    labelWith "ProcedureTest"    ProcedureTest.tests
  ]

labelWith s ts = map (\(n,t) -> fmap (`reviseError` ("In " ++ s ++ " (#" ++ show n ++ "):")) t) (zip [1..] ts)
