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

{-# LANGUAGE Safe #-}

module Cli.TestRunner (
  runSingleTest,
) where

import System.IO
import Text.Parsec

import Builtin
import CompileInfo
import IntegrationTest
import SourceFile
import TypeCategory
import TypesBase
import CompilerCxx.Category
import CompilerCxx.Naming
import Cli.CxxCommand


runSingleTest :: [String] -> CategoryMap SourcePos -> (String,String) -> IO (CompileInfo ())
runSingleTest paths tm (f,s) = do
  hPutStrLn stderr $ "Executing tests from " ++ f
  fmap (flip reviseError $ "In test file " ++ f) $ checkAndRun (parseTestSource (f,s)) where
    checkAndRun ts
      | isCompileError ts = return (ts >> return ())
      | otherwise = fmap mergeAllM $ sequence $ map runSingle $ getCompileSuccess ts
    runSingle t = do
      let name = ithTestName $ itHeader t
      let context = "[" ++ formatFullContext (ithContext $ itHeader t) ++ "]"
      hPutStrLn stderr $ "Executing test \"" ++ name ++ "\""
      fmap (flip reviseError ("In test \"" ++ name ++ "\" " ++ context)) $
        run name (ithResult $ itHeader t) (itCategory t) (itDefinition t)

    run n (ExpectCompileError _ rs es) cs ds = do
      let result = compileAll cs ds :: CompileInfo [CxxOutput]
      if not $ isCompileError result
         then return $ compileError "Expected compiler errors"
         else return $ do
           let warnings = concat $ map (++ "\n") $ getCompileWarnings result
           let errors = show $ getCompileError result
           checkRequired rs $ warnings ++ errors
           checkExcluded es $ warnings ++ errors

    run n (ExpectRuntimeError _ e rs es) cs ds = do
      let result = compileAll cs ds :: CompileInfo [CxxOutput]
      if isCompileError result
         then return $ compileError "Expected compiler success"
         else do
           let warnings = concat $ map (++ "\n") $ getCompileWarnings result
           binaryName <- createBinary e (getCompileSuccess result)
           return $ return () -- TODO: Execute the binary and check the patterns.

    run n (ExpectRuntimeSuccess _ e rs es) cs ds = do
      let result = compileAll cs ds :: CompileInfo [CxxOutput]
      if isCompileError result
         then return $ compileError "Expected compiler success"
         else do
           let warnings = concat $ map (++ "\n") $ getCompileWarnings result
           binaryName <- createBinary e (getCompileSuccess result)
           return $ return () -- TODO: Execute the binary and check the patterns.

    compileAll cs ds = do
      let namespace = privateNamepace s
      let cs' = map (setCategoryNamespace namespace) cs
      tm' <- includeNewTypes tm cs'
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs'
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition tm' [namespace]) ds
      return $ hxx ++ cxx
    checkRequired rs m = return () -- TODO: Check patterns.
    checkExcluded es m = return () -- TODO: Check patterns.
    createBinary e fs = do
      return "" -- TODO: Write the files and compile the binary.
