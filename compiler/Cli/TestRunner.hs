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

module Cli.TestRunner (
  runSingleTest,
) where

import Control.Monad (when)
import Data.List (isSuffixOf)
import System.IO
import System.Posix.Temp (mkdtemp,mkstemps)
import System.FilePath
import Text.Parsec
import Text.Regex.TDFA -- Not safe!

import Builtin
import CompileInfo
import IntegrationTest
import SourceFile
import TypeCategory
import TypesBase
import CompilerCxx.Category
import CompilerCxx.Naming
import Cli.CxxCommand


runSingleTest :: [String] -> [String] -> CategoryMap SourcePos ->
                 (String,String) -> IO (CompileInfo ())
runSingleTest paths os tm (f,s) = do
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
      let result = compileAll Nothing cs ds :: CompileInfo ([String],[CxxOutput])
      if not $ isCompileError result
         then return $ compileError "Expected compiler errors"
         else return $ do
           let warnings = concat $ map (++ "\n") $ getCompileWarnings result
           let errors = show $ getCompileError result
           checkRequired rs $ warnings ++ errors
           checkExcluded es $ warnings ++ errors

    run n (ExpectRuntimeError _ e rs es) cs ds = do
      let result = compileAll (Just e) cs ds :: CompileInfo ([String],[CxxOutput])
      if isCompileError result
         then return $ compileError "Expected compiler success"
         else do
           let warnings = concat $ map (++ "\n") $ getCompileWarnings result
           let (main,fs) = getCompileSuccess result
           binaryName <- createBinary main fs
           return $ return () -- TODO: Execute the binary and check the patterns.

    run n (ExpectRuntimeSuccess _ e rs es) cs ds = do
      let result = compileAll (Just e) cs ds :: CompileInfo ([String],[CxxOutput])
      if isCompileError result
         then return $ compileError "Expected compiler success"
         else do
           let warnings = concat $ map (++ "\n") $ getCompileWarnings result
           let (main,fs) = getCompileSuccess result
           binaryName <- createBinary main fs
           return $ return () -- TODO: Execute the binary and check the patterns.

    compileAll e cs ds = do
      let namespace = privateNamepace s
      let cs' = map (setCategoryNamespace namespace) cs
      tm' <- includeNewTypes tm cs'
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs'
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition tm' [namespace]) ds
      main <- case e of
                   Just e -> createTestFile tm' e namespace
                   Nothing -> return []
      return (main,hxx ++ cxx)
    checkRequired rs ms = mergeAllM $ map (checkForRegex True  $ lines ms) rs
    checkExcluded es ms = mergeAllM $ map (checkForRegex False $ lines ms) es
    checkForRegex :: Bool -> [String] -> String -> CompileInfo ()
    checkForRegex expected ms r = do
      let found = any id $ map (=~ r) ms
      when (found && not expected) $ compileError $ "Pattern \"" ++ r ++ "\" present in output"
      when (not found && expected) $ compileError $ "Pattern \"" ++ r ++ "\" missing from output"
    createBinary main fs = do
      dir <- mkdtemp "/tmp/ztest_"
      hPutStrLn stderr $ "Writing temporary files to " ++ dir
      sources <- fmap concat $ sequence $ map (writeSingleFile dir) fs
      (o',h) <- mkstemps (dir </> "zmain_") ".cpp"
      let binary = dir </> "testcase"
      hPutStr h $ concat $ map (++ "\n") main
      hClose h
      let command = CompileToBinary ([o'] ++ sources ++ os) binary (dir:paths)
      runCxxCommand command
      return binary
    writeSingleFile d (CxxOutput f _ os) = do
      writeFile (d </> f) $ concat $ map (++ "\n") os
      if isSuffixOf ".cpp" f
         then return [d </> f]
         else return []
