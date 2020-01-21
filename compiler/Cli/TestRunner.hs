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
import Data.List (isSuffixOf,nub)
import System.Directory (setCurrentDirectory)
import System.IO
import System.Posix.Temp (mkdtemp)
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
import Cli.CompileMetadata
import Cli.CompilerCommand


runSingleTest :: [String] -> [String] -> CategoryMap SourcePos ->
                 (String,String) -> IO (CompileInfo ())
runSingleTest paths os tm (f,s) = do
  hPutStrLn stderr $ "\nExecuting tests from " ++ f
  fmap (flip reviseError $ "In test file " ++ f) $ checkAndRun (parseTestSource (f,s)) where
    checkAndRun ts
      | isCompileError ts = return (ts >> return ())
      | otherwise = fmap mergeAllM $ sequence $ map runSingle $ getCompileSuccess ts
    runSingle t = do
      let name = ithTestName $ itHeader t
      let context = "[" ++ formatFullContext (ithContext $ itHeader t) ++ "]"
      hPutStrLn stderr $ "\n*** Executing test \"" ++ name ++ "\" ***"
      outcome <- fmap (flip reviseError ("In test \"" ++ name ++ "\" " ++ context)) $
                   run name (ithResult $ itHeader t) (itCategory t) (itDefinition t)
      if isCompileError outcome
         then hPutStrLn stderr $ "*** Test \"" ++ name ++ "\" failed ***"
         else hPutStrLn stderr $ "*** Test \"" ++ name ++ "\" passed ***"
      return outcome

    run n (ExpectCompileError _ rs es) cs ds = do
      let result = compileAll Nothing cs ds :: CompileInfo ([String],[CxxOutput])
      if not $ isCompileError result
         then return $ compileError "Expected compiler error"
         else return $ do
           let warnings = getCompileWarnings result
           let errors = show $ getCompileError result
           checkContent rs es (warnings ++ lines errors)

    run n (ExpectRuntimeError   _ e rs es) cs ds = execute False n e rs es cs ds
    run n (ExpectRuntimeSuccess _ e rs es) cs ds = execute True  n e rs es cs ds

    checkContent rs es content = do
      let cr = checkRequired rs content
      let ce = checkExcluded es content
      let contentError = (mergeAllM $ map compileError content) `reviseError`
                           "Output from test run:"
      if isCompileError cr || isCompileError ce
         then mergeAllM [cr,ce,contentError]
         else mergeAllM [cr,ce]

    execute s n e rs es cs ds = do
      let result = compileAll (Just e) cs ds :: CompileInfo ([String],[CxxOutput])
      if isCompileError result
         then return $ result >> return ()
         else do
           let warnings = getCompileWarnings result
           let (main,fs) = getCompileSuccess result
           binaryName <- createBinary main fs
           let command = TestCommand binaryName (takeDirectory binaryName)
           (TestCommandResult s' ms1 ms2) <- runTestCommand command
           case (s,s') of
                (True,False) -> return $ mergeAllM $ map compileError $ warnings ++ ms1 ++ ms2
                (False,True) -> return $ compileError "Expected runtime failure"
                _ -> return $ checkContent rs es (warnings ++ ms1 ++ ms2)

    -- TODO: Combine this with the logic in runCompiler.
    compileAll e cs ds = do
      let namespace = privateNamepace s
      let cs' = map (setCategoryNamespace namespace) cs
      tm' <- includeNewTypes tm cs'
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs'
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition tm' [namespace]) ds
      let interfaces = filter (not . isValueConcrete) cs'
      cxx2 <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      main <- case e of
                   Just e -> createTestFile tm' e namespace
                   Nothing -> return []
      return (main,hxx ++ cxx ++ cxx2)
    checkRequired rs ms = mergeAllM $ map (checkForRegex True  ms) rs
    checkExcluded es ms = mergeAllM $ map (checkForRegex False ms) es
    checkForRegex :: Bool -> [String] -> String -> CompileInfo ()
    checkForRegex expected ms r = do
      let found = any id $ map (=~ r) ms
      when (found && not expected) $ compileError $ "Pattern \"" ++ r ++ "\" present in output"
      when (not found && expected) $ compileError $ "Pattern \"" ++ r ++ "\" missing from output"
    createBinary c fs = do
      dir <- mkdtemp "/tmp/ztest_"
      hPutStrLn stderr $ "Writing temporary files to " ++ dir
      sources <- fmap concat $ sequence $ map (writeSingleFile dir) fs
      let main   = dir </> "testcase.cpp"
      let binary = dir </> "testcase"
      writeFile main $ concat $ map (++ "\n") c
      let paths' = nub $ map fixPath (dir:paths)
      let command = CompileToBinary ([main] ++ sources ++ os) binary paths'
      runCxxCommand command
      return binary
    writeSingleFile d (CxxOutput f _ c) = do
      writeFile (d </> f) $ concat $ map (++ "\n") c
      if isSuffixOf ".cpp" f
         then return [d </> f]
         else return []
