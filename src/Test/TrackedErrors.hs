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

module Test.TrackedErrors (tests) where

import Base.CompilerError
import Base.TrackedErrors


tests :: [IO (TrackedErrors ())]
tests = [
    checkSuccess 'a' (return 'a'),
    checkError "error\n" (compilerErrorM "error" :: TrackedErrorsIO Char),
    checkError "" (compilerErrorM "" :: TrackedErrorsIO Char),

    checkSuccess ['a','b']          (collectAllM [return 'a',return 'b']),
    checkSuccess []                 (collectAllM [] :: TrackedErrorsIO [Char]),
    checkError   "error1\nerror2\n" (collectAllM [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccess "ab" (collectAnyM [return 'a',return 'b']),
    checkSuccess ""   (collectAnyM [] :: TrackedErrorsIO [Char]),
    checkSuccess "b"  (collectAnyM [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccess 'a' (collectFirstM [return 'a',return 'b']),
    checkError   ""  (collectFirstM [] :: TrackedErrorsIO Char),
    checkSuccess 'b' (collectFirstM [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccessAndWarnings "warning1\nwarning2\n" ()
      (compilerWarningM "warning1" >> return () >> compilerWarningM "warning2"),
    checkErrorAndWarnings "warning1\n" "error\n"
      (compilerWarningM "warning1" >> compilerErrorM "error" >> compilerWarningM "warning2" :: TrackedErrorsIO ()),

    checkSuccess ['a','b']  (sequence [return 'a',return 'b']),
    checkSuccess []         (sequence [] :: TrackedErrorsIO [Char]),
    checkError   "error1\n" (sequence [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccess 'a' (return 'a' `withContextM` "message"),
    checkError "message\n  error\n" (compilerErrorM "error" `withContextM` "message" :: TrackedErrorsIO ()),
    checkSuccessAndWarnings "message\n  warning\n" ()
      (compilerWarningM "warning" `withContextM` "message" :: TrackedErrorsIO ()),
    checkErrorAndWarnings "message\n  warning\n" "message\n  error\n"
      ((compilerWarningM "warning" >> compilerErrorM "error") `withContextM` "message" :: TrackedErrorsIO ()),
    checkSuccessAndWarnings "" () (return () `withContextM` "message"),
    checkErrorAndWarnings "" "message\n" (compilerErrorM "" `withContextM` "message" :: TrackedErrorsIO ()),

    checkSuccess 'a' (return 'a' `summarizeErrorsM` "message"),
    checkError "message\n  error\n" (compilerErrorM "error" `summarizeErrorsM` "message" :: TrackedErrorsIO ()),
    checkSuccessAndWarnings "warning\n" ()
      (compilerWarningM "warning" `summarizeErrorsM` "message" :: TrackedErrorsIO ()),
    checkErrorAndWarnings "warning\n" "message\n  error\n"
      ((compilerWarningM "warning" >> compilerErrorM "error") `summarizeErrorsM` "message" :: TrackedErrorsIO ()),
    checkSuccessAndWarnings "" () (return () `summarizeErrorsM` "message"),
    checkErrorAndWarnings "" "message\n" (compilerErrorM "" `summarizeErrorsM` "message" :: TrackedErrorsIO ()),

    checkSuccessAndWarnings "error\n" ()
      (asCompilerWarnings $ compilerErrorM "error" :: TrackedErrorsIO ()),
    checkErrorAndWarnings "" "warning\n"
      (asCompilerError $ compilerWarningM "warning" :: TrackedErrorsIO ()),

    checkSuccess 'a' (compilerBackgroundM "background" >> return 'a'),
    checkError "error\n  background\n"
      (compilerBackgroundM "background" >> compilerErrorM "error" :: TrackedErrorsIO ()),
    checkError "error\n  background\n"
      (collectAllM [compilerBackgroundM "background"] >> compilerErrorM "error" :: TrackedErrorsIO [()]),
    checkError "error\n  background\n"
      (collectFirstM [compilerBackgroundM "background"] >> compilerErrorM "error" :: TrackedErrorsIO ()),

    checkSuccess 'a' ((resetBackgroundM $ compilerBackgroundM "background") >> return 'a'),
    checkError "error\n"
      ((resetBackgroundM $ compilerBackgroundM "background") >> compilerErrorM "error" :: TrackedErrorsIO ())
  ]

checkSuccess :: (Eq a, Show a) => a -> TrackedErrorsIO a -> IO (TrackedErrors ())
checkSuccess x y = do
  y' <- toTrackedErrors y
  if isCompilerError y' || getCompilerSuccess y' == x
     then return $ y' >> return ()
     else return $ compilerErrorM $ "Expected value " ++ show x ++ " but got value " ++ show (getCompilerSuccess y')

checkError :: (Eq a, Show a) => String -> TrackedErrorsIO a -> IO (TrackedErrors ())
checkError e y = do
  y' <- toTrackedErrors y
  if not (isCompilerError y')
     then return $ compilerErrorM $ "Expected error \"" ++ e ++ "\" but got value " ++ show (getCompilerSuccess y')
     else if show (getCompilerError y') == e
          then return $ return ()
          else return $ compilerErrorM $ "Expected error \"" ++ e ++ "\" but got error \"" ++ show (getCompilerError y') ++ "\""

checkSuccessAndWarnings :: (Eq a, Show a) => String -> a -> TrackedErrorsIO a -> IO (TrackedErrors ())
checkSuccessAndWarnings w x y = do
  y' <- toTrackedErrors y
  outcome <- checkSuccess x y
  if show (getCompilerWarnings y') == w
     then return $ outcome >> return ()
     else return $ compilerErrorM $ "Expected warnings " ++ show w ++ " but got warnings \"" ++ show (getCompilerWarnings y') ++ "\""

checkErrorAndWarnings :: (Eq a, Show a) => String -> String -> TrackedErrorsIO a -> IO (TrackedErrors ())
checkErrorAndWarnings w e y = do
  y' <- toTrackedErrors y
  outcome <- checkError e y
  if show (getCompilerWarnings y') == w
     then return $ outcome >> return ()
     else return $ compilerErrorM $ "Expected warnings " ++ show w ++ " but got warnings \"" ++ show (getCompilerWarnings y') ++ "\""
