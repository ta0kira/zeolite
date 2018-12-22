{-# LANGUAGE Safe #-}

module TestBase (
  checkDefinesFail,
  checkDefinesSuccess,
  checkTypeFail,
  checkTypeSuccess,
  forceParse,
  parseFilterMap,
  parseTheTest,
  readSingle,
  readMulti,
  runAllTests,
  showParams,
) where

import Data.Either
import Data.List
import System.IO
import Text.Parsec
import qualified Data.Map as Map

import CompileInfo
import TypeInstance
import ParseInstance
import ParserBase
import TypesBase


runAllTests :: [IO (CompileInfo ())] -> IO ()
runAllTests ts = do
  results <- sequence ts
  let (es,ps) = partitionEithers $ zipWith numberError [1..] results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e)) es
  hPutStr stderr $ show (length ps) ++ " tests passed + " ++
                   show (length es) ++ " tests failed\n"

numberError :: a -> Either b c -> Either (a,b) c
numberError n (Left e)  = Left (n,e)
numberError _ (Right x) = Right x -- Not the same Either!

forceParse :: ParseFromSource a => String -> a
forceParse s = force $ parse sourceParser "(string)" s where
  force (Right x) = x

readSingle :: (CompileErrorM m, Monad m) =>
  ParseFromSource a => String -> String -> m a
readSingle f s =
  unwrap $ parse (between optionalSpace endOfDoc sourceParser) f s
  where
    unwrap (Left e)  = compileError (show e)
    unwrap (Right t) = return t

readMulti :: (CompileErrorM m, Monad m) =>
  ParseFromSource a => String -> String -> m [a]
readMulti f s =
  unwrap $ parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  where
    unwrap (Left e)  = compileError (show e)
    unwrap (Right t) = return t

parseFilterMap :: (CompileErrorM m, Monad m) =>
  [(String,[String])] -> m ParamFilters
parseFilterMap pa = do
  pa2 <- collectAllOrErrorM $ map parseFilters pa
  return $ Map.fromList pa2
  where
    parseFilters (n,fs) = do
      fs2 <- collectAllOrErrorM $ map (readSingle "(string)") fs
      return (ParamName n,fs2)

parseTheTest :: (ParseFromSource a, CompileErrorM m, Monad m) =>
  [(String,[String])] -> [String] -> m ([a],ParamFilters)
parseTheTest pa xs = do
  ts <- collectAllOrErrorM $ map (readSingle "(string)") xs
  pa2 <- parseFilterMap pa
  return (ts,pa2)

showParams :: [(String,[String])] -> String
showParams pa = "[" ++ intercalate "," (concat $ map expand pa) ++ "]" where
  expand (n,ps) = map (\p -> n ++ " " ++ p) ps

checkTypeSuccess :: TypeResolver CompileInfo () -> [(String,[String])] ->
                     String -> CompileInfo ()
checkTypeSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check = flip reviseError (prefix ++ ":")

checkTypeFail :: TypeResolver CompileInfo () -> [(String,[String])] ->
                  String -> CompileInfo ()
checkTypeFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateGeneralInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check (Right _) = compileError $ prefix ++ ": Expected failure\n"
    check _ = return ()

checkDefinesSuccess :: TypeResolver CompileInfo () -> [(String,[String])] ->
                     String -> CompileInfo ()
checkDefinesSuccess r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check = flip reviseError (prefix ++ ":")

checkDefinesFail :: TypeResolver CompileInfo () -> [(String,[String])] ->
                  String -> CompileInfo ()
checkDefinesFail r pa x = do
  ([t],pa2) <- parseTheTest pa [x]
  check $ validateDefinesInstance r pa2 t
  where
    prefix = x ++ " " ++ showParams pa
    check (Right _) = compileError $ prefix ++ ": Expected failure\n"
    check _ = return ()
