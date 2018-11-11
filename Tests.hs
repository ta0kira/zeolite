import Control.Monad
import Data.Either
import System.IO
import Text.ParserCombinators.ReadP
import qualified Control.Monad.Trans.Class as Trans

import CompileInfo
import Resolver
import TypesBase
import Unresolved

testCases = [
    -- Loading Type Specs.
    ("testfiles/simple.txt",                   expectLoaded),
    ("testfiles/crazy_formatting.txt",         expectLoaded),
    ("testfiles/valid_missing.txt",            expectLoaded),
    ("testfiles/valid_functions.txt",          expectLoaded),
    ("testfiles/type_to_instance.txt",         expectLoaded),
    ("testfiles/duplicate_typeclass.txt",      expectNotLoaded),
    ("testfiles/duplicate_param.txt",          expectNotLoaded),
    ("testfiles/missing_after_nonmissing.txt", expectNotLoaded),
    ("testfiles/requires_nonmissing.txt",      expectNotLoaded),
    ("testfiles/requires_nonmissing2.txt",     expectNotLoaded),
    ("testfiles/requires_missing.txt",         expectNotLoaded),
    ("testfiles/requires_missing2.txt",        expectNotLoaded),
    -- Inheritance.
    ("testfiles/valid_inherits.txt",       expectLoaded),
    ("testfiles/covariance_error.txt",     expectNotLoaded),
    ("testfiles/contravariance_error.txt", expectNotLoaded),
    -- Filters.
    ("testfiles/valid_filters.txt",    expectLoaded),
    ("testfiles/missing_filter.txt",   expectNotLoaded),
    ("testfiles/incorrect_filter.txt", expectNotLoaded),
    -- Simple Instantiation.
    ("testfiles/simple.txt", expectParsed    "Read<a,b>"),
    ("testfiles/simple.txt", expectParsed    "Write<a,b>"),
    ("testfiles/simple.txt", expectParsed    "Something<a,b,c,d>"),
    ("testfiles/simple.txt", expectNotParsed "Read<b>"),
    ("testfiles/simple.txt", expectNotParsed "Write<a,b,c>"),
    ("testfiles/simple.txt", expectNotParsed "Something<a,b>"),
    ("testfiles/crazy_formatting.txt", expectParsed    "Read<a,b>"),
    ("testfiles/crazy_formatting.txt", expectNotParsed "Read<b>"),
    ("testfiles/crazy_formatting.txt", expectNotParsed "Read<a,b,c>"),
    -- Instantiation with Filters.
    ("testfiles/valid_filters.txt", expectParsed    "Value"),
    ("testfiles/valid_filters.txt", expectParsed    "Value2"),
    ("testfiles/valid_filters.txt", expectParsed    "Read<Value>"),
    ("testfiles/valid_filters.txt", expectParsed    "Queue<Value>"),
    ("testfiles/valid_filters.txt", expectNotParsed "Read<Value2>"),
    ("testfiles/valid_filters.txt", expectNotParsed "Queue<Value2>"),
    ("testfiles/valid_filters.txt", expectNotParsed "Read<x>"),
    ("testfiles/valid_filters.txt", expectNotParsed "Queue<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext    "Read"   "Read<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext    "Queue"  "Queue<z>"),
    ("testfiles/valid_filters.txt", expectParsedInContext    "Queue2" "Queue2<q>"),
    ("testfiles/valid_filters.txt", expectParsedInContext    "Test"   "Read<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext    "Test2"  "Queue<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext    "Test2"  "Queue2<x>"),
    ("testfiles/valid_filters.txt", expectNotParsedInContext "Test"   "Queue2<x>"),
    ("testfiles/valid_filters.txt", expectNotParsedInContext "Test3"  "Queue<x>"),
    -- Instantiation with Missingness.
    ("testfiles/valid_missing.txt", expectParsed    "Value2"),
    ("testfiles/valid_missing.txt", expectParsed    "Queue<Value>"),
    ("testfiles/valid_missing.txt", expectParsed    "Write<Value>"),
    ("testfiles/valid_missing.txt", expectParsed    "Read<Value2>"),
    ("testfiles/valid_missing.txt", expectParsed    "Queue<Read<Value2>>"),
    ("testfiles/valid_missing.txt", expectNotParsed "Read<x>"),
    ("testfiles/valid_missing.txt", expectNotParsed "Read<Value>"),
    ("testfiles/valid_missing.txt", expectNotParsed "Read<Queue<Value>>"),
    ("testfiles/valid_missing.txt", expectNotParsed "Queue<Value2>"),
    ("testfiles/valid_missing.txt", expectNotParsed "Read<Queue<Value2>>"),
    ("testfiles/valid_missing.txt", expectNotParsed "Queue<x>"),
    ("testfiles/valid_missing.txt", expectParsedInContext    "Queue" "Queue<y>"),
    ("testfiles/valid_missing.txt", expectParsedInContext    "Read"  "Read<x>"),
    ("testfiles/valid_missing.txt", expectNotParsedInContext "Queue" "Read<y>"),
    ("testfiles/valid_missing.txt", expectNotParsedInContext "Read"  "Queue<x>"),
    -- Simple Conversion.
    ("testfiles/valid_inherits.txt", expectConverted    "Queue<x>" "Queue<x>"),
    ("testfiles/valid_inherits.txt", expectConverted    "Queue<x>" "Read<x>"),
    ("testfiles/valid_inherits.txt", expectNotConverted "Read<x>"  "Queue<x>"),
    -- Nested Conversion.
    ("testfiles/valid_inherits.txt",
     expectConverted
       "Function<Read<x>,y>"
       "Function<Read<x>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted
       "Function<Read<x>,y>"
       "Function<Queue<x>,y>"),
    ("testfiles/valid_inherits.txt",
     expectNotConverted
       "Function<Queue<x>,y>"
       "Function<Read<x>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted
       "Function<Write<Queue<x>>,y>"
       "Function<Queue<Write<x>>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted
       "Function<Read<Queue<x>>,y>"
       "Function<Queue<Queue<x>>,y>"),
    ("testfiles/valid_inherits.txt",
     expectNotConverted
       "Queue<Read<x>>"
       "Queue<Queue<x>>"),
    ("testfiles/valid_inherits.txt",
     expectNotConverted
       "Queue<Queue<x>>"
       "Queue<Read<x>>")
    -- TODO: Test conversion with filters.
  ]

main = do
  results <- sequence $ map (\(f,t) -> testFile f t) testCases
  (es,_) <- return $ partitionEithers $ zipWith numberError [1..] results
  mapM_ (\(n,e) -> hPutStr stderr ("Test " ++ show n ++ ": " ++ show e ++ "\n")) es

numberError :: a -> Either b c -> Either (a,b) c
numberError n (Left e)  = Left (n,e)
numberError _ (Right x) = Right x -- Not the same Either!

manyTypeCategoryes = between (return ())
                         skipSpaces $
                         sepBy (unresolvedParser :: ReadP UnresolvedTypeCategory) (return ())

onlyComplete ((a,[]):xs) f = f a
onlyComplete (_:x:xs)    f = onlyComplete (x:xs) f
onlyComplete ((_,x):[])  _ = compileError $ "Incomplete parse: " ++ x
onlyComplete _           _ = compileError $ "Failed to parse"

testFile :: String -> (String -> CompileInfo TypeCategoryGraph -> CompileInfo ()) -> IO (CompileInfo ())
testFile n f = do
  contents <- readFile n
  parsed <- return $ onlyComplete (readP_to_S manyTypeCategoryes contents) return
  errors <- return $ checkParsed n parsed
  if (isCompileError errors)
     then (return errors)
     else return $ f n (parsed >>= createTypeCategoryGraph)

checkParsed n (Left es) = compileError $ "Parse error in " ++ n ++ ": " ++ show es
checkParsed n _         = return ()

expectLoaded    = tryLoading True
expectNotLoaded = tryLoading False

tryLoading b f (Right v) =
  if b
    then (return ())
    else compileError $ "Expected load failure in " ++ f ++ ": " ++ show v
tryLoading b f (Left e) =
  if b
     then compileError $ "Unexpected load failure in " ++ f ++ ": " ++ show e
     else (return ())

showDebugGraph f (Right g) = Left (show g)
showDebugGraph f (Left e) =
  Left ("Unexpected load failure in " ++ f ++ ": " ++ show e)

tryParse c x = resolved where
  parsed = readP_to_S (unresolvedParser :: ReadP UnresolvedType) x
  resolved = flip (>>=) $ \g -> onlyComplete parsed (resolveTypeCategoryInstance g c)

tryParseNoContext = tryParse Nothing
tryParseWithContext s = tryParse (Just s)

expectParsed    = tryParsing True
expectNotParsed = tryParsing False

tryParsing b x f g = check (tryParseNoContext x g) where
  check (Right v) =
    if b
      then (return ())
      else compileError $ "Expected parse failure for \"" ++ x ++ "\": " ++ show v
  check (Left e) =
    if b
      then compileError $ "Unexpected parse failure for \"" ++ x ++ "\": " ++ show e
      else (return ())

expectParsedInContext    = tryParsingInContext True
expectNotParsedInContext = tryParsingInContext False

tryParsingInContext b s x f g = check (tryParseWithContext s x g) where
  check (Right v) =
    if b
      then (return ())
      else compileError $ "Expected parse failure for \"" ++ x ++ "\": " ++ show v
  check (Left e) =
    if b
      then compileError $ "Unexpected parse failure for \"" ++ x ++ "\": " ++ show e
      else (return ())

expectConverted    = tryConverting True
expectNotConverted = tryConverting False

tryConverting b x y f g = check (tryParseNoContext x g) (tryParseNoContext y g) where
  check (Left e) _ =
    if b
      then compileError $ "Unexpected parse failure for \"" ++ x ++ "\": " ++ show e
      else (return ())
  check _ (Left e) =
    if b
      then compileError $ "Unexpected parse failure for \"" ++ y ++ "\": " ++ show e
      else (return ())
  check (Right x) (Right y) = do
    expectLoaded f g
    graph <- return $ fromRight g
    recheck $ checkConversion graph x y
  fromRight (Right g) = g
  recheck (Right _) =
    if b
      then (return ())
      else compileError $ "Expected conversion failure for \"" ++ x ++ "\" -> \"" ++
                  y ++ "\": [\"" ++ x ++ "\" -> \"" ++ y ++ "\"]"
  recheck (Left e) =
    if b
      then compileError $ "Unexpected conversion failure for \"" ++ x ++ "\" -> \"" ++
                  y ++ "\": " ++ show e
      else (return ())
