import Control.Monad
import Data.Either
import System.IO
import Text.ParserCombinators.ReadP
import qualified Control.Monad.Trans.Class as Trans

import Resolver
import Unresolved
import Variance

testCases = [
    -- Basics.
    ("testfiles/simple.txt", expectLoaded True),
    -- Inheritance.
    ("testfiles/valid_inherits.txt",       expectLoaded True),
    ("testfiles/covariance_error.txt",     expectLoaded False),
    ("testfiles/contravariance_error.txt", expectLoaded False),
    -- Filters.
    ("testfiles/valid_filters.txt",    expectLoaded True),
    ("testfiles/missing_filter.txt",   expectLoaded False),
    ("testfiles/incorrect_filter.txt", expectLoaded False),
    -- Simple Instantiation.
    ("testfiles/simple.txt", expectParsed True  "Read<a,b>"),
    ("testfiles/simple.txt", expectParsed True  "Write<a,b>"),
    ("testfiles/simple.txt", expectParsed True  "Something<a,b,c,d>"),
    ("testfiles/simple.txt", expectParsed False "Read<b>"),
    ("testfiles/simple.txt", expectParsed False "Write<a,b,c>"),
    ("testfiles/simple.txt", expectParsed False "Something<a,b>"),
    -- Instantiation with Filters.
    ("testfiles/valid_filters.txt", expectParsed True  "Read<Value>"),
    ("testfiles/valid_filters.txt", expectParsed True  "Queue<Value>"),
    ("testfiles/valid_filters.txt", expectParsed False "Read<Value2>"),
    ("testfiles/valid_filters.txt", expectParsed False "Queue<Value2>"),
    ("testfiles/valid_filters.txt", expectParsed False "Read<x>"),
    ("testfiles/valid_filters.txt", expectParsed False "Queue<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext True  "Test"  "Read<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext False "Test"  "Queue2<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext False "Test3" "Queue<x>"),
    ("testfiles/valid_filters.txt", expectParsedInContext True  "Test2" "Queue<x>"),
    -- Simple Conversion.
    ("testfiles/valid_inherits.txt",
     expectConverted True  "Queue<x>" "Queue<x>"),
    ("testfiles/valid_inherits.txt",
     expectConverted True  "Queue<x>" "Read<x>"),
    ("testfiles/valid_inherits.txt",
     expectConverted False "Read<x>"  "Queue<x>"),
    -- Nested Conversion.
    ("testfiles/valid_inherits.txt",
     expectConverted True  "Function<Read<x>,y>"  "Function<Read<x>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted True  "Function<Read<x>,y>"  "Function<Queue<x>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted False "Function<Queue<x>,y>" "Function<Read<x>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted True  "Function<Write<Queue<x>>,y>" "Function<Queue<Write<x>>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted True  "Function<Read<Queue<x>>,y>" "Function<Queue<Queue<x>>,y>"),
    ("testfiles/valid_inherits.txt",
     expectConverted False "Queue<Read<x>>"  "Queue<Queue<x>>"),
    ("testfiles/valid_inherits.txt",
     expectConverted False "Queue<Queue<x>>" "Queue<Read<x>>")
    -- TODO: Test conversion with filters.
  ]

main = do
  results <- sequence $ map (\(f,t) -> testFile f t) testCases
  (e,_) <- return $ partitionEithers results
  return e

manyTypeClasses = between (return ())
                          skipSpaces $
                          sepBy (unresolvedParser :: ReadP UnresolvedTypeClass) (return ())

onlyComplete ((a,[]):xs) f = f a
onlyComplete (_:x:xs)    f = onlyComplete (x:xs) f
onlyComplete ((_,x):[])  _ = Left ["Incomplete parse: " ++ x]
onlyComplete _           _ = Left ["Failed to parse"]

testFile :: String -> (String -> Either [String] TypeClassGraph -> Either String ()) -> IO (Either String ())
testFile n f = do
  contents <- readFile n
  parsed <- return $ onlyComplete (readP_to_S manyTypeClasses contents) return
  errors <- return $ checkParsed n parsed
  if (isLeft errors)
     then (return errors)
     else return $ f n (parsed >>= createTypeClassGraph)

checkParsed n (Left es) = Left $ "Parse error in " ++ n ++ ": " ++ show es
checkParsed n _         = return ()

expectLoaded b f (Right _) =
  if b
    then (return ())
    else Left $ "Expected load failure in " ++ f
expectLoaded b f (Left e) =
  if b
     then Left $ "Unexpected load failure in " ++ f ++ ": " ++ show e
     else (return ())

tryParse c x = resolved where
  parsed = readP_to_S (unresolvedParser :: ReadP UnresolvedType) x
  resolved = flip (>>=) $ \g -> onlyComplete parsed (resolveTypeClassInstance g c)

tryParseNoContext = tryParse Nothing
tryParseWithContext s = tryParse (Just s)

expectParsed b x f g = check (tryParseNoContext x g) where
  check (Right _) =
    if b
      then (return ())
      else Left $ "Expected parse failure for \"" ++ x ++ "\""
  check (Left e) =
    if b
      then Left $ "Unexpected parse failure for \"" ++ x ++ "\": " ++ show e
      else (return ())

expectParsedInContext b s x f g = check (tryParseWithContext s x g) where
  check (Right _) =
    if b
      then (return ())
      else Left $ "Expected parse failure for \"" ++ x ++ "\""
  check (Left e) =
    if b
      then Left $ "Unexpected parse failure for \"" ++ x ++ "\": " ++ show e
      else (return ())

expectConverted b x y f g = check (tryParseNoContext x g) (tryParseNoContext y g) where
  check (Left e) _ =
    if b
      then Left $ "Unexpected parse failure for \"" ++ x ++ "\": " ++ show e
      else (return ())
  check _ (Left e) =
    if b
      then Left $ "Unexpected parse failure for \"" ++ y ++ "\": " ++ show e
      else (return ())
  check (Right x) (Right y) = do
    expectLoaded True f g
    graph <- return $ fromRight g
    recheck $ checkConversion graph x y
  fromRight (Right g) = g
  recheck (Right _) =
    if b
      then (return ())
      else Left $ "Expected conversion failure for \"" ++ x ++ "\" -> \"" ++
                  y ++ "\": [\"" ++ x ++ "\" -> \"" ++ y ++ "\"]"
  recheck (Left e) =
    if b
      then Left $ "Unexpected conversion failure for \"" ++ x ++ "\" -> \"" ++
                  y ++ "\": " ++ show e
      else (return ())
