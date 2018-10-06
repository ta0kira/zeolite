import Control.Monad
import System.IO
import Text.ParserCombinators.ReadP
import qualified Control.Monad.Trans.Class as Trans

import Resolver
import Unresolved
import Variance

testType = resolve "Writer<Queue<Function<x,y>>>"
testType2 = resolve "Writer<x>"
testType3 = resolve "x"
testType4 = resolve "Iterator<x>"
testType5 = resolve "Function<x,y>"

manyTypeClasses = between skipSpaces
                          skipSpaces $
                          sepBy (unresolvedParser :: ReadP UnresolvedTypeClass) skipSpaces

onlyComplete ((a,[]):xs) f = f a
onlyComplete (_:x:xs)    f = onlyComplete (x:xs) f
onlyComplete ((_,x):[])  _ = Left ["Incomplete parse: " ++ x]
onlyComplete _           _ = Left ["Failed to parse"]

graph = do
  contents <- readFile "examples.txt"
  parsed <- return $ readP_to_S manyTypeClasses $ contents
  return $ onlyComplete parsed createTypeClassGraph

resolve x = unresolved `liftM` graph where
  parsed = readP_to_S (unresolvedParser :: ReadP UnresolvedType) x
  unresolved = flip (>>=) $ \g -> onlyComplete parsed (resolveTypeClassInstance g)

checkPath :: (Monad m1, Monad m2) => m1 (m2 TypeClassGraph) -> m1 (m2 (TypeClassArg,a)) -> m1 (m2 (TypeClassArg,a)) -> m1 (m2 [[TypeClassArg]])
checkPath = liftM3 $ liftM3 getTypePaths
