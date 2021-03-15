{- -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

module CompilerCxx.LanguageModule (
  LanguageModule(..),
  PrivateSource(..),
  compileLanguageModule,
  compileModuleMain,
  compileTestsModule,
) where

import Control.Monad (foldM,when)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Compilation.CompilerState
import Compilation.ProcedureContext (ExprMap)
import CompilerCxx.CxxFiles
import CompilerCxx.Naming
import Types.Builtin
import Types.DefinedCategory
import Types.Procedure
import Types.TypeCategory
import Types.TypeInstance


data LanguageModule c =
  LanguageModule {
    lmPublicNamespaces :: Set.Set Namespace,
    lmPrivateNamespaces :: Set.Set Namespace,
    lmLocalNamespaces :: Set.Set Namespace,
    lmPublicDeps :: [AnyCategory c],
    lmPrivateDeps :: [AnyCategory c],
    lmTestingDeps :: [AnyCategory c],
    lmPublicLocal :: [AnyCategory c],
    lmPrivateLocal :: [AnyCategory c],
    lmTestingLocal :: [AnyCategory c],
    lmExternal :: [CategoryName],
    lmStreamlined :: [CategoryName],
    lmExprMap :: ExprMap c
  }

data PrivateSource c =
  PrivateSource {
    psNamespace :: Namespace,
    psTesting :: Bool,
    psCategory :: [AnyCategory c],
    psDefine :: [DefinedCategory c]
  }

compileLanguageModule :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> [PrivateSource c] -> m [CxxOutput]
compileLanguageModule (LanguageModule ns0 ns1 ns2 cs0 ps0 ts0 cs1 ps1 ts1 ex ss em) xa = do
  checkSupefluous $ Set.toList $ (Set.fromList ex) `Set.difference` ca
  -- Check public sources up front so that error messages aren't duplicated for
  -- every source file.
  ta <- tmTesting
  xx1 <- fmap concat $ mapErrorsM (compileSourceP False tmPublic  nsPublic)  cs1
  xx2 <- fmap concat $ mapErrorsM (compileSourceP False tmPrivate nsPrivate) ps1
  xx3 <- fmap concat $ mapErrorsM (compileSourceP True  tmTesting nsTesting) ts1
  (ds,xx4) <- fmap mergeGeneratedX $ mapErrorsM compileSourceX xa
  xx5 <- fmap concat $ mapErrorsM (\s -> compileConcreteStreamlined (s `Set.member` testingCats) ta s) ss
  -- TODO: This should account for a name clash between a category declared in a
  -- TestsOnly .0rp and one declared in a non-TestOnly .0rx.
  let dm = mapByName ds
  checkDefined dm ex $ filter isValueConcrete (cs1 ++ ps1 ++ ts1)
  checkStreamlined
  return $ xx1 ++ xx2 ++ xx3 ++ xx4 ++ xx5 where
    testingCats = Set.fromList $ map getCategoryName ts1
    tmPublic  = foldM includeNewTypes defaultCategories [cs0,cs1]
    tmPrivate = tmPublic  >>= \tm -> foldM includeNewTypes tm [ps0,ps1]
    tmTesting = tmPrivate >>= \tm -> foldM includeNewTypes tm [ts0,ts1]
    nsPublic = ns0 `Set.union` ns2
    nsPrivate = ns1 `Set.union` nsPublic
    nsTesting = nsPrivate
    compileSourceP testing tm ns c = do
      tm' <- tm
      hxx <- compileCategoryDeclaration testing tm' ns c
      cxx <- if isValueConcrete c
                then return []
                else compileInterfaceDefinition testing c >>= return . (:[])
      return (hxx:cxx)
    compileSourceX (PrivateSource ns testing cs2 ds) = do
      tm <- if testing
               then tmTesting
               else tmPrivate
      let ns4 = if testing
                then nsTesting
                else nsPrivate
      let cs = if testing
                  then cs1 ++ ps1 ++ ts1
                  else cs1 ++ ps1
      checkLocals ds (ex ++ map getCategoryName (cs2 ++ cs))
      when testing $ checkTests ds (cs1 ++ ps1)
      let dm = mapByName ds
      checkDefined dm [] $ filter isValueConcrete cs2
      tm' <- includeNewTypes tm cs2
      -- Ensures that there isn't an inavertent collision when resolving
      -- dependencies for the module later on.
      tmTesting' <- tmTesting
      _ <- (includeNewTypes tmTesting' cs2)
      hxx <- mapErrorsM (compileCategoryDeclaration testing tm' ns4) cs2
      let interfaces = filter (not . isValueConcrete) cs2
      cxx1 <- mapErrorsM (compileInterfaceDefinition testing) interfaces
      cxx2 <- mapErrorsM (compileDefinition testing tm' (ns `Set.insert` ns4)) ds
      return (ds,hxx ++ cxx1 ++ cxx2)
    mergeGeneratedX ((ds,xx):xs2) = let (ds2,xx2) = mergeGeneratedX xs2 in (ds++ds2,xx++xx2)
    mergeGeneratedX _             = ([],[])
    compileDefinition testing tm ns4 d = do
      tm' <- mergeInternalInheritance tm d
      let refines = dcName d `Map.lookup` tm >>= return . getCategoryRefines
      compileConcreteDefinition testing tm' em ns4 refines d
    mapByName = Map.fromListWith (++) . map (\d -> (dcName d,[d]))
    ca = Set.fromList $ map getCategoryName $ filter isValueConcrete (cs1 ++ ps1 ++ ts1)
    checkLocals ds cs2 = mapErrorsM_ (checkLocal $ Set.fromList cs2) ds
    checkLocal cs2 d =
      when (not $ dcName d `Set.member` cs2) $
        compilerErrorM ("Definition for " ++ show (dcName d) ++
                       formatFullContextBrace (dcContext d) ++
                       " does not correspond to a visible category in this module")
    checkTests ds ps = do
      let pa = Map.fromList $ map (\c -> (getCategoryName c,getCategoryContext c)) $ filter isValueConcrete ps
      mapErrorsM_ (checkTest pa) ds
    checkTest pa d =
      case dcName d `Map.lookup` pa of
           Nothing -> return ()
           Just c  ->
             compilerErrorM ("Category " ++ show (dcName d) ++
                            formatFullContextBrace (dcContext d) ++
                            " was not declared as $TestsOnly$" ++ formatFullContextBrace c)
    checkDefined dm ex2 = mapErrorsM_ (checkSingle dm (Set.fromList ex2))
    checkSingle dm es t =
      case (getCategoryName t `Set.member` es, getCategoryName t `Map.lookup` dm) of
           (False,Just [_]) -> return ()
           (True,Nothing)   -> return ()
           (True,Just [d]) ->
             compilerErrorM ("Category " ++ show (getCategoryName t) ++
                           formatFullContextBrace (getCategoryContext t) ++
                           " was declared external but is also defined at " ++ formatFullContext (dcContext d))
           (False,Nothing) ->
             compilerErrorM ("Category " ++ show (getCategoryName t) ++
                           formatFullContextBrace (getCategoryContext t) ++
                           " has not been defined or declared external")
           (_,Just ds) ->
             ("Category " ++ show (getCategoryName t) ++
              formatFullContextBrace (getCategoryContext t) ++
              " is defined " ++ show (length ds) ++ " times") !!>
                mapErrorsM_ (\d -> compilerErrorM $ "Defined at " ++ formatFullContext (dcContext d)) ds
    checkSupefluous es2
      | null es2 = return ()
      | otherwise = compilerErrorM $ "External categories either not concrete or not present: " ++
                                    intercalate ", " (map show es2)
    checkStreamlined =  mapErrorsM_  streamlinedError $ Set.toList $ Set.difference (Set.fromList ss) (Set.fromList ex)
    streamlinedError n =
      compilerErrorM $ "Category " ++ show n ++ " cannot be streamlined because it was not declared external"

compileTestsModule :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> Namespace -> [String] -> [AnyCategory c] -> [DefinedCategory c] ->
  [TestProcedure c] -> m ([CxxOutput],CxxOutput,[(FunctionName,[c])])
compileTestsModule cm ns args cs ds ts = do
  let xs = PrivateSource {
      psNamespace = ns,
      psTesting = True,
      psCategory = cs,
      psDefine = ds
    }
  xx <- compileLanguageModule cm [xs]
  (main,fs) <- compileTestMain cm args xs ts
  return (xx,main,fs)

compileTestMain :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> [String] -> PrivateSource c -> [TestProcedure c] ->
  m (CxxOutput,[(FunctionName,[c])])
compileTestMain (LanguageModule ns0 ns1 ns2 cs0 ps0 ts0 cs1 ps1 ts1 _ _ em) args ts2 tests = do
  tm' <- tm
  (CompiledData req main) <- createTestFile tm' em args tests
  let output = CxxOutput Nothing testFilename NoNamespace (psNamespace ts2 `Set.insert` Set.unions [ns0,ns1,ns2]) req main
  let tests' = map (\t -> (tpName t,tpContext t)) tests
  return (output,tests') where
  tm = foldM includeNewTypes defaultCategories [cs0,cs1,ps0,ps1,ts0,ts1,psCategory ts2]

compileModuleMain :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> [PrivateSource c] -> CategoryName -> FunctionName -> m CxxOutput
compileModuleMain (LanguageModule ns0 ns1 ns2 cs0 ps0 _ cs1 ps1 _ _ _ em) xa n f = do
  let resolved = filter (\d -> dcName d == n) $ concat $ map psDefine $ filter (not . psTesting) xa
  reconcile resolved
  tm' <- tm
  let cs = filter (\c -> getCategoryName c == n) $ concat $ map psCategory xa
  tm'' <- includeNewTypes tm' cs
  (ns,main) <- createMainFile tm'' em n f
  return $ CxxOutput Nothing mainFilename NoNamespace (ns `Set.insert` Set.unions [ns0,ns1,ns2]) (Set.fromList [n]) main where
    tm = foldM includeNewTypes defaultCategories [cs0,cs1,ps0,ps1]
    reconcile [_] = return ()
    reconcile []  = compilerErrorM $ "No matches for main category " ++ show n ++ " ($TestsOnly$ sources excluded)"
    reconcile ds  =
      "Multiple matches for main category " ++ show n !!>
        mapErrorsM_ (\d -> compilerErrorM $ "Defined at " ++ formatFullContext (dcContext d)) ds
