{- -----------------------------------------------------------------------------
Copyright 2019-2021,2023 Kevin P. Barry

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

module CompilerCxx.LanguageModule (
  LanguageModule(..),
  PrivateSource(..),
  compileLanguageModule,
  compileModuleMain,
  compileTestsModule,
) where

import Control.Monad (foldM,foldM_,when)
import Data.List (intercalate,nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Compilation.CompilerState
import Compilation.ProcedureContext (ExprMap)
import CompilerCxx.CxxFiles
import CompilerCxx.Naming
import Module.CompileMetadata (CategorySpec(..))
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
    lmPublicTestingDeps :: [AnyCategory c],
    lmPrivateTestingDeps :: [AnyCategory c],
    lmPublicLocal :: [AnyCategory c],
    lmPrivateLocal :: [AnyCategory c],
    lmPublicTestingLocal :: [AnyCategory c],
    lmPrivateTestingLocal :: [AnyCategory c],
    lmStreamlined :: [CategoryName],
    lmExprMap :: ExprMap c,
    lmEmptyCategories :: CategoryMap c
  }

data PrivateSource c =
  PrivateSource {
    psNamespace :: Namespace,
    psTesting :: Bool,
    psCategory :: [AnyCategory c],
    psDefine :: [DefinedCategory c]
  }

compileLanguageModule :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> Map.Map CategoryName (CategorySpec c) ->
  [PrivateSource c] -> m [CxxOutput]
compileLanguageModule (LanguageModule ns0 ns1 ns2 cs0 ps0 tc0 tp0 cs1 ps1 tc1 tp1 ss em cm0) sm xa = do
  let dm = mapDefByName $ concat $ map psDefine xa
  checkDefined dm extensions allExternal $ filter isValueConcrete (cs1 ++ ps1 ++ tc1 ++ tp1)
  checkSupefluous $ Set.toList $ extensions `Set.difference` ca
  tmPublic         <- foldM includeNewTypes cm0             [cs0,cs1]
  tmPrivate        <- foldM includeNewTypes tmPublic        [ps0,ps1]
  tmPublicTesting  <- foldM includeNewTypes tmPublic        [tc0,tc1]
  tmPrivateTesting <- foldM includeNewTypes tmPublicTesting [ps0,tp0,ps1,tp1]
  xxInterfaces <- fmap concat $ collectAllM $
    map (generateNativeInterface False nsPublic)  (onlyNativeInterfaces cs1) ++
    map (generateNativeInterface False nsPrivate) (onlyNativeInterfaces ps1) ++
    map (generateNativeInterface True  nsPublic)  (onlyNativeInterfaces tc1) ++
    map (generateNativeInterface True  nsPrivate) (onlyNativeInterfaces tp1)
  xxPrivate <- fmap concat $ mapCompilerM (compilePrivate tmPrivate tmPrivateTesting) xa
  xxStreamlined <- fmap concat $ mapCompilerM (streamlined tmPrivate tmPrivateTesting) $ nub ss
  let allFiles = xxInterfaces ++ xxPrivate ++ xxStreamlined
  noDuplicateFiles $ map (\f -> (coFilename f,coNamespace f)) allFiles
  return allFiles where
    nsPublic  = ns0 `Set.union` ns2
    nsPrivate = ns1 `Set.union` nsPublic
    extensions = Set.fromList ss
    allExternal = Set.unions [extensions,Map.keysSet sm]
    testingCats = Set.fromList $ (map getCategoryName tc1) ++ (map getCategoryName tp1)
    onlyNativeInterfaces = filter (not . (`Set.member` extensions) . getCategoryName) . filter (not . isValueConcrete)
    localCats = Set.fromList $ map getCategoryName $ cs1 ++ ps1 ++ tc1 ++ tp1
    streamlined tm0 tm2 n = do
      checkLocal localCats ([] :: [String]) n
      let testing = n `Set.member` testingCats
      let tm = if testing then tm2 else tm0
      (_,t) <- getConcreteCategory tm ([],n)
      let ctx = FileContext testing tm nsPrivate Map.empty
      let spec = Map.findWithDefault (CategorySpec [] [] []) (getCategoryName t) sm
      generateStreamlinedExtension ctx t spec
    compilePrivate tmPrivate tmTesting (PrivateSource ns3 testing cs2 ds) = do
      let tm = if testing
                  then tmTesting
                  else tmPrivate
      let cs = Set.fromList $ map getCategoryName $ if testing
                                                       then cs2 ++ cs1 ++ ps1 ++ tc1 ++ tp1
                                                       else cs2 ++ cs1 ++ ps1
      tm' <- includeNewTypes tm cs2
      let ctx = FileContext testing tm' (ns3 `Set.insert` nsPrivate) em
      checkLocals ds $ Map.keysSet $ cmAvailable tm'
      when testing $ checkTests ds (cs1 ++ ps1)
      let dm = mapDefByName ds
      checkDefined dm Set.empty Set.empty $ filter isValueConcrete cs2
      xxInterfaces <- fmap concat $ mapCompilerM (generateNativeInterface testing nsPrivate) (filter (not . isValueConcrete) cs2)
      xxConcrete   <- fmap concat $ mapCompilerM (generateConcrete cs ctx) ds
      return $ xxInterfaces ++ xxConcrete
    generateConcrete cs (FileContext testing tm ns em2) d = do
      t <- getCategoryDecl cs tm d
      let ctx = FileContext testing tm ns em2
      generateNativeConcrete ctx (t,d)
    getCategoryDecl cs tm d = do
      checkLocal cs (dcContext d) (dcName d)
      fmap snd $ getConcreteCategory tm (dcContext d,dcName d)
    mapDefByName = Map.fromListWith (++) . map (\d -> (dcName d,[d]))
    ca = Set.fromList $ map getCategoryName $ filter isValueConcrete (cs1 ++ ps1 ++ tc1 ++ tp1)
    checkLocals ds tm = mapCompilerM_ (\d -> checkLocal tm (dcContext d) (dcName d)) ds
    checkLocal cs2 c n =
      when (not $ n `Set.member` cs2) $
        compilerErrorM ("Category " ++ show n ++
                        formatFullContextBrace c ++
                        " does not correspond to a visible category in this module")
    checkTests ds ps = do
      let pa = Map.fromList $ map (\c -> (getCategoryName c,getCategoryContext c)) $ filter isValueConcrete ps
      mapCompilerM_ (checkTest pa) ds
    checkTest pa d =
      case dcName d `Map.lookup` pa of
           Nothing -> return ()
           Just c  ->
             compilerErrorM ("Category " ++ show (dcName d) ++
                            formatFullContextBrace (dcContext d) ++
                            " was not declared as $TestsOnly$" ++ formatFullContextBrace c)
    checkDefined dm ext extAll = mapCompilerM_ (checkSingle dm ext extAll)
    checkSingle dm ext extAll t =
      case (getCategoryName t `Set.member` ext,getCategoryName t `Set.member` extAll,getCategoryName t `Map.lookup` dm) of
           (False,False,Just [_]) -> return ()
           (True,_,Nothing) -> return ()
           (False,_,Nothing) ->
             compilerErrorM ("Category " ++ show (getCategoryName t) ++
                             formatFullContextBrace (getCategoryContext t) ++
                             " has not been defined or declared external")
           (_,True,Just [d]) ->
             compilerErrorM ("Category " ++ show (getCategoryName t) ++
                             formatFullContextBrace (getCategoryContext t) ++
                             " was declared external but is also defined at " ++ formatFullContext (dcContext d))
           (_,_,Just ds) ->
             ("Category " ++ show (getCategoryName t) ++
              formatFullContextBrace (getCategoryContext t) ++
              " is defined " ++ show (length ds) ++ " times") !!>
                mapCompilerM_ (\d -> compilerErrorM $ "Defined at " ++ formatFullContext (dcContext d)) ds
    checkSupefluous es2
      | null es2 = return ()
      | otherwise = compilerErrorM $ "External categories either not concrete or not present: " ++
                                     intercalate ", " (map show es2)
    noDuplicateFiles = foldM_ checkFileUsed Set.empty
    checkFileUsed used (f,ns3) = do
      when ((f,ns3) `Set.member` used) $
        compilerErrorM $ "Filename " ++ f ++ " in namespace " ++ show ns3 ++
                         " was already generated (internal compiler error)"
      return $ (f,ns3) `Set.insert` used

compileTestsModule :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> Namespace -> [String] -> Maybe ([c],TypeInstance) -> [AnyCategory c] ->
  [DefinedCategory c] -> [TestProcedure c] -> m ([CxxOutput],CxxOutput,[(FunctionName,[c])])
compileTestsModule cm ns args t cs ds ts = do
  let xs = PrivateSource {
      psNamespace = ns,
      psTesting = True,
      psCategory = cs,
      psDefine = ds
    }
  xx <- compileLanguageModule cm Map.empty [xs]
  (main,fs) <- compileTestMain cm args t xs ts
  return (xx,main,fs)

compileTestMain :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> [String] -> Maybe ([c],TypeInstance) -> PrivateSource c -> [TestProcedure c] ->
  m (CxxOutput,[(FunctionName,[c])])
compileTestMain (LanguageModule ns0 ns1 ns2 cs0 ps0 tc0 tp0 cs1 ps1 tc1 tp1 _ em cm0) args t ts2 tests = do
  tm' <- tm
  (CompiledData req traces main) <- generateTestFile tm' em args t tests
  let output = CxxOutput Nothing testFilename NoNamespace (psNamespace ts2 `Set.insert` Set.unions [ns0,ns1,ns2]) req traces main
  let tests' = map (\t2 -> (tpName t2,tpContext t2)) tests
  return (output,tests') where
  tm = foldM includeNewTypes cm0 [cs0,cs1,ps0,ps1,tc0,tp0,tc1,tp1,psCategory ts2]

compileModuleMain :: (Ord c, Show c, CollectErrorsM m) =>
  LanguageModule c -> [PrivateSource c] -> CategoryName -> FunctionName -> m CxxOutput
compileModuleMain (LanguageModule ns0 ns1 ns2 cs0 ps0 _ _ cs1 ps1 _ _ _ em cm0) xa n f = do
  let resolved = filter (\d -> dcName d == n) $ concat $ map psDefine $ filter (not . psTesting) xa
  reconcile resolved
  tm' <- tm
  let cs = filter (\c -> getCategoryName c == n) $ concat $ map psCategory xa
  tm'' <- includeNewTypes tm' cs
  (ns,main) <- generateMainFile tm'' em n f
  return $ CxxOutput Nothing mainFilename NoNamespace (ns `Set.insert` Set.unions [ns0,ns1,ns2]) (Set.fromList [n]) Set.empty main where
    tm = foldM includeNewTypes cm0 [cs0,cs1,ps0,ps1]
    reconcile [_] = return ()
    reconcile []  = compilerErrorM $ "No matches for main category " ++ show n ++ " ($TestsOnly$ sources excluded)"
    reconcile ds  =
      "Multiple matches for main category " ++ show n !!>
        mapCompilerM_ (\d -> compilerErrorM $ "Defined at " ++ formatFullContext (dcContext d)) ds
