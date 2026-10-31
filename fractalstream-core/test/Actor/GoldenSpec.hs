module Actor.GoldenSpec (spec) where

-- | Golden tests pinning down the fully-typechecked 'Code' produced for
-- every example project (including those under @examples/templates/@): each viewer body, its
-- preparation script (if any), and every tool event handler, pretty-printed
-- with 'pprintCode'. These exist to guard the move from the closure-based
-- typechecker to the surface-AST + inference + elaboration pipeline: the
-- elaborated ASTs should come out identical, down to conversion placement.
--
-- A missing golden file is written out (and the test passes); delete
-- @test/golden/@ files to regenerate them.

import Test.Hspec

import Actor.Ensemble
import Actor.Viewer (SomeViewerWithContext(..), PrepScript(..))
import Actor.Viewer.Complex (ComplexViewer(..))
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Configuration (Configuration(..))
import Actor.Event (runParsedToolScript, CombinedEventHandler(..))
import Language.Environment (SomeContext(..), SomeContext'(..), Context(EmptyContext), contextToEnv)
import Language.Code (Code, pprintCode)
import Data.DynamicValue

import Control.Exception (SomeException, try, evaluate)
import Control.Monad (forM, forM_)
import Data.List (isSuffixOf, sort)
import qualified Data.Map as Map
import System.Directory (doesFileExist, listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), isPathSeparator)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8, hGetContents', hPutStr)

examplesDir, goldenDir :: FilePath
examplesDir = "../examples"
goldenDir   = "test/golden"

spec :: Spec
spec = describe "typed AST golden tests" $ do
  let yamlFiles dir = runIO $
        sort . filter (".yaml" `isSuffixOf`) <$> listDirectory (examplesDir </> dir)
  examples  <- yamlFiles ""
  templates <- map ("templates" </>) <$> yamlFiles "templates"
  forM_ (examples ++ templates) $ \file ->
    it ("example " ++ file) $
      parseEnsembleFromFile (examplesDir </> file) >>= \case
        Left err  -> golden (goldenName file) ("BUILD ERROR\n" ++ err)
        Right ens -> dumpEnsemble ens >>= golden (goldenName file)
  where
    -- e.g. "templates/newtons-method.yaml" -> "templates-newtons-method.yaml"
    goldenName = map (\c -> if isPathSeparator c then '-' else c)

golden :: String -> String -> Expectation
golden name actual = do
  createDirectoryIfMissing True goldenDir
  let path = goldenDir </> (name ++ ".golden")
  exists <- doesFileExist path
  if exists
    then do
      -- Golden files are UTF-8, whatever the locale's encoding (on
      -- Windows it usually isn't UTF-8).
      expected <- withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h
      actual `shouldBe` expected
    else withFile path WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h actual

-- | Pretty-print all typed code reachable from an ensemble.
dumpEnsemble :: Ensemble -> IO String
dumpEnsemble ens = do
  viewers <- getDynamic (ensembleViewers ens)
  ensembleCfg <- getDynamic (ensembleConfiguration ens)
  parts <- forM (zip [0 :: Int ..] viewers) $ \(i, viewer) -> do
    title <- getDynamic (cvTitle viewer)
    code <- getDynamic (cvCode viewer)
    let header = "=== viewer " ++ show i ++ ": " ++ either id id title
    viewerPart <- safely $ case code of
      Left (_, err) -> "ERROR: " ++ err
      Right (SomeViewerWithContext _ mprep body) -> unlines $
        [ "--- body", pprintCode body ] ++
        case mprep of
          Nothing -> []
          Just (PrepScript _ prep) -> [ "--- prepare", pprintCode prep ]
    tools <- getDynamic (cvTools viewer)
    toolParts <- forM tools $ \tool -> do
      name <- getDynamic (tiName (toolInfo tool))
      script <- getDynamic (toolScript tool)
      mcfg <- getDynamic (toolConfig tool)
      ensembleArgs <- case ensembleCfg of
        Nothing -> pure (Right (SomeContext EmptyContext))
        Just cfg -> layoutToArgs (coContents cfg) >>= \(SomeContext' e) -> pure e
      body <- case ensembleArgs of
        Left err -> pure ("ERROR (ensemble args): " ++ err)
        Right (SomeContext ctx) -> buildToolContext (SomeContext ctx) mcfg >>= \case
          Left err -> pure ("ERROR (tool context): " ++ err)
          Right (SomeContext tctx) -> safely $
            case runParsedToolScript script (contextToEnv tctx) of
              Left (_, err) -> "ERROR: " ++ err
              Right h -> dumpHandlers h
      pure ("--- tool " ++ either id id name ++ "\n" ++ body)
    pure (unlines (header : viewerPart : toolParts))
  pure (concat parts)

dumpHandlers :: CombinedEventHandler env -> String
dumpHandlers CombinedEventHandler{..} = unlines $ concat
  [ one "click" onClick
  , one "double-click" onDoubleClick
  , one "drag" onDrag
  , one "drag-done" onDragDone
  , concat [ section ("timer " ++ k ++ " every " ++ show n) c | (k, (n, c)) <- Map.toList onTimer ]
  , concat [ section ("button " ++ k) c | (k, c) <- Map.toList onButton ]
  , one "refresh" onRefresh
  , one "activated" onActivated
  , one "deactivated" onDeactivated
  ]
  where
    one :: String -> Maybe (Code e) -> [String]
    one name = maybe [] (section name)
    section :: String -> Code e -> [String]
    section name c = ["  -- on " ++ name, pprintCode c]

-- | Force a rendered string, turning exceptions (e.g. 'error' calls in a
-- not-yet-implemented typechecker case) into a recorded failure line.
safely :: String -> IO String
safely s = try (evaluate (length s `seq` s)) >>= \case
  Left (e :: SomeException) -> pure ("EXCEPTION: " ++ show e)
  Right x -> pure x
