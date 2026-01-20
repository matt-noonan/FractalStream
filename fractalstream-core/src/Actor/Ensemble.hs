{-# language OverloadedStrings, TemplateHaskell #-}
module Actor.Ensemble
  ( Ensemble(..)
  , parseEnsembleFromFile
  , runEnsemble
--  , Template(..)
--  , Project(..)
--  , runTemplate
--  , parseTemplate
--  , parseTemplateFromFile
--  , allTemplates
  ) where

import FractalStream.Prelude

import Data.DynamicValue
import Actor.UI
import Actor.Configuration
import Actor.Layout
import Actor.Viewer.Complex
import Language.Environment
import Language.Value.Typecheck (Splices)

--import Data.Char (isSpace)
import qualified Data.ByteString as BS
--import Data.Aeson
--import qualified Data.Yaml as YAML
import qualified Data.Map as Map
--import qualified Data.ByteString.UTF8 as UTF8

--import Development.IncludeFile

import Data.Codec

data Ensemble = Ensemble
  { ensembleSetup         :: Variable (Maybe Configuration)
  , ensembleConfiguration :: Variable (Maybe Configuration)
  , ensembleViewers       :: Variable [ComplexViewer]
  }

instance Codec Ensemble where
  codec = do
    setup <-ensembleSetup-< optionalField "setup"
      (newVariable "" Nothing) (fmap isNothing . getDynamic) $ do
      codecWith (pure . pure . pure $ Map.empty)

    splices <- purely $ \use -> use setup >>= \case
      Nothing                -> pure (pure Map.empty)
      Just Configuration{..} -> layoutToSplices coContents

    config <-ensembleConfiguration-< optionalField "configuration"
      (newVariable "" Nothing) (fmap isNothing . getDynamic) $
      codecWith splices

    context <- purely $ \use -> use config >>= \case
      Nothing                -> pure (Right $ SomeContext EmptyContext)
      Just Configuration{..} -> layoutContext coContents

    ctx <- purely $ \use -> do
      c <- use context
      s <- use splices
      pure ((,) <$> c <*> s)

    viewers <-ensembleViewers-< newOf $ match
      [ Fragment (:[]) (\case { [x] -> Just x; _ -> Nothing }) $ do
          debugDump "trying [_]"
          field "viewer" (codecWith ctx)
      , Fragment id Just $ do
          debugDump "trying [...]"
          field "viewers" (codecWith ctx)
      , Fragment (const []) (\case { [] -> Just (); _ -> Nothing }) $ do
          debugDump "trying []"
          pure (pure ())
      ]

    build Ensemble setup config viewers

parseEnsembleFromFile :: FilePath -> IO (Either String Ensemble)
parseEnsembleFromFile path = do
  contents <- BS.readFile path
  BS.length contents `seq` pure ()
  deserializeYAML contents

{-
data Project = Project
  { projectConfiguration :: Maybe Configuration
  , projectViewers :: [ComplexViewer]
  }

instance FromJSON Project where
  parseJSON = withObject "project" $ \o -> do
    projectConfiguration <- o .:? "configuration"
    singleViewer <- o .:? "viewer"
    projectViewers <- case singleViewer of
      Just viewer -> pure [viewer]
      Nothing -> o .:? "viewers" .!= []
    pure Project{..}

instance FromJSON Ensemble where
  parseJSON = withObject "ensemble" $ \o -> do
    ensembleSetup <- o .:? "setup"
    ensembleConfiguration <- o .:? "configuration"
    singleViewer <- o .:? "viewer"
    ensembleViewers <- case singleViewer of
      Just viewer -> pure [viewer]
      Nothing -> o .:? "viewers" .!= []
    pure Ensemble{..}

data Template
  = WithSetup Configuration (Map String String -> Either String Project)
  | WithoutSetup Project

newtype ProjectSetup = ProjectSetup (Maybe Configuration)

instance FromJSON ProjectSetup where
  parseJSON = withObject "ensemble" (fmap ProjectSetup . (.:? "setup"))

parseTemplateFromFile :: FilePath -> IO (Either String Template)
parseTemplateFromFile path = do
  contents <- BS.readFile path
  BS.length contents `seq` pure ()
  pure (parseTemplate contents)

parseTemplate :: ByteString -> Either String Template
parseTemplate bs = do
  ProjectSetup msetup <- first show (YAML.decodeEither' bs)
  case msetup of
    Nothing -> do
      Ensemble{..} <- first show (YAML.decodeEither' bs)
      pure (WithoutSetup $ Project ensembleConfiguration ensembleViewers)
    Just setup -> do
      pure (WithSetup setup $ first show
                            . (\x -> trace (UTF8.toString x) $ YAML.decodeEither' x)
                            . UTF8.fromString
                            . substitute (UTF8.toString bs))

substitute :: String -> Map String String -> String
substitute input0 splices = unlines . concatMap substituteLine . lines $ input0
  where
    substituteLine il =
      let leadingSpaces = takeWhile isSpace il
          il' = drop (length leadingSpaces) il
      in case il' of
        ('$' : input) ->
          let name = takeWhile (/= '$') input
              input' = drop (length name + 1) input
          in case Map.lookup name splices of
            Nothing -> error ("No splice named " ++ show name)
            Just s -> map (leadingSpaces ++) (lines s) ++ [leadingSpaces ++ go "" input']
        _ -> [go "" il]

    go acc = \case
      ('$' : input) -> go acc (splice input)
      (c : input) -> go (c : acc) input
      "" -> reverse acc

    splice input =
      let name = takeWhile (/= '$') input
          input' = drop (length name + 1) input
      in case Map.lookup name splices of
           Nothing -> error ("No splice named " ++ show name)
           Just s  -> concat ["{", s, "}", input']
-}

{-
runTemplate :: ComplexViewerCompiler
            -> UI
            -> Template
            -> IO ()
runTemplate jit UI{..} wiz = do
  -- Get a handle for the project
  project <- newEnsemble

  let runProj :: Project -> IO ()
      runProj Project{..} = do

        let withContextFromConfiguration :: (forall env. Context DynamicValue env -> IO ())
                                         -> IO ()
            withContextFromConfiguration k = case projectConfiguration of
              Nothing -> k EmptyContext
              Just config -> do
                configUI <- runExceptTIO (allocateUIConstants (coContents config))
                makeLayout project (coTitle config) (toSomeDynamic configUI)
                withDynamicBindings configUI k

        withContextFromConfiguration $ \config -> do
          ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #blockWidth")
                                                  (contextToEnv config) "internal error"
          ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #blockHeight")
                                                  (contextToEnv config) "internal error"
          ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #subsamples")
                                                  (contextToEnv config) "internal error"
          ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"color")
                                                  (contextToEnv config)
                                                  "internal error, `color` already defined"
          forM_ projectViewers $ \viewer ->
            withComplexViewer' jit config Map.empty viewer $ \vu cv' -> do
              makeViewer project vu cv'

  case wiz of
    WithoutSetup proj -> runProj proj

    WithSetup setup toProject -> do
      let withSplicesFromSetup :: (Map String String -> IO ())
                               -> IO ()
          withSplicesFromSetup k = do
            setupUI <- runExceptTIO (allocateUIExpressions (coContents setup))
            runSetup project (coTitle setup) (toSomeDynamic setupUI) (withStrings setupUI k)

      withSplicesFromSetup $ \splices -> do
        proj <- either error pure (toProject splices)
        runProj proj

withStrings :: Layout Expression -> (Map String String -> IO ()) -> IO ()
withStrings lo action = do
  let getNameAndString :: forall t. Expression t -> IO (String, String)
      getNameAndString = \case
        Expression name _ _ v -> (name,) . fst <$> getDynamic v
        ScriptExpression name _ v -> (name,) . fst . first unCodeString <$> getDynamic v
        BoolExpression {} -> error "TODO: withStrings BoolExpression"
        ColorExpression {} -> error "TODO: withStrings ColorExpression"
  contents <- sequence (extractAllBindings getNameAndString lo)
  action (Map.fromList contents)
-}

runEnsemble :: ComplexViewerCompiler
            -> UI
            -> Ensemble
            -> IO ()
runEnsemble jit UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble

  -- Make the setup window and let it run
  let withSplicesFromSetup :: (Splices -> IO ())
                           -> IO ()
      withSplicesFromSetup k = getDynamic ensembleSetup >>= \case
        Nothing -> k Map.empty
        Just setup -> do
          title <- fromRight (const "???") <$> getDynamic (coTitle setup)
          runSetup project title (coContents setup) $ do
              getDynamic (layoutToSplices $ coContents setup) >>= \case
                Left err -> error err
                Right splices -> k splices

      withContextFromConfiguration :: (forall env. Context DynamicValue env -> IO ())
                                   -> IO ()
      withContextFromConfiguration k = getDynamic ensembleConfiguration >>= \case
        Nothing -> k EmptyContext
        Just config -> do
          title <- fromRight (const "???") <$> getDynamic (coTitle config)
          makeLayout project title (coContents config)
          getDynamic (layoutContext $ coContents config) >>= \case
            Left err -> putStrLn ("ERROR (withContextFromConfiguration): " ++ err)
            Right (SomeContext ctx) -> k ctx

  withSplicesFromSetup $ \splices -> do
    withContextFromConfiguration $ \config -> do
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #blockWidth")
                                              (contextToEnv config) "internal error"
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #blockHeight")
                                              (contextToEnv config) "internal error"
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #subsamples")
                                              (contextToEnv config) "internal error"
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"color")
                                              (contextToEnv config)
                                              "internal error, `color` already defined"
      viewers <- getDynamic ensembleViewers
      forM_ viewers $ \viewer -> error "TODO"
--        withComplexViewer' jit config splices viewer $ \vu cv' -> do
--          makeViewer project vu cv'

runExceptTIO :: ExceptT String IO a -> IO a
runExceptTIO = fmap (either error id) . runExceptT

{-
$(includeFileInSource "../examples/templates/simple-complex-dynamics.yaml"
  "simpleComplexDynamicsTemplate")

$(includeFileInSource "../examples/templates/simple-parametric-complex-dynamics.yaml"
  "simpleParametricComplexDynamicsTemplate")

$(includeFileInSource "../examples/templates/one-variable-complex-dynamics.yaml"
  "complexDynamicsTemplate")

$(includeFileInSource "../examples/templates/one-variable-parametric-complex-dynamics.yaml"
  "parametricComplexDynamicsTemplate")

allTemplates :: [(String, Template)]
allTemplates =
  [ template "Simplified complex dynamics" simpleComplexDynamicsTemplate
  , template "Simplified parametric complex dynamics" simpleParametricComplexDynamicsTemplate
  , template "General complex dynamics" complexDynamicsTemplate
  , template "General parametric complex dynamics" parametricComplexDynamicsTemplate
  ]
 where
   template name bs = case parseTemplate bs of
     Left e -> error ("Internal error when parsing a template: " ++ e)
     Right t -> (name, t)
-}
