{-# language OverloadedStrings #-}
module Actor.Ensemble
  ( Ensemble(..)
  , runEnsemble
  ) where

import Data.DynamicValue
import Actor.UI
import Actor.Configuration
import Actor.Layout
import Actor.Viewer.Complex
import Language.Environment
import Language.Value.Parser (Splice)

import Data.Aeson
import Control.Monad.Except
import Data.Proxy

data Ensemble = Ensemble
  { ensembleSetup :: Maybe Configuration
  , ensembleConfiguration :: Maybe Configuration
  , ensembleViewers :: [ComplexViewer]
  }
  deriving Show

instance FromJSON Ensemble where
  parseJSON = withObject "ensemble" $ \o -> do
    ensembleSetup <- o .:? "setup"
    ensembleConfiguration <- o .:? "configuration"
    singleViewer <- o .:? "viewer"
    ensembleViewers <- case singleViewer of
      Just viewer -> pure [viewer]
      Nothing -> o .:? "viewers" .!= []
    pure Ensemble{..}

runEnsemble :: ComplexViewerCompiler
            -> UI
            -> Ensemble
            -> IO ()
runEnsemble jit UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble

  -- Make the setup window and let it run
  let withSplicesFromSetup :: (forall splices. Context Splice splices -> IO ())
                           -> IO ()
      withSplicesFromSetup k = case ensembleSetup of
        Nothing -> k EmptyContext
        Just setup -> do
          setupUI <- runExceptTIO (allocateUIExpressions (coContents setup))
          runSetup project (coTitle setup) (toSomeDynamic setupUI) (withSplices setupUI k)

      withContextFromConfiguration :: (forall env. Context DynamicValue env -> IO ())
                                   -> IO ()
      withContextFromConfiguration k = case ensembleConfiguration of
        Nothing -> k EmptyContext
        Just config -> do
          configUI <- runExceptTIO (allocateUIConstants (coContents config))
          makeLayout project "Configuration" (toSomeDynamic configUI)
          withDynamicBindings configUI k

  withSplicesFromSetup $ \splices -> do
    withContextFromConfiguration $ \config -> do
      case lookupEnv' (Proxy @"[internal argument] #blockWidth") (contextToEnv config) of
        Absent' pf1 -> recallIsAbsent pf1 $
          case lookupEnv' (Proxy @"[internal argument] #blockHeight") (contextToEnv config) of
            Absent' pf2 -> recallIsAbsent pf2 $
              case lookupEnv' (Proxy @"[internal argument] #subsamples") (contextToEnv config) of
                Absent' pf3 -> recallIsAbsent pf3 $ do
                  forM_ ensembleViewers $ \viewer ->
                    withComplexViewer' jit config splices viewer $ \vu cv' -> do
                      makeViewer project vu cv'
                _ -> error "internal error"
            _ -> error "internal error"
        _ -> error "internal error"

runExceptTIO :: ExceptT String IO a -> IO a
runExceptTIO = fmap (either error id) . runExceptT
