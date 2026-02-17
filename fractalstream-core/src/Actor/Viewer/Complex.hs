{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
--  , listenForChanges
  ) where

import FractalStream.Prelude

import Actor.Viewer
import Actor.Layout
import Actor.Tool
import Actor.Event

import Language.Type
import Language.Code
import Data.DynamicValue
import Data.Codec

import Language.Value.Parser
import Language.Value.Typecheck (InternalVanishingRadius, InternalEscapeRadius)
import Language.Typecheck
import Language.Parser.SourceRange

import qualified Data.Map as Map

data ComplexViewer = ComplexViewer
  { cvTitle :: Parsed String
  , cvSize :: Variable Dimensions
  , cvPosition :: Variable Dimensions
  , cvCanResize :: Variable Bool
  , cvCenter :: Parsed (Complex Double)
  , cvPixelSize :: Parsed Double
  , cvCoord :: Parsed String
  , cvPixel :: Mapped (Maybe String) (Either String (Maybe String))
  , cvEscapeRadius :: Parsed (Maybe ParsedValue)
  , cvVanishRadius :: Parsed (Maybe ParsedValue)
  , cvIterationLimit :: Parsed (Maybe ParsedValue)
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeViewerWithContext)
  --, cvOverlay :: Variable (Maybe String)
  , cvTools :: Variable [Tool]
  }

instance CodecWith ScriptDependencies ComplexViewer where
  codecWith_ ctx = do
    title  <-cvTitle-< mapped (key "title") $ \_ -> pure nonEmptyString
    size   <-cvSize-< key "size"
    pos    <-cvPosition-<  keyWithDefaultValue (Dimensions (100, 100)) "position"
    resize <-cvCanResize-< keyWithDefaultValue True "resizable"
    center <-cvCenter-< mapped (keyWithDefaultValue "0" "initial-center") $ \_ ->
      pure (parseConstant' ComplexType)
    pxSize <-cvPixelSize-< mapped (keyWithDefaultValue "1/128" "initial-pixel-size") $ \_ ->
      pure (parseConstant' RealType)
    coord  <-cvCoord-< mapped (key "z-coord") $ \_ -> pure nonEmptyString
    pixel  <-cvPixel-< mapped (keyWithDefaultValue Nothing "pixel-size") $ \_ ->
      pure (traverse nonEmptyString)

    esc  <-cvEscapeRadius-<   mapped (optionalKey "escape-radius")    $ \_ -> pure $ \case
      "" -> pure Nothing
      s  -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s
    van  <-cvVanishRadius-<   mapped (optionalKey "vanishing-radius") $ \_ -> pure $ \case
      "" -> pure Nothing
      s  -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s
    iter <-cvIterationLimit-< mapped (optionalKey "iteration-limit")  $ \_ -> pure $ \case
      "" -> pure Nothing
      s  -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s

    code   <-cvCode-< mapped (key "code") $ \use -> do
      let complain err = pure . const . Left . (NoSourceRange,)
                    $ ("Cannot parse the script because of previous errors: " ++ err)
          (dcontext, dsplices) = use ctx
      dsplices >>= \case
        Left err -> complain err
        Right vcSplices -> dcontext >>= \case
          Left err -> complain err
          Right (SomeContext (vcContext :: Context DynamicValue env)) -> do
            let env = contextToEnv vcContext
                assertAbsentViewerArgs :: forall e t
                                        . EnvironmentProxy e
                                       -> (MissingViewerArgs e => Either String t)
                                       -> Either String t
                assertAbsentViewerArgs e action =
                  fromMaybe (Left "Internal error") (assertMissingViewerArgs e action)

            vc :: Either String (ViewerContext env) <- withEnvironment env $
              (fromMaybe (pure $ Left "Internal error") . assertMissingViewerArgs env) $
              runExceptT $ do
              vcCoord <- ExceptT (dyn $ use coord)

              vcIterLimit <- ExceptT $ (dyn $ use iter) <&> \case
                Left err -> Left err
                Right Nothing -> Right Nothing
                Right (Just pv) -> assertAbsentViewerArgs env $ do
                  case lookupEnv (Proxy @InternalVanishingRadius) RealType env of
                    Absent pf -> recallIsAbsent pf $
                      case lookupEnv (Proxy @InternalEscapeRadius) RealType env of
                        Absent pf' -> recallIsAbsent pf' $ case pv `atType` IntegerType of
                          TC (Left err) -> Left ("Error with internal iteration limit: " ++ ppError err)
                          TC (Right x)  -> pure (Just x)
                        _ -> Left "Internal error"
                    _ -> Left "Internal error"

              vcEscapes <- ExceptT $ (dyn $ use esc) <&> \case
                Left err -> Left err
                Right Nothing -> Right Nothing
                Right (Just pv) -> assertAbsentViewerArgs env $ do
                  case lookupEnv (Proxy @InternalVanishingRadius) RealType env of
                    Absent pf -> recallIsAbsent pf $ case pv `atType` RealType of
                      TC (Left err) -> Left ("Error with internal escape radius: " ++ ppError err)
                      TC (Right x)  -> pure (Just x)
                    _ -> Left "Internal error"

              vcVanishes <- ExceptT $ (dyn $ use van) <&> \case
                Left err -> Left err
                Right Nothing -> Right Nothing
                Right (Just pv) -> assertAbsentViewerArgs env $ do
                  case pv `atType` RealType of
                    TC (Left err) -> Left ("Error with internal vanishing radius: " ++ ppError err)
                    TC (Right x)  -> pure (Just x)

              pure ViewerContext{..}
            case vc of
              Left err   -> complain err
              Right args -> dyn (use pixel) >>= \case
                Left err -> complain err
                Right mpx -> case assertMissingViewerArgs (envProxy (Proxy @env))
                                  (pure (fmap (SomeViewerWithContext vcContext) . parseViewerScript mpx args)) of
                  Nothing -> pure . const. Left . (NoSourceRange,) $ "INTERNAL ERROR: redefined internal argument"
                  Just ok -> ok

    ctx' <- purely $ \use -> ( snd (use ctx)
                             , fmap ComplexCoordinate <$> dyn (use coord)
                             , (\x y z -> (,,) <$> x <*> y <*> z)
                               <$> (fmap (fromMaybe defaultIterLimit) <$> (dyn $ use iter))
                               <*> (fmap (fromMaybe defaultMaxRadius) <$> (dyn $ use esc))
                               <*> (fmap (fromMaybe defaultMinRadius) <$> (dyn $ use van))
                             , dyn (use pixel))

    tools  <-cvTools-< optionalField "tools" (newVariable []) (fmap null . getDynamic) $ do
      codecWith ctx'
    build ComplexViewer title size pos resize center pxSize coord pixel esc van iter code tools
