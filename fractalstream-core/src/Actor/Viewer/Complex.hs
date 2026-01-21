{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
  , cloneComplexViewer
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
import Language.Parser.SourceRange

import qualified Data.Map as Map

data ComplexViewer = ComplexViewer
  { cvTitle :: Parsed String
  , cvSize :: Variable Dimensions
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
    resize <-cvCanResize-< keyWithDefaultValue True "resizable"
    center <-cvCenter-< mapped (keyWithDefaultValue "0" "initial-center") $ \_ ->
      pure (parseConstant' ComplexType)
    pxSize <-cvPixelSize-< mapped (keyWithDefaultValue "1/128" "initial-pixel-size") $ \_ ->
      pure (parseConstant' RealType)
    coord  <-cvCoord-< mapped (key "z-coord") $ \_ -> pure nonEmptyString
    pixel  <-cvPixel-< mapped (keyWithDefaultValue Nothing "pixel-size") $ \_ ->
      pure (traverse nonEmptyString)

    code   <-cvCode-< mapped (key "code") $ \use -> do
      let complain err = pure . const . Left . (NoSourceRange,)
                    $ ("Cannot parse the script because of previous errors: " ++ err)
      use ctx >>= \case
        Left err -> complain err
        Right (SomeContext (vcContext :: Context DynamicValue env), vcSplices) -> do
          vc :: Either String (ViewerContext env) <- runExceptT $ do
            vcCoord <- ExceptT (use coord)
            pure ViewerContext{..}
          case vc of
            Left err   -> complain err
            Right args -> pure (fmap (SomeViewerWithContext vcContext) . parseViewerScript args)

    ctx' <- purely $ \use -> do
      c <- use ctx
      v <- fmap ComplexCoordinate <$> use coord
      pure ((,,) <$> (fst <$> c) <*> (snd <$> c) <*> v)

    tools  <-cvTools-< optionalField "tools" (newVariable "" []) (fmap null . getDynamic) $ do
      codecWith ctx'
    esc    <-cvEscapeRadius-<   mapped (optionalKey "escape-radius")    $ \_ ->
      pure (\s -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s)
    van    <-cvVanishRadius-<   mapped (optionalKey "vanishing-radius") $ \_ ->
      pure (\s -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s)
    iter   <-cvIterationLimit-< mapped (optionalKey "iteration-limit")  $ \_ ->
      pure (\s -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s)
    build ComplexViewer title size resize center pxSize coord pixel esc van iter code tools

cloneComplexViewer :: Viewer -> IO Viewer
cloneComplexViewer (Viewer v) = do
  newTitle     <- newVariable "" =<< getDynamic (vTitle v)
  newSize      <- newVariable "" =<< getDynamic (vSize v)
  newCenter    <- newVariable "" =<< getDynamic (vCenter v)
  newPixelSize <- newVariable "" =<< getDynamic (vPixelSize v)
  pure . Viewer $
    ViewerInfo { vTitle = newTitle
               , vSize  = newSize
               , vCanResize = vCanResize v
               , vCenter = newCenter
               , vPixelSize = newPixelSize
               , vSaveView = vSaveView v
               , vGetArgs = vGetArgs v
               , vCode = vCode v
               }

{-
consDrawCmd :: Draw_ value env
            -> Maybe [Draw_ value env]
            -> Maybe [Draw_ value env]
consDrawCmd = \case
  Clear{} -> const (Just [])
  c -> Just . \case
    Nothing -> [c]
    Just cs -> c:cs

makeDrawCommandGetter :: IO (IO [[DrawCommand]], IO Bool, Int -> DrawHandler ScalarIORefM)
makeDrawCommandGetter = do
  layersMVar <- newMVar (Map.empty :: Map Int [DrawCommand])
  dcChanged <- newMVar False
  let drawTo :: Int -> DrawHandler ScalarIORefM
      drawTo n = DrawHandler $ \cmd -> do
        let emit :: DrawCommand -> StateT (Context IORefTypeOfBinding e) IO ()
            emit cmd' = liftIO $ do
              modifyMVar_ dcChanged (pure . const True)
              modifyMVar_ layersMVar (pure . Map.alter (consDrawCmd cmd') n)
        case cmd of
          DrawPoint _env pv -> do
            p <- eval pv
            emit (DrawPoint EmptyEnvProxy p)
          DrawCircle _env doFill rv pv -> do
            r    <- eval rv
            p    <- eval pv
            emit (DrawCircle EmptyEnvProxy doFill r p)
          DrawLine _env fromv tov -> do
            from <- eval fromv
            to   <- eval tov
            emit (DrawLine EmptyEnvProxy from to)
          DrawRect _env doFill fromv tov -> do
            from <- eval fromv
            to   <- eval tov
            emit (DrawRect EmptyEnvProxy doFill from to)
          SetStroke _env cv -> do
            c <- eval cv
            emit (SetStroke EmptyEnvProxy c)
          SetFill _env cv -> do
            c <- eval cv
            emit (SetFill EmptyEnvProxy c)
          Clear _env -> emit (Clear EmptyEnvProxy)
          Write _env txtv ptv -> do
            txt <- eval txtv
            pt <- eval ptv
            emit (Write EmptyEnvProxy txt pt)

  let getDrawCommands = do
        tryTakeMVar dcChanged >>= \case
          Nothing -> pure ()
          Just _  -> putMVar dcChanged False
        tryReadMVar layersMVar >>= \case
          Nothing -> pure [[]]
          Just m  -> pure (map reverse (Map.elems m))

      drawCommandsChanged = tryReadMVar dcChanged >>= \case
        Nothing -> pure True
        Just tf -> pure tf
  pure (getDrawCommands, drawCommandsChanged, drawTo)

runExceptTIO :: ExceptT String IO a -> IO a
runExceptTIO action = runExceptT action >>= \case
  Right result -> pure result
  Left err     -> throwIO (BadProject "there was a problem building the viewer's configuration values." err)
-}
