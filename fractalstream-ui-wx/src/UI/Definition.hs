{-# language OverloadedStrings #-}
{-# options_ghc -Wno-orphans #-}

module UI.Definition where

import Language.Environment
import Language.Type
import Language.Value hiding (get)
import Language.Code.Parser
import Language.Value.Parser (parseValue, parseUntypedValue, Splice)
import Language.Value.Evaluator (HaskellTypeOfBinding, evaluate)
import qualified Language.Effect as Effect
import Control.Concurrent.MVar
import Control.Applicative

import GHC.TypeLits
import Control.Monad.Except
import Fcf (Eval, Exp)
import Data.Kind
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.String (IsString(..))

import Data.Aeson
import qualified Data.Yaml as YAML

import Graphics.UI.WX hiding (glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WX as WX

import Data.Color (Color)

defToUI :: FilePath -> IO ()
defToUI yamlFile = start $ do

  ensemble <- parseEnsembleFile yamlFile
  result <- runExceptT (buildEnsemble ensemble)
  case result of
    Left e -> error ("ERROR: " ++ e)
    Right _ -> putStrLn ("OKOKOK")

  Just setup <- pure (ensembleSetup ensemble)
  uiResult <- runExceptT (allocateDummyUIValues (coContents setup))

  case uiResult of
    Left e -> error ("ERROR2: " ++ e)
    Right ui -> do
      configFrame <- frame [ text := coTitle setup
                           , on resize := propagateEvent
                           ]
      innerLayout <- generateWxLayout setTextOnly configFrame ui
      compileButton <- button configFrame [ text := "Go!" ]
      set configFrame [ layout := fill
                                $ margin 5
                                $ column 5
                                $ [ innerLayout
                                  , widget compileButton
                                  ] ]

data Ensemble = Ensemble
  { ensembleSetup :: Maybe Configuration
  , ensembleConfiguration :: Maybe Configuration
  , ensembleViewers :: [ComplexViewer]
  }

parseEnsembleFile :: String -> IO Ensemble
parseEnsembleFile filePath = YAML.decodeFileThrow filePath

instance FromJSON Ensemble where
  parseJSON = withObject "ensemble" $ \o -> do
    ensembleSetup <- o .:? "setup"
    ensembleConfiguration <- o .:? "configuration"
    singleViewer <- o .:? "viewer"
    ensembleViewers <- case singleViewer of
      Just viewer -> pure [viewer]
      Nothing -> o .:? "viewers" .!= []
    pure Ensemble{..}

instance FromJSON Configuration where
  parseJSON = withObject "configuration" $ \o -> do
    coTitle <- o .: "title"
    Dimensions coSize <- o .: "size"
    coContents <- parseLayout o
    pure Configuration{..}

instance FromJSON (Layout Dummy) where
  parseJSON = withObject "layout" parseLayout

parseLayout :: Object -> YAML.Parser (Layout Dummy)
parseLayout o
  =   (Vertical <$> (o .: "vertical-contents"))
  <|> (Horizontal <$> (o .: "horizontal-contents"))
  <|> (uncurry Panel <$> (titled =<< (o .: "panel")))
  <|> (Tabbed <$> ((o .: "tabbed") >>= mapM titled))
  <|> (textBoxLayout =<< (o .: "text-entry"))
  <|> (checkBoxLayout =<< (o .: "checkbox"))
  <|> (colorPickerLayout =<< (o .: "color-picker"))
  <|> fail "bad layout description"
 where
   titled p = (,) <$> (p .: "title") <*> parseLayout p

   textBoxLayout p = do
     lab <- p .: "label"
     StringOrNumber varValue <- p .: "value"
     (p .: "type") >>= \(SomeType ty) -> do
       varVariable <- p .: "variable"
       pure (TextBox lab ty (Dummy YamlVar{..}))

   checkBoxLayout p = do
     lab <- p .: "label"
     val <- p .: "value"
     let varValue = if val then "true" else "false"
     varVariable <- p .: "variable"
     pure (CheckBox lab (Dummy YamlVar{..}))

   colorPickerLayout p = do
     lab <- p .: "label"
     varValue <- p .: "value"
     varVariable <- p .: "variable"
     pure (ColorPicker lab (Dummy YamlVar{..}))

newtype StringOrNumber t = StringOrNumber t

instance (IsString s) => FromJSON (StringOrNumber s) where
  parseJSON v
    =   (withText "string" (pure . StringOrNumber . fromString . Text.unpack) v)
    <|> (withScientific "number" (pure . StringOrNumber . fromString . show) v)

instance FromJSON ComplexViewer where
  parseJSON = withObject "complex viewer" $ \o -> do
    cvTitle <- o .: "title"
    Dimensions cvSize <- o .: "size"
    cvCanResize <- o .:? "resizable" .!= True
    cvCoord <- o .: "z-coord"
    cvPixel <- o .:? "pixel-size"
    StringOrNumber cvCenter <- o .: "initial-center"
    StringOrNumber cvPixelSize <- o .: "initial-pixel-size"
    cvCode <- Text.unpack <$> o .: "code"
    cvOverlay <- o .:? "overlay"
    cvOnSelect <- o .:? "on-select"
    pure ComplexViewer{..}

instance FromJSON (StringOf t) where
  parseJSON = fmap StringOf . parseJSON

instance FromJSON Label where
  parseJSON = withText "label" (pure . Label . Text.unpack)

instance FromJSON SomeType where
  parseJSON = withText "type" $ \case
    "C"       -> pure $ SomeType ComplexType
    "R"       -> pure $ SomeType RealType
    "Z"       -> pure $ SomeType IntegerType
    "N"       -> pure $ SomeType IntegerType
    "Boolean" -> pure $ SomeType BooleanType
    "2"       -> pure $ SomeType BooleanType
    "Q"       -> pure $ SomeType RationalType
    "Color"   -> pure $ SomeType ColorType
    _         -> fail "unknown type"

data Dimensions = Dimensions (Int, Int)

instance FromJSON Dimensions where
  parseJSON = withText "dimensions" $ \txt -> do
    case Text.splitOn "x" txt of
      [xStr, yStr] -> do
        case (,) <$> readMaybe (Text.unpack xStr) <*> readMaybe (Text.unpack yStr) of
          Just dim -> pure (Dimensions dim)
          Nothing  -> fail "could not parse dimension descriptor"
      _ -> fail "expected a dimension descriptor, e.g. 400x200"

buildEnsemble :: Ensemble -> ExceptT String IO ()
buildEnsemble Ensemble{..} =
  withConfigurationEnv ensembleConfiguration EmptyEnvProxy $ \env0 -> do
    lift (putStrLn (maybe "n/a" (show . allBindings . coContents) ensembleConfiguration))
    lift (putStrLn (maybe "n/a" (show . allBindings . coContents) ensembleSetup))
    let effectParser = Effect.EP Effect.NoEffs

    forM_ ensembleViewers $ \ComplexViewer{..} ->
      (bindInEnv cvCoord ComplexType env0 $ \env ->
        withConfigurationSplices ensembleSetup env $ \splices -> do
          lift (putStrLn cvCode)
          case parseCode effectParser env splices ColorType cvCode of
            Left err -> fail ("bad parse: " ++ show err)
            Right code -> (lift $ print code)) :: ExceptT String IO ()

-- | Allocate UI glue to read and write to configuration values
newtype UIValue t = UIValue (MVar (t, [t -> t -> IO ()]))

newtype UIValueRepr t = UIValueRepr (UIValue (String, t))

newUIValue :: MonadIO m => t -> m (UIValue t)
newUIValue v = UIValue <$> liftIO (newMVar (v, []))

onChange :: MonadIO m => UIValue t -> (t -> t -> IO ()) -> m ()
onChange (UIValue glue) action =
  liftIO (modifyMVar_ glue (\(v, actions) -> pure (v, action:actions)))

modifyUIValue :: MonadIO m => UIValue t -> (t -> t) -> m ()
modifyUIValue (UIValue glue) f = liftIO $ modifyMVar_ glue $ \(old, actions) -> do
  -- Run the on-changed handlers while holding the lock, which will
  -- prevent another change to this element from performing its callbacks
  -- until this one is finished.
  -- NOTE: if any of these handlers change this same element, then they
  --       they will deadlock. If they must set this element, they should
  --       fork a thread to do it asynchronously, and not wait for completion.
  let !new = f old
  forM_ actions (\action -> action old new)
  pure (new, actions)

setUIValue :: MonadIO m => UIValue t -> t -> m ()
setUIValue v = liftIO . modifyUIValue v . const

getUIValue :: MonadIO m => UIValue t -> m t
getUIValue (UIValue glue) = fst <$> liftIO (readMVar glue)

allocateUIValues :: (forall ty. TypeProxy ty -> String -> Either String (HaskellType ty))
                 -> Layout Dummy
                 -> ExceptT String IO (Layout UIValueRepr) -- aka UILayout
allocateUIValues pValue = go
  where
    go = \case
      Vertical xs -> Vertical <$> mapM go xs
      Horizontal xs -> Horizontal <$> mapM go xs
      Panel lab x -> Panel lab <$> go x
      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps
      TextBox lab ty (Dummy YamlVar{..}) ->
        case pValue ty varValue of
          Left err -> fail err
          Right v  -> TextBox lab ty . UIValueRepr <$> newUIValue (varValue, v)
      CheckBox lab (Dummy YamlVar{..}) ->
        case pValue BooleanType varValue of
          Left err -> fail err
          Right v  -> CheckBox lab . UIValueRepr <$> newUIValue (varValue, v)
      ColorPicker lab (Dummy YamlVar{..}) ->
        case pValue ColorType varValue of
          Left err -> fail err
          Right v  -> ColorPicker lab . UIValueRepr <$> newUIValue (varValue, v)

allocateDummyUIValues :: Layout Dummy -> ExceptT String IO (Layout UIValueRepr)
allocateDummyUIValues = go
  where
    go = \case
      Vertical xs -> Vertical <$> mapM go xs
      Horizontal xs -> Horizontal <$> mapM go xs
      Panel lab x -> Panel lab <$> go x
      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps
      TextBox lab ty (Dummy YamlVar{..}) ->
        fmap (TextBox lab ty . UIValueRepr)
          $ case ty of
              ComplexType -> newUIValue (varValue, 0)
              RealType    -> newUIValue (varValue, 0)
              IntegerType -> newUIValue (varValue, 0)
              BooleanType -> newUIValue (varValue, False)
              _ -> fail "todo, textbox"
      CheckBox lab (Dummy YamlVar{..}) ->
        CheckBox lab . UIValueRepr <$> newUIValue (varValue, False)
      ColorPicker lab (Dummy YamlVar{}) ->
        ColorPicker lab . UIValueRepr <$> fail "todo, colorpicker"

data UIValueRepr_ :: Symbol -> FSType -> Exp Type
type instance Eval (UIValueRepr_ name ty) = UIValueRepr (HaskellType ty)

{-
withConfigurationGlue :: Configuration
                         -> (forall splices. Context UIValue_ splices -> IO t)
                         -> IO t
withConfigurationGlue Configuration{..} action = do
  _
-}

withConfigurationEnv :: forall m t env
                           . MonadFail m
                          => Maybe Configuration
                          -> EnvironmentProxy env
                          -> (forall env'. EnvironmentProxy env' -> m t)
                          -> m t
withConfigurationEnv Nothing env0 k = k env0
withConfigurationEnv (Just Configuration{..}) env0 k
   = go (allBindings coContents) env0
 where
   go :: forall e. [(String, SomeType)] -> EnvironmentProxy e -> m t
   go [] env = k env
   go ((nameStr, SomeType ty) : etc) env =
     bindInEnv nameStr ty env (go etc)

withConfigurationSplices :: forall m t env
                          . MonadFail m
                         => Maybe Configuration
                         -> EnvironmentProxy env
                         -> (forall splices. Context (Splice env) splices -> m t)
                         -> m t
withConfigurationSplices Nothing _env k = k EmptyContext
withConfigurationSplices (Just Configuration{..}) _env k
   = go (allBindingVars coContents) EmptyEnvProxy EmptyContext
 where
   go :: forall e. [(YamlVar, SomeType)] -> EnvironmentProxy e -> Context (Splice env) e ->  m t
   go [] _ ctx = k ctx
   go ((YamlVar valStr nameStr, SomeType ty) : etc) ctxEnv ctx =
     case someSymbolVal nameStr of
       SomeSymbol name -> bindInEnv' name ty ctxEnv $ \ctxEnv' ->
         case parseUntypedValue valStr of -- case parseValue env EmptyContext ty valStr of
           Left e -> fail ("parse error when parsing config argument `" <> nameStr <> "`: " <> show (snd e))
           Right v -> go etc ctxEnv' (Bind name ty v ctx)

bindInEnv :: (MonadFail m)
          => String
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (forall name. NotPresent name env => EnvironmentProxy ( '(name, ty) ': env) -> m t)
          -> m t
bindInEnv nameStr ty env k = case someSymbolVal nameStr of
  SomeSymbol name -> case lookupEnv' name env of
    Absent' proof -> recallIsAbsent proof (k (bindNameEnv name ty proof env))
    _ -> fail (symbolVal name <> " is defined twice")

bindInEnv' :: (MonadFail m, KnownSymbol name)
          => Proxy name
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (NotPresent name env => EnvironmentProxy ( '(name, ty) ': env) -> m t)
          -> m t
bindInEnv' name ty env k = case lookupEnv' name env of
  Absent' proof -> recallIsAbsent proof (k (bindNameEnv name ty proof env))
  _ -> fail (symbolVal name <> " is defined twice")

data ComplexViewer = ComplexViewer
  { cvTitle :: String
  , cvSize :: (Int, Int)
  , cvCanResize :: Bool
  , cvCenter :: StringOf 'ComplexT
  , cvPixelSize :: StringOf 'RealT
  , cvCoord :: String
  , cvPixel :: Maybe String
  , cvCode :: String
  , cvOverlay :: Maybe String
  , cvOnSelect :: Maybe String
  }

data Configuration = Configuration
  { coTitle :: String
  , coSize :: (Int, Int)
  , coContents :: Layout Dummy
  }

newtype StringOf (t :: FSType) = StringOf String
  deriving IsString

data Dummy t = Dummy YamlVar
  deriving Show

data YamlVar = YamlVar
  { varValue :: String
  , varVariable :: String
  }
  deriving Show

allBindings :: Layout Dummy -> [(String, SomeType)]
allBindings = map (\(x,t) -> (varVariable x, t)) . allBindingVars

allBindingVars :: Layout Dummy -> [(YamlVar, SomeType)]
allBindingVars = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ ty (Dummy x) -> [(x, SomeType ty)]
      CheckBox _ (Dummy x) -> [(x, SomeType BooleanType)]
      ColorPicker _ (Dummy x) -> [(x, SomeType ColorType)]

newtype Label = Label String

data Layout f where
  Vertical :: [Layout f] -> Layout f
  Horizontal :: [Layout f] -> Layout f
  Panel :: String -> Layout f -> Layout f
  Tabbed :: [(String, Layout f)] -> Layout f
  TextBox :: Label -> TypeProxy ty -> f (HaskellType ty) -> Layout f
  CheckBox :: Label -> f Bool -> Layout f
  ColorPicker :: Label -> f Color -> Layout f

type UILayout = Layout UIValueRepr

generateWxLayout :: (forall ty. TypeProxy ty
                           -> String
                           -> UIValue (String, HaskellType ty)
                           -> IO (Maybe String))
              -> Window a
              -> UILayout
              -> IO WX.Layout

generateWxLayout setValueText frame0 wLayout = do
  panel0 <- panel frame0 []
  computedLayout <- go panel0 wLayout
  pure (container panel0 computedLayout)

 where

   go p = \case

     Panel _pTitle inner -> do
       p' <- panel p []
       go p' inner
       pure (fill $ widget p')

     Vertical parts ->
       fill . column 5 <$> mapM (go p) parts

     Horizontal parts ->
         fill
       . hstretch
       . margin 10
       . row 5
       <$> mapM (go p) parts

     Tabbed _ -> error "Todo, tabbed"

     ColorPicker{} -> error "todo, colorpicker"

     CheckBox (Label lab) (UIValueRepr v) -> do
       initial <- snd <$> getUIValue v
       cb <- checkBox p [ text := lab
                        , checkable := True
                        , checked := initial
                        , visible := True
                        ]
       set cb [ on command := do
                  isChecked <- get cb checked
                  setUIValue v ("", isChecked)
              ]
       onChange v $ \_ (_, isChecked) -> set cb [ checked := isChecked ]
       pure (widget cb)

     TextBox (Label lab) ty (UIValueRepr v) -> do
       initial <- fst <$> getUIValue v
       te <- textEntry p [ text := initial
                         , processEnter := True
                         ]
       set te [ on command := do
                  newText <- get te text
                  setValueText ty newText v >>= \case
                    Nothing -> pure ()
                    Just _err -> error "bad parse of text field"
              ]
       onChange v $ \_ (newText, _) -> set te [ text := newText ]
       pure (fill $ row 5 [ label lab, hfill (widget te) ])

parseToHaskellValue :: Context HaskellTypeOfBinding env
                    -> TypeProxy ty
                    -> String
                    -> Either String (HaskellType ty)
parseToHaskellValue ctx ty input = do
  case parseValue (contextToEnv ctx) EmptyContext ty input of
    Left err -> Left (show err)
    Right v  -> pure (evaluate v ctx)

setTextOnly :: forall ty
             . TypeProxy ty
            -> String
            -> UIValue (String, HaskellType ty)
            -> IO (Maybe String)
setTextOnly _ s ui = do
  modifyUIValue ui (\(_, v) -> (s, v))
  pure Nothing

setToParsed :: forall ty
             . (TypeProxy ty -> String -> Either String (HaskellType ty))
            -> TypeProxy ty
            -> String
            -> UIValue (String, HaskellType ty)
            -> IO (Maybe String)
setToParsed parse ty input ui = do
  case parse ty input of
    Left err -> pure (Just err)
    Right v  -> setUIValue ui (input, v) >> pure Nothing
