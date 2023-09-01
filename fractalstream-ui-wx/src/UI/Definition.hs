{-# language OverloadedStrings, UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}

module UI.Definition where

import Language.Environment
import Language.Type
import Language.Value hiding (get)
import Language.Code (Code, let_)
import Language.Code.Parser
import Language.Value.Parser (parseValue, parseUntypedValue, Splice)
import qualified Language.Untyped.Value as U
import Language.Value.Evaluator (HaskellTypeOfBinding, evaluate)
import qualified Language.Effect as Effect
import Control.Concurrent.MVar
import Control.Applicative hiding (Const)
import Data.Indexed.Functor

import GHC.TypeLits
import Control.Monad.Except
import Data.Kind
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.String (IsString(..))
import Data.Maybe

import Data.Aeson hiding (Value)
import qualified Data.Yaml as YAML
import Fcf (Eval, Exp)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Graphics.UI.WX hiding (pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.Draw
import           Graphics.UI.WXCore.WxcClassTypes
import           Graphics.UI.WXCore.WxcTypes      (rgba)
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Planar
import UI.Tile
import Data.IORef
import Data.Word
import Foreign (Ptr)

import Data.Color (Color)

import Backend.LLVM (withViewerCode)

runExceptTIO :: ExceptT String IO a -> IO a
runExceptTIO = fmap (either error id) . runExceptT

defToUI :: FilePath -> IO ()
defToUI yamlFile = start $ do

  ensemble <- parseEnsembleFile yamlFile
  result <- runExceptT (buildEnsemble ensemble)
  case result of
    Left e -> error ("ERROR: " ++ e)
    Right _ -> putStrLn ("OKOKOK")

  -- TODO: verify that the code for each viewer, tool, etc works properly
  --       with the splices declared by the setup config. e.g. all code
  --       typechecks with the splice's types, each splice's environment
  --       is contained in the actual code environment at each use, etc.
  --
  --       If the ensemble passes this verification, then the end-user
  --       should not be able to cause a compilation failure via the
  --       UI.
  runEnsemble WX ensemble

runEnsemble :: forall ui. ToUI ui => ui -> Ensemble -> IO ()
runEnsemble ui Ensemble{..} = do
  -- Get a handle for the ensemble
  project <- newEnsemble ui

  -- Make the setup window and let it run
  let withSplicesFromSetup :: (forall splices. Context Splice splices -> IO ())
                           -> IO ()
      withSplicesFromSetup k = case ensembleSetup of
        Nothing -> k EmptyContext
        Just setup -> do
          setupUI <- runExceptTIO (allocateUIExpressions (coContents setup))
          runSetup ui project (coTitle setup) setupUI (withSplices setupUI k)

      withContextFromConfiguration :: (forall env. Context DynamicValue env -> IO ())
                                   -> IO ()
      withContextFromConfiguration k = case ensembleConfiguration of
        Nothing -> k EmptyContext
        Just config -> do
          configUI <- runExceptTIO (allocateUIConstants (coContents config))
          makeLayout ui project "Configuration" configUI
          withDynamicBindings configUI k

  withSplicesFromSetup $ \splices -> do
    putStrLn ("SPLICES: " ++ show (contextToEnv splices))
    putStrLn ("parsed config: " ++ show ensembleConfiguration)
    withContextFromConfiguration $ \config -> do
      putStrLn ("CONFIGURATION: " ++ show (contextToEnv config))
      [oneViewer] <- pure ensembleViewers
      case lookupEnv' (Proxy @"[internal argument] #blockSize") (contextToEnv config) of
        Absent' pf1 -> recallIsAbsent pf1 $
          case lookupEnv' (Proxy @"[internal argument] #subsamples") (contextToEnv config) of
            Absent' pf2 -> recallIsAbsent pf2 $ do
              withComplexViewer' config splices oneViewer $ \vu cv' -> do
                makeViewer ui project vu cv'
                putStrLn "made it!"
            _ -> error "internal error"
        _ -> error "internal error"

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m k = case m of
  Nothing -> pure ()
  Just x  -> k x


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
     varType <- p .: "type"
     varVariable <- p .: "variable"
     varEnv <- p .:? "environment" .!= Map.empty
     pure (TextBox lab (Dummy YamlVar{..}))

   checkBoxLayout p = do
     lab <- p .: "label"
     val <- p .: "value"
     let varValue = if val then "true" else "false"
         varType = SomeType BooleanType
         varEnv = Map.empty
     varVariable <- p .: "variable"
     pure (CheckBox lab (Dummy YamlVar{..}))

   colorPickerLayout p = do
     lab <- p .: "label"
     varValue <- p .: "value"
     varVariable <- p .: "variable"
     let varType = SomeType ColorType
         varEnv = Map.empty
     pure (ColorPicker lab (Dummy YamlVar{..}))

newtype StringOrNumber t = StringOrNumber { unStringOrNumber :: t }

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

instance KnownType t => FromJSON (StringOf t) where
  parseJSON = fmap unStringOrNumber . parseJSON

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
        withConfigurationSplices ensembleSetup $ \splices -> do
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

withConfigurationSplices :: forall m t
                          . MonadFail m
                         => Maybe Configuration
                         -> (forall splices. Context Splice splices -> m t)
                         -> m t
withConfigurationSplices Nothing k = k EmptyContext
withConfigurationSplices (Just Configuration{..}) k
   = go (allBindingVars coContents) EmptyEnvProxy EmptyContext
 where
   go :: forall e. [YamlVar] -> EnvironmentProxy e -> Context Splice e ->  m t
   go [] _ ctx = k ctx
   go ((YamlVar valStr (SomeType ty) envMap nameStr) : etc) ctxEnv ctx =
     case someSymbolVal nameStr of
       SomeSymbol name -> bindInEnv' name ty ctxEnv $ \ctxEnv' -> do
         let die e = fail $ "parse error when parsing config argument `" <>
               nameStr <> "`: " <> show (snd e)

         withEnvFromMap envMap $ \env ->
           -- see if we can parse this as a value at all
           case parseUntypedValue valStr of
             Left e -> die e
             Right v ->
               -- now see if we can parse at the right type,
               -- in the expected environment
               case parseValue env EmptyContext ty valStr of
                 Left e -> die e
                 Right _ -> go etc ctxEnv' (Bind name ty v ctx)

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

data ViewerUIProperties = ViewerUIProperties
  { vpTitle :: String
  , vpSize :: (Int, Int)
  , vpCanResize :: Bool
  }

data ComplexViewer' where
  ComplexViewer' :: forall z px env. (KnownSymbol z, KnownSymbol px) =>
    { cvCenter'    :: UIValue (Complex Double)
    , cvPixelSize' :: UIValue Double
    , cvConfig' :: Context DynamicValue env
    , cvCoord' :: Proxy z
    , cvPixel' :: Proxy px
    , cvCode' :: Code '[] ( '(px, 'RealT)
                         ': '(z, 'ComplexT)
                         ': '("[internal] x", 'RealT)
                         ': '("[internal] y", 'RealT)
                         ': '("[internal] px", 'RealT)
                         ': env
                          ) 'ColorT
    , cvGetFunction :: IO (Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
    } -> ComplexViewer'

withComplexViewer' :: ( NotPresent "[internal argument] #blockSize" env
                      , NotPresent "[internal argument] #subsamples" env )
                   => Context DynamicValue env
                   -> Context Splice splices
                   -> ComplexViewer
                   -> (ViewerUIProperties ->  ComplexViewer' -> IO t)
                   -> IO t
withComplexViewer' cvConfig' splices ComplexViewer{..} action = withEnvironment (contextToEnv cvConfig') $ do
  let cvPixelName = fromMaybe "#pixel" cvPixel
      argX = Proxy @"[internal] x"
      argY = Proxy @"[internal] y"
      argPx = Proxy @"[internal] px"
  case (someSymbolVal cvCoord, someSymbolVal cvPixelName) of
    (SomeSymbol cvCoord', SomeSymbol cvPixel') -> do
      let env0 = contextToEnv cvConfig'
      case lookupEnv argPx RealType env0 of
        Absent proof -> recallIsAbsent proof $ do
          let env1 = BindingProxy argPx RealType env0
          case lookupEnv argY ComplexType env1 of
            Absent proofY -> recallIsAbsent proofY $ do
              let env2 = BindingProxy argY RealType env1
              case lookupEnv argX RealType env2 of
                Absent proofX -> recallIsAbsent proofX $ do
                  let env3 = BindingProxy argX RealType env2
                  case lookupEnv cvCoord' ComplexType env3 of
                    Absent proof' -> recallIsAbsent proof' $ do
                      let env4 = BindingProxy cvCoord' ComplexType env3
                      case lookupEnv cvPixel' RealType env4 of
                        Absent proof'' -> recallIsAbsent proof'' $ do
                          let env = BindingProxy cvPixel' RealType env4

                          cvCenter' <- newUIValue (valueOf cvCenter)
                          cvPixelSize' <- newUIValue (valueOf cvPixelSize)

                          let vpTitle = cvTitle
                              vpSize  = cvSize
                              vpCanResize = cvCanResize

                          let effectParser = Effect.EP Effect.NoEffs

                          case parseCode effectParser env splices ColorType cvCode of
                            Left err -> error ("bad parse: " ++ show err)
                            Right cvCode' -> do
                              Found pfX <- pure (lookupEnv argX RealType env3)
                              Found pfY <- pure (lookupEnv argY RealType env3)
                              Found pfPx <- pure (lookupEnv argPx RealType env4)
                              let realCode = complexToReal pfX pfY pfPx cvCode'
                              withViewerCode argX argY argPx argPx realCode $ \fun -> do
                                let cvGetFunction = do
                                      args <- mapContextM (\_ _ -> getDynamic) cvConfig'
                                      pure $ \blockSize subsamples (x :+ y) (dx :+ _dy) buf -> do
                                        let fullArgs = Bind argX RealType x
                                                     $ Bind argY RealType y
                                                     $ Bind argPx RealType dx
                                                     $ args
                                        fun (fromIntegral blockSize) (fromIntegral subsamples) fullArgs buf
                                action ViewerUIProperties{..} ComplexViewer'{..}

                        _ -> error "could not define viewer-internal pixel size"
                    _ -> error "could not define viewer-internal Y coordinate"
                _ -> error "could not define viewer-internal X coordinate"

            _ -> error (cvCoord ++ " defined in both the viewer and the configuration")
        _ -> error (cvPixelName ++ " defined in both the viewer and the configuration")

complexToReal :: ( KnownType rt, KnownEnvironment env
                 , KnownSymbol reZ, KnownSymbol imZ, KnownSymbol vpx
                 , KnownSymbol z, KnownSymbol px
                 , NotPresent z env, NotPresent px ( '(z, 'ComplexT) ': env ) )
              => NameIsPresent reZ 'RealT env
              -> NameIsPresent imZ 'RealT env
              -> NameIsPresent vpx 'RealT ( '(z, 'ComplexT) ': env )
              -> Code eff ( '(px, 'RealT) ': '(z, 'ComplexT) ': env ) rt
              -> Code eff env rt
complexToReal reZ imZ vpx =
  let z = Fix (AddC (Fix (R2C (Fix (Var Proxy RealType reZ))))
                    (Fix (MulC (Fix (Const (Scalar ComplexType (0 :+ 1))))
                               (Fix (R2C (Fix (Var Proxy RealType imZ)))))))
      px = Fix (Var Proxy RealType vpx)
  in  let_ z  typeProxy
    . let_ px typeProxy

data Configuration = Configuration
  { coTitle :: String
  , coSize :: (Int, Int)
  , coContents :: Layout Dummy
  }
  deriving Show

newtype StringOf (t :: FSType) =
  StringOf { valueOf :: HaskellType t }

instance KnownType t => IsString (StringOf t) where
  fromString s =
    case parseValue EmptyEnvProxy EmptyContext (typeProxy @t) s of
      Left err -> error (show err)
      Right v  -> StringOf (evaluate v EmptyContext)

data Dummy t = Dummy YamlVar
  deriving Show

data YamlVar = YamlVar
  { varValue :: String
  , varType :: SomeType
  , varEnv :: Map String SomeType
  , varVariable :: String
  }
  deriving Show


allBindings :: Layout Dummy -> [(String, SomeType)]
allBindings = map (\YamlVar{..} -> (varVariable, varType)) . allBindingVars

allBindingVars :: Layout Dummy -> [YamlVar]
allBindingVars = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ (Dummy x) -> [x]
      CheckBox _ (Dummy x) -> [x]
      ColorPicker _ (Dummy x) -> [x]

extractAllBindings :: (forall t. f t -> a)
                   -> Layout f
                   -> [a]
extractAllBindings extractor = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ x -> [extractor x]
      CheckBox _ x -> [extractor x]
      ColorPicker _ x -> [extractor x]

newtype Label = Label String
  deriving Show

data Layout f
  = Vertical [Layout f]
  | Horizontal [Layout f]
  | Panel String (Layout f)
  | Tabbed [(String, Layout f)]
  | TextBox Label (f String)
  | CheckBox Label (f Bool)
  | ColorPicker Label (f Color)

deriving instance (Show (f String), Show (f Bool), Show (f Color)) => Show (Layout f)

class ToUI ui where
  type EnsembleHandle ui

  newEnsemble :: ui -> IO (EnsembleHandle ui)

  runSetup :: Dynamic dyn
           => ui
           -> EnsembleHandle ui
           -> String
           -> Layout dyn
           -> IO ()
           -> IO ()

  makeLayout :: Dynamic dyn
             => ui
             -> EnsembleHandle ui
             -> String
             -> Layout dyn
             -> IO ()

  makeViewer :: ui
             -> EnsembleHandle ui
             -> ViewerUIProperties
             -> ComplexViewer'
             -> IO ()

data WX = WX

instance ToUI WX where
  type EnsembleHandle WX = ()

  newEnsemble _ = pure ()

  runSetup _ _ title setupUI continue = do
    f <- frame [ text := title
               , on resize := propagateEvent
               ]

    innerLayout <- generateWxLayout f setupUI
    compileButton <- button f [ text := "Go!"
                              , on command := do
                                  set f [ visible := False ]
                                  continue
                              ]
    set f [ layout := fill . margin 5 . column 5
                      $ [ innerLayout, widget compileButton ]
          ]

  makeLayout _ _ title ui = do
    f <- frame [ text := title
               , on resize := propagateEvent
               ]

    innerLayout <- generateWxLayout f ui
    set f [ layout := fill . margin 5 . column 5 $ [ innerLayout ] ]

  makeViewer _ _ ViewerUIProperties{..} ComplexViewer'{..} = do
    f <- frame [ text := vpTitle
               , on resize := propagateEvent
               ]
    let (width, height) = (fst vpSize, snd vpSize)
    p <- panel f [ clientSize := sz width height ]


    status <- statusField [text := "Pointer location"]
    set f [ statusBar := [status]
          , layout := fill (minsize (sz 128 128) (widget p))
          ]

   -- trigger repaint
    let triggerRepaint = do
          repaint p
          windowRefresh p True -- True=redraw background
          windowUpdateWindowUI p

    model <- variable [value := Model (0,0) (1/128,1/128)]
    renderId <- newIORef (0 :: Int)

    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]
    pendingResize <- variable [value := False]

    viewerTile     <- do
      renderAction <- cvGetFunction
      renderTile' renderId renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    lastTileImage  <- variable [value := Nothing]
    animate        <- variable [value := Nothing]

    let startAnimatingFrom oldModel = do
            now <- getCurrentTime
            img <- get savedTileImage value >>= traverse imageCopy
            set lastTileImage [value := img]
            set animate [value := Just (now, oldModel, img)]

    -- Set paint handlers
    set p [ on paintRaw := \dc r _dirty -> get animate value >>= \case
              Nothing -> do
                -- Normal paint. Draw current rendering, then layer
                -- tool imagery on top.
                viewRect <- windowGetViewRect f
                curTile <- get currentTile value
                let (w, h) = tileRect curTile
                get savedTileImage value >>= \case
                  Nothing -> pure ()
                  Just im -> drawCenteredImage im dc viewRect (w, h)
                paintToolLayer lastClick draggedTo dc r viewRect

              Just (startTime, oldModel, oldImage) -> do
                -- Animated paint. Zoom and blend smoothly between
                -- the new and old images.
                now <- getCurrentTime
                let speed :: forall n. Num n => n
                    speed = 6
                    blend = min 255 (round (speed * 255 * toRational (diffUTCTime now startTime)) :: Integer)
                    t = min 1.0 (speed * fromRational (toRational (diffUTCTime now startTime)) :: Double)
                when (blend >= 255) (set animate [value := Nothing])
                curTile <- get currentTile value
                let (w, h) = tileRect curTile

                gc <- graphicsContextCreate dc
                newModel <- get model value
                let midModel = interpolateModel t oldModel newModel
                    withLayer opacity action = do
                      graphicsContextBeginLayer gc opacity
                      action
                      graphicsContextEndLayer gc
                    restoringContext action = do
                      graphicsContextPushState gc
                      action
                      graphicsContextPopState gc

                let zoom :: (Double, Double) -> (Double, Double) -> IO () -> IO ()
                    zoom (scaleX, scaleY) (cx, cy) action = do
                      restoringContext $ do
                        graphicsContextTranslate gc cx cy
                        graphicsContextScale gc (sz scaleX scaleY)
                        action

                    viewCenterX = fromIntegral w / 2
                    viewCenterY = fromIntegral h / 2
                    dx = (fst (modelCenter newModel) - fst (modelCenter oldModel))
                         / fst (modelPixelDim oldModel)
                    dy = negate (snd (modelCenter newModel) - snd (modelCenter oldModel))
                         / snd (modelPixelDim oldModel)

                      -- draw the old image
                let k = 1 / sqrt ( (fst (modelPixelDim oldModel) * snd (modelPixelDim oldModel))
                                   / (fst (modelPixelDim newModel) * snd (modelPixelDim newModel)))
                    t' = if (k - 1)^(2 :: Int) < 0.05 then t else (1 - k ** t) / (1 - k)
                graphicsContextTranslate gc viewCenterX viewCenterY
                graphicsContextScale gc (sz (fst (modelPixelDim oldModel)
                                              / fst (modelPixelDim midModel))
                                            (snd (modelPixelDim oldModel)
                                              / snd (modelPixelDim midModel)))
                graphicsContextTranslate gc (negate viewCenterX) (negate viewCenterY)
                graphicsContextTranslate gc (negate $ dx * t') (negate $ dy * t')

                withLayer 1 $ restoringContext $ do
                  zoom (1.0, 1.0) (viewCenterX, viewCenterY) $
                    case oldImage of
                      Nothing -> pure ()
                      Just im -> drawImage dc im (WX.pt
                                                   (round $ negate viewCenterX)
                                                   (round $ negate viewCenterY))
                                 []
                -- draw the new image
                withLayer (min 1 t) $ do
                  let zoomRatioX = fst (modelPixelDim newModel) / fst (modelPixelDim oldModel)
                      zoomRatioY = snd (modelPixelDim newModel) / snd (modelPixelDim oldModel)
                  restoringContext $ do
                    zoom (zoomRatioX, zoomRatioY)
                         (viewCenterX + dx, viewCenterY + dy)
                      $ get savedTileImage value >>= \case
                          Nothing -> pure ()
                          Just im -> drawImage dc im (WX.pt
                                                       (round $ negate viewCenterX)
                                                       (round $ negate viewCenterY)) []
          ]

    let viewToModel pt = do
            Size { sizeW = w, sizeH = h } <- get f clientSize
            let dim = (w,h)
                fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
            modelRect <- modelToRect @(Double,Double) dim <$> get model value
            pure (convertRect fullViewRect modelRect $ Viewport (pointX pt, pointY pt))

    -- Set click and drag event handlers
    set p [ on mouse   := \case
              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers -> do
                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing  -> do
                        -- Completed a click, recenter to the clicked point.
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        oldModel <- get model value
                        newCenter <- viewToModel pt
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter }]

                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]

                        startAnimatingFrom oldModel
                        triggerRepaint
                    Just box -> do
                        -- Completed a drag. Zoom in to the dragged box, unless
                        -- the box is pathologically small; in that case, treat
                        -- the action as if it were a simple click.
                        selectRegion box
                        oldModel <- get model value
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        newCenter <- viewToModel (viewportToPoint $ rectCenter box)
                        let (px, py) = modelPixelDim oldModel
                            (boxW, boxH) = dimensions box
                            oldArea = fromIntegral (w * h)
                            newArea = boxW * boxH
                            literalScale = sqrt (newArea / oldArea)
                            scale = if literalScale < 0.001 then 1 else literalScale
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter
                                    , modelPixelDim = (px * scale, py * scale)
                                    }]
                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]
                        startAnimatingFrom oldModel
                        triggerRepaint

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                mpt <- viewToModel pt
                set status [text := show mpt]

                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing -> return ()
                    Just _  -> triggerRepaint

                propagateEvent

              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                set status [text := show mpt]
                propagateEvent

              -- other mouse events
              _ -> propagateEvent
          ]

    -- Add a timer which will check for repainting requests, ~10Hz
    _ <- timer f [ interval := 100
                 , enabled := True
                 , on command := do
                        curTile <- get currentTile value
                        ifModified curTile $ do
                            viewRect <- windowGetViewRect f
                            tileImage <- generateTileImage curTile viewRect
                            saved <- imageCopy tileImage
                            set savedTileImage [value := Just saved]
                            triggerRepaint
                 ]

    -- Animation timer. At ~65Hz, check if we are animating between
    -- two views. If so, step the animation and repaint.
    _ <- timer f [ interval := 16
                 , enabled := True
                 , on command := get animate value >>= \case
                         Nothing -> pure ()
                         Just _  -> triggerRepaint
                 ]

    -- onResizeTimer is a one-shot timer that fires 100ms after the
    -- frame has been resized. If another resize event comes in during
    -- that interval, the timer is reset to 100ms. When the timer fires,
    -- we kick off a new rendering task to build the contents of the
    -- window. Using a timer lets us avoid starting hundreds of rendering
    -- tasks while the user adjusts their window size.
    onResizeTimer <- timer f [ interval := 100
                             , enabled := False ]
    set onResizeTimer [ on command := do
                              set onResizeTimer [enabled := False] -- one-shot
                              needResize <- get pendingResize value
                              when needResize $ do
                                  set pendingResize [value := False]
                                  Size { sizeW = w0, sizeH = h0 } <- get f clientSize
                                  let w = roundUp w0 16
                                      h = roundUp h0 16
                                      roundUp x n = case x `mod` n of
                                          0 -> x
                                          k -> x + (n - k)
                                  get currentTile value >>= cancelTile
                                  renderAction <- cvGetFunction
                                  newViewerTile <- renderTile' renderId renderAction (w, h) model
                                  set currentTile [value := newViewerTile]
                                  -- no animation?
                                  triggerRepaint ]

    set f [ on resize := do
                  set onResizeTimer [enabled := False]
                  set pendingResize [value := True]
                  set onResizeTimer [enabled := True]
                  propagateEvent ]

data Model = Model
  { modelCenter   :: (Double, Double)
  , modelPixelDim :: (Double, Double)
  }

modelToRect :: Planar a => (Int,Int) -> Model -> Rectangle a
modelToRect (w,h) Model{..} = flippedRectangle (fromCoords ul) (fromCoords lr)
  where
    ul = (cx - px * fromIntegral w / 2, cy - py * fromIntegral h / 2)
    lr = (cx + px * fromIntegral w / 2, cy + py * fromIntegral h / 2)
    (cx, cy) = modelCenter
    (px, py) = modelPixelDim

interpolateModel :: Double -> Model -> Model -> Model
interpolateModel t m1 m2 = m2
    { modelCenter   = interpolate modelCenter
    , modelPixelDim = logInterpolate modelPixelDim
    }
  where
    interp p q = p + t * (q - p)
    interp2 (p1,p2) (q1,q2) = (interp p1 q1, interp p2 q2)
    interpolate f = interp2 (f m1) (f m2)
    logInterp p q = p * (q/p) ** t
    logInterp2  (p1,p2) (q1,q2) = (logInterp p1 q1, logInterp p2 q2)
    logInterpolate f = logInterp2 (f m1) (f m2)


generateWxLayout :: Dynamic dyn
                 => Window a
                 -> Layout dyn
                 -> IO WX.Layout

generateWxLayout frame0 wLayout = do
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

     CheckBox (Label lab) v -> do
       initial <- getDynamic v
       cb <- checkBox p [ text := lab
                        , checkable := True
                        , checked := initial
                        , visible := True
                        ]
       set cb [ on command := do
                  isChecked <- get cb checked
                  void (setDynamic v isChecked)
              ]
       listenWith v (\_ isChecked -> set cb [ checked := isChecked ])
       pure (widget cb)

     TextBox (Label lab) v -> do
       initial <- getDynamic v
       te <- textEntry p [ text := initial
                         , processEnter := True
                         , tooltip := ""
                         ]
       normalBG <- get te bgcolor
       set te [ on command := do
                  newText <- get te text
                  setDynamic v newText >>= \case
                    Nothing -> set te [ bgcolor := normalBG
                                      , tooltip := "" ]
                    Just err -> do
                      set te [ bgcolor := rgb 160 100 (100 :: Int)
                             , tooltip := unlines
                                 [ "Could not parse an expression"
                                 , ""
                                 , show err ]
                             ]
              ]
       listenWith v (\_ newText -> set te [ text := newText ])
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


class Dynamic (e :: Type -> Type) where
  getDynamic :: e t -> IO t
  setDynamic :: e t -> t -> IO (Maybe String)
  listenWith :: e t -> (t -> t -> IO ()) -> IO ()

instance Dynamic UIValue where
  getDynamic = getUIValue
  setDynamic d v = setUIValue d v >> pure Nothing
  listenWith = onChange

data SomeDynamic t where
  SomeDynamic :: forall dyn t. Dynamic dyn => dyn t -> SomeDynamic t

instance Dynamic SomeDynamic where
  getDynamic (SomeDynamic d) = getDynamic d
  setDynamic (SomeDynamic d) = setDynamic d
  listenWith (SomeDynamic d) = listenWith d

data Expression t where
  Expression :: forall env ty
              . String
             -> EnvironmentProxy env
             -> TypeProxy ty
             -> UIValue (String, Value '(env, ty))
             -> Expression String
  BoolExpression :: String -> UIValue Bool -> Expression Bool
  ColorExpression :: String -> UIValue Color -> Expression Color

instance Dynamic Expression where
  getDynamic = \case
    BoolExpression _ b -> getDynamic b
    ColorExpression _ c -> getDynamic c
    Expression _ _ _ v -> fst <$> getDynamic v

  setDynamic d new = case d of
    BoolExpression _ b -> setDynamic b new
    ColorExpression _ c -> setDynamic c new
    Expression _ env ty v -> do
      case parseValue env EmptyContext ty new of
        Left (_, err) -> pure (Just (show err))
        Right newV    -> do
          setDynamic v (new, newV)
          pure Nothing

  listenWith d action = case d of
    BoolExpression _ b -> listenWith b action
    ColorExpression _ c -> listenWith c action
    Expression _ _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))

data ConstantExpression t where
  ConstantExpression :: forall ty
                      . String
                     -> TypeProxy ty
                     -> UIValue (String, HaskellType ty)
                     -> ConstantExpression String
  ConstantBoolExpression :: String -> UIValue Bool -> ConstantExpression Bool
  ConstantColorExpression :: String -> UIValue Color -> ConstantExpression Color

instance Dynamic ConstantExpression where
  getDynamic = \case
    ConstantBoolExpression _ b -> getDynamic b
    ConstantColorExpression _ c -> getDynamic c
    ConstantExpression _ _ v -> fst <$> getDynamic v

  setDynamic d new = case d of
    ConstantBoolExpression _ b -> setDynamic b new
    ConstantColorExpression _ c -> setDynamic c new
    ConstantExpression _ ty v -> do
      case parseValue EmptyEnvProxy EmptyContext ty new of
        Left (_, err) -> pure (Just (show err))
        Right newV    -> do
          setDynamic v (new, evaluate newV EmptyContext)
          pure Nothing

  listenWith d action = case d of
    ConstantBoolExpression _ b -> listenWith b action
    ConstantColorExpression _ c -> listenWith c action
    ConstantExpression _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))

allocateUIExpressions :: Layout Dummy
                      -> ExceptT String IO (Layout Expression)
allocateUIExpressions = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy YamlVar{..}) -> case varType of
        SomeType ty ->
          withEnvFromMap varEnv $ \env ->
            case parseValue env EmptyContext ty varValue of
              Left (_, err) -> fail (show err)
              Right v  ->
                TextBox lab . Expression varVariable env ty
                <$> newUIValue (varValue, v)

      CheckBox lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext BooleanType varValue of
          Left (_, err) -> fail (show err)
          Right v  -> CheckBox lab . BoolExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext ColorType varValue of
          Left (_, err) -> fail (show err)
          Right v  -> ColorPicker lab . ColorExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

allocateUIConstants :: Layout Dummy
                    -> ExceptT String IO (Layout ConstantExpression)
allocateUIConstants = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy YamlVar{..}) -> case varType of
        SomeType ty ->
          case parseValue EmptyEnvProxy EmptyContext ty varValue of
            Left (_, err) -> fail (show err)
            Right v       -> TextBox lab . ConstantExpression varVariable ty
                             <$> newUIValue (varValue, evaluate v EmptyContext)

      CheckBox lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext BooleanType varValue of
          Left (_, err) -> fail (show err)
          Right v       -> CheckBox lab . ConstantBoolExpression varVariable
                           <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext ColorType varValue of
          Left (_, err) -> fail (show err)
          Right v       -> ColorPicker lab . ConstantColorExpression varVariable
                           <$> newUIValue (evaluate v EmptyContext)

data DynamicValue :: Symbol -> FSType -> Exp Type
type instance Eval (DynamicValue name ty) = SomeDynamic (HaskellType ty)

data SomeUIValue where
  SomeUIValue :: forall name ty
               . (KnownSymbol name)
              => Proxy name
              -> TypeProxy ty
              -> SomeDynamic (HaskellType ty)
              -> SomeUIValue

data SomeUIExpr where
  SomeUIExpr :: forall name ty
               . (KnownSymbol name)
              => Proxy name
              -> TypeProxy ty
              -> IO U.Value
              -> SomeUIExpr

withSplices :: forall t
             . Layout Expression
            -> (forall splices. Context Splice splices -> IO t)
            -> IO t
withSplices lo action = go EmptyContext (extractAllBindings toSomeUIExpr lo)
  where
    go :: forall splices. Context Splice splices -> [SomeUIExpr] -> IO t
    go ctx = \case
      [] -> action ctx
      (SomeUIExpr name ty getExpr : etc) ->
        case lookupEnv name ty (contextToEnv ctx) of
          Absent proof -> recallIsAbsent proof $ do
            expr <- getExpr
            go (Bind name ty expr ctx) etc
          _ -> error ("`" ++ symbolVal name ++ "` is re-defined")

    pValue :: String -> U.Value
    pValue input = case parseUntypedValue input of
      Left err -> error (show err)
      Right v  -> v

    toSomeUIExpr :: forall a. Expression a -> SomeUIExpr
    toSomeUIExpr = \case
      Expression nameStr _ ty v ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ty (pValue . fst <$> getDynamic v)
      BoolExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name BooleanType (pValue . showValue BooleanType <$> getDynamic b)
      ColorExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ColorType (pValue . showValue ColorType <$> getDynamic b)

withDynamicBindings :: forall t
                     . Layout ConstantExpression
                    -> (forall env. Context DynamicValue env -> t)
                    -> t
withDynamicBindings lo action =
    go EmptyContext (extractAllBindings toSomeUIValue lo)
  where
    go :: forall env. Context DynamicValue env -> [SomeUIValue] -> t
    go ctx [] = action ctx
    go ctx ( (SomeUIValue name ty dyn) : xs ) =
      case lookupEnv name ty (contextToEnv ctx) of
        Absent proof -> recallIsAbsent proof
                        $ go (Bind name ty dyn ctx) xs
        _ -> error ("`" ++ symbolVal name ++ "` is re-defined")

    toSomeUIValue :: forall a. ConstantExpression a -> SomeUIValue
    toSomeUIValue = \case
      ConstantExpression nameStr ty v ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name ty (SomeDynamic (ConstantExpression' ty v))
      ConstantBoolExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name BooleanType (SomeDynamic b)
      ConstantColorExpression nameStr c ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name ColorType (SomeDynamic c)

data ConstantExpression' t where
  ConstantExpression' :: forall ty t
                      . (t ~ HaskellType ty)
                     => TypeProxy ty
                     -> UIValue (String, t)
                     -> ConstantExpression' t

instance Dynamic ConstantExpression' where
  getDynamic (ConstantExpression' _ d) =
    snd <$> getDynamic d

  setDynamic (ConstantExpression' ty d) v =
    setDynamic d (showValue ty v, v)

  listenWith (ConstantExpression' _ d) action =
    listenWith d (\(_, old) (_, new) -> action old new)

renderTile' :: Valued w
            => IORef Int
            -> (Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId action dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    modelRect <- modelToRect dim <$> get model value
    let action' p q x y c = do
            curId <- readIORef renderId
            if (curId == iD) then action p q x y c else pure ()
    renderTile action' dim modelRect

drawCenteredImage :: Image b -> DC d -> Rect -> (Int,Int) -> IO ()

drawCenteredImage img dc windowRect (width, height) = do
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )
    drawImage dc img (WX.pt x0 y0) []

viewportToPoint :: Viewport -> Point
viewportToPoint (Viewport (x,y)) = Point { pointX = x, pointY = y }


getDragBox :: Var (Maybe Viewport)
           -> Var (Maybe Viewport)
           -> IO (Maybe (Rectangle Viewport))
getDragBox lastClick draggedTo = do
    dragSrc <- get lastClick value
    dragTgt <- get draggedTo value
    return $ case (dragSrc, dragTgt) of
        (Just p1, Just p2) -> Just $ rectangle p1 p2
        _                  -> Nothing

drawBox :: DC d
        -> WX.Color
        -> WX.Color
        -> [Point]
        -> IO ()
drawBox dc fillColor lineColor coords =
    polygon dc coords [ brush := brushSolid fillColor
                      , pen := penColored lineColor 2
                      ]

selectRegion :: Rectangle Viewport -> IO ()
selectRegion r = do
    putStrLn $ "selected region " ++ show r
    return ()

-- | Paint the state of a tile into a device context.
generateTileImage
    :: Tile    -- ^ A tile to convert to an image
    -> Rect    -- ^ The enclosing view rectangle
    -> IO (Image ())
generateTileImage viewerTile _windowRect = do
    let (width, height) = tileRect viewerTile
    --let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    --let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
    --                 (fHeight + height) `div` 2 - height )
    --putStrLn "generateTileImage"
    withSynchedTileBuffer viewerTile (imageCreateFromData (sz width height))

paintToolLayer :: Var (Maybe Viewport)
               -> Var (Maybe Viewport)
               -> DC d
               -> Rect
               -> Rect
               -> IO ()
paintToolLayer lastClick draggedTo dc _ _ = dcEncapsulate dc $ do
    dragBox <- getDragBox lastClick draggedTo
    case dragBox of
        Nothing  -> return ()
        Just box -> do
            let boxPts = map viewportToPoint (rectPoints box)
            drawBox dc (rgba @Word8 0 128 255 128) white boxPts
