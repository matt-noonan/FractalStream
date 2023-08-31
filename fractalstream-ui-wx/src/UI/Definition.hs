{-# language OverloadedStrings #-}
{-# options_ghc -Wno-orphans #-}

module UI.Definition where

import Language.Environment
import Language.Type
import Language.Value hiding (get)
import Language.Code (Code)
import Language.Code.Parser
import Language.Value.Parser (parseValue, parseUntypedValue, Splice)
import Language.Value.Evaluator (HaskellTypeOfBinding, evaluate)
import qualified Language.Effect as Effect
import Control.Concurrent.MVar
import Control.Applicative

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

import Graphics.UI.WX hiding (glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WX as WX

import Data.Color (Color)

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

runEnsemble :: ToUI ui => ui -> Ensemble -> IO ()
runEnsemble ui Ensemble{..} = do
  -- Get a handle for the ensemble
  project <- newEnsemble ui

  -- Make the setup window and let it run
  Just setup <- pure ensembleSetup
  setupUI <- runExceptTIO (allocateUIExpressions (coContents setup))
  runSetup ui project (coTitle setup) setupUI $ \_splices -> do

    -- Build the configuration window, if any.
    whenJust ensembleConfiguration $ \config -> do
      configUI <- runExceptTIO (allocateUIConstants (coContents config))
      makeLayout ui project "Configuration" configUI
      --withDynamicBindings configUI $ \configContext -> do

    putStrLn "made it!"

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
    , cvCode' :: Code '[] ( '(z, 'ComplexT) ': '(px, 'RealT) ': env ) 'ColorT
    } -> ComplexViewer'

makeComplexViewer' :: Context DynamicValue env
                   -> Context Splice splices
                   -> ComplexViewer
                   -> IO (ViewerUIProperties, ComplexViewer')
makeComplexViewer' cvConfig' _splices ComplexViewer{..} = do
  let cvPixelName = fromMaybe "#pixel" cvPixel
  case (someSymbolVal cvCoord, someSymbolVal cvPixelName) of
    (SomeSymbol cvCoord', SomeSymbol cvPixel') -> do
      let env0 = contextToEnv cvConfig'
      case lookupEnv cvPixel' RealType env0 of
        Absent proof -> recallIsAbsent proof $ do
          let env1 = BindingProxy cvPixel' RealType env0
          case lookupEnv cvCoord' ComplexType env1 of
            Absent proof' -> recallIsAbsent proof' $ do
              let env = BindingProxy cvCoord' ComplexType env1
              cvCenter' <- newUIValue (valueOf cvCenter)
              cvPixelSize' <- newUIValue (valueOf cvPixelSize)

              let vpTitle = cvTitle
                  vpSize  = cvSize
                  vpCanResize = cvCanResize

              let effectParser = Effect.EP Effect.NoEffs
                  splices = EmptyContext -- FIXME

              case parseCode effectParser env splices ColorType cvCode of
                Left err -> error ("bad parse: " ++ show err)
                Right cvCode' -> pure (ViewerUIProperties{..}, ComplexViewer'{..})

            _ -> error (cvCoord ++ " defined in both the viewer and the configuration")
        _ -> error (cvPixelName ++ " defined in both the viewer and the configuration")


data Configuration = Configuration
  { coTitle :: String
  , coSize :: (Int, Int)
  , coContents :: Layout Dummy
  }

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

data Layout f
  = Vertical [Layout f]
  | Horizontal [Layout f]
  | Panel String (Layout f)
  | Tabbed [(String, Layout f)]
  | TextBox Label (f String)
  | CheckBox Label (f Bool)
  | ColorPicker Label (f Color)

class ToUI ui where
  type EnsembleHandle ui

  newEnsemble :: ui -> IO (EnsembleHandle ui)

  runSetup :: Dynamic dyn
           => ui
           -> EnsembleHandle ui
           -> String
           -> Layout dyn
           -> (forall splices. Context Splice splices -> IO ())
           -> IO ()

  makeLayout :: Dynamic dyn
             => ui
             -> EnsembleHandle ui
             -> String
             -> Layout dyn
             -> IO ()

data WX = WX

instance ToUI WX where
  type EnsembleHandle WX = ()

  newEnsemble _ = pure ()

  runSetup _ _ title setupUI k = do
    f <- frame [ text := title
               , on resize := propagateEvent
               ]

    innerLayout <- generateWxLayout f setupUI
    compileButton <- button f [ text := "Go!"
                              , on command := do
                                  set f [ visible := False ]
                                  k EmptyContext -- fixme
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
      ConstantExpression nameStr ty v -> case someSymbolVal nameStr of
        SomeSymbol name -> SomeUIValue name ty (SomeDynamic (ConstantExpression' ty v))
      ConstantBoolExpression nameStr b -> case someSymbolVal nameStr of
        SomeSymbol name -> SomeUIValue name BooleanType (SomeDynamic b)
      ConstantColorExpression nameStr c -> case someSymbolVal nameStr of
        SomeSymbol name -> SomeUIValue name ColorType (SomeDynamic c)

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
