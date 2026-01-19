{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)

--  , ComplexViewer'(..)
  , ComplexViewerCompiler(..)
--  , withComplexViewer'
--  , ViewerUIProperties(..)
  , cloneComplexViewer
{-
  , StringOf(..)
  , BadProject(..)
-}
  , ViewerInfo(..)
  , ViewerArgs(..)
  , Viewer(..)
  --, makeComplexViewer
  , parseViewerScript
  , SomeViewerCode(..)

  , InternalX
  , InternalY
  , InternalPx

  , BadProject(..)
  ) where

import FractalStream.Prelude

import Actor.Layout
--import Actor.Configuration
import Actor.Tool
--import Actor.Event

import Language.Type
import Language.Code
import Data.DynamicValue
import Data.Codec

--import Data.Color (grey)

--import Language.Draw
--import Language.Code.InterpretIO (ScalarIORefM, IORefTypeOfBinding, eval)

import Language.Value
import Language.Value.Parser
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Parser.SourceRange
import Language.Value.Evaluator
import Language.Typecheck

import Foreign (Ptr)
--import Data.Aeson
--import qualified Data.Text as Text
import qualified Data.Map as Map
--import qualified Data.Set as Set
--import Control.Concurrent.MVar
import Control.Exception (Exception(..))

data BadProject = BadProject String String
  deriving Show

instance Exception BadProject

type InternalX  = "[internal] x"
type InternalY  = "[internal] y"
type InternalPx = "[internal] px"

type InternalViewerEnv env =
    ( '(InternalIterationLimit, 'IntegerT) ': '(InternalIterations, 'IntegerT) ':
      '(InternalEscapeRadius, 'RealT) ': '(InternalVanishingRadius, 'RealT) ':
      '(InternalStuck, 'BooleanT) ':
      ViewerEnv env)

type ViewerEnv env =
  ( '(InternalX, 'RealT) ': '(InternalY, 'RealT) ':
    '(InternalPx, 'RealT) ': '("color", 'ColorT) ': env )

data SomeViewerCode where
  SomeViewerCode :: forall env
    . EnvironmentProxy env
   -> Code (ViewerEnv env)
   -> SomeViewerCode

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
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeViewerCode)
  --, cvOverlay :: Variable (Maybe String)
  , cvTools :: Variable [Tool]
  }


data ViewerArgs env = ViewerArgs
  { vaPoint      :: (Double, Double)
  , vaStep       :: (Double, Double)
  , vaWidth      :: Int32
  , vaHeight     :: Int32
  , vaSubsamples :: Int32
  , vaBuffer     :: Ptr Word8
  , vaArgs       :: Context HaskellTypeOfBinding env
  }

data ViewerInfo env = ViewerInfo
  { vTitle     :: Variable String
  , vSize      :: Variable Dimensions
  , vCanResize :: Bool
  , vCenter    :: Variable (Double, Double)
  , vPixelSize :: Variable Double
  , vSaveView  :: IO ()
  , vGetArgs   :: IO (Maybe (Context HaskellTypeOfBinding env))
  , vCode      :: Dynamic (ViewerArgs env -> IO ())
  }

data Viewer where
  Viewer :: forall env. ViewerInfo env -> Viewer

instance CodecWith (Dynamic (Either String SomeEnvironment, Splices)) ComplexViewer where
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
    code   <-cvCode-< mapped (key "code") $ \use -> (fst <$> use ctx) >>= \case
      Right (SomeEnvironment env) -> (fmap (SomeViewerCode env) .) <$>
        (parseViewerScript <$> pure (Right env) <*> (Right . snd <$> use ctx) <*> use coord)
      Left err -> pure (const (Left (NoSourceRange,
                                     "Cannot parse the script because of other errors: " ++ err)))
    tools  <-cvTools-< optionalField "tools" (newVariable "" []) (fmap null . getDynamic) (codecWith ctx)
    esc    <-cvEscapeRadius-<   mapped (optionalKey "escape-radius")    $ \_ ->
      pure (\s -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s)
    van    <-cvVanishRadius-<   mapped (optionalKey "vanishing-radius") $ \_ ->
      pure (\s -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s)
    iter   <-cvIterationLimit-< mapped (optionalKey "iteration-limit")  $ \_ ->
      pure (\s -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s)
    build ComplexViewer title size resize center pxSize coord pixel esc van iter code tools

parseViewerScript :: forall env
                   . Either String (EnvironmentProxy env)
                  -> Either String Splices
                  -> Either String String
                  -> CodeString
                  -> Either (SourceRange, String) (Code (ViewerEnv env))
parseViewerScript menv msplices mvar (CodeString src) = do
  let liftE :: String -> Either String a -> Either (SourceRange, String) a
      liftE what = let msg = "I cannot compile this script due to an error in " ++ what ++ ": "
                   in left ((NoSourceRange,) . (msg  ++))
  env <- liftE "the environment" menv
  withEnvironment env $ do
    splices0 <- liftE "the setup" msplices

    let spliceVar :: forall name -> forall ty. KnownSymbol name
                  => TypeProxy ty -> String -> String -> (String, ParsedValue)
        spliceVar name ty what whatTy =
          (symbolVal (Proxy @name), ParsedValue NoSourceRange $ \ty' -> case sameHaskellType ty ty' of
            Nothing   -> throwError (Surprise NoSourceRange what whatTy (Expected $ an ty'))
            Just Refl -> do
              pf <- findVarAtType NoSourceRange (Proxy @name) ty (envProxy Proxy)
              pure (Var (Proxy @name) ty pf))

        declareE :: forall n -> forall t e. KnownSymbol n => TypeProxy t -> EnvironmentProxy e
                 -> Either (SourceRange, String) (EnvironmentProxy ( '(n,t) ': e ))
        declareE n t e = case lookupEnv' (Proxy @n) e of
          Found'{} -> throwError (NoSourceRange, "Internal error: duplicate definition of `" ++
                                   symbolVal (Proxy @n) ++ "`.")
          Absent' pf -> pure (recallIsAbsent pf $ declare t e)

    let splices = Map.union splices0 . Map.fromList $
          [ spliceVar InternalEscapeRadius    RealType    "the hidden escape radius"       "a real number"
          , spliceVar InternalVanishingRadius RealType    "the hidden vanishing tolerance" "a real number"
          , spliceVar InternalIterationLimit  IntegerType "the hidden iteration limit"     "an integer"
          , spliceVar InternalIterations      IntegerType "the hidden iteration counter"   "an integer"
          , spliceVar InternalStuck           BooleanType "the `stuck` loop status"        "a truth value"
          ]

    -- Bind all of the internal bookkeeping variables
    env' :: EnvironmentProxy (InternalViewerEnv env) <-
      (     declareE InternalIterationLimit  IntegerType
        <=< declareE InternalIterations      IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalX               RealType
        <=< declareE InternalY               RealType
        <=< declareE InternalPx              RealType
        <=< declareE "color"                 ColorType
      ) env

    withEnvironment env' $ do

      let exists :: forall n -> forall t. KnownSymbol n => TypeProxy t
                 -> Either (SourceRange, String) (NameIsPresent n t (InternalViewerEnv env))
          exists n t = case lookupEnv (Proxy @n) t env' of
            Found pf -> pure pf
            _ -> throwError (NoSourceRange,
                             "INTERNAL ERROR, there was a problem locating `" ++ symbolVal (Proxy @n) ++ "`")
      let getVar :: forall n -> forall t e.  KnownSymbol n
                 => TypeProxy t -> EnvironmentProxy e -> Either (SourceRange, String) (Value '(e, t))
          getVar n t e = withEnvironment e $ case lookupEnv (Proxy @n) t e of
            Found pf -> pure (Var (Proxy @n) t pf)
            _ -> throwError (NoSourceRange,
                             "INTERNAL ERROR, there was a problem locating `" ++ symbolVal (Proxy @n) ++ "`")

      let i = Const (Scalar ComplexType (0 :+ 1))

      -- If the viewer variable is already defined in the environment, ensure that it also has
      -- complex type. Otherwise, extend the environment with the viewer variable. Then parse
      -- the code in this extended environment.
      var <- liftE "the viewer variable" mvar
      SomeSymbol (coord :: Proxy coordT) <- pure (someSymbolVal var)

      code :: Code (InternalViewerEnv env) <- case lookupEnv coord ComplexType env' of
        Found pf      -> do
          code0 <- left (errorLocation &&& unlines . pp) (parseCode env' splices src)
          -- Set the viewer variable to x + i y
          x <- getVar InternalX RealType env'
          y <- getVar InternalY RealType env'
          pure $ Block [ Set pf coord (R2C x + i * R2C y), code0 ]
        WrongType ty -> throwError (NoSourceRange,
                                    "Viewer variable `" ++ "` should be complex, not " ++ show ty)
        Absent pf    -> do
          let env'' = recallIsAbsent pf $ BindingProxy coord ComplexType env'
          code0 <- left (errorLocation &&& unlines . pp) (parseCode env'' splices src)
          x <- getVar InternalX RealType env'
          y <- getVar InternalY RealType env'
          pure (snd $ letInEnv @coordT (R2C x + i * R2C y) (env'', code0))

      -- Now bind all of the bookkeeping variables
      let (_, code') = (env', code)
                     & letInEnv (Const (Scalar typeProxy 100))
                     & letInEnv (Const (Scalar typeProxy 0))
                     & letInEnv (Const (Scalar typeProxy 10.0))
                     & letInEnv (Const (Scalar typeProxy 0.0001))
                     & letInEnv (Const (Scalar typeProxy False))
      pure code'

{-
makeComplexViewer :: ComplexViewerCompiler -> ComplexViewer -> IO Viewer
makeComplexViewer jit ComplexViewer{..} = do

  -- Build all of the easy Viewer fields
  let getd :: Parsed a -> IO a
      getd x = getDynamic x >>= \case
        Left err -> throwIO (BadProject "Could not compile the viewer." err)
        Right v  -> pure v

  vTitle <- newVariable g"" =<< getd cvTitle
  vSize  <- clone cvSize
  vCanResize <- getDynamic cvCanResize
  x :+ y <- getd cvCenter
  vCenter <- newVariable "" (x, y)
  vPixelSize <- newVariable "" =<< getd cvPixelSize

  let vSaveView = do
        setValue (source cvTitle) =<< getDynamic vTitle
        setValue cvSize =<< getDynamic vSize
        -- TODO: set center etc

  let vGetArgs = pure Nothing -- TODO FIXME, need to thread arguments from config

  code <- getDynamic cvCode >>= \case
    Left err -> throwIO (BadProject "Could not compile the viewer's script." (snd err))
    Right c  -> pure c

  _
  pure (Viewer ViewerInfo{..})
-}

{-
instance FromJSON ComplexViewer where
  parseJSON = withObject "complex viewer" $ \o -> do
    cvTitle <- o .: "title"
    Dimensions cvSize <- o .: "size"
    cvCanResize <- o .:? "resizable" .!= True
    cvCoord <- o .: "z-coord"
    cvPixel <- o .:? "pixel-size"
    cvEscapeRadius <- o .:? "escape-radius"
    cvVanishRadius <- o .:? "vanishing-radius"
    cvIterationLimit <- o .:? "iteration-limit"
    StringOrNumber cvCenter <- o .: "initial-center"
    StringOrNumber cvPixelSize <- o .: "initial-pixel-size"
    cvCode <- Text.unpack <$> o .: "code"
    cvOverlay <- o .:? "overlay"
    cvTools <- (o .:? "tools" .!= []) >>= (either fail pure . sequence . map ($ cvCoord))
    pure ComplexViewer{..}

-}
{-
data ViewerUIProperties = ViewerUIProperties
  { vpTitle :: String
  , vpSize :: (Int, Int)
  , vpCanResize :: Bool
  }


data ComplexViewer' where
  ComplexViewer' :: forall z px env
    . (KnownSymbol z, KnownSymbol px) =>
    { cvCenter'    :: Variable (Complex Double)
    , cvPixelSize' :: Variable Double
    , cvConfig' :: Context DynamicValue env
    , cvCoord' :: Proxy z
    , cvPixel' :: Proxy px
    , cvCode' :: Code (RealViewer env)
    , cvTools' :: [Tool]
    , cvGetDrawCommands :: IO [[DrawCommand]]
    , cvDrawCommandsChanged :: IO Bool
    , cvGetFunction :: IO (Word32 -> Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
    } -> ComplexViewer'
-}
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
newtype StringOf (t :: FSType) =
  StringOf { valueOf :: Either String (HaskellType t) }

instance KnownType t => Show (StringOf t) where
  show (StringOf v) = either id (showValue (typeProxy @t)) v

instance KnownType t => IsString (StringOf t) where
  fromString s =
    case parseValue @_ @t Map.empty s of
      Left err -> StringOf (Left $ ppFullError err s)
      Right v  -> StringOf (Right $ evaluate v EmptyContext)

instance KnownType t => FromJSON (StringOf t) where
  parseJSON = fmap unStringOrNumber . parseJSON
-}

{-
bindContextIO :: KnownSymbol name
              => Proxy name
              -> TypeProxy ty
              -> Eval (a name ty)
              -> Context a env
              -> IO (Context a ( '(name, ty) ': env))
bindContextIO name ty v ctx =
  case lookupEnv name ty (contextToEnv ctx) of
    Absent pf -> recallIsAbsent pf (pure (Bind name ty v ctx))
    _ -> throwIO (BadProject (symbolVal name ++ " is defined twice") [])

declareIO :: forall a name ty env
           . KnownSymbol name
          => Proxy name
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (NotPresent name env => EnvironmentProxy ( '(name,ty) ': env ) -> IO a)
          -> IO a
declareIO name ty env k =
  case lookupEnv name ty env of
    Absent pf -> recallIsAbsent pf (k $ BindingProxy name ty env)
    _ -> throwIO (BadProject (symbolVal name ++ " is defined twice") [])

declareOrGetIO :: forall a name ty env
                . KnownSymbol name
               => Proxy name
               -> TypeProxy ty
               -> EnvironmentProxy env
               -> (forall env'.
                   Either (NameIsPresent name ty env, env' :~: env)
                          (NameIsAbsent name env, env' :~: ( '(name,ty) ': env))
                   -> EnvironmentProxy env'
                   -> IO a)
               -> IO a
declareOrGetIO name ty env k =
  case lookupEnv name ty env of
    Absent pf -> recallIsAbsent pf $ k (Right (pf, Refl)) (BindingProxy name ty env)
    Found pf  -> k (Left  (pf, Refl)) env
    WrongType {} -> throwIO (BadProject (symbolVal name ++ " was defined at two different types") [])

toRealViewerCode :: forall z px env envZ envZZ
                  . (KnownSymbol z, KnownSymbol px)
                 => EnvironmentProxy env
                 -> EnvironmentProxy envZ
                 -> EnvironmentProxy envZZ
                 -> Proxy z
                 -> Proxy px
                 -> Either (NameIsPresent z 'ComplexT env, envZ :~: env)
                           (NameIsAbsent z env, envZ :~: ( '(z, 'ComplexT) ': env))
                 -> Either (NameIsPresent px 'RealT envZ, envZZ :~: envZ)
                           (NameIsAbsent px envZ, envZZ :~: ( '(px, 'RealT) ': envZ))
                 -> NameIsPresent InternalX 'RealT env
                 -> NameIsPresent InternalY 'RealT env
                 -> NameIsPresent InternalPx 'RealT envZ
                 -> Code envZZ
                 -> Code env
toRealViewerCode env envX envXX z px pfZ pfPx pfX pfY pfP code =
  withEnvironment env $ withEnvironment envX $ withEnvironment envXX $
  case pfZ of
    Right (pf, Refl) ->  case pfPx of
      Right (pf', Refl) ->
        Let (bindName z ComplexType pf) z
            (R2C (Var Proxy RealType pfX) +
             (Const (Scalar ComplexType (0 :+ 1)) * R2C (Var Proxy RealType pfY))) $
        Let (bindName px RealType pf') px (Var Proxy RealType pfP) $
        code
      Left (pf', Refl) ->
        Let (bindName z ComplexType pf) z
            (R2C (Var Proxy RealType pfX) +
              (Const (Scalar ComplexType (0 :+ 1)) * R2C (Var Proxy RealType pfY))) $
        Block [ Set pf' px (Var Proxy RealType pfP)
              , code]
    Left (pf, Refl) -> case pfPx of
      Right (pf', Refl) -> Block
        [ Set pf z (R2C (Var Proxy RealType pfX) +
                    (Const @_ @env (Scalar ComplexType (0 :+ 1)) * R2C (Var Proxy RealType pfY)))
        , Let (bindName px RealType pf') px (Var Proxy RealType pfP)
          code
        ]
      Left (pf', Refl) -> Block
        [ Set pf z (R2C (Var Proxy RealType pfX) +
                    (Const @_ @env (Scalar ComplexType (0 :+ 1)) * R2C (Var Proxy RealType pfY)))
        , Set pf' px (Var Proxy RealType pfP)
        , code ]

withComplexViewer' :: forall env
                    . ( NotPresent "[internal argument] #blockWidth" env
                      , NotPresent "[internal argument] #blockHeight" env
                      , NotPresent "[internal argument] #subsamples" env
                      , NotPresent "color" env )
                   => ComplexViewerCompiler
                   -> Context DynamicValue env
                   -> Splices
                   -> ComplexViewer
                   -> (  ViewerUIProperties
                      -> ComplexViewer'
                      -> IO ())
                   -> IO ()
withComplexViewer' jit cvConfig' splices0 ComplexViewer{..} action = withEnvironment (contextToEnv cvConfig') $ do

  escapeRadius <- getDynamic cvEscapeRadius >>= \case
    Left err -> throwIO (BadProject "I couldn't parse the viewer's escape-radius field." err)
    Right x  -> pure x

  vanishRadius <- getDynamic cvVanishRadius >>= \case
    Left err -> throwIO (BadProject "I couldn't parse the viewer's vanishing-radius field." err)
    Right x  -> pure x

  iterationLimit <- getDynamic cvIterationLimit >>= \case
    Left err -> throwIO (BadProject "I couldn't parse the viewer's iteration-limit field." err)
    Right x  -> pure x

  SomeSymbol it <- pure (someSymbolVal internalIterations)
  let iterations = ParsedValue NoSourceRange $ \case
        IntegerType -> do
          pf <- findVarAtType NoSourceRange it IntegerType (envProxy Proxy)
          pure (Var it IntegerType pf)
        ty -> throwError (Surprise NoSourceRange "the hidden iteration counter"
                          "an integer" (Expected $ an ty))

  SomeSymbol stuckVar <- pure (someSymbolVal internalStuck)
  let stuck = ParsedValue NoSourceRange $ \case
        BooleanType -> do
          pf <- findVarAtType NoSourceRange stuckVar BooleanType (envProxy Proxy)
          pure (Var stuckVar BooleanType pf)
        ty -> throwError (Surprise NoSourceRange "the `stuck` loop status"
                          "a truth value" (Expected $ an ty))

  pixelName <- getDynamic cvPixel >>= \case
    Left err -> throwIO (BadProject "I couldn't parse the pixel variable." err)
    Right v  -> pure (fromMaybe "#pixel" v)

  let argX = Proxy @InternalX
      argY = Proxy @InternalY
      argPx = Proxy @InternalPx
      argOutput = Proxy @"color"

      splices = Map.unions $ concat
                [ [Map.singleton internalEscapeRadius x    | x <- maybeToList escapeRadius]
                , [Map.singleton internalVanishingRadius x | x <- maybeToList vanishRadius]
                , [Map.singleton internalIterationLimit x  | x <- maybeToList iterationLimit]
                , [Map.fromList [ (internalIterations, iterations)
                                , (internalStuck, stuck) ]]
                , [splices0] ]

  coord <- getDynamic (source cvCoord)

  case (someSymbolVal coord, someSymbolVal pixelName) of
    (SomeSymbol (cvCoord' :: Proxy cvCoord'), SomeSymbol (cvPixel' :: Proxy cvPixel')) -> do
     let envOrig = contextToEnv cvConfig'

     declareIO argOutput ColorType envOrig
       $ \env1 -> declareIO argPx RealType env1
       $ \env2 -> declareIO argY RealType env2
       $ \env3 -> declareIO argX RealType env3
       $ \env -> declareOrGetIO cvCoord' ComplexType env
       $ \pfCoord' envX -> declareOrGetIO cvPixel' RealType envX
       $ \pfPixel' envX' -> declareIO it IntegerType envX'
       $ \envX'' -> declareIO stuckVar BooleanType envX''
       $ \envX''' -> do

      cvCenter' <- getDynamic cvCenter >>= \case
        Right v  -> newVariable "" v
        Left err -> throwIO (BadProject "I couldn't parse the viewer's initial-center field." err)
      cvPixelSize' <- getDynamic cvPixelSize >>= \case
        Right v  -> newVariable "" v
        Left err -> throwIO (BadProject "I couldn't parse the viewer's initial-pixel-size field" err)

      vpTitle <- getDynamic (source cvTitle)
      Dimensions vpSize  <- getDynamic cvSize
      vpCanResize <- getDynamic cvCanResize

      CodeString src <- getDynamic (source cvCode)
      case parseCode envX''' splices src of
        Left err -> throwIO (BadProject "there was an error parsing the viewer's script" (ppFullError err src))
        Right cvCode0 -> do
          Found pfX <- pure (lookupEnv argX RealType env)
          Found pfY <- pure (lookupEnv argY RealType env)
          Found pfPx <- pure (lookupEnv argPx RealType envX)

          let cvCode1 = withEnvironment envX'
                      $ let_ 0
                      $ let_ (Const (Scalar BooleanType False))
                      $ cvCode0
              cvCode' = toRealViewerCode env envX envX' cvCoord' cvPixel' pfCoord' pfPixel' pfX pfY pfPx cvCode1

          withCompiledComplexViewer jit argX argY argPx argPx argOutput cvCode' $ \fun -> do
            let cvGetFunction = do
                  args <-  mapContextM (\_ _ -> getDynamic) cvConfig'
                  pure $ \blockWidth blockHeight subsamples (dx :+ _dy) (x :+ y) buf -> do
                    let fullArgs = Bind argX RealType x
                                 $ Bind argY RealType y
                                 $ Bind argPx RealType dx
                                 $ Bind argOutput ColorType grey
                                 $ args
                    fun (fromIntegral blockWidth) (fromIntegral blockHeight)
                        (fromIntegral subsamples) fullArgs buf

            inheritedContext <- bindContextIO cvPixel' RealType
                                  (dyn cvPixelSize')
                                  cvConfig'

            (cvGetDrawCommands, cvDrawCommandsChanged, drawTo) <- makeDrawCommandGetter

            {-
            let cvToolsX = case pfCoord' of
                  Right{} -> cvTools
                  Left{}  -> ComplexTool (defaultComplexSelectionTool cvCoord) : cvTools

            cvTools' <- forM cvToolsX $ \(ComplexTool ParsedTool{..}) -> do

              toolConfig <- case ptoolConfig of
                Nothing -> pure Nothing
                Just cfg -> fmap Just . runExceptTIO $ allocateUIConstants (coContents cfg)

              withDynamicBindings (fromMaybe (Vertical []) toolConfig) $ \innerContext -> do
                -- Build the event handler actions using the viewer context
                -- extended by the tool's configuration context.
                cvToolContext <- case innerContext <#> inheritedContext of
                  Left msg -> throwIO (BadProject "there was a problem parsing the tool's configuration" msg)
                  Right ctx -> pure ctx
                putStrLn ("building tool `" ++ tiName ptoolInfo ++ "`")
                putStrLn ("environment: " ++ show (contextToEnv cvToolContext))

                let eventHandlers0 = case pfCoord' of
                      Left  {} -> ptoolEventHandlers
                      Right {} -> prependHandlerCode (cvCoord ++ " : C <- 0\n") ptoolEventHandlers
                      {-
                    eventHandlers = case pfPixel' of
                      Left  {} -> eventHandlers0
                      Right {} -> prependHandlerCode (cvPixelName ++ " : R <- 0\n") eventHandlers0
-}
                    eventHandlers = eventHandlers0

                (toolVars, h) <- case toEventHandlers
                                      (contextToEnv cvToolContext)
                                      (Set.singleton cvCoord)
                                      splices eventHandlers of
                       Left err -> throwIO (BadProject "there was a problem parsing the tool's event handler" err)
                       Right ok -> pure ok
                let toolInfo = ptoolInfo
                    toolDrawLayer = ptoolDrawLayer
                    toolRefreshOnActivate = ptoolRefreshOnActivate
                    toolEventHandler = const Nothing -- to be replaced below
                    tool = Tool{..}
                pure (tool { toolEventHandler = handleEvent
                                                  cvToolContext
                                                  ptoolRefreshCanUpdate
                                                  (drawTo ptoolDrawLayer) h })
-}
            let cvTools' = []
            action ViewerUIProperties{..} ComplexViewer'{..}
-}
newtype ComplexViewerCompiler = ComplexViewerCompiler
  { withCompiledComplexViewer
    :: forall x y dx dy out env t
     . ( KnownEnvironment env
       , NotPresent "[internal argument] #blockWidth" env
       , NotPresent "[internal argument] #blockHeight" env
       , NotPresent "[internal argument] #subsamples" env
       , KnownSymbol x, KnownSymbol y
       , KnownSymbol dx, KnownSymbol dy
       , KnownSymbol out
       , Required x env ~ 'RealT
       , NotPresent x (env `Without` x)
       , Required y env ~ 'RealT
       , NotPresent y (env `Without` y)
       , Required dx env ~ 'RealT
       , NotPresent dx (env `Without` dx)
       , Required dy env ~ 'RealT
       , NotPresent dy (env `Without` dy)
       , Required out env ~ 'ColorT
       , NotPresent out (env `Without` out)
       )
    => Proxy x
    -> Proxy y
    -> Proxy dx
    -> Proxy dy
    -> Proxy out
    -> Code env
    -> ((Int32 -> Int32 -> Int32 -> Context HaskellTypeOfBinding env -> Ptr Word8 -> IO ())
         -> IO t)
    -> IO t
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
