{-# language OverloadedStrings #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
{-
  , ViewerUIProperties(..)
  , ComplexViewer'(..)
  , ComplexViewerCompiler(..)
  , withComplexViewer'
  , cloneComplexViewer
  , StringOf(..)
  , BadProject(..)
-}
  , InternalX
  , InternalY
  , InternalPx
  , RealViewer
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

import Language.Value.Parser
{-
import Language.Value.Typecheck ( internalVanishingRadius
                                , internalEscapeRadius
                                , internalIterationLimit
                                , internalIterations
                                , internalStuck )
-}
--import Language.Code.Parser
import Language.Parser.SourceRange
--import Language.Value.Evaluator
--import Language.Typecheck

--import Foreign (Ptr)
--import Data.Aeson
--import qualified Data.Text as Text
--import qualified Data.Map as Map
--import qualified Data.Set as Set
--import Control.Concurrent.MVar
import Control.Exception (Exception(..)) --, throwIO)

data BadProject = BadProject String String
  deriving Show

instance Exception BadProject

type InternalX  = "[internal] x"
type InternalY  = "[internal] y"
type InternalPx = "[internal] px"

type RealViewer env =
  ( '(InternalX, 'RealT) ': '(InternalY, 'RealT) ': '(InternalPx, 'RealT) ':
    '("color", 'ColorT) ': env)

data ComplexViewer = ComplexViewer
  { cvTitle :: Parsed String
  , cvSize :: Variable Dimensions
  , cvCanResize :: Variable Bool
  , cvCenter :: Parsed (Complex Double)
  , cvPixelSize :: Parsed Double
  , cvCoord :: Parsed String
  , cvPixel :: Mapped (Maybe String) (Either String (Maybe String))
  --, cvEscapeRadius :: Parsed (Maybe String)
  --, cvVanishRadius :: Parsed (Maybe String)
  --, cvIterationLimit :: Parsed (Maybe String)
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeCode)
  --, cvOverlay :: Variable (Maybe String)
  , cvTools :: Variable [Tool]
  }

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
    code   <-cvCode-< mapped (key "code") $ \use -> uncurry parseScript' <$> use ctx
    tools  <-cvTools-< optionalField "tools" (newVariable "" []) (fmap null . getDynamic) (codecWith ctx)
    build ComplexViewer title size resize center pxSize coord pixel code tools

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


data ViewerUIProperties = ViewerUIProperties
  { vpTitle :: String
  , vpSize :: (Int, Int)
  , vpCanResize :: Bool
  }

data ComplexViewer' where
  ComplexViewer' :: forall z px env
    . (KnownSymbol z, KnownSymbol px) =>
    { cvCenter'    :: UIValue (Complex Double)
    , cvPixelSize' :: UIValue Double
    , cvConfig' :: Context DynamicValue env
    , cvCoord' :: Proxy z
    , cvPixel' :: Proxy px
    , cvCode' :: Code (RealViewer env)
    , cvTools' :: [Tool]
    , cvGetDrawCommands :: IO [[DrawCommand]]
    , cvDrawCommandsChanged :: IO Bool
    , cvGetFunction :: IO (Word32 -> Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
    } -> ComplexViewer'

cloneComplexViewer :: ComplexViewer' -> IO ComplexViewer'
cloneComplexViewer cv = do
  newCenter <- newUIValue =<< getUIValue (cvCenter' cv)
  newPixelSize <- newUIValue =<< getUIValue (cvPixelSize' cv)
  pure (cv { cvCenter' = newCenter
           , cvPixelSize' = newPixelSize
           })

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

  escapeRadius <- case cvEscapeRadius of
    Nothing -> pure Nothing
    Just x -> case parseParsedValue Map.empty x of
      Right v  -> pure (Just v)
      Left err -> throwIO (BadProject "I couldn't parse the viewer's escape-radius field."
                            (ppFullError err x))

  vanishRadius <- case cvVanishRadius of
    Nothing -> pure Nothing
    Just x -> case parseParsedValue Map.empty x of
      Right v  -> pure (Just v)
      Left err -> throwIO (BadProject "I couldn't parse the viewer's vanishing-radius field."
                            (ppFullError err x))

  iterationLimit <- case cvIterationLimit of
    Nothing -> pure Nothing
    Just x -> case parseParsedValue Map.empty x of
      Right v  -> pure (Just v)
      Left err -> throwIO (BadProject "I couldn't parse the viewer's iteration-limit field."
                            (ppFullError err x))

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

  let cvPixelName = fromMaybe "#pixel" cvPixel
      argX = Proxy @InternalX
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

  case (someSymbolVal cvCoord, someSymbolVal cvPixelName) of
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

      cvCenter' <- case valueOf cvCenter of
        Right v  -> newUIValue v
        Left err -> throwIO (BadProject "I couldn't parse the viewer's initial-center field." err)
      cvPixelSize' <- case valueOf cvPixelSize of
        Right v  -> newUIValue v
        Left err -> throwIO (BadProject "I couldn't parse the viewer's initial-pixel-size field" err)

      let vpTitle = cvTitle
          vpSize  = cvSize
          vpCanResize = cvCanResize

      case parseCode envX''' splices cvCode of
        Left err -> throwIO (BadProject "there was an error parsing the viewer's script" (ppFullError err cvCode))
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
                                  (SomeDynamic cvPixelSize')
                                  cvConfig'

            (cvGetDrawCommands, cvDrawCommandsChanged, drawTo) <- makeDrawCommandGetter

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

            action ViewerUIProperties{..} ComplexViewer'{..}


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
