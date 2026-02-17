module Actor.Viewer
  ( ViewerContext(..)
  , parseViewerScript
  , InternalX
  , InternalY
  , InternalDX
  , InternalDY
  , ViewerEnv
  , InternalViewerEnv
  , MissingViewerArgs
  , SomeViewerWithContext(..)
  , ViewerCompiler(..)
  , ViewerArgs(..)
  , ViewerFunction(..)
  , Viewer(..)
  , CodeWithArgs(..)
  , cloneViewer
  , onParameterChanges
  , invokeViewerFunction
  , assertMissingViewerArgs
  , snapshotToFile
  , allGrey
  , defaultIterLimit
  , defaultMaxRadius
  , defaultMinRadius
  ) where

import FractalStream.Prelude

import Actor.Viewer.Types
import Data.DynamicValue
import Actor.Layout (CodeString(..), Dimensions(..), UIScript)
import Actor.Tool
import Language.Environment
import Language.Value.Parser
import Language.Code
import Language.Draw
import Language.Code.Parser
import Language.Code.InterpretIO (ScalarIORefM)
import Language.Parser.SourceRange
import Language.Value.Evaluator
import Foreign
import Data.PNG
import qualified Data.Map as Map

data ViewerContext env = ViewerContext
  { vcContext :: Context DynamicValue env
  , vcSplices :: Splices
  , vcCoord   :: String
  , vcIterLimit :: Maybe (Value
      '( '(InternalEscapeRadius, RealT) ':
         '(InternalVanishingRadius, RealT) ':
         ViewerEnv env, 'IntegerT))
  , vcEscapes  :: Maybe (Value
      '( '(InternalVanishingRadius, RealT) ':
         ViewerEnv env, 'RealT))
  , vcVanishes   :: Maybe (Value
      '( ViewerEnv env, 'RealT))
  }

data SomeViewerWithContext where
  SomeViewerWithContext :: forall env
     . MissingViewerArgs env
    => Context DynamicValue env
    -> Code (ViewerEnv env)
    -> SomeViewerWithContext

data ViewerArgs env = ViewerArgs
  { vaPoint      :: (Double, Double)
  , vaStep       :: (Double, Double)
  , vaWidth      :: Int32
  , vaHeight     :: Int32
  , vaSubsamples :: Int32
  , vaBuffer     :: Ptr Word8
  , vaArgs       :: Context HaskellValue env
  }

data CodeWithArgs where
  CodeWithArgs :: forall env
                . MissingViewerArgs env
               => IO (Either String (Context HaskellValue env))
               -> Maybe (Code (ViewerEnv env))
               -> Dynamic (ViewerFunction env)
               -> CodeWithArgs

data Viewer = Viewer
  { vTitle     :: Variable String
  , vSize      :: Variable Dimensions
  , vPosition  :: Variable Dimensions
  , vCanResize :: Bool
  , vCenter    :: Variable (Double, Double)
  , vPixelSize :: Variable Double
  , vSaveView  :: IO ()
  , vListen    :: IO () -> IO (IO ())
  , vDrawCmds  :: IO [[DrawCommand]]
  , vDrawCmdsChanged :: IO Bool
  , vDrawTo    :: Int -> DrawHandler ScalarIORefM
  , vCodeWithArgs :: CodeWithArgs
  , vTools     :: Dynamic [Tool]
  , vScript    :: UIScript
  }

snapshotToFile :: Viewer -> Bool -> FilePath -> IO (Maybe String)
snapshotToFile Viewer{..} downsample path = case vCodeWithArgs of
  CodeWithArgs vGetArgs _ vCode -> vGetArgs >>= \case
    Left err -> pure (Just err)
    Right vaArgs -> do
      Dimensions (w, h) <- getDynamic vSize
      let vaWidth  = fromIntegral w * if downsample then 2 else 1
          vaHeight = fromIntegral h * if downsample then 2 else 1
          vaSubsamples = 1
      px <- getDynamic vPixelSize <&> if downsample then (/ 2) else id
      let vaStep = (px, px)
      (cx, cy) <- getDynamic vCenter
      let vaPoint = (cx - fromIntegral vaWidth * px / 2, cy + fromIntegral vaHeight * px / 2)
      ViewerFunction fn <- getDynamic vCode
      allocaBytes (fromIntegral $ 3 * vaWidth * vaHeight) $ \vaBuffer -> do
        fn ViewerArgs{..}
        if downsample
          then do
            let width  = fromIntegral (vaWidth `div` 2)
                height = fromIntegral (vaHeight `div` 2)
            allocaBytes (3 * width * height) $ \buf -> do
              forM_ [0 .. width - 1] $ \i ->
                forM_ [0 .. height - 1] $ \j ->
                  forM_ [0,1,2] $ \k -> do
                    let inputIx = 6 * i + 6 * width * 2 * j + k
                    c1 :: Word16 <- fromIntegral <$>
                      peekElemOff vaBuffer inputIx
                    c2 :: Word16 <- fromIntegral <$>
                      peekElemOff vaBuffer (inputIx + 6 * width)
                    c3 :: Word16 <- fromIntegral <$>
                      peekElemOff vaBuffer (inputIx + 3)
                    c4 :: Word16 <- fromIntegral <$>
                      peekElemOff vaBuffer (inputIx + 3 + 6 * width)
                    pokeElemOff buf (3 * i + 3 * width * j + k) (fromIntegral $ (c1 + c2 + c3 + c4) `div` 4)
              encodeBufferToPngFile (fromIntegral width, fromIntegral height) buf path
          else
            encodeBufferToPngFile (fromIntegral vaWidth, fromIntegral vaHeight) vaBuffer path
        pure Nothing

onParameterChanges :: Viewer -> IO () -> IO (IO ())
onParameterChanges = vListen

newtype ViewerFunction env =
  ViewerFunction (ViewerArgs env -> IO ())

assertMissingViewerArgs :: forall env t
                         . EnvironmentProxy env
                        -> (MissingViewerArgs env => t)
                        -> Maybe t
assertMissingViewerArgs env ok = do
  let assertAbsent :: forall name -> KnownSymbol name => Maybe (AssertAbsent name env)
      assertAbsent name = case lookupEnv' (Proxy @name) env of
        Absent' pf -> recallIsAbsent pf (Just AssertAbsent)
        _          -> Nothing

  AssertAbsent <- assertAbsent InternalX
  AssertAbsent <- assertAbsent InternalY
  AssertAbsent <- assertAbsent InternalDX
  AssertAbsent <- assertAbsent InternalDY
  AssertAbsent <- assertAbsent "color"
  pure ok

data AssertAbsent name env where
  AssertAbsent :: forall name env. NotPresent name env => AssertAbsent name env

invokeViewerFunction :: MissingViewerArgs env
                     => ViewerFunction env
                     -> Context HaskellValue env
                     -> Word32
                     -> Word32
                     -> Word32
                     -> Complex Double
                     -> Complex Double
                     -> Ptr Word8
                     -> IO ()
invokeViewerFunction (ViewerFunction fn) vaArgs =
  \w h subsamples (dx :+ dy) (x :+ y) vaBuffer -> do
    let vaPoint = (x, y)
        vaStep  = (dx, dy)
        vaWidth = fromIntegral w
        vaHeight = fromIntegral h
        vaSubsamples = fromIntegral subsamples
    fn ViewerArgs{..}


newtype ViewerCompiler = ViewerCompiler
  { withCompiledViewer :: forall env t
                        . (MissingViewerArgs env, KnownEnvironment env)
                       => Code (ViewerEnv env)
                       -> (ViewerFunction env -> IO t)
                       -> IO t }

parseViewerScript :: forall env
                   . Maybe String
                  -> ViewerContext env
                  -> CodeString
                  -> Either (SourceRange, String) (Code (ViewerEnv env))
parseViewerScript mpx ViewerContext{..} (CodeString src) = do
  let env = contextToEnv vcContext
  withEnvironment env $ do

    let declareE :: forall n -> forall t e. KnownSymbol n => TypeProxy t -> EnvironmentProxy e
                 -> Either (SourceRange, String) (EnvironmentProxy ( '(n,t) ': e ))
        declareE n t e = case lookupEnv' (Proxy @n) e of
          Found'{} -> throwError (NoSourceRange, "Internal error: duplicate definition of `" ++
                                   symbolVal (Proxy @n) ++ "`.")
          Absent' pf -> pure (recallIsAbsent pf $ declare t e)

    -- Bind all of the internal bookkeeping variables
    env' :: EnvironmentProxy (InternalViewerEnv env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
        <=< declareE InternalX               RealType
        <=< declareE InternalY               RealType
        <=< declareE InternalDX              RealType
        <=< declareE InternalDY              RealType
        <=< declareE "color"                 ColorType
      ) env

    withEnvironment env' $ do

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
      SomeSymbol (coord :: Proxy coordT) <- pure (someSymbolVal vcCoord)
      SomeSymbol (px :: Proxy px) <- pure (someSymbolVal $ fromMaybe "[unused] pixel size" mpx)

      code :: Code (InternalViewerEnv env) <- case lookupEnv coord ComplexType env' of

        Found pf -> case lookupEnv px RealType env' of
          Absent pf' -> do
            let env2 = recallIsAbsent pf' $ BindingProxy px RealType env'
            code0 <- left (errorLocation &&& unlines . pp) (parseCode env2 vcSplices src)
            -- Set the viewer variable to x + i y
            x <- getVar InternalX RealType env'
            y <- getVar InternalY RealType env'
            dx <- getVar InternalDX RealType env'
            pure $ Block [ Set pf coord (R2C x + i * R2C y), (snd $ letInEnv @px dx (env2, code0)) ]
          _ -> Left (NoSourceRange, "Duplicate definitions of pixel size variable `" ++ symbolVal px ++ "`.")

        WrongType ty -> throwError (NoSourceRange,
                                    "Viewer variable `" ++ "` should be complex, not " ++ show ty)
        Absent pf    -> do
            let env2 = recallIsAbsent pf $ BindingProxy coord ComplexType env'
            case lookupEnv px RealType env2 of
              Absent pf' -> do
                let env3 = recallIsAbsent pf' $ BindingProxy px RealType env2

                code0 <- left (errorLocation &&& unlines . pp) (parseCode env3 vcSplices src)
                x  <- getVar InternalX  RealType env'
                y  <- getVar InternalY  RealType env'
                dx <- getVar InternalDX RealType env2
                pure (snd $ (letInEnv @coordT (R2C x + i * R2C y) $ letInEnv @px dx $ (env3, code0)))
              _ -> Left (NoSourceRange, "Duplicate definitions of pixel size variable `" ++ symbolVal px ++ "`.")

      -- Now bind all of the bookkeeping variables
      let (_, code') = (env', code)
                     & letInEnv (Const (Scalar typeProxy 0))
                     & letInEnv (Const (Scalar typeProxy False))
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 100)) vcIterLimit)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 10.0)) vcEscapes)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 0.0001)) vcVanishes)

      pure code'

cloneViewer :: Viewer -> IO Viewer
cloneViewer v = do
  newTitle     <- newVariable =<< getDynamic (vTitle v)
  newSize      <- newVariable =<< getDynamic (vSize v)
  newPosition  <- newVariable =<< getDynamic (vPosition v)
  newCenter    <- newVariable =<< getDynamic (vCenter v)
  newPixelSize <- newVariable =<< getDynamic (vPixelSize v)
  pure $ Viewer
    { vTitle = newTitle
    , vSize  = newSize
    , vPosition = newPosition
    , vCanResize = vCanResize v
    , vCenter = newCenter
    , vPixelSize = newPixelSize
    , vSaveView = vSaveView v
    , vListen = vListen v
    , vTools = vTools v
    , vCodeWithArgs = vCodeWithArgs v
    , vDrawCmds = vDrawCmds v
    , vDrawCmdsChanged = vDrawCmdsChanged v
    , vDrawTo = vDrawTo v
    , vScript = vScript v
    }

-- | A fallback viewer function that paints every point grey
allGrey :: ViewerFunction '[]
allGrey = ViewerFunction $ \ViewerArgs{..} -> do
  let (w, h) = (fromIntegral vaWidth, fromIntegral vaHeight)
  forM_ [0..w - 1] $ \j ->
    forM_ [0..h - 1] $ \i -> do
      pokeElemOff vaBuffer (3 * j * w + 3 * i + 0) 0x7f
      pokeElemOff vaBuffer (3 * j * w + 3 * i + 1) 0x7f
      pokeElemOff vaBuffer (3 * j * w + 3 * i + 2) 0x7f

defaultIterLimit, defaultMaxRadius, defaultMinRadius :: ParsedValue
defaultIterLimit = fromRight (error "INTERNAL ERROR: defaultIterLimit") $ parseParsedValue Map.empty "100"
defaultMaxRadius = fromRight (error "INTERNAL ERROR: defaultMaxRadius") $ parseParsedValue Map.empty "10"
defaultMinRadius = fromRight (error "INTERNAL ERROR: defaultMinRadius") $ parseParsedValue Map.empty "0.0001"
