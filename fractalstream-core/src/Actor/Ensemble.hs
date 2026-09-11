{-# language OverloadedStrings, TemplateHaskell #-}
module Actor.Ensemble
  ( Ensemble(..)
  , parseEnsembleFromFile
  , runEnsemble
  , runEnsembleFromSetup

  , contextValuesChanged
  , layoutToArgs

  , BadEnsemble(..)

  , allTemplates

  ) where

import FractalStream.Prelude

import Data.DynamicValue
import Actor.UI
import Actor.Configuration
import Actor.Layout
import Actor.Event
import Actor.Tool (Tool)
import Actor.Viewer
import Actor.Viewer.Complex
import Language.Environment
import Language.Draw
import Language.Value hiding (Join)
import Language.Code.Parser (Splices(..), noSplices)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Code (usedVarsInCode, SomeCode(..))
import Language.Value.Evaluator (evaluate)
import Language.Code.InterpretIO
import Development.IncludeFile

import Control.Concurrent
import Control.Exception (Exception, throwIO)

import Data.Codec

data BadEnsemble = BadEnsemble String deriving Show
instance Exception BadEnsemble


data Ensemble = Ensemble
  { ensembleSetup         :: Variable (Maybe Configuration)
  , ensembleSplices       :: Dynamic (Either String Splices)
  , ensembleConfiguration :: Variable (Maybe Configuration)
  , ensembleViewers       :: Variable [ComplexViewer]
  }

instance Codec Ensemble where
  codec = do
    setup <-ensembleSetup-< optionalField "setup"
      (newVariable Nothing) (fmap isNothing . getDynamic) $ do
      codecWith (pure . pure . pure $ noSplices)

    splices <-ensembleSplices-< purely $ \use -> dyn (use setup) >>= \case
      Nothing                -> pure (pure noSplices)
      Just Configuration{..} -> layoutToSplices coContents

    config <-ensembleConfiguration-< optionalField "configuration"
      (newVariable Nothing) (fmap isNothing . getDynamic) $
      codecWith splices

    context <- purely $ \use -> dyn (use config) >>= \case
      Nothing                -> pure (Right $ SomeContext EmptyContext)
      Just Configuration{..} -> layoutContext coContents

    ctx <- purely $ \use -> (use context, use splices)

    viewers <-ensembleViewers-< newOf $ match
      [ Fragment (:[]) (\case { [x] -> Just x; _ -> Nothing }) $ field "viewer" (codecWith ctx)
      , Fragment id Just $ field "viewers" (codecWith ctx)
      , Fragment (const []) (\case { [] -> Just (); _ -> Nothing }) $ pure (pure ())
      ]

    build Ensemble setup splices config viewers

contextValuesChanged :: SomeContext DynamicValue -> Dynamic ()
contextValuesChanged (SomeContext ctx0) = go ctx0
  where
    go :: forall env. Context DynamicValue env -> Dynamic ()
    go = \case
      Bind _ _ v ctx' -> (<>) <$> (const () <$> v) <*> go ctx'
      EmptyContext    -> pure ()


parseEnsembleFromFile :: FilePath -> IO (Either String Ensemble)
parseEnsembleFromFile path = do
  contents <- BS.readFile path
  BS.length contents `seq` pure ()
  deserializeYAML contents

data HaskellValueOrError :: Symbol -> FSType -> Exp Type
type instance Eval (HaskellValueOrError name t) = Either String (HaskellType t)

runEnsembleFromSetup :: ViewerCompiler -> UI -> Ensemble -> IO ()
runEnsembleFromSetup jit UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble
  let rerunSetup = pure ()
  let runPostSetup = runEnsemblePostSetup project jit makeLayout makeViewer rerunSetup Ensemble{..}

  -- Run the setup panel
  getDynamic ensembleSetup >>= \case
    Nothing -> runPostSetup
    Just Configuration{..} -> do
      title <- either (throwIO . BadEnsemble) pure =<< getDynamic coTitle
      runSetup project title coContents runPostSetup

runEnsemble :: ViewerCompiler -> UI -> Ensemble -> IO ()
runEnsemble jit UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble
  let rerunSetup = pure ()
  runEnsemblePostSetup project jit makeLayout makeViewer rerunSetup Ensemble{..}


runEnsemblePostSetup :: forall ensembleHandle
                      . ensembleHandle
                     -> ViewerCompiler
                     -> (ensembleHandle -> String -> Layout -> IO (IO ()))
                     -> (ensembleHandle -> IO () -> Dynamic (Either String SomeEnvironment) -> SomeContext EventArgument_ -> IO () -> IO () -> Viewer -> IO (IO ()))
                     -> IO ()
                     -> Ensemble
                     -> IO ()
runEnsemblePostSetup project jit mkLayout mkViewer rerunSetup Ensemble{..} = do

  -- Make the configuration panel
  (someContext, configArgs, showConfig) <- getDynamic ensembleConfiguration >>= \case
    Nothing -> pure (SomeContext EmptyContext, SomeContext EmptyContext, pure ())
    Just Configuration{..} -> do
      title <- fromRight "???" <$> getDynamic coTitle
      showConfig <- mkLayout project title coContents
      layoutContext' coContents >>= \case
        Left err -> throwIO (BadEnsemble err)
        Right c  -> layoutToArgs coContents >>= \case
          SomeContext' (Left err)   -> throwIO (BadEnsemble err)
          SomeContext' (Right args) -> pure (c, args, showConfig)

  let env = maybe (pure $ Right $ SomeEnvironment EmptyEnvProxy) (layoutEnv . coContents) =<< dyn ensembleConfiguration
  watchDynamic env $ \env' -> putStrLn ("ENV: " ++ show env')
  let mkViewer' x1 x2 = mkViewer x1 x2 env

  -- Make the viewers
  viewers <- getDynamic ensembleViewers
  forM_ viewers $ makeComplexViewer project jit mkViewer' someContext configArgs showConfig rerunSetup

makeComplexViewer :: forall ensembleHandle
                   . ensembleHandle
                  -> ViewerCompiler
                  -> (ensembleHandle -> IO () -> SomeContext EventArgument_ -> IO () -> IO () -> Viewer -> IO (IO ()))
                  -> SomeContext DynamicValue'
                  -> SomeContext EventArgument_
                  -> IO ()
                  -> IO ()
                  -> ComplexViewer
                  -> IO ()
makeComplexViewer project jit mkViewer someContext configArgs showConfig rerunSetup ComplexViewer{..} = do
   rebuildScript
  where
   rebuildScript = do

    let throwLeft :: IO (Either String a) -> IO a
        throwLeft action = action >>= \case
          Left err -> throwIO (BadEnsemble $ show err)
          Right x  -> pure x

    coord <- fromRight "?coord" <$> getDynamic cvCoord
    let vTitle    = source cvTitle
        vSize     = cvSize
        vPosition = cvPosition

    let scriptCode = cvCode <&> right (\(SomeViewerWithContext _ _ c) -> SomeCode c)
    scriptName <- newMapped (pure $ \n -> if null n then Left "Script title must be non-empty" else Right n)
                  vTitle
    scriptEnv <- newVariable (SomeEnvironment endOfDecls)
    let vScript = UIScript{..}

    vCanResize <- getDynamic cvCanResize
    x0 :+ y0 <- throwLeft (getDynamic cvCenter)
    vCenter <- newVariable (x0, y0)
    vPixelSize <- throwLeft (getDynamic cvPixelSize) >>= newVariable
    let vSaveView = pure ()

    (vDrawCmds, vDrawCmdsChanged, vDrawTo) <- makeDrawCommandGetter

    getDynamic cvCode >>= \case

      Left (_, err) -> do
        let vTools = dyn cvTools
            vCodeWithArgs = CodeWithArgs (pure $ Right EmptyContext) Nothing (pure allGrey)
            vListen = const (pure $ pure ())

        putStrLn ("Can't build viewer: " ++ err)
        void $ mkViewer project showConfig configArgs rerunSetup rebuildScript Viewer{..}

      Right (SomeViewerWithContext context mprep code) -> do

        let env = contextToEnv context
            prepUsedVars = case mprep of
              Nothing -> Set.empty
              Just (PrepScript _ prepCode) -> execState (usedVarsInCode prepCode) Set.empty
            usedVars = Set.delete coord (execState (usedVarsInCode code) Set.empty `Set.union` prepUsedVars)

            vListen :: IO () -> IO (IO ())
            vListen = \action -> do
              let changed :: forall env. Context DynamicValue' env -> Dynamic ()
                  changed = \case
                    Bind n _t v ctx'
                      | symbolVal n `Set.member` usedVars
                        -> ((<>) <$> (const () <$> v) <*> changed ctx')
                      | otherwise -> changed ctx'
                    EmptyContext  -> pure ()
              let cc = (\(SomeContext c) -> changed c) someContext
              watchDynamic cc (\_ -> action)

        withEnvironment env $ do

          let vGetArgs = case someContext of
                SomeContext context' -> case sameEnvironment (contextToEnv context) (contextToEnv context') of
                  Just Refl -> do
                    rawResult <- mapContextM @DynamicValue' @HaskellValueOrError (\_ _ -> getDynamic) context'
                    case mapContextM (\_ _ -> id) rawResult of
                      Left err -> pure $
                        Left ("Viewer paused until this issue with the configuration is addressed:\n" ++
                              err)
                      r -> pure r
                  Nothing ->
                    -- Env has prep output vars appended beyond the config env.
                    -- Config vars: read live from someContext (which uses layoutContext').
                    -- Prep output vars: not in someContext, so fall back to snapshot default.
                    let liveEnv = contextToEnv context'
                    in do
                      result <- mapContextM @DynamicValue @HaskellValueOrError
                        (\name ty val ->
                          case lookupEnv name ty liveEnv of
                            Found pf -> withKnownType ty $ getDynamic (getBinding context' pf)
                            _        -> Right <$> getDynamic val)
                        context
                      pure (mapContextM (\_ _ -> id) result)

          withSelectTool <- if coord `Map.member` envToMap env then (:) <$> makeSelectTool coord else pure id
          let vTools = withSelectTool <$> dyn cvTools

          withCompiledViewer jit mprep code $ \fun -> do
            let vCodeWithArgs = CodeWithArgs vGetArgs (Just code) (pure fun)
            -- FIXME, we should grab the "close this window" action and do something with it
            void $ mkViewer project showConfig configArgs rerunSetup rebuildScript Viewer{..}

layoutToArgs :: Layout -> IO (SomeContext' EventArgument_)
layoutToArgs = \case
  Vertical   xs -> getDynamic xs >>= mapM layoutToArgs <&> mconcat
  Horizontal xs -> getDynamic xs >>= mapM layoutToArgs <&> mconcat
  Panel _ lo    -> getDynamic lo >>= layoutToArgs
  Tabbed ts     -> getDynamic ts >>= mapM (getDynamic . dyn . tiBody >=> layoutToArgs) <&> mconcat
  PlainText _   -> pure mempty
  Button _      -> pure mempty
  Selection _ n x _ -> getDynamic (dyn n) <&> \case
    Left err -> SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name ->
        SomeContext' $ Right $ SomeContext $ Bind name IntegerType (mutableVar x) EmptyContext
  CheckBox _ n b -> getDynamic (dyn n) <&> \case
    Left err -> SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name ->
        SomeContext' $ Right $ SomeContext $ Bind name BooleanType (mutableVar b) EmptyContext
  ColorPicker _ n c -> getDynamic (dyn n) <&> \case
    Left err -> SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name ->
        SomeContext' $ Right $ SomeContext $ Bind name ColorType (mutableArg c) EmptyContext
  ScriptBox {} -> pure $ SomeContext' (Left "Tools cannot accept scripts as configuration variables")
  TextBox _ UIVariable{..} -> getDynamic (dyn exprName) >>= \case
    Left err -> pure $ SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name -> getDynamic (dyn exprValue) <&> \case
        Left err -> SomeContext' (Left err)
        Right (SomeValue env (ty :: TypeProxy ty) _) -> case env of
          BindingProxy{} -> SomeContext' (Left "Tool configuration variables must have empty environments")
          EmptyEnvProxy  -> withKnownType ty $
            let arg :: EventArgument ty
                arg = EventArgument getter setter
                getter = getDynamic (dyn exprValue) <&> \case
                  Left _ -> Nothing
                  Right (SomeValue env' ty' v') -> case env' of
                    BindingProxy{} -> Nothing
                    EmptyEnvProxy -> case sameHaskellType ty ty' of
                      Nothing -> Nothing
                      Just Refl -> Just (evaluate v' EmptyContext)
                setter = Just (\t -> setValue' (source exprValue) . toUserString t)
            in SomeContext' (Right $ SomeContext $ Bind name ty arg EmptyContext)


makeDrawCommandGetter :: IO (IO [[DrawCommand]], IO Bool, Int -> DrawHandler ScalarIORefM)
makeDrawCommandGetter = do
  layersMVar <- newMVar (Map.empty :: Map Int [DrawCommand])
  dcChanged <- newMVar False

  let consDrawCmd :: Draw_ value env
                  -> Maybe [Draw_ value env]
                  -> Maybe [Draw_ value env]
      consDrawCmd = \case
        Clear{} -> const (Just [])
        c -> Just . \case
          Nothing -> [c]
          Just cs -> c:cs

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

  -- | A standard tool for selecting the clicked point
makeSelectTool :: String -> IO Tool
makeSelectTool coord = do
  let dep1 = pure (Right noSplices)
      dep2 = pure (Right (ComplexCoordinate coord))
      dep3 = pure (Right (defaultIterLimit, defaultMaxRadius, defaultMinRadius))
      dep4 = pure (Right Nothing)
  tool <- deserializeWith (dep1, dep2, dep3, dep4) (selectToolJson coord)
  either (error . ("Internal error: " ++)) pure tool

selectToolJson :: String -> ByteString
selectToolJson n = "{ \"name\": \"Select " <> UTF8.fromString n <> "\", \"shortcut\": \"S\", \"actions\": [ {\"event\": \"click-or-drag\", \"code\": \"pass\", \"can-update-viewer-coords\": false }]}"

$(includeFileInSource "../examples/templates/simple-complex-dynamics.yaml"
  "simpleComplexDynamicsTemplate")

$(includeFileInSource "../examples/templates/simple-parametric-complex-dynamics.yaml"
  "simpleParametricComplexDynamicsTemplate")

$(includeFileInSource "../examples/templates/one-variable-complex-dynamics.yaml"
  "complexDynamicsTemplate")

$(includeFileInSource "../examples/templates/one-variable-parametric-complex-dynamics.yaml"
  "parametricComplexDynamicsTemplate")

allTemplates :: [(String, IO Ensemble)]
allTemplates =
  [ template "Simplified complex dynamics" simpleComplexDynamicsTemplate
  , template "Simplified parametric complex dynamics" simpleParametricComplexDynamicsTemplate
  , template "General complex dynamics" complexDynamicsTemplate
  , template "General parametric complex dynamics" parametricComplexDynamicsTemplate
  ]
 where
   template name bs = (name,) $
     either (error . ("Internal error when parsing a template: " ++)) pure =<< deserializeYAML bs
