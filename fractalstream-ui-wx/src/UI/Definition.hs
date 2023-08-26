module UI.Definition where

import Language.Environment
import Language.Type
import Language.Code.Parser
import qualified Language.Effect as Effect

import Data.Proxy
import GHC.TypeLits
import Control.Monad.Except

import Graphics.UI.WX hiding (when, tool, Horizontal, Vertical, Layout)

defToUI :: FilePath -> IO ()
defToUI yf =
  if False
  then defToUI_ yf
  else do
    result <- runExceptT (buildViewer sampleCV)
    case result of
      Left e -> error ("ERROR: " ++ e)
      Right _ -> putStrLn ("OKOK OK!")

defToUI_ :: FilePath -> IO ()
defToUI_ _yamlFile = start $ do

  let fakeSignal = Signal (const (pure ())) . pure

  let lo = Vertical
        [ Horizontal
            [ CheckBox (Label "check1: ") cb1
            , TextBox (Label "Hello: ") tb1
            ]
        , TextBox (Label "World: ") tb2
        , CheckBox (Label "check2: ") cb2
        ]
      cb1 = fakeSignal True
      cb2 = fakeSignal False
      tb1 = fakeSignal "hello"
      tb2 = fakeSignal "world"

  _ <- layoutToWindow "test window" lo
  pure ()

parseYAML :: String -> Either String ComplexViewer
parseYAML = error "todo, parseYAML"

buildViewer :: ComplexViewer -> ExceptT String IO (Layout Signal)
buildViewer ComplexViewer{..} =
  bindInEnv cvCoord ComplexType EmptyEnvProxy $ \env0 -> do
    withConfigurationBindings cvSetup env0 $ \env1 -> do
      lift (print (allBindings (coContents cvSetup)))
      withConfigurationBindings cvConfig env1 $ \env2 -> do
        -- parse the code block in env2
        lift (print (allBindings (coContents cvConfig)))
        let effectParser = Effect.EP Effect.NoEffs
        case parseCode effectParser env2 ColorType cvCode of
          Left err -> fail ("bad parse: " ++ show err)
          Right _code -> error "TIDO"

withConfigurationBindings :: forall m t env
                           . MonadFail m
                          => Configuration
                          -> EnvironmentProxy env
                          -> (forall env'. EnvironmentProxy env' -> m t)
                          -> m t
withConfigurationBindings Configuration{..} env0 k
   = go (allBindings coContents) env0
 where
   go :: forall e. [(String, SomeType)] -> EnvironmentProxy e -> m t
   go [] env = k env
   go ((nameStr, SomeType ty) : etc) env =
     bindInEnv nameStr ty env (go etc)

bindInEnv :: (MonadFail m)
          => String
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (forall name. EnvironmentProxy ( '(name, ty) ': env) -> m t)
          -> m t

bindInEnv nameStr ty env k = case someSymbolVal nameStr of
  SomeSymbol name -> case lookupEnv' name env of
    Absent' proof -> k (bindNameEnv name ty proof env)
    _ -> fail (symbolVal name <> " is defined twice")

bindInEnv' :: (MonadFail m, KnownSymbol name)
          => Proxy name
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (EnvironmentProxy ( '(name, ty) ': env) -> m t)
          -> m t

bindInEnv' name ty env k = case lookupEnv' name env of
  Absent' proof -> k (bindNameEnv name ty proof env)
  _ -> fail (symbolVal name <> " is defined twice")

data ComplexViewer = ComplexViewer
  { cvTitle :: String
  , cvSize :: (Int, Int)
  , cvCanResize :: Bool
  , cvCoord :: String
  , cvSetup :: Configuration
  , cvConfig :: Configuration
  , cvCode :: String
  }

data Configuration = Configuration
  { coTitle :: String
  , coSize :: (Int, Int)
  , coContents :: Layout Dummy
  }

dummy :: String -> String -> TypeProxy ty -> Dummy t
dummy var val ty = Dummy (YamlVar val (SomeType ty) var)

sampleCV :: ComplexViewer
sampleCV = ComplexViewer{..}
  where
    textBox a b c d = TextBox (Label a) (dummy b c d)
    cvTitle = "C plane"
    cvSize = (256, 256)
    cvCanResize = True
    cvCoord = "z"
    cvSetup = Configuration "Parametric complex dynamics" (200, 200)
      $ Vertical
      [ textBox "Iterate f(z) = " "f" "z^2 + C" ComplexType
      , textBox "until" "stop" "|z| > maxRadius" BooleanType
      ]
    cvConfig = Configuration "Parameters" (400, 200)
      $ Vertical
      [ textBox "C = " "C" "0.11 + 0.78i" ComplexType
      , textBox "Max. iterations: " "maxIters" "100" IntegerType
      , textBox "Max. radius: "  "maxRadius" "10" RealType
      ]
    cvCode = "init k : Z to 0\nloop\n  set z to {f}\n  set k to k + 1\n  k < maxIters and not {stop}\nif k = maxIters then black else blue\n"

data Dummy t = Dummy YamlVar
  deriving Show

data YamlVar = YamlVar
  { varValue :: String
  , varType :: SomeType
  , varVariable :: String
  }
  deriving Show

allBindings :: Layout Dummy -> [(String, SomeType)]
allBindings = go
  where
    toBinding (Dummy YamlVar{..}) = [(varVariable, varType)]
    go = \case
      Vertical xs -> concatMap allBindings xs
      Horizontal xs -> concatMap allBindings xs
      Panel _ x -> allBindings x
      Tabbed xs -> concatMap (allBindings . snd) xs
      TextBox _ x -> toBinding x
      CheckBox _ x -> toBinding x
      ColorPicker _ x -> toBinding x


newtype Label = Label String

data Signal t = Signal (t -> IO ()) (IO t)

data Layout f
  = Vertical [Layout f]
  | Horizontal [Layout f]
  | Panel String (Layout f)
  | Tabbed [(String, Layout f)]
  | TextBox Label (f String)
  | CheckBox Label (f Bool)
  | ColorPicker Label (f Color)

layoutToWindow :: String -> Layout Signal -> IO ()
layoutToWindow wTitle wLayout = do
  frame0 <- frame [ text := wTitle
                  , on resize := propagateEvent
                  ]
  panel0 <- panel frame0 []
  computedLayout <- go panel0 wLayout
  set frame0 [ layout := container panel0 computedLayout ]

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

     CheckBox (Label lab) (Signal setter getter) -> do
       initial <- getter
       cb <- checkBox p [ text := lab
                        , checkable := True
                        , checked := initial
                        , visible := True
                        ]
       set cb [ on command := do
                  isChecked <- get cb checked
                  setter isChecked
              ]
       pure (widget cb)

     TextBox (Label lab) (Signal _setter getter) -> do
       initial <- getter
       te <- textEntry p [ text := initial ]
       pure (fill $ row 5 [ label lab, hfill (widget te) ])
