{-# language OverloadedStrings #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
  , ViewerUIProperties(..)
  , ComplexViewer'(..)
  , ComplexViewerCompiler(..)
  , withComplexViewer'
  , cloneComplexViewer
  ) where

import Actor.Layout
import Actor.Tool
import Actor.Event

import Language.Type
import Language.Code
import Data.DynamicValue

import Language.Effect.Draw
import Language.Code.InterpretIO (ScalarIORefM)

import Language.Value.Parser
import Language.Code.Parser

import Language.Value.Evaluator

import Data.Word
import Data.Int
import Data.String
import Data.Maybe (fromMaybe)
import Foreign (Ptr)
import GHC.TypeLits
import Control.Monad.State
import Fcf
import Data.Aeson
import qualified Data.Text as Text

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
  , cvTools :: [ComplexTool]
  }
  deriving Show

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
    cvTools <- (o .:? "tools" .!= []) >>= (either fail pure . sequence . map ($ cvCoord))
    pure ComplexViewer{..}


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
    , cvTools' :: (Int -> EffectHandler Draw ScalarIORefM) -> [Tool]
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
  StringOf { valueOf :: HaskellType t }

instance KnownType t => Show (StringOf t) where
  show (StringOf v) = showValue (typeProxy @t) v

instance KnownType t => IsString (StringOf t) where
  fromString s =
    case parseValue EmptyEnvProxy EmptyContext (typeProxy @t) s of
      Left err -> error (show err)
      Right v  -> StringOf (evaluate v EmptyContext)

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
    _ -> error (symbolVal name ++ " is defined twice")


withComplexViewer' :: ( NotPresent "[internal argument] #blockWidth" env
                      , NotPresent "[internal argument] #blockHeight" env
                      , NotPresent "[internal argument] #subsamples" env )
                   => ComplexViewerCompiler
                   -> Context DynamicValue env
                   -> Context Splice splices
                   -> ComplexViewer
                   -> (ViewerUIProperties -> ComplexViewer' -> IO ())
                   -> IO ()
withComplexViewer' jit cvConfig' splices ComplexViewer{..} action = withEnvironment (contextToEnv cvConfig') $ do
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

                          let effectParser = EP NoEffs

                          case parseCode effectParser env splices ColorType cvCode of
                            Left err -> error ("bad parse: " ++ show err)
                            Right cvCode' -> do
                              Found pfX <- pure (lookupEnv argX RealType env3)
                              Found pfY <- pure (lookupEnv argY RealType env3)
                              Found pfPx <- pure (lookupEnv argPx RealType env4)
                              let realCode = complexToReal pfX pfY pfPx cvCode'
                              withCompiledComplexViewer jit argX argY argPx argPx realCode $ \fun -> do
                                let cvGetFunction = do
                                      args <- mapContextM (\_ _ -> getDynamic) cvConfig'
                                      pure $ \blockWidth blockHeight subsamples (dx :+ _dy) (x :+ y) buf -> do
                                        let fullArgs = Bind argX RealType x
                                                     $ Bind argY RealType y
                                                     $ Bind argPx RealType dx
                                                     $ args
                                        fun (fromIntegral blockWidth) (fromIntegral blockHeight) (fromIntegral subsamples) fullArgs buf
                                -- For tools, bind the viewer coordinate to the
                                -- view's center point. Maybe this is going to be too
                                -- confusing...
                                cvToolContext
                                  <-  bindContextIO cvCoord' ComplexType
                                        (SomeDynamic cvCenter')
                                  =<< bindContextIO cvPixel' RealType
                                        (SomeDynamic cvPixelSize')
                                  cvConfig'
                                cvToolsFns <- forM cvTools $ \(ComplexTool ParsedTool{..}) -> do
                                  -- FIXME: currently parses in the global context,
                                  -- not the viewer context extended by the tool's
                                  -- configuration context.
                                  putStrLn ("building tool `" ++ tiName ptoolInfo ++ "`")
                                  putStrLn ("environment: " ++ show (contextToEnv cvToolContext))
                                  h <- case toEventHandlers (contextToEnv cvToolContext) ptoolEventHandlers of
                                         Left err -> error ("toEventHandlers returned " ++ err)
                                         Right ok -> pure ok
                                  let toolInfo = ptoolInfo
                                      toolDrawLayer = ptoolDrawLayer
                                      toolConfig = Nothing -- TODO
                                      toolEventHandler = const (pure ()) -- to be replaced below
                                      tool = Tool{..}
                                  pure (\drawTo -> tool { toolEventHandler = handleEvent cvToolContext (drawTo ptoolDrawLayer) h })
                                let cvTools' = sequence cvToolsFns
                                action ViewerUIProperties{..} ComplexViewer'{..}

                        _ -> error "could not define viewer-internal pixel size"
                    _ -> error "could not define viewer-internal Y coordinate"
                _ -> error "could not define viewer-internal X coordinate"

            _ -> error (cvCoord ++ " defined in both the viewer and the configuration")
        _ -> error (cvPixelName ++ " defined in both the viewer and the configuration")

newtype ComplexViewerCompiler = ComplexViewerCompiler
  { withCompiledComplexViewer
    :: forall x y dx dy env t
     . ( KnownEnvironment env
       , NotPresent "[internal argument] #blockWidth" env
       , NotPresent "[internal argument] #blockHeight" env
       , NotPresent "[internal argument] #subsamples" env
       , KnownSymbol x, KnownSymbol y
       , KnownSymbol dx, KnownSymbol dy
       , Required x env ~ 'RealT
       , NotPresent x (env `Without` x)
       , Required y env ~ 'RealT
       , NotPresent y (env `Without` y)
       , Required dx env ~ 'RealT
       , NotPresent dx (env `Without` dx)
       , Required dy env ~ 'RealT
       , NotPresent dy (env `Without` dy)
       )
    => Proxy x
    -> Proxy y
    -> Proxy dx
    -> Proxy dy
    -> Code '[] env 'ColorT
    -> ((Int32 -> Int32 -> Int32 -> Context HaskellTypeOfBinding env -> Ptr Word8 -> IO ())
         -> IO t)
    -> IO t
  }


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
