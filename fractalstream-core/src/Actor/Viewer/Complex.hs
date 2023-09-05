{-# language OverloadedStrings #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
  , ViewerUIProperties(..)
  , ComplexViewer'(..)
  , ComplexViewerCompiler(..)
  , withComplexViewer'
  , runOnSelectHandler
  ) where

import Actor.Layout
import Language.Type
import Language.Code
import Data.DynamicValue

import Language.Effect.Output

import Language.Value.Parser
import Language.Code.Parser

import Language.Code.InterpretIO (interpretToIO, eval', ScalarIORefM, IORefTypeOfBinding)
import Language.Value.Evaluator

import Data.Word
import Data.Int
import Data.String
import Data.IORef
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
  , cvOnSelect :: Maybe String
  }

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
    , cvOnSelect' :: Maybe (Code '[Output env]
                                 ( '(px, 'RealT)
                                 ': '(z, 'ComplexT)
                                 ': env
                                 ) 'VoidT)

    , cvGetFunction :: IO (Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
    } -> ComplexViewer'

newtype StringOf (t :: FSType) =
  StringOf { valueOf :: HaskellType t }

instance KnownType t => IsString (StringOf t) where
  fromString s =
    case parseValue EmptyEnvProxy EmptyContext (typeProxy @t) s of
      Left err -> error (show err)
      Right v  -> StringOf (evaluate v EmptyContext)

instance KnownType t => FromJSON (StringOf t) where
  parseJSON = fmap unStringOrNumber . parseJSON

parseOnSelect :: (KnownSymbol px, KnownSymbol z)
              => Proxy px
              -> Proxy z
              -> EnvironmentProxy env
              -> Context Splice splices
              -> Maybe String
              -> IO (Maybe (Code '[Output env] ( '(px, 'RealT) ': '(z, 'ComplexT) ': env) 'VoidT))
parseOnSelect _ _ _ _ Nothing = pure Nothing
parseOnSelect px z env splices (Just input) = do
  let effectParser = EP (ParseEff (outputEffectParser env) NoEffs)
  env'  <- bindEnvProxyIO z  ComplexType env
  env'' <- bindEnvProxyIO px RealType    env'
  case parseCode effectParser env'' splices VoidType input of
    Left err -> error ("bad parse of on-select handler: " ++ show err)
    Right code -> pure (Just code)

-- | Run the code and find any variables in the environment that were modified by
-- `output` statements. Reflect those changes back into the environment.
runOnSelectHandler :: ComplexViewer' -> Complex Double -> IO ()
runOnSelectHandler ComplexViewer'{..} z = do
   px <- getDynamic cvPixelSize'
   let ctx = cvConfig'
   case cvOnSelect' of
     Nothing -> pure ()
     Just code -> do
       -- Copy the current environment into a bunch of IORefs
       iorefs :: Context IORefTypeOfBinding env <-
         mapContextM (\_ _ d -> getDynamic d >>= newIORef) ctx

       -- Build an handler for the Output effect that outputs values
       -- into the corresponding IORef in `iorefs`.
       let handle :: forall e t
                   . EnvironmentProxy e
                  -> TypeProxy t
                  -> Output env ScalarIORefM '(e,t)
                  -> StateT (Context IORefTypeOfBinding e) IO (HaskellType t)
           handle _ _ (Output _ pf _ v) = do
             x <- eval' v
             let ioref = getBinding iorefs pf
             liftIO (writeIORef ioref x)
           outputHandler = Handle (Proxy @ScalarIORefM) handle
           handlers = Handler outputHandler NoHandler

       -- Build the initial evaluation environment by reading the current
       -- value out of each IORef, also prepending the pixel size and
       -- selected coordinate.
       pxRef <- newIORef px
       zRef <- newIORef z
       iorefs' <- bindContextIO cvPixel' RealType pxRef =<<
                  bindContextIO cvCoord' ComplexType zRef iorefs

       inValues :: Context HaskellTypeOfBinding env <-
         mapContextM (\_ _ -> readIORef) iorefs

       -- Run the code and then read values back from the `iorefs`
       void (runStateT (interpretToIO handlers code) iorefs')
       outValues :: Context HaskellTypeOfBinding env <-
         mapContextM (\_ _ -> readIORef) iorefs

       -- Find values that were updated by an output effect, and
       -- update the corresponding dynamic values
       let finalCtx :: Context ((HaskellTypeOfBinding :**: HaskellTypeOfBinding)
                                :**: DynamicValue) env
           finalCtx = zipContext (zipContext inValues outValues) ctx
       fromContextM_ (\_ ty ((old, new), v) ->
                        if Scalar ty old == Scalar ty new
                        then pure ()
                        else void (setDynamic v new))
                     finalCtx

bindEnvProxyIO :: KnownSymbol name
               => Proxy name
               -> TypeProxy ty
               -> EnvironmentProxy env
               -> IO (EnvironmentProxy ( '(name, ty) ': env))
bindEnvProxyIO name ty env =
  case lookupEnv name ty env of
    Absent pf -> recallIsAbsent pf (pure (BindingProxy name ty env))
    _ -> error (symbolVal name ++ " is defined twice")

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


withComplexViewer' :: ( NotPresent "[internal argument] #blockSize" env
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
                                      pure $ \blockSize subsamples (dx :+ _dy) (x :+ y) buf -> do
                                        let fullArgs = Bind argX RealType x
                                                     $ Bind argY RealType y
                                                     $ Bind argPx RealType dx
                                                     $ args
                                        fun (fromIntegral blockSize) (fromIntegral subsamples) fullArgs buf
                                cvOnSelect' <- parseOnSelect cvPixel' cvCoord' env0 splices cvOnSelect
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
       , NotPresent "[internal argument] #blockSize" env
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
    -> ((Int32 -> Int32 -> Context HaskellTypeOfBinding env -> Ptr Word8 -> IO ())
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
