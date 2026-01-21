module Actor.Viewer
  ( ViewerContext(..)
  , parseViewerScript
  , InternalX
  , InternalY
  , InternalPx
  , ViewerEnv
  , InternalViewerEnv
  , SomeViewerWithContext(..)
  , ViewerCompiler(..)
  , ViewerInfo(..)
  , ViewerArgs(..)
  , Viewer(..)
  , type ViewerFunction
  ) where

import FractalStream.Prelude

import Data.DynamicValue
import Actor.Layout (CodeString(..), Dimensions)
import Language.Environment
import Language.Code
import Language.Code.Parser
import Language.Parser.SourceRange
import Language.Typecheck
import Language.Value.Typecheck
import Language.Value.Evaluator
import Foreign (Ptr)

import qualified Data.Map as Map

data ViewerContext env = ViewerContext
  { vcContext :: Context DynamicValue env
  , vcSplices :: Splices
  , vcCoord   :: String
  }

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

data SomeViewerWithContext where
  SomeViewerWithContext :: forall env
    . Context DynamicValue env
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

data ViewerInfo env = ViewerInfo
  { vTitle     :: Variable String
  , vSize      :: Variable Dimensions
  , vCanResize :: Bool
  , vCenter    :: Variable (Double, Double)
  , vPixelSize :: Variable Double
  , vSaveView  :: IO ()
  , vGetArgs   :: IO (Context HaskellValue env)
  , vCode      :: Dynamic (ViewerFunction env)
  }

data Viewer where
  Viewer :: forall env. ViewerInfo env -> Viewer

type ViewerFunction env = ViewerArgs env -> IO ()

newtype ViewerCompiler = ViewerCompiler
  { withCompiledViewer :: forall env t. Code (ViewerEnv env) -> (ViewerFunction env -> IO t) -> IO t }

parseViewerScript :: forall env
                   . ViewerContext env
                  -> CodeString
                  -> Either (SourceRange, String) (Code (ViewerEnv env))
parseViewerScript ViewerContext{..} (CodeString src) = do

  let env = contextToEnv vcContext
  withEnvironment env $ do

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

    let splices = Map.union vcSplices . Map.fromList $
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
