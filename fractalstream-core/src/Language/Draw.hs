{-# language OverloadedStrings, RecursiveDo #-}
module Language.Draw
  ( type Draw
  , Draw_(..)
  , type DrawCommand
  , finiteDrawCommand
  , transformDrawValues
  , transformDrawValuesM
  , DrawHandler(..)
  , runDrawHandler
  , DrawSink(..)
  , RadiusUnit(..)
  , bufferedSink
  , ignoreDrawing
  ) where

import FractalStream.Prelude

import Language.Value
import Control.Monad.Identity (runIdentity)
import Data.IORef (IORef, modifyIORef')

type Draw = Draw_ Value_

type DrawCommand = Draw_ ConcreteValue '[]

data ConcreteValue :: Environment -> FSType -> Exp Type
type instance Eval (ConcreteValue env t) = HaskellType t

-- | A backend-agnostic sink for draw commands, in concrete Haskell values.
-- A point is homogeneous, @[a : b]@ (see 'Language.Code.Projective'): on a
-- sphere viewer, any point of the Riemann sphere, ∞ included; on any other
-- viewer, @b@ is 1, and @a@ is the point (@x + iy@ for a real viewer's
-- @(x, y)@).  The interpreter adapts a 'DrawHandler' onto one of these;
-- the LLVM backend wraps each field as a C callback for compiled tools.
data DrawSink = DrawSink
  { dsClear  :: IO ()
  , dsStroke :: HaskellType 'ColorT -> IO ()
  , dsFill   :: HaskellType 'ColorT -> IO ()
  , dsPoint  :: HaskellType ('Pair 'ComplexT 'ComplexT) -> IO ()
  , dsLine   :: HaskellType ('Pair 'ComplexT 'ComplexT)
             -> HaskellType ('Pair 'ComplexT 'ComplexT) -> IO ()
  , dsCircle :: Bool -> RadiusUnit -> HaskellType 'RealT
             -> HaskellType ('Pair 'ComplexT 'ComplexT) -> IO ()
  , dsRect   :: Bool -> HaskellType ('Pair 'ComplexT 'ComplexT)
             -> HaskellType ('Pair 'ComplexT 'ComplexT) -> IO ()
  , dsWrite  :: HaskellType 'TextT
             -> HaskellType ('Pair 'ComplexT 'ComplexT) -> IO ()
  }

-- | What a circle's radius is measured in: the viewer's own coordinates
-- (so the circle is a shape in the plane, moving and growing with the
-- view), or pixels of the screen (a marker, the same size at any zoom
-- and in every tile of a group).
data RadiusUnit = ModelUnits | ScreenPixels
  deriving (Eq, Show)

-- | A sink that records draw commands (newest first) instead of drawing
-- them, to be replayed on the given sink later: so that a tool handler's
-- run only draws once it has finished without an error.
-- | A 'DrawSink' that ignores everything, for code with nowhere to draw.
ignoreDrawing :: DrawSink
ignoreDrawing = DrawSink
  { dsClear = pure (), dsStroke = const (pure ()), dsFill = const (pure ())
  , dsPoint = const (pure ()), dsLine = \_ _ -> pure ()
  , dsCircle = \_ _ _ _ -> pure (), dsRect = \_ _ _ -> pure ()
  , dsWrite = \_ _ -> pure () }

bufferedSink :: IORef [IO ()] -> DrawSink -> DrawSink
bufferedSink ref sink = DrawSink
  { dsClear  = later (dsClear sink)
  , dsStroke = later . dsStroke sink
  , dsFill   = later . dsFill sink
  , dsPoint  = later . dsPoint sink
  , dsLine   = \p q -> later (dsLine sink p q)
  , dsCircle = \f u r p -> later (dsCircle sink f u r p)
  , dsRect   = \f p q -> later (dsRect sink f p q)
  , dsWrite  = \t p -> later (dsWrite sink t p)
  }
  where later action = modifyIORef' ref (action :)

data DrawHandler (code :: Environment -> Exp Type) where
  DrawHandler :: forall code
               . (forall env. Draw env -> Eval (code env))
              -> DrawHandler code

runDrawHandler :: forall env code
                . DrawHandler code
               -> Draw env
               -> Eval (code env)
runDrawHandler (DrawHandler f) d = f d

data Draw_ (value :: Environment -> FSType -> Exp Type)
           (env :: Environment) where

  -- | draw point at POINT
  DrawPoint :: forall env value
             . EnvironmentProxy env
            -> Eval (value env  ('Pair 'ComplexT 'ComplexT))
            -> Draw_ value env

  -- | draw circle at POINT with radius VALUE (in the viewer's own metric:
  -- Euclidean in the plane, chordal on the sphere, hyperbolic in the disk
  -- or the half-plane)
  -- | draw filled circle at POINT with radius VALUE
  -- | (either one) ... with radius VALUE pixels
  DrawCircle :: forall env value
              . EnvironmentProxy env
             -> Bool
             -> RadiusUnit
             -> Eval (value env 'RealT)
             -> Eval (value env ('Pair 'ComplexT 'ComplexT))
             -> Draw_ value env

  -- | draw line from POINT to POINT: the viewer's geodesic between them
  -- (a straight segment in the plane, the shorter arc of a great circle
  -- on the sphere, the hyperbolic geodesic in the disk or the half-plane)
  DrawLine :: forall env value
            . EnvironmentProxy env
           -> Eval (value env ('Pair 'ComplexT 'ComplexT))
           -> Eval (value env ('Pair 'ComplexT 'ComplexT))
           -> Draw_ value env

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env value
            . EnvironmentProxy env
           -> Bool
           -> Eval (value env ('Pair 'ComplexT 'ComplexT))
           -> Eval (value env ('Pair 'ComplexT 'ComplexT))
           -> Draw_ value env

  -- | use COLOR for stroke
  SetStroke :: forall env value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value env

  -- | use COLOR for fill
  SetFill :: forall env value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value env

  -- | erase
  Clear :: forall env value
         . EnvironmentProxy env
        -> Draw_ value env

  -- | write TEXT at POINT
  Write :: forall env value
         . EnvironmentProxy env
        -> Eval (value env 'TextT)
        -> Eval (value env ('Pair 'ComplexT 'ComplexT))
        -> Draw_ value env

deriving instance Show DrawCommand

-- | Are all of a draw command's coordinates and lengths finite? A script
-- can easily produce NaN or infinite points (e.g. from an escaping orbit);
-- frontends skip such commands instead of drawing garbage, or crashing.
finiteDrawCommand :: DrawCommand -> Bool
finiteDrawCommand = \case
  DrawPoint _ p        -> finitePt p
  DrawLine _ p q       -> finitePt p && finitePt q
  DrawCircle _ _ _ r p -> finite r && finitePt p
  DrawRect _ _ p q     -> finitePt p && finitePt q
  Write _ _ p          -> finitePt p
  SetStroke {}         -> True
  SetFill {}           -> True
  Clear {}             -> True
  where
    finite x = not (isNaN x || isInfinite x)
    -- (a homogeneous point: its coordinates finite, and not both 0)
    finitePt (a, b) = all finite [realPart a, imagPart a, realPart b, imagPart b] && (a /= 0 || b /= 0)

transformDrawValues :: forall env
                     . (forall et. Value et -> Value et)
                    -> Draw env
                    -> Draw env
transformDrawValues f = runIdentity . transformDrawValuesM (pure . f)

transformDrawValuesM :: forall env m
                      . Monad m
                     => (forall et. Value et -> m (Value et))
                     -> Draw env
                     -> m (Draw env)
transformDrawValuesM f = \case
  DrawPoint env pt -> DrawPoint env <$> f pt
  DrawCircle env fill unit r c -> DrawCircle env fill unit <$> f r <*> f c
  DrawLine env pt1 pt2 -> DrawLine env <$> f pt1 <*> f pt2
  DrawRect env fill pt1 pt2 -> DrawRect env fill <$> f pt1 <*> f pt2
  SetStroke env c -> SetStroke env <$> f c
  SetFill env c -> SetFill env <$> f c
  Clear env -> pure $ Clear env
  Write env txt pt -> Write env <$> f txt <*> f pt
