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

-- | A backend-agnostic sink for draw commands, in concrete (plane-coordinate)
-- Haskell values.  The interpreter adapts a 'DrawHandler' onto one of these;
-- the LLVM backend wraps each field as a C callback for compiled tools.
data DrawSink = DrawSink
  { dsClear  :: IO ()
  , dsStroke :: HaskellType 'ColorT -> IO ()
  , dsFill   :: HaskellType 'ColorT -> IO ()
  , dsPoint  :: HaskellType ('Pair 'RealT 'RealT) -> IO ()
  , dsLine   :: HaskellType ('Pair 'RealT 'RealT)
             -> HaskellType ('Pair 'RealT 'RealT) -> IO ()
  , dsCircle :: Bool -> RadiusUnit -> HaskellType 'RealT
             -> HaskellType ('Pair 'RealT 'RealT) -> IO ()
  , dsRect   :: Bool -> HaskellType ('Pair 'RealT 'RealT)
             -> HaskellType ('Pair 'RealT 'RealT) -> IO ()
  , dsWrite  :: HaskellType 'TextT
             -> HaskellType ('Pair 'RealT 'RealT) -> IO ()
  , dsGeodesic :: HaskellType ('Pair 'RealT 'RealT)
               -> HaskellType ('Pair 'RealT 'RealT) -> IO ()
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
  , dsWrite = \_ _ -> pure (), dsGeodesic = \_ _ -> pure () }

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
  , dsGeodesic = \p q -> later (dsGeodesic sink p q)
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
            -> Eval (value env  ('Pair 'RealT 'RealT))
            -> Draw_ value env

  -- | draw circle at POINT with radius VALUE
  -- | draw filled circle at POINT with radius VALUE
  -- | (either one) ... with radius VALUE pixels
  DrawCircle :: forall env value
              . EnvironmentProxy env
             -> Bool
             -> RadiusUnit
             -> Eval (value env 'RealT)
             -> Eval (value env ('Pair 'RealT 'RealT))
             -> Draw_ value env

  -- | draw line from POINT to POINT
  DrawLine :: forall env value
            . EnvironmentProxy env
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value env

  -- | draw geodesic from POINT to POINT (in a viewer of the hyperbolic
  -- plane: the shortest path there, a circular arc or a straight segment)
  DrawGeodesic :: forall env value
                . EnvironmentProxy env
               -> Eval (value env ('Pair 'RealT 'RealT))
               -> Eval (value env ('Pair 'RealT 'RealT))
               -> Draw_ value env

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env value
            . EnvironmentProxy env
           -> Bool
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
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
        -> Eval (value env ('Pair 'RealT 'RealT))
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
  DrawGeodesic _ p q   -> finitePt p && finitePt q
  DrawRect _ _ p q     -> finitePt p && finitePt q
  Write _ _ p          -> finitePt p
  SetStroke {}         -> True
  SetFill {}           -> True
  Clear {}             -> True
  where
    finite x = not (isNaN x || isInfinite x)
    finitePt (x, y) = finite x && finite y

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
  DrawGeodesic env pt1 pt2 -> DrawGeodesic env <$> f pt1 <*> f pt2
  DrawLine env pt1 pt2 -> DrawLine env <$> f pt1 <*> f pt2
  DrawRect env fill pt1 pt2 -> DrawRect env fill <$> f pt1 <*> f pt2
  SetStroke env c -> SetStroke env <$> f c
  SetFill env c -> SetFill env <$> f c
  Clear env -> pure $ Clear env
  Write env txt pt -> Write env <$> f txt <*> f pt
