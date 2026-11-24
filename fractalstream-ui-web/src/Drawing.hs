{-# LANGUAGE OverloadedStrings #-}
{- |
Module       : Drawing
Description  : A viewer's drawings, as the page draws them

What a viewer's tools and overlay have drawn (its layers of draw
commands), as JSON for the page, which draws them over the picture with
Canvas2D (@web/src/drawing.ts@), as "Actor.Viewer.Overlay" draws them for
fsrun's captures: each layer starts with white for the pen and the fill.

The page places a shape by its screen-plane coordinates: on the plane,
those are the model's own; on the sphere (where a viewer's center is the
globe's), they're in units of its radius, so a shape is projected here,
as the overlay does it ('lineOnScreen', 'circleOnScreen': points sampled
along it), into the runs of it that are visible. A drawing's points are
homogeneous (see 'Language.Draw.DrawSink').

Shapes:

> { "k": "point", "p": [x, y], "s": stroke }
> { "k": "line", "a": [x, y], "b": [x, y], "s": stroke }
> { "k": "circle", "c": [x, y], "r": radius (in model units), "f": fill or null, "s": stroke }
> { "k": "rect", "a": [x, y], "b": [x, y], "f": fill or null, "s": stroke }
> { "k": "text", "p": [x, y], "t": text, "s": stroke }
> { "k": "runs", "runs": [ [ [x, y], ... ], ... ], "s": stroke }      (the sphere)
> { "k": "polygon", "pts": [ [x, y], ... ], "f": fill, "s": stroke }  (the sphere)

with colors as #rrggbb. A shape at a point that isn't finite is left out
(as the overlay skips it).
-}
module Drawing
  ( drawingJSON
  ) where

import Actor.Viewer.Sphere (Projection(..), HPoint, homPlane, homToScreen, lineOnScreen, circleOnScreen, flatShift)
import Data.Color (Color, colorToRGB, white)
import Data.Maybe (fromMaybe)
import Language.Draw

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Text.Printf (printf)

-- | A viewer's drawings, one list per layer: drawn with each projection
-- (a grouped viewer's are repeated in each tile on the screen: see
-- 'Actor.Viewer.overlayProjections').
drawingJSON :: [Projection] -> [[DrawCommand]] -> Aeson.Value
drawingJSON projs layers = Aeson.toJSON [ concat [ drawingLayer p l | p <- projs ] | l <- layers ]

drawingLayer :: Projection -> [DrawCommand] -> [Aeson.Value]
drawingLayer proj = layer white white
  where
    layer :: Color -> Color -> [DrawCommand] -> [Aeson.Value]
    layer _ _ [] = []
    layer stroke fill (cmd : cmds) = case cmd of
      SetStroke _ c -> layer c fill cmds
      SetFill _ c -> layer stroke c cmds
      Clear _ -> layer stroke fill cmds
      _ -> shape stroke fill cmd ++ layer stroke fill cmds

    shape :: Color -> Color -> DrawCommand -> [Aeson.Value]
    shape stroke fill = \case
      DrawPoint _ p -> [ obj "point" [ "p" .= q, "s" .= hex stroke ] | Just q <- [at p] ]
      -- (the viewer's geodesic: see 'lineOnScreen')
      DrawLine _ a b -> case proj of
        _ | Just _ <- flatShift proj -> [ obj "line" [ "a" .= plane a, "b" .= plane b, "s" .= hex stroke ]
                          | finite (plane a), finite (plane b) ]
        _ -> runs stroke (lineOnScreen proj a b)
      -- (a marker: a circle on the screen, its radius in pixels)
      DrawCircle _ filled ScreenPixels r c ->
        [ obj "marker" [ "c" .= q, "px" .= r, "f" .= fillOf filled, "s" .= hex stroke ] | Just q <- [at c], finite (r, r) ]
      -- (its radius in the viewer's own metric: see 'circleOnScreen')
      DrawCircle _ filled ModelUnits r c -> case proj of
        _ | Just _ <- flatShift proj ->
          [ obj "circle" [ "c" .= plane c, "r" .= r, "f" .= fillOf filled, "s" .= hex stroke ]
          | finite (plane c), finite (r, r) ]
        _ -> case circleOnScreen proj c r of
          (rs, True) | filled, [run] <- clean rs ->
            [ obj "polygon" [ "pts" .= run, "f" .= hex fill, "s" .= hex stroke ] ]
          (rs, _) -> runs stroke rs
      -- (only on the plane: a sphere's or a hyperbolic plane's tool can't
      -- draw rectangles)
      DrawRect _ filled a b -> case proj of
        _ | Just _ <- flatShift proj ->
          [ obj "rect" [ "a" .= plane a, "b" .= plane b, "f" .= fillOf filled, "s" .= hex stroke ]
          | finite (plane a), finite (plane b) ]
        _ -> []
      Write _ t p -> [ obj "text" [ "p" .= q, "t" .= t, "s" .= hex stroke ] | Just q <- [at p] ]
      _ -> []
      where
        fillOf filled = if filled then Just (hex fill) else Nothing
        -- (a point of the plane, moved by the copy's lattice translation,
        -- for a lattice viewer's copies in other tiles)
        plane p = let (x, y) = homPlane p
                      (dx, dy) = fromMaybe (0, 0) (flatShift proj)
                  in (x + dx, y + dy)

    -- where a point is on the screen plane (nowhere, behind the globe)
    at :: HPoint -> Maybe (Double, Double)
    at p = case homToScreen proj p of
      Just q | finite q -> Just q
      _ -> Nothing

    clean = filter (\r -> length r >= 2) . map (filter finite)

    runs stroke rs = case clean rs of
      [] -> []
      rs' -> [ obj "runs" [ "runs" .= rs', "s" .= hex stroke ] ]

    obj :: Text.Text -> [(Aeson.Key, Aeson.Value)] -> Aeson.Value
    obj k fields = Aeson.object (("k" .= k) : fields)

finite :: (Double, Double) -> Bool
finite (x, y) = not (isNaN x || isInfinite x || isNaN y || isInfinite y)

hex :: Color -> String
hex c = let (r, g, b) = colorToRGB c in printf "#%02x%02x%02x" r g b
