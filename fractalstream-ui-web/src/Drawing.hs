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
as the overlay does it ('visibleRuns' of points sampled along it), into
the runs of it on the near side of the globe.

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

import Actor.Viewer.Sphere (Projection(..), projectToScreen, visibleRuns, sampleSegment, sampleCircle)
import Data.Color (Color, colorToRGB, white)
import Language.Draw

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Text.Printf (printf)

drawingJSON :: Projection -> [[DrawCommand]] -> Aeson.Value
drawingJSON proj = Aeson.toJSON . map (layer white white)
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
      DrawLine _ a b -> case proj of
        FlatProjection -> [ obj "line" [ "a" .= a, "b" .= b, "s" .= hex stroke ] | finite a, finite b ]
        GlobeProjection _ -> runs stroke (sampleSegment 64 a b)
      DrawCircle _ filled r c -> case proj of
        FlatProjection ->
          [ obj "circle" [ "c" .= c, "r" .= r, "f" .= fillOf filled, "s" .= hex stroke ] | finite c, finite (r, r) ]
        GlobeProjection _ -> outline filled (sampleCircle 96 c r)
      DrawRect _ filled a@(x1, y1) b@(x2, y2) -> case proj of
        FlatProjection ->
          [ obj "rect" [ "a" .= a, "b" .= b, "f" .= fillOf filled, "s" .= hex stroke ] | finite a, finite b ]
        GlobeProjection _ ->
          let corners = [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]
          in outline filled (concat (zipWith (\p q -> init (sampleSegment 32 p q)) corners (drop 1 corners)) ++ [(x1, y1)])
      Write _ t p -> [ obj "text" [ "p" .= q, "t" .= t, "s" .= hex stroke ] | Just q <- [at p] ]
      _ -> []
      where
        fillOf filled = if filled then Just (hex fill) else Nothing
        -- a closed outline on the sphere: filled (and outlined) if it's
        -- all on the near side, else the runs of it that are
        outline filled pts
          | filled, [r] <- visible pts, length r == length pts =
              [ obj "polygon" [ "pts" .= r, "f" .= hex fill, "s" .= hex stroke ] ]
          | otherwise = runs stroke pts

    -- where a point is on the screen plane (nowhere, behind the globe)
    at :: (Double, Double) -> Maybe (Double, Double)
    at p = case proj of
      FlatProjection -> if finite p then Just p else Nothing
      GlobeProjection _ -> projectToScreen proj p

    visible = filter (not . null) . map (filter finite) . visibleRuns proj . filter finite

    runs stroke pts = case visible pts of
      [] -> []
      rs -> [ obj "runs" [ "runs" .= rs, "s" .= hex stroke ] ]

    obj :: Text.Text -> [(Aeson.Key, Aeson.Value)] -> Aeson.Value
    obj k fields = Aeson.object (("k" .= k) : fields)

finite :: (Double, Double) -> Bool
finite (x, y) = not (isNaN x || isInfinite x || isNaN y || isInfinite y)

hex :: Color -> String
hex c = let (r, g, b) = colorToRGB c in printf "#%02x%02x%02x" r g b
