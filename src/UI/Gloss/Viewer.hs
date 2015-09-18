{- |
Module      : UI.Gloss.Viewer
Description : Very simple gloss-based viewer, for initial experiments.
-}
module UI.Gloss.Viewer ( glossView
                       , GlossState
                       ) where 

import Lang.Numbers
import Lang.Planar
import Exec.Region
import Color.Colorize
import UI.Tile

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data GlossState = GlossState { viewRect :: Rectangle Viewport
                             , dragging :: Maybe (Float,Float)
                             , zoomBox :: Maybe ((Float,Float), (Float,Float))
                             }

glossView :: Dynamics C -> Colorizer C -> IO ()

glossView dyn col = do
    let (width, height) = (512, 512)

    let vRect = rectangle (fromCoords $ (0,0)) (fromCoords $ (fromIntegral width, fromIntegral height))
    let mRect = rectangle (complex (-2) 2) (complex 2 (-2))

    tile <- renderTile dyn col (width, height) mRect

    play 
        (InWindow "FractalStream" (width, height) (10, 10))
        white
        5
        (GlossState { viewRect = vRect
                    , dragging = Nothing
                    , zoomBox  = Nothing
                    })
        (withOverlays $ drawGlossyTile tile)
        eventHandler
        (\_ world -> world)

withOverlays :: (GlossState -> Picture) -> (GlossState -> Picture)

withOverlays f s@GlossState { zoomBox = Just (p,q) } = Pictures [f s, drawZoomBox p q]
withOverlays f s = f s

drawGlossyTile :: Tile a -> GlossState -> Picture

drawGlossyTile tile s = translate (realToFrac dx) (realToFrac dy) pic
    where (w,h,buf) = tileData tile
          (dx,dy) = toCoords $ upperLeft $ viewRect s
          pic = bitmapOfForeignPtr w h buf False

eventHandler :: Event -> GlossState -> GlossState

eventHandler (EventKey (MouseButton LeftButton) Down (Modifiers { shift=Up, ctrl=Down, alt=Up }) p) s = s { dragging = Just p }
eventHandler (EventKey (MouseButton LeftButton) Down (Modifiers { shift=Up, ctrl=Up,   alt=Up }) p) s = s { zoomBox = Just (p,p) }
eventHandler (EventKey (MouseButton LeftButton) Up   _ _) s = s { dragging = Nothing, zoomBox = Nothing }

eventHandler (EventMotion _) s@GlossState { dragging = Nothing, zoomBox = Nothing } = s

eventHandler (EventMotion (x1,y1)) s@GlossState { dragging = Just (x0,y0), zoomBox = Nothing } = s'
    where s' = s { viewRect = translateRect (realToFrac $ x1 - x0, realToFrac $ y1 - y0) (viewRect s)
                 , dragging = Just (x1,y1)
                 }

eventHandler (EventMotion q) s@GlossState { dragging = Nothing, zoomBox = Just (p,_) } = s'
    where s' = s { zoomBox = Just (p,q) }

eventHandler _ s = s

clearBlue :: Color
clearBlue = makeColor 0.75 0.75 1.0 0.5

drawZoomBox :: (Float,Float) -> (Float,Float) -> Picture

drawZoomBox (x,y) (x',y') = Pictures [ Color clearBlue $ Polygon [(x0,y0), (x1,y0), (x1,y1), (x0,y1)]
                                     , Color white     $ Line    [(x0,y0), (x1,y0), (x1,y1), (x0,y1), (x0,y0)]
                                     ]
    where (x0,y0,x1,y1) = (min x x', min y y', max x x', max y y')

