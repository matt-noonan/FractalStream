
module UI.Gloss.Viewer (glossView) where 

import Lang.Numbers
import Lang.Planar
import Exec.Region
import Color.Colorize
import UI.Tile

import Graphics.Gloss

glossView :: Dynamics C -> Colorizer C -> IO ()

glossView dyn col = do
    let (width, height) = (512, 512)

    let mRect = rectangle (complex (-2) 2) (complex 2 (-2))

    tile <- renderTile dyn col (width, height) mRect

    simulate 
        (InWindow "FractalStream" (width, height) (10, 10))
        white
        5
        "world"
        (drawGlossyTile tile)
        (\_ _ world -> world)
