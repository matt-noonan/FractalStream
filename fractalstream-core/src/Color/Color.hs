{- |
Module      : Color.Color
Description : Color definitions, modifiers, and combinators.
-}
module Color.Color (
-- * Constructors and destructors
      Color
    , colorToRGB
    , rgbToColor
-- * Basic colors
    , black
    , red
    , green
    , blue
    , white
    , grey
    , orange
    , violet
    , purple
    , yellow
-- * Color modifiers and combinators
    , light
    , dark
--    , clear
    , lightenBy
    , darkenBy
    , mixColors
    , averageColor
    , invertColor
-- * Manipulating color buffers
    , peekColor
    , pokeColor
    ) where

import Data.Word
import Foreign.Storable
import Foreign.Ptr

data Color = Color Word8 Word8 Word8
  deriving (Eq, Ord)

-- | Extract the red/green/blue channels from a color
colorToRGB :: Color -> (Word8, Word8, Word8)
colorToRGB (Color r g b) = (r, g, b)

-- | Construct a color from its red/green/blue channels
rgbToColor :: (Word8, Word8, Word8) -> Color
rgbToColor (r,g,b) = Color r g b

-- | Black color.
black  :: Color
black  = rgbToColor (  0,   0,   0)

-- | White color.
white  :: Color
white  = rgbToColor (255, 255, 255)

-- | Grey color.
grey   :: Color
grey   = rgbToColor (128, 128, 128)

-- | Red color.
red    :: Color
red    = rgbToColor (255,   0,   0)

-- | Orange color.
orange :: Color
orange = rgbToColor (255, 100,   0)

-- | Yellow color.
yellow :: Color
yellow = rgbToColor (255, 255,   0)

-- | Green color.
green  :: Color
green  = rgbToColor (  0, 128,   0)

-- | Blue color.
blue   :: Color
blue   = rgbToColor (  0,   0, 255)

-- | Purple color.
purple :: Color
purple = rgbToColor (128,   0, 128)

-- | Violet color.
violet :: Color
violet = rgbToColor (255,   0, 160)

-- | Lighten the color
light :: Color -> Color
light = lightenBy 0.5

-- | Lighten the color by an amount in the range [0 .. 1]
lightenBy :: Double -> Color -> Color
lightenBy p c = rgbToColor (tint r, tint g, tint b)
    where (r,g,b) = colorToRGB c
          tint x = let x' = fromIntegral x in round $ x' + p * (255 - x')

-- | Darken the color
dark :: Color -> Color
dark = darkenBy 0.5

-- | Darken the color by an amount in the range [0 .. 1]
darkenBy :: Double -> Color -> Color
darkenBy p c = rgbToColor (shade r, shade g, shade b)
    where (r,g,b) = colorToRGB c
          shade x = let x' = fromIntegral x in round $ x' - p * x'

{-
-- | Make a transparent color
clear :: Color -> Color
clear c = rgba r g b 128 where (r,g,b) = colorToRGB c
-}

-- | Blend two colors
mixColors :: Double  -- ^ The ratio to take from the first color
          -> Color
          -> Color
          -> Color
mixColors pct c1 c2 = rgbToColor (mix r1 r2, mix g1 g2, mix b1 b2)
    where (r1,g1,b1) = colorToRGB c1
          (r2,g2,b2) = colorToRGB c2
          mix :: Word8 -> Word8 -> Word8
          mix x y = floor $ pct * fromIntegral x + (1.0 - pct) * fromIntegral y

-- | Average a list of colors.
averageColor :: [Color] -> Color
averageColor [] = grey
averageColor colors = go colors (0,0,0) 0
    where toInts   (x,y,z) = (toInteger x, toInteger y, toInteger z)
          fromInts (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)
          go cols (r,g,b) n = case cols of
            []     -> rgbToColor $ fromInts (r `div` n, g `div` n, b `div` n)
            (c:cs) -> let (r',g',b') = toInts $ colorToRGB c in go cs (r + r', g + g', b + b') (n + 1)

-- | Invert the color in RGB space.
invertColor :: Color -> Color
invertColor c = rgbToColor (255 - r, 255 - g, 255 - b)
    where (r,g,b) = colorToRGB c

-- | Read a color from a buffer of bytes
peekColor :: Ptr Word8 -> Int -> IO Color
peekColor buf idx = do
    let index = 3 * idx
    r <- peekByteOff buf $ index + 0
    g <- peekByteOff buf $ index + 1
    b <- peekByteOff buf $ index + 2
    return $ rgbToColor (r,g,b)

-- | Write a color into a buffer of bytes
pokeColor :: Ptr Word8 -> Int -> Color -> IO ()
pokeColor buf idx col = do
    let index = 3 * idx
    let (r,g,b) = colorToRGB col
    pokeByteOff buf (index + 2) b
    pokeByteOff buf (index + 1) g
    pokeByteOff buf (index + 0) r
