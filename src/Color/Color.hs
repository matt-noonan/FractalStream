module Color.Color (  Color
                     , light
                     , dark
                     , black
                     , red
                     , green
                     , blue
                     , white
                     , grey
                     , cyan
                     , purple
                     , yellow
                     , colorToRGB
                     , rgbToColor
                     , mixColors
                     , averageColor
                     , peekColor
                     , pokeColor
                     ) where

import Graphics.UI.WXCore.WxcTypes

import Foreign.Storable

-- Constructors and destructors

colorToRGB :: Color -> (Word8, Word8, Word8)
colorToRGB c = (colorRed c, colorGreen c, colorBlue c)

rgbToColor :: (Word8, Word8, Word8) -> Color
rgbToColor (r,g,b) = rgb r g b

-- Basic colors

black  :: Color
red    :: Color
green  :: Color
blue   :: Color
white  :: Color
grey   :: Color
cyan   :: Color
yellow :: Color
purple :: Color

black  = rgbToColor (  0,   0,   0)
red    = rgbToColor (255,   0,   0)
green  = rgbToColor (  0, 255,   0)
blue   = rgbToColor (  0,   0, 255)
white  = rgbToColor (255, 255, 255)
grey   = rgbToColor (128, 128, 128)
cyan   = rgbToColor (  0, 255, 255)
purple = rgbToColor (255,   0, 255)
yellow = rgbToColor (255, 255,   0)

-- Color combinators

light :: Color -> Color
light = mixColors 0.5 white

dark :: Color -> Color
dark  = mixColors 0.5 black

mixColors :: Double -> Color -> Color -> Color

mixColors pct c1 c2 = rgb (mix r1 r2) (mix g1 g2) (mix b1 b2)
    where (r1,g1,b1) = colorToRGB c1
          (r2,g2,b2) = colorToRGB c2
          mix :: Word8 -> Word8 -> Word8
          mix x y = round $ pct * fromIntegral x + (1.0 - pct) * fromIntegral y

averageColor :: [Color] -> Color

averageColor [] = grey
averageColor colors = go colors (0,0,0) 0
    where toInts   (x,y,z) = (toInteger x, toInteger y, toInteger z)
          fromInts (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)
          go cols (r,g,b) n = case cols of 
            []     -> rgbToColor $ fromInts (r `div` n, g `div` n, b `div` n)
            (c:cs) -> let (r',g',b') = toInts $ colorToRGB c in go cs (r + r', g + g', b + b') (n + 1)

peekColor :: Ptr Word8 -> Int -> IO Color
peekColor buf idx = do
    let index = 4 * idx
    r <- peekByteOff buf $ index + 3
    g <- peekByteOff buf $ index + 2
    b <- peekByteOff buf $ index + 1            
    return $ rgbToColor (r,g,b)

pokeColor :: Ptr Word8 -> Int -> Color -> IO ()

pokeColor buf idx col = do
    let index = 4 * idx
    let (r,g,b) = colorToRGB col
    pokeByteOff buf (index + 3) r
    pokeByteOff buf (index + 2) g
    pokeByteOff buf (index + 1) b
    pokeByteOff buf (index + 0) (0xff :: Word8)    

