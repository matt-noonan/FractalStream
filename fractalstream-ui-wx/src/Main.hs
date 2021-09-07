{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import qualified Data.Color as FSColor
import UI.WX.Viewer
import Graphics.UI.WXCore.WxcTypes (rgb)
import Control.Concurrent
import Control.Monad
import Data.Planar
import Data.Complex

main :: IO ()
main = do
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("Hello from main, on thread " ++ show tid ++ " " ++ show capInfo ++ " " ++ show bound)
    wxMain
    putStrLn "main is done"

wxMain :: IO ()
wxMain = do
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("Hello from wxMain, on thread " ++ show tid ++ " " ++ show capInfo ++ " " ++ show bound)
    wxView viewport (fmap (map colorConvert) . action)
  where
    viewport = flippedRectangle (-2.5, 2) (1.5, -2)
    colorConvert c =
      let (r,g,b) = FSColor.colorToRGB c
      in rgb r g b
    action pts = forM pts $ \(x,y) -> pure (mandel 100 (x :+ y))

mandel :: Int -> Complex Double -> FSColor.Color
mandel n c = go 0 0
  where
    go i z = let z' = z*z + c
             in if magnitude z' > 10
                then if imagPart z' > 0
                     then FSColor.red
                     else FSColor.yellow
                else if i == n
                     then FSColor.black
                     else go (i + 1) z'

{-
init z to 0
init i to 0
repeat
  set z to z^2 + c
  set i to i + 1
while |z| < maxRadius and i < maxIters
if i >= 100 then
  output color black
else
  output color red
-}
