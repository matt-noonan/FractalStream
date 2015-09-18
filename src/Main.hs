{- |
Module       : Main
Description  : Main entry point into FractalStream 
-}
module Main where

import Lang.Numbers

import Exec.Haskell
import Exec.Placeholder

import Color.Colorize

import UI.WX.Viewer

main :: IO ()

main = wxView (complex (-2.5) 2, complex 1.5 (-2))
              (runParametric mandelbrot)
              (blackInterior $ darkChecker $ smoothedRainbow (loglogSmoothing 2) 20)
    where darkChecker c = checker c (darker c)