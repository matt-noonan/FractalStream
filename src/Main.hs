
module Main where

import UI.Gloss.Viewer
import Exec.Haskell
import Exec.Placeholder
import Color.Colorize

import Graphics.Gloss

main :: IO ()
main = glossView (runComplexDynamics rabbit) (checkers yellow blue)
