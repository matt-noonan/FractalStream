
module Main where

import Exec.Haskell
import Exec.Placeholder

import Color.Color
import Color.Colorize

import UI.WX.Viewer

main :: IO ()
main = wxView (runComplexDynamics rabbit) checkers
    where checkers = checker (grad yellow white) (grad (dark blue) blue)
          grad c1 c2 = gradient 5 (solid c1) (solid c2)
