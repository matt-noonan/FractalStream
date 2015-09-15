
module Main where

import Exec.Haskell
import Exec.Placeholder

import Color.Color
import Color.Colorize

import UI.WX.Viewer

main :: IO ()
main = wxView (runComplexDynamics rabbit) checkers
    where checkers = checker (grad yellow) (grad blue)
          grad c = gradient 5 (solid $ dark c) (solid $ light c)
