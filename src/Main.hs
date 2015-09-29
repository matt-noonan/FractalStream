{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

{-
import Lang.Numbers
import Lang.Planar

import Exec.Haskell
import Exec.Placeholder

import Color.Colorize

import UI.WX.Viewer

main :: IO ()
main = wxView (flippedRectangle (complex (-2.5) 2) (complex 1.5 (-2)))
              (runParametric mandelbrot)
              (blackInterior $ darkChecker $ smoothedRainbow (loglogSmoothing 2) 20)
    where darkChecker c = checker c (darker c)
-}

import Lang.Expr
import Lang.Parse.Expr

main :: IO ()
main = do
  input <- getLine
  let pResult = parseCExpr input
  case pResult of
    Left  err -> putStrLn $ show err
    Right e   -> do
      putStrLn $ "parsed AST " ++ show e
      let (e'_r, e'_i) = toRExpr e
      putStrLn ""
      putStrLn $ "real part: " ++ show e'_r
      putStrLn $ "imag part: " ++ show e'_i
      let (e_r, e_i) = (toPureRExpr e'_r, toPureRExpr e'_i)
      putStrLn ""
      putStrLn $ "real part: " ++ show e_r
      putStrLn $ "imag part: " ++ show e_i
      let (e1_r, e1_i) = (simplifyRExpr e_r, simplifyRExpr e_i)
      putStrLn ""
      putStrLn $ "real part: " ++ show e1_r
      putStrLn $ "imag part: " ++ show e1_i
      let (e2_r, e2_i) = (constProp e1_r, constProp e1_i)
      putStrLn ""
      putStrLn "------------------------------------------"
      putStrLn ""
      putStrLn $ "real part: " ++ show e2_r
      putStrLn $ "imag part: " ++ show e2_i

  return ()
