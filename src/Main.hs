{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

{-}
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
import Lang.Expr.Transform
import Lang.Expr.Typing
import Lang.Expr.Print

main :: IO ()
main = do
  input <- getLine
  let pResult = parseExpr input
  case pResult of
    Left  err -> putStrLn $ show err
    Right e0  -> do
      let e = Fix (Lambda "z" e0)
      putStrLn $ "parsed AST: " ++ show e
      tt <- runTCStateT emptyContext $ do
              setType' "z" Complex_T
              setType' "C" Complex_T
              typeExpr e
      case tt of
        Left err -> putStrLn $ show err
        Right (te, ctx) -> do
          let e' = precompile te
          putStrLn $ "precompiled AST: " ++ show (forget e')

  return ()
