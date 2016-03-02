{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

-- for wxMain
import Lang.Numbers
import Lang.Planar

import Exec.Haskell
import Exec.Placeholder
import Exec.Region

import Color.Colorize

import UI.WX.Viewer


-- for parserMain
import Lang.Expr
import Lang.Parse.Expr
import Lang.Expr.Transform
import Lang.Expr.Typing
import Lang.Expr.Print

import System.IO

-- tinkering with accelerate
import Exec.Accelerate (computeMandel)

-- the mains

main :: IO ()
main = wxMain

wxMain :: IO ()
wxMain = wxView viewport action
    where viewport = flippedRectangle (complex (-2.5) 2) (complex 1.5 (-2))
          darkChecker c = checker c (darker c) :: Colorizer C
          dyn = runParametric mandelbrot
          col = blackInterior $ darkChecker $ smoothedRainbow (loglogSmoothing 2) 20
          action = computeMandel col
          --action = return . map (runColorizer col . runDynamics dyn)
          
parserMain :: IO ()
parserMain = do
  putStr "fs> "
  hFlush stdout
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
