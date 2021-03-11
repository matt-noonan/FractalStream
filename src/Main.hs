{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

-- for wxMain
import           Color.Colorize
import           Lang.Numbers
import           Lang.Planar

import           UI.WX.Viewer


-- for parserMain
import           Lang.Expr
import           Lang.Expr.Transform
import           Lang.Expr.Typing
import           Lang.Parse.Expr

import           System.IO

import           Exec.Haskell
import           Exec.Placeholder
import           Exec.Region


-- tinkering with accelerate
import           Exec.Accelerate     (computeMandel)

-- tinkering with llvm, sans accelerate
import           Exec.LLVM


import           Control.Concurrent


-- the mains

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
    wxView viewport action
  where
    viewport = flippedRectangle (complex (-2.5) 2) (complex 1.5 (-2))
    darkChecker c = checker c (darker c) :: Colorizer C
    col = blackInterior $ darkChecker $ smoothedRainbow (loglogSmoothing 2) 20
    action =
      case 0 + 1 of
          0 -> computeMandel col  -- accelerate
          1 -> computeMandel' col -- llvm
          (_ :: Int) -> pure . map (runColorizer col .  -- pure haskell
                           runDynamics (runParametric mandelbrot))

parserMain :: IO ()
parserMain = do
  putStr "fs> "
  hFlush stdout
  input <- getLine
  case parseExpr input of
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
        Right (te, _ctx) -> do
          let e' = precompile te
          putStrLn $ "precompiled AST: " ++ show (forget e')
