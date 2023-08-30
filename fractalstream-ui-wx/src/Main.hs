{-# language QuasiQuotes #-}
{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import qualified Data.Color as FSColor
import UI.WX.Viewer
import Control.Concurrent
import Control.Monad.State
import Data.Planar
import Data.Complex
import Text.RawString.QQ

import Language.Code.Simulator
import Language.Type
import Language.Environment
import Language.Effect
import Language.Code.Parser
import Language.Value.Parser
import Data.Proxy

import Actor.Settings
import Actor.Tool
import Actor.Viewer
import Event
import Language.Effect.Draw
import Language.Effect.Render
import Language.Effect.Provide
import Language.Code
import Backend.LLVM

import UI.Definition

main :: IO ()
main = do
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("Hello from main, on thread " ++ show tid ++ " "
              ++ show capInfo ++ " " ++ show bound)
    if False then wxMain else defToUI "/Users/mnoonan/FractalStream/wiz0.yaml"
    putStrLn "main is done"

wxMain :: IO ()
wxMain = do
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("Hello from wxMain, on thread " ++ show tid ++ " " ++ show capInfo ++ " " ++ show bound)
    let env = declare @"maxIters" IntegerType
            $ declare @"maxRadius" RealType
            $ declare @"x" RealType
            $ declare @"y" RealType
            $ endOfDecls
    withCompiledCode env juliaProgram0 $ \kernel -> do
      let action bs ss dz z out = runJX kernel out (fromIntegral bs) (fromIntegral ss) dz 100 10 z
      wxView viewport action mainViewer
  where
    viewport = flippedRectangle (-2.5, 2) (1.5, -2)

flag :: String
flag = [r|
if x < -0.65 then
  dark green
else if x > 0.65 then
  orange
else
  white|]

grid :: String
grid = [r|
init z : C to x + i y
init fz : C to sin z
init dfz : C to cos z
init size : R to 0.01 * |dfz|
if |sin (pi re(fz))| < size or |sin (pi im(fz))| < size then
  light blue
else
  white|]

mandelProgram0 :: String
mandelProgram0 = [r|
init C : C to x + i y
init z : C to 0
init k : Z to 0
loop
  set z to z z + C
  set k to k + 1
  |z| < maxRadius and k < maxIters
if k = maxIters then
  black
else
  init c1 : Color to if im(z) > 0 then blue else yellow
  init c2 : Color to if im(z) > 0 then green else orange
  init s : R to k + 1 - (log (log (|z|^2) / 2)) / log 2
  set s to cos (s pi / 10)
  set s to s s
  blend (s, c1, c2)|]

juliaProgram0 :: String
juliaProgram0 = [r|
init C : C to -0.12256 + 0.74486i
init z : C to x + i y
init k : Z to 0
init r2 : R to maxRadius * maxRadius
loop
  set z to z z + C
  set k to k + 1
  re(z) re(z) + im(z) im(z) < r2 and k < maxIters
if k = maxIters then
  black
else
  init c1 : Color to if im(z) > 0 then blue else yellow
  init c2 : Color to if im(z) > 0 then green else orange
  init s : R to k + 1 - (log (log (|z|^2) / 2)) / log 2
  set s to cos (s pi / 10)
  set s to s s
  blend (s, c1, c2)
|]

mandelProgram :: String
mandelProgram = [r|
init C : C to -0.11 + 0.75i
init z : C to x + i y
init k : Z to 0
init r2 : R to maxRadius * maxRadius
loop
  set z to z z + C
  set k to k + 1
  |z|^2 < r2 and k < maxIters
if k >= maxIters then
  black
else
  init s : R to k + 1 - (log ((log (|z|^2)) / 2)) / log 2
  set s to mod(s, 10) / 10
  init c1 : Color to blue
  init c2 : Color to white
  if im z > 0 then
    set c1 to yellow
    set c2 to red
  else
    pass
  if s < 0.5 then
    blend(2s, c1, c2)
  else
    blend(2s - 1, c2, c1)|]

mandelProgram' :: String
mandelProgram' = [r|
init curImage : bitmap <- render in x y plane (viewWidth,viewHeight) (-2,2) (1 / 128, 1 / 128)
  init C : C to -0.11 + 0.75i
  set complex z to x + i y
  set integer k to 0
  set real r2 to maxRadius * maxRadius
  loop
    set z to z z + C
    set k to k + 1
    re z re z + im z im z < r2 and k < maxIters
  if k >= maxIters then
    black
  else
    set s : R to k + 1 - (log ((log (|z|^2)) / 2)) / log 2
    set s to mod(s, 10) / 10
    set c1 : Color to blue
    set c2 : Color to white
    if im z > 0 then
      set c1 to yellow
      set c2 to red
    else
      pass
    if s < 0.5 then
      blend(2s, c1, c2)
    else
      blend(2s - 1, c2, c1)|]

mandelProgram'' :: String
mandelProgram'' = [r|
curImage : Image
curImage ⟵ render in x y plane (viewWidth,viewHeight) (-2,2) (1 / 128, 1 / 128)
  C : Complex
  C = -0.11 + 0.75i
  z : Complex
  z = x + i y

  repeat maxIters times with counter k
      z = z² + C
  while |z| < maxRadius

  if k = maxIters then
    black
  else
    s : Real
    s = if use_smoothing then k + 1 - log (log |z|² / 2) / log 2 else k
    s = mod (s, speed) / speed
    c₁ = blue  : Color
    c₂ = white : Color
    if im z > 0 then
        c₁ = yellow
        c₂ = red
    if s < 0.5 then
      blend(2s, c₁, c₂)
    else
      blend(2s - 1, c₂, c₁)|]

traceProgram :: String
traceProgram = [r|
init C : C to -0.11 + 0.75i
init z : C to posX + posY i
init z0 : C to 0
init k : Z to 0
erase
use white for line
loop
    set z0 to z
    set z to z z + C
    draw point at z0
    draw line from z0 to z
    set k to k + 1
    k < 100|]

traceProgram' :: String
traceProgram' = [r|
C : ℂ
C = -0.11 + 0.75i

z : ℂ
z = x + i y

z₀ : ℂ
z₀ = 0

erase
use white for line
repeat 100 times
    z₀ = z
    z  = z² + C
    draw point at z₀
    draw line from z₀ to z
|]

traceTool :: Tool '[Draw]
traceTool = Tool{..}
  where
    toolName = "Trace"
    toolHelp = "Click on a point to draw its trace"

    toolSettings = Settings{..}
    settingsList  = Bind (Proxy @"steps") IntegerType
                    (Setting Proxy (Scalar IntegerType 100)
                      (Just ("Trace steps",
                              [ InputValidator
                                "Trace steps must be non-negative"
                                validator ])))
                    $ EmptyContext
    settingsEnv = declare @"steps" IntegerType endOfDecls
    settingsTitle = "Trace settings"
    onChanged     = Nothing

    onClick = Just ( Fix
                   $ Effect Proxy Proxy VoidType
                   $ Provide posEnv settingsEnv VoidType trace)
    onMouseDown = Nothing
    onMouseUp = Nothing
    onMotion = Nothing
    onDrag = Nothing
    buttons = []
    env  = declare @"posX" RealType
         $ declare @"posY" RealType
         $ settingsEnv
    trace = case parseCode (EP (ParseEff noParser $ ParseEff drawEffectParser NoEffs)) env EmptyContext VoidType traceProgram of
      Right p -> p
      Left e  -> error (show e)

    validator = case parseValue settingsEnv EmptyContext BooleanType "steps > 0" of
      Right p -> p
      Left e -> error (show e)

mainViewer :: Viewer
mainViewer = Viewer{..}
  where
    onTimer = Nothing
    viewerTools = [traceTool]
    viewerSettings = Settings{..}
    settingsList = Bind (Proxy @"maxRadius") RealType
                     (Setting Proxy (Scalar RealType 10)
                       (Just ("Max. radius", [])))
                 $ Bind (Proxy @"maxIters") IntegerType
                     (Setting Proxy (Scalar IntegerType 100)
                       (Just ("Max. iterations", [])))
                 $ EmptyContext
    settingsEnv = contextToEnv settingsList

    settingsTitle = "FractalStream demo viewer settings"
    onChanged = Nothing
    onResize  = Nothing
    onRefresh = Just ( Fix
                     $ Effect Proxy Proxy VoidType
                     $ Provide EmptyEnvProxy settingsEnv VoidType mandelCode)
    viewToModel = case parseValue envV2M EmptyContext (PairType RealType RealType) v2m of
      Right c -> c
      Left e -> error (show e)
    modelToView = case parseValue envM2V EmptyContext (PairType RealType RealType) m2v of
      Right c -> c
      Left e -> error (show e)

    envV2M = declare @"viewX" RealType
           $ declare @"viewY" RealType
           $ env

    envM2V = declare @"modelX" RealType
           $ declare @"modelY" RealType
           $ env

    env = declare @"maxRadius" RealType
        $ declare @"maxIters"  IntegerType
        $ endOfDecls

    v2m = "((viewX - 256) / 128, (viewY - 256) / 128)"
    m2v = "(128 modelX + 256, 128 modelY + 256)"

    mandelCode = case parseCode (EP (ParseEff noParser $ ParseEff renderEffectParser NoEffs)) settingsEnv EmptyContext VoidType mandelProgram' of
      Right c -> c
      Left e  -> error (show e)

mandel' :: Int -> Double -> Complex Double -> FSColor.Color
mandel' maxIters maxRadius (x :+ y) =
  let ctx = Bind (Proxy @"x") RealType x
          $ Bind (Proxy @"y") RealType y
          $ Bind (Proxy @"maxRadius") RealType maxRadius
          $ Bind (Proxy @"maxIters") IntegerType (fromIntegral maxIters)
          $ EmptyContext
  in evalState (simulate NoHandler prog) (ctx, ())
 where
   prog = case parseCode (EP NoEffs) env EmptyContext ColorType mandelProgram of
     Right p -> p
     Left _  -> case parseCode (EP NoEffs) env EmptyContext ColorType "dark red" of
       Right p -> p
       Left e  -> error (show e) -- should be unreachable
   env = declare @"x" RealType
       $ declare @"y" RealType
       $ declare @"maxRadius" RealType
       $ declare @"maxIters" IntegerType
       $ endOfDecls
