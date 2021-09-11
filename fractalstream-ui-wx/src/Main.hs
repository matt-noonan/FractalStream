{-# language QuasiQuotes #-}
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

import Actor
import Actor.Settings
import Actor.Tool
import Actor.Viewer
import Event
import Language.Effect.Draw
import Language.Effect.Render
import Language.Effect.Provide
import Language.Code

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
    wxView viewport (fmap (map colorConvert) . action) mainViewer
  where
    viewport = flippedRectangle (-2.5, 2) (1.5, -2)
    colorConvert c =
      let (rd,g,b) = FSColor.colorToRGB c
      in rgb rd g b
    action pts = forM pts $ \(x,y) -> pure (mandel' 100 10 (x :+ y))

mandelProgram :: String
mandelProgram = [r|
init C : C to -0.11 + 0.75i
init z : C to x + i y
init k : Z to 0
init r2 : R to maxRadius * maxRadius
loop
  set z to z z + C
  set k to k + 1
  re z re z + im z im z < r2 and k < maxIters
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
init bitmap : Z <- render in x y plane (viewWidth,viewHeight) (-2,2) (1 / 128, 1 / 128)
  init C : C to -0.11 + 0.75i
  init z : C to x + i y
  init k : Z to 0
  init r2 : R to maxRadius * maxRadius
  loop
    set z to z z + C
    set k to k + 1
    re z re z + im z im z < r2 and k < maxIters
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
      blend(2s - 1, c2, c1)
|]

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
  draw point at (re z0, im z0)
  draw line from (re z0, im z0) to (re z, im z)
  set k to k + 1
  k < 100
|]

traceTool :: Tool '[Draw]
traceTool = Tool{..}
  where
    toolName = "Trace"
    toolHelp = "Click on a point to draw its trace"

    toolSettings = Settings{..}
    settingsList  = Bind (Proxy @"steps") IntegerProxy
                    (Setting Proxy (Scalar IntegerProxy 100)
                      (Just ("Trace steps",
                              [ InputValidator
                                "Trace steps must be non-negative"
                                validator ])))
                    $ EmptyContext
    settingsEnv = BindingProxy (Proxy @"steps") IntegerProxy EmptyEnvProxy
    settingsTitle = "Trace settings"
    parentActor   = SomeActor mainViewer
    onChanged     = Nothing

    onClick = Just ( Fix
                   $ Effect Proxy Proxy VoidProxy
                   $ Provide posEnv settingsEnv VoidProxy trace)
    onMouseDown = Nothing
    onMouseUp = Nothing
    onMotion = Nothing
    onDrag = Nothing
    buttons = []
    env  = BindingProxy (Proxy @"posX") RealProxy
         $ BindingProxy (Proxy @"posY") RealProxy
         $ env'
    env' = BindingProxy (Proxy @"steps") IntegerProxy
         $ EmptyEnvProxy
    trace = case parseCode (EP (ParseEff noParser $ ParseEff drawEffectParser NoEffs)) env VoidProxy traceProgram of
      Right p -> p
      Left e  -> error (show e)

    validator = case parseValue env' BooleanProxy "steps > 0" of
      Right p -> p
      Left e -> error (show e)

mainViewer :: Viewer
mainViewer = Viewer{..}
  where
    onTimer = Nothing
    viewerTools = [traceTool]
    viewerSettings = Settings{..}
    settingsList = Bind (Proxy @"maxRadius") RealProxy
                     (Setting Proxy (Scalar RealProxy 10)
                       (Just ("Max. radius", [])))
                 $ Bind (Proxy @"maxIters") IntegerProxy
                     (Setting Proxy (Scalar IntegerProxy 100)
                       (Just ("Max. iterations", [])))
                 $ EmptyContext
    settingsEnv = contextToEnv settingsList

    settingsTitle = "FractalStream demo viewer settings"
    parentActor = error "undefined parent actor"
    onChanged = Nothing
    onResize  = Nothing
    onRefresh = Just ( Fix
                     $ Effect Proxy Proxy VoidProxy
                     $ Provide EmptyEnvProxy settingsEnv VoidProxy mandelCode)
    viewToModel = case parseValue envV2M (PairProxy RealProxy RealProxy) v2m of
      Right c -> c
      Left e -> error (show e)
    modelToView = case parseValue envM2V (PairProxy RealProxy RealProxy) m2v of
      Right c -> c
      Left e -> error (show e)

    envV2M = BindingProxy (Proxy @"viewX") RealProxy
           $ BindingProxy (Proxy @"viewY") RealProxy
           $ env

    envM2V = BindingProxy (Proxy @"modelX") RealProxy
           $ BindingProxy (Proxy @"modelY") RealProxy
           $ env

    env = BindingProxy (Proxy @"maxRadius")  RealProxy
        $ BindingProxy (Proxy @"maxIters")   IntegerProxy
        $ EmptyEnvProxy

    v2m = "((viewX - 256) / 128, (viewY - 256) / 128)"
    m2v = "(128 modelX + 256, 128 modelY + 256)"

    mandelCode = case parseCode (EP (ParseEff noParser $ ParseEff renderEffectParser NoEffs)) settingsEnv VoidProxy mandelProgram' of
      Right c -> c
      Left e  -> error (show e)

mandel' :: Int -> Double -> Complex Double -> FSColor.Color
mandel' maxIters maxRadius (x :+ y) =
  let ctx = Bind (Proxy @"x") RealProxy x
          $ Bind (Proxy @"y") RealProxy y
          $ Bind (Proxy @"maxRadius") RealProxy maxRadius
          $ Bind (Proxy @"maxIters") IntegerProxy (fromIntegral maxIters)
          $ EmptyContext
  in evalState (simulate NoHandler prog) (ctx, ())
 where
   prog = case parseCode (EP NoEffs) env ColorProxy mandelProgram of
     Right p -> p
     Left e  -> error (show e)
   env = BindingProxy (Proxy @"x") RealProxy
       $ BindingProxy (Proxy @"y") RealProxy
       $ BindingProxy (Proxy @"maxRadius") RealProxy
       $ BindingProxy (Proxy @"maxIters") IntegerProxy
       $ EmptyEnvProxy
