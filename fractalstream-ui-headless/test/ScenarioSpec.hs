{-# language QuasiQuotes #-}
module ScenarioSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Value
import Language.Value.Typecheck (ParsedValue, atType)
import Language.Value.Evaluator (evaluate)
import Language.Typecheck (TC(..), TCError)

import Scenario

import Text.RawString.QQ

-- | Resolve a closed (variable-free) value expression to a concrete
-- Double, for asserting on what the parser produced. Scenario-language
-- numeric literals never reference project variables (that's what
-- "set parameter"'s own value is for), so every 'ParsedValue' a command
-- carries ought to type-check against the empty environment.
resolveReal :: ParsedValue -> Either String Double
resolveReal pv = case (atType pv RealType :: TC (Value '( '[], 'RealT))) of
  TC (Left e)  -> Left (ppErr e)
  TC (Right v) -> Right (evaluate v EmptyContext)

resolveComplex :: ParsedValue -> Either String (Complex Double)
resolveComplex pv = case (atType pv ComplexType :: TC (Value '( '[], 'ComplexT))) of
  TC (Left e)  -> Left (ppErr e)
  TC (Right v) -> Right (evaluate v EmptyContext)

ppErr :: TCError -> String
ppErr = unlines . pp

shouldResolveTo :: ParsedValue -> Double -> Expectation
shouldResolveTo pv expected = case resolveReal pv of
  Left err -> expectationFailure ("expected " ++ show expected ++ ", got error: " ++ err)
  Right x  -> x `shouldBe` expected

onlyCommand :: String -> (Command -> Expectation) -> Expectation
onlyCommand src check = case parseScenario src of
  Left (_, err)  -> expectationFailure ("parse failed: " ++ err)
  Right [cmd]    -> check cmd
  Right cmds     -> expectationFailure ("expected exactly one command, got " ++ show (length cmds))

shouldFailToParse :: String -> Expectation
shouldFailToParse src = case parseScenario src of
  Left _  -> pure ()
  Right _ -> expectationFailure "expected a parse error, but it succeeded"

spec :: Spec
spec = describe "parseScenario" $ do

  it "parses select viewer" $
    onlyCommand [r|select viewer "Mandelbrot set"
|] $ \case
      SelectViewer n -> n `shouldBe` "Mandelbrot set"
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses select tool" $
    onlyCommand [r|select tool "Trace"
|] $ \case
      SelectTool n -> n `shouldBe` "Trace"
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses deselect tool" $
    onlyCommand [r|deselect tool
|] $ \case
      DeselectTool -> pure ()
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses set parameter with an arithmetic value expression" $
    onlyCommand [r|set parameter max_iters to 100 + 50
|] $ \case
      SetParameter n pv -> do
        n `shouldBe` "max_iters"
        pv `shouldResolveTo` 150
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses set tool parameter" $
    onlyCommand [r|set tool parameter radius to 2
|] $ \case
      SetToolParameter n pv -> do
        n `shouldBe` "radius"
        pv `shouldResolveTo` 2
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses resize viewer" $
    onlyCommand [r|resize viewer to 800x600
|] $ \case
      ResizeViewer w h -> (w, h) `shouldBe` (800, 600)
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses set viewer center" $
    onlyCommand [r|set viewer center to (1/2, 0)
|] $ \case
      SetViewerCenter (CartesianPoint px py) -> do
        px `shouldResolveTo` 0.5
        py `shouldResolveTo` 0
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses set viewer pixel size" $
    onlyCommand [r|set viewer pixel size to 1/128
|] $ \case
      SetViewerPixelSize pv -> pv `shouldResolveTo` (1/128)
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses click at, Cartesian form" $
    onlyCommand [r|click at (-0.75, 0.1)
|] $ \case
      ClickAt (CartesianPoint px py) -> do
        px `shouldResolveTo` (-0.75)
        py `shouldResolveTo` 0.1
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses click at, complex form" $
    onlyCommand [r|click at -0.75 + 0.1i
|] $ \case
      ClickAt (ComplexPoint pz) -> case resolveComplex pz of
        Left err -> expectationFailure ("expected -0.75 + 0.1i, got error: " ++ err)
        Right z  -> z `shouldBe` ((-0.75) :+ 0.1)
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses double-click at" $
    onlyCommand [r|double-click at (0, 0)
|] $ \case
      DoubleClickAt _ -> pure ()
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses drag from...to..." $
    onlyCommand [r|drag from (-0.75, 0.1) to (-0.7, 0.15)
|] $ \case
      DragFromTo (CartesianPoint x1 y1) (CartesianPoint x2 y2) -> do
        x1 `shouldResolveTo` (-0.75)
        y1 `shouldResolveTo` 0.1
        x2 `shouldResolveTo` (-0.7)
        y2 `shouldResolveTo` 0.15
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses press button" $
    onlyCommand [r|press button "Erase annotations"
|] $ \case
      PressButton n -> n `shouldBe` "Erase annotations"
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses trigger refresh" $
    onlyCommand [r|trigger refresh
|] $ \case
      TriggerRefresh -> pure ()
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses trigger timer" $
    onlyCommand [r|trigger timer "tick"
|] $ \case
      TriggerTimer n -> n `shouldBe` "tick"
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses wait for viewer" $
    onlyCommand [r|wait for viewer "Mandelbrot set"
|] $ \case
      WaitForViewer n -> n `shouldBe` "Mandelbrot set"
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses capture viewer ... to ..." $
    onlyCommand [r|capture viewer "Mandelbrot set" to "out.png"
|] $ \case
      CaptureViewer n f -> (n, f) `shouldBe` ("Mandelbrot set", "out.png")
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses capture viewer ... base to ..." $
    onlyCommand [r|capture viewer "Mandelbrot set" base to "out.png"
|] $ \case
      CaptureViewerBase n f -> (n, f) `shouldBe` ("Mandelbrot set", "out.png")
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses assert ... is ... without a tolerance" $
    onlyCommand [r|assert viewer "Mandelbrot set" pixel (128, 128) is (0, 0, 0)
|] $ \case
      AssertPixel n px color tol -> do
        n `shouldBe` "Mandelbrot set"
        px `shouldBe` (128, 128)
        color `shouldBe` (0, 0, 0)
        tol `shouldBe` Nothing
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses assert ... is ... within a tolerance" $
    onlyCommand [r|assert viewer "Mandelbrot set" pixel (128, 128) is (0, 0, 0) within 4
|] $ \case
      AssertPixel _ _ _ tol -> tol `shouldBe` Just 4
      cmd -> expectationFailure ("wrong command shape: " ++ describeShape cmd)

  it "parses a multi-command scenario, with comments" $
    case parseScenario [r|
# set up the view
select viewer "Mandelbrot set"
set parameter max_iters to 200

select tool "Trace"
click at (-0.75, 0.1)
capture viewer "Mandelbrot set" to "trace.png"
|] of
      Left (_, err) -> expectationFailure ("parse failed: " ++ err)
      Right cmds -> length cmds `shouldBe` 5

  it "rejects an unknown command" $
    shouldFailToParse [r|frobnicate viewer "x"
|]

  it "rejects a command missing its argument" $
    shouldFailToParse [r|select viewer
|]

describeShape :: Command -> String
describeShape = \case
  SelectViewer{}       -> "SelectViewer"
  SelectTool{}         -> "SelectTool"
  DeselectTool         -> "DeselectTool"
  SetParameter{}       -> "SetParameter"
  SetToolParameter{}   -> "SetToolParameter"
  ResizeViewer{}       -> "ResizeViewer"
  SetViewerCenter{}    -> "SetViewerCenter"
  SetViewerPixelSize{} -> "SetViewerPixelSize"
  ClickAt{}            -> "ClickAt"
  DoubleClickAt{}      -> "DoubleClickAt"
  DragFromTo{}         -> "DragFromTo"
  PressButton{}        -> "PressButton"
  TriggerRefresh       -> "TriggerRefresh"
  TriggerTimer{}       -> "TriggerTimer"
  WaitForViewer{}      -> "WaitForViewer"
  CaptureViewer{}      -> "CaptureViewer"
  CaptureViewerBase{}  -> "CaptureViewerBase"
  AssertPixel{}        -> "AssertPixel"
