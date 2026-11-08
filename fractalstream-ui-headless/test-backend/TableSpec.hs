-- | Tables, end to end in the compiled backend: a tool allocates a table
-- over its viewer's window and fills it in, viewers read it (by index or
-- by point), and writes are copy-on-write.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module TableSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"P\""
  , "T : Table 2 of C"
  , "C : C := 0"
  , "r : R := 0"
  , ""
  -- a parameter plane, colored by the table at each pixel
  , "@viewer \"Par\" var=C pixelSize=1/100"
  , "(w, h) := table_size(T)"
  , "color <- if w = 0 then black else rgb(re(T @ point C) / 255, im(T @ point C) / 255, 0)"
  , ""
  -- a dynamical plane, reading the table at its parameter
  , "@viewer \"Dyn\""
  , "(w, h) := table_size(T)"
  , "color <- if w = 0 then black else rgb(re(T @ point C) / 255, r / 255, 0)"
  , ""
  , "@tool \"Fill\" for=\"Par\""
  , "on click at p:"
  , "  allocate T of size (viewer_width, viewer_height)"
  , "  for each j in range(1, viewer_width):"
  , "    for each k in range(1, viewer_height):"
  , "      T @ (j, k) <- j + k i"
  , ""
  , "@tool \"Bump\" for=\"Par\""
  , "on click at p:"
  , "  T @ point p <- 255"
  , ""
  , "@tool \"Alias\" for=\"Par\""
  , "on click at p:"
  , "  U := T"
  , "  T @ (1, 1) <- 99"
  , "  r <- re(U @ (1, 1))"
  ]

scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

at :: String -> (Int, Int) -> String -> String
at viewer (x, y) rgb = "assert viewer \"" ++ viewer ++ "\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb ++ " within 1"

fill :: [String]
fill = [ "select viewer \"Par\"", "resize viewer to 200x100", "select tool \"Fill\"", "click at 0" ]

spec :: Spec
spec = describe "tables" $ do

  it "start out empty" $ scenario
    [ "select viewer \"Par\"", at "Par" (10, 10) "(0, 0, 0)" ]

  it "are laid over the tool's viewer, one cell per pixel" $ scenario $ fill ++
    [ at "Par" (0, 0) "(1, 1, 0)"
    , at "Par" (10, 20) "(11, 21, 0)"
    , at "Par" (150, 50) "(151, 51, 0)" ]

  it "are read at a parameter by another viewer" $ scenario $ fill ++
    [ "set parameter C to -0.995 + 0.495i", at "Dyn" (5, 5) "(1, 0, 0)"
    , "set parameter C to 0.005 - 0.005i", at "Dyn" (5, 5) "(101, 0, 0)" ]

  it "are written at the cell nearest a point" $ scenario $ fill ++
    [ "select tool \"Bump\"", "click at 0.005 - 0.005i"
    , at "Par" (100, 50) "(255, 0, 0)"
    , at "Par" (101, 50) "(102, 51, 0)" ]

  it "are copied before being written while another variable holds them" $ scenario $ fill ++
    [ "select tool \"Alias\"", "click at 0"
    , at "Par" (0, 0) "(99, 0, 0)"
    , "set parameter C to 0.005 - 0.005i", at "Dyn" (5, 5) "(101, 1, 0)" ]
