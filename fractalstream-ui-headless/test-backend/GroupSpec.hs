-- | Viewers with a group (@group=@): each pixel's point folded into the
-- group's fundamental polygon, tools' points too, tools' drawings repeated
-- in each tile, and views kept precise however far they're moved, end to
-- end.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module GroupSpec (spec) where

import Test.Hspec

import Data.List (isInfixOf)

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

-- | The modular group on the half-plane: its domain |re z| <= 1/2,
-- |z| >= 1 red, the tiles one step away blue, the rest grey, edges black;
-- yellow near `mark`. 1/150 per pixel, the real axis along the bottom: the
-- point x + y i is at pixel (300 + 150 x, 600 - 150 y).
project :: String
project = unlines
  [ "@config \"Mark\""
  , "mark : C := 5 + 5i"
  , ""
  , "@viewer \"G\" view=halfplane group=modular"
  , "color <- if side_distance < 1.5 pixel_size / im(z) then black ..."
  , "         else if |z - mark| < 0.08 then yellow ..."
  , "         else if fold_count = 0 then red else if fold_count = 1 then blue else grey"
  , ""
  -- (colored by the folded point alone, which the group doesn't change:
  -- red on the right half of the domain, blue on the left)
  , "@viewer \"I\" view=halfplane group=modular"
  , "color <- if side_distance < 1.5 pixel_size / im(z) then black else if re(z) > 0 then red else blue"
  , ""
  , "@tool \"Set\" for=\"G\""
  , "on click at p:"
  , "  mark <- p"
  , ""
  , "@tool \"Dot\" for=\"G\""
  , "on click at p:"
  , "  erase"
  , "  use rgb(0, 1, 0) for fill"
  , "  use rgb(0, 1, 0) for stroke"
  , "  draw filled circle at p with radius 0.05"
  , ""
  , "@tool \"Marker\" for=\"G\""
  , "on click at p:"
  , "  erase"
  , "  use rgb(0, 1, 0) for fill"
  , "  use rgb(0, 1, 0) for stroke"
  , "  draw filled circle at p with radius 6 pixels"
  ]

-- | The modular group again, from a table that the viewer's overlay fills
-- in (z + 1, z - 1, -1/z), unless @bad@: then its one side is the
-- identity, which can't be a side of anything. The tiles one step away
-- are blue.
tableProject :: Bool -> String
tableProject bad = unlines $
  [ "@config \"Group\""
  , "T : Table 2 of C"
  , ""
  , "@viewer \"G\" view=halfplane group=T groupCenter=\"2i\""
  , "color <- if fold_count = 0 then red else blue"
  , ""
  , "@tool overlay for=\"G\""
  , "on refresh:"
  , "  (rows, n) := table_size(T)"
  , "  if n = 0:" ]
  ++ (if bad
        then [ "    reset T to (4, 1)", "    T @ (1, 1) <- 1", "    T @ (4, 1) <- 1" ]
        else [ "    reset T to (4, 3)"
             , "    T @ (1, 1) <- 1", "    T @ (2, 1) <- 1", "    T @ (4, 1) <- 1"
             , "    T @ (1, 2) <- 1", "    T @ (2, 2) <- -1", "    T @ (4, 2) <- 1"
             , "    T @ (2, 3) <- -1", "    T @ (3, 3) <- 1" ])

scenario :: [String] -> Expectation
scenario = scenarioOn project

scenarioOn :: String -> [String] -> Expectation
scenarioOn proj cmds = runOn proj cmds >>= either expectationFailure pure

runOn :: String -> [String] -> IO (Either String ())
runOn proj cmds = do
  ens <- buildEnsembleFromScript proj >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands)

pixel :: (Int, Int) -> String -> String
pixel = at "G"

at :: String -> (Int, Int) -> String -> String
at v (x, y) rgb = "assert viewer \"" ++ v ++ "\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb ++ " within 8"

red, blue, black, yellow, green :: String
-- (green: the tool's rgb(0, 1, 0))
red = "(255, 0, 0)"
blue = "(0, 0, 255)"
black = "(0, 0, 0)"
yellow = "(255, 255, 0)"
green = "(0, 255, 0)"

spec :: Spec
spec = describe "a viewer with a group" $ do

  it "folds each pixel's point into the group's polygon" $ scenario
    -- 2i is in the domain; 0.8 + 2i is one step away (z - 1), and so is
    -- 0.5 i (-1/z); re z = 1/2 is a side
    [ "select viewer \"G\"", pixel (300, 300) red, pixel (420, 300) blue
    , pixel (300, 525) blue, pixel (375, 300) black ]

  it "gives its tools points folded into the polygon" $ scenario
    -- 1.3 + 1.5i folds to 0.3 + 1.5i
    [ "select viewer \"G\"", "select tool \"Set\"", "click at 1.3 + 1.5i"
    , pixel (345, 375) yellow, pixel (495, 375) yellow ]

  it "draws its tools' drawings in each tile" $ scenario
    -- 0.2 + 1.5i, and its copies one tile left and right
    [ "select viewer \"G\"", "select tool \"Dot\"", "click at 0.2 + 1.5i"
    , pixel (330, 375) green, pixel (480, 375) green, pixel (180, 375) green ]

  it "draws a marker its size on the screen, however the view is zoomed" $ scenario
    -- 0.2 + 1.5i, and its copies one tile left and right; then zoomed out
    -- (1/75 per pixel), where it is at (315, 488): 6 pixels in radius each
    -- time
    [ "select viewer \"G\"", "select tool \"Marker\"", "click at 0.2 + 1.5i"
    , pixel (335, 375) green, pixel (338, 375) red, pixel (485, 375) green, pixel (185, 375) green
    , "set viewer pixel size to 1/75"
    , pixel (320, 488) green, pixel (323, 488) red ]

  it "uses a group's table filled in by the overlay, from the first picture" $ scenarioOn (tableProject False)
    [ "select viewer \"G\"", pixel (300, 300) red, pixel (420, 300) blue, pixel (300, 525) blue ]

  it "says what's wrong with a group's table it can't use" $ do
    r <- runOn (tableProject True) [ "select viewer \"G\"", pixel (300, 300) red ]
    r `shouldSatisfy` either ("The group's table, `T`, can't be used: " `isInfixOf`) (const False)

  it "keeps its view precise however far it's moved" $ scenario
    -- forty drags along the axis of A(z) = (2z + 1)/(z + 1), each by its
    -- translation length: each moves the view by A, an element of the
    -- group, so a picture of the folded points is the same (whatever
    -- views are reduced to); but without reducing it, the view would be
    -- about 77 units out, and its numbers 10^16 or so
    ( ["select viewer \"I\""]
      ++ replicate 40 "drag view from 0.5 + 1.118033988749895i to 1.5714285714285714 + 0.31943828249996997i"
      ++ [ at "I" (330, 300) red, at "I" (270, 300) blue, at "I" (375, 300) black ] )
