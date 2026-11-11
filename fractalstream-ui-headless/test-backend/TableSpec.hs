-- | Tables, end to end in the compiled backend: a tool resets a table
-- over its viewer's window and fills it in, viewers read it (by index or
-- by point), and writes are copy-on-write. A table with more dimensions
-- has layers over the plane, which tools resize, copy between, and read
-- and write as lists.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module TableSpec (spec) where

import Test.Hspec

import Control.Exception (IOException)
import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as BS

import Actor.Ensemble (Ensemble(..), buildEnsembleFromScript, encodeEnsemble, ensembleTables, parseEnsembleFromBytes)
import Actor.SavedTables (captureTables, writeSavedTables)
import Data.DynamicValue (modifyValue)
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
  , "  reset T to (viewer_width, viewer_height)"
  , "  for each j in range(1, viewer_width):"
  , "    for each k in range(1, viewer_height):"
  , "      T @ (j, k) <- j + k i"
  , ""
  , "@tool \"Bump\" for=\"Par\""
  , "on click at p:"
  , "  T @ point p <- 255"
  , ""
  , "@tool \"Fill with 7\" for=\"Par\""
  , "on click at p:"
  , "  reset T to (viewer_width, viewer_height) filled with 7 + 3i"
  , "  T @ (1, 1) <- 1"
  , ""
  , "@tool \"Alias\" for=\"Par\""
  , "on click at p:"
  , "  U := T"
  , "  T @ (1, 1) <- 99"
  , "  r <- re(U @ (1, 1))"
  ]

scenario :: [String] -> Expectation
scenario = scenarioIn project

scenarioIn :: String -> [String] -> Expectation
scenarioIn proj cmds = do
  ens <- buildEnsembleFromScript proj >>= either (fail . snd) pure
  scenarioOn ens cmds

scenarioOn :: Ensemble -> [String] -> Expectation
scenarioOn ens cmds = runOn ens cmds >>= either expectationFailure pure

runOn :: Ensemble -> [String] -> IO (Either String ())
runOn ens cmds = do
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands)

at :: String -> (Int, Int) -> String -> String
at viewer (x, y) rgb = "assert viewer \"" ++ viewer ++ "\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb ++ " within 1"

fill :: [String]
fill = [ "select viewer \"Par\"", "resize viewer to 200x100", "select tool \"Fill\"", "click at 0" ]

spec :: Spec
spec = tableSpec >> layerSpec >> tupleSpec >> savedSpec

tableSpec :: Spec
tableSpec = describe "tables" $ do

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

  it "can be filled with a value when they're reset" $ scenario
    [ "select viewer \"Par\"", "resize viewer to 200x100", "select tool \"Fill with 7\"", "click at 0"
    , at "Par" (0, 0) "(1, 0, 0)", at "Par" (120, 70) "(7, 3, 0)", at "Par" (199, 99) "(7, 3, 0)" ]

  it "are copied before being written while another variable holds them" $ scenario $ fill ++
    [ "select tool \"Alias\"", "click at 0"
    , at "Par" (0, 0) "(99, 0, 0)"
    , "set parameter C to 0.005 - 0.005i", at "Dyn" (5, 5) "(101, 1, 0)" ]

-- | A table with layers over the plane: the viewer shows layer @m@ (in
-- red), the number of layers (in green), and @r@ (in blue).
layered :: String
layered = unlines
  [ "@config \"P\""
  , "T : Table 3 of R"
  , "L : Table 1 of R"
  , "C : C := 0"
  , "m : Z := 1"
  , "r : R := 0"
  , ""
  , "@viewer \"Par\" var=C pixelSize=1/100"
  , "(w, h, n) := table_size(T)"
  , "color <- if w = 0 or m > n then black else rgb((T @ (point C, m) else 0) / 255, n / 255, r / 255)"
  , ""
  -- two layers, the first all 10s, the second 20s with a top row of 25s
  , "@tool \"Fill\" for=\"Par\""
  , "on click at p:"
  , "  reset T to (viewer_width, viewer_height, 2) filled with 10"
  , "  T @ (*, *, 2) <- T @ (*, *, 1)"
  , "  for each j in range(1, viewer_width):"
  , "    for each k in range(1, viewer_height):"
  , "      T @ (j, k, 2) <- if k = 1 then 25 else 20"
  , ""
  , "@tool \"Grow\" for=\"Par\""
  , "on click at p:"
  , "  (w, h, n) := table_size(T)"
  , "  resize T to (w, h, n + 1) filled with 30"
  , ""
  , "@tool \"Crop\" for=\"Par\""
  , "on click at p:"
  , "  (w, h, n) := table_size(T)"
  , "  resize T to (w // 2, h, n)"
  , ""
  , "@tool \"Copy\" for=\"Par\""
  , "on click at p:"
  , "  T @ (*, *, 1) <- T @ (*, *, 2)"
  , ""
  , "@tool \"Stack\" for=\"Par\""
  , "on click at p:"
  , "  T @ (point p, *) <- [40, 50]"
  , ""
  , "@tool \"Sum\" for=\"Par\""
  , "on click at p:"
  , "  xs := T @ (point p, *)"
  , "  s := 0"
  , "  for each x in xs:"
  , "    s <- s + x"
  , "  r <- s"
  , ""
  -- all of a 1-dimensional table, as a list
  , "@tool \"Line\" for=\"Par\""
  , "on click at p:"
  , "  reset L to 3 from 0 to 1"
  , "  L @ * <- [1, 2, 3]"
  , "  L @ 2 <- 10 * (L @ 2)"
  , "  s := 0"
  , "  for each x in L @ *:"
  , "    s <- s + x"
  , "  r <- s"
  , ""
  , "@tool \"Bad\" for=\"Par\""
  , "on click at p:"
  , "  T @ (point p, 1) <- 99"
  , "  T @ (point p, *) <- [1, 2, 3]"
  ]

fillLayers :: [String]
fillLayers = [ "select viewer \"Par\"", "resize viewer to 200x100", "select tool \"Fill\"", "click at 0" ]

layerSpec :: Spec
layerSpec = describe "tables with layers" $ do

  it "are read at a point and a layer" $ scenarioIn layered $ fillLayers ++
    [ at "Par" (10, 10) "(10, 2, 0)"
    , "set parameter m to 2"
    , at "Par" (10, 10) "(20, 2, 0)", at "Par" (10, 0) "(25, 2, 0)" ]

  it "grow, keeping their cells, with new ones filled in" $ scenarioIn layered $ fillLayers ++
    [ "select tool \"Grow\"", "click at 0"
    , at "Par" (10, 10) "(10, 3, 0)"
    , "set parameter m to 3", at "Par" (10, 10) "(30, 3, 0)" ]

  it "shrink in the plane, at the right" $ scenarioIn layered $ fillLayers ++
    [ "select tool \"Crop\"", "click at 0"
    , at "Par" (10, 10) "(10, 2, 0)", at "Par" (150, 10) "(0, 2, 0)" ]

  it "copy one layer onto another" $ scenarioIn layered $ fillLayers ++
    [ "select tool \"Copy\"", "click at 0"
    , at "Par" (10, 10) "(20, 2, 0)", at "Par" (10, 0) "(25, 2, 0)" ]

  it "write and read the layers at a point as a list" $ scenarioIn layered $ fillLayers ++
    [ "select tool \"Stack\"", "click at 0.005 - 0.005i"
    , at "Par" (100, 50) "(40, 2, 0)", at "Par" (101, 50) "(10, 2, 0)"
    , "set parameter m to 2", at "Par" (100, 50) "(50, 2, 0)"
    , "select tool \"Sum\"", "click at 0.005 - 0.005i"
    , at "Par" (101, 50) "(20, 2, 90)" ]

  it "of one dimension are read and written whole, as lists" $ scenarioIn layered $ fillLayers ++
    [ "select tool \"Line\"", "click at 0"
    , at "Par" (10, 10) "(10, 2, 24)" ]

  it "can't be written from a list of the wrong length" $
    scenarioIn layered (fillLayers ++ [ "select tool \"Bad\"", "click at 0.005 - 0.005i" ])
      `shouldThrow` (\e -> "out of range" `isInfixOf` show (e :: IOException))

-- | Tables of tuples: the viewer shows a complex number's parts in red
-- and green, and an integer in blue; "Mixed" shows a color from a nested
-- tuple instead.
tupled :: String
tupled = unlines
  [ "@config \"P\""
  , "T : Table 2 of C x Z"
  , "Q : Table 1 of (Boolean x (Color x Z))"
  , "C : C := 0"
  , "mixed : Boolean := false"
  , ""
  , "@viewer \"Par\" var=C pixelSize=1/100"
  , "(z, k) := T @ point C else (0, 0)"
  , "n := table_size(Q)"
  , "(b, (col, m)) := if n = 0 then (false, (black, 0)) else Q @ 2"
  , "color <- if mixed then (if b and m = 9 then col else red) else rgb(re(z) / 255, im(z) / 255, k / 255)"
  , ""
  , "@tool \"Fill\" for=\"Par\""
  , "on click at p:"
  , "  reset T to (viewer_width, viewer_height) filled with (10 + 20i, 30)"
  , "  for each j in range(1, viewer_width):"
  , "    T @ (j, 1) <- (j + 2i, j)"
  , ""
  , "@tool \"Copy row\" for=\"Par\""
  , "on click at p:"
  , "  T @ (*, 2) <- T @ (*, 1)"
  , ""
  , "@tool \"Grow\" for=\"Par\""
  , "on click at p:"
  , "  (w, h) := table_size(T)"
  , "  resize T to (w, h + 10) filled with (5 + 6i, 7)"
  , ""
  , "@tool \"Mixed\" for=\"Par\""
  , "on click at p:"
  , "  reset Q to 3 from 0 to 1 filled with (false, (white, 1))"
  , "  Q @ 2 <- (true, (rgb(0.2, 0.4, 0.6), 9))"
  , "  mixed <- true"
  ]

fillTuples :: [String]
fillTuples = [ "select viewer \"Par\"", "resize viewer to 200x100", "select tool \"Fill\"", "click at 0" ]

tupleSpec :: Spec
tupleSpec = describe "tables of tuples" $ do

  it "are written and read whole" $ scenarioIn tupled $ fillTuples ++
    [ at "Par" (10, 10) "(10, 20, 30)", at "Par" (4, 0) "(5, 2, 5)" ]

  it "are copied a slice at a time" $ scenarioIn tupled $ fillTuples ++
    [ "select tool \"Copy row\"", "click at 0"
    , at "Par" (4, 1) "(5, 2, 5)", at "Par" (4, 2) "(10, 20, 30)" ]

  it "are resized, filling new cells with a tuple" $ scenarioIn tupled $ fillTuples ++
    [ "set viewer center to 0 - 0.1i", "select tool \"Grow\"", "click at 0"
    -- (the view is 10 pixels lower: rows 101 to 110 are new)
    , at "Par" (10, 89) "(10, 20, 30)", at "Par" (10, 90) "(5, 6, 7)", at "Par" (10, 99) "(5, 6, 7)" ]

  it "hold nested tuples of mixed sizes" $ scenarioIn tupled $ fillTuples ++
    [ "select tool \"Mixed\"", "click at 0"
    , at "Par" (10, 10) "(51, 102, 153)" ]

-- | The project, saved (as the app saves it) after the given commands,
-- with its tables, and loaded again.
savedAfter :: String -> [String] -> IO Ensemble
savedAfter proj cmds = do
  ens <- buildEnsembleFromScript proj >>= either (fail . snd) pure
  scenarioOn ens cmds
  saved <- captureTables =<< ensembleTables ens
  modifyValue (ensembleFrontendProperties ens) (writeSavedTables saved)
  bytes <- encodeEnsemble ens
  parseEnsembleFromBytes "saved.fs" bytes >>= either fail pure

-- | Two tables, both filled by one tool.
twoTables :: String
twoTables = unlines
  [ "@config \"P\""
  , "T : Table 2 of C"
  , "U : Table 2 of R"
  , "C : C := 0"
  , ""
  , "@viewer \"Par\" var=C pixelSize=1/100"
  , "(w, h) := table_size(T)"
  , "(w2, h2) := table_size(U)"
  , "a := if w = 0 then 0 else re(T @ point C)"
  , "b := if w2 = 0 then 0 else U @ point C"
  , "color <- rgb(a / 255, b / 255, 0)"
  , ""
  , "@tool \"Fill\" for=\"Par\""
  , "on click at p:"
  , "  reset T to (viewer_width, viewer_height) filled with 10"
  , "  reset U to (viewer_width, viewer_height) filled with 20"
  ]

savedSpec :: Spec
savedSpec = describe "saved tables" $ do
  let filled = savedAfter twoTables fill
      px = at "Par" (50, 50)

  it "aren't loaded unless a scenario asks" $ do
    ens <- filled
    scenarioOn ens [ "select viewer \"Par\"", "resize viewer to 200x100", px "(0, 0, 0)" ]

  it "are loaded by name" $ do
    ens <- filled
    scenarioOn ens [ "select viewer \"Par\"", "resize viewer to 200x100"
                   , "load saved table U", px "(0, 20, 0)"
                   , "load saved tables T, U", px "(10, 20, 0)" ]

  it "are all loaded at once" $ do
    ens <- filled
    scenarioOn ens [ "select viewer \"Par\"", "resize viewer to 200x100"
                   , "load all saved tables", px "(10, 20, 0)" ]

  it "can't be loaded if the file didn't save them" $ do
    ens <- filled
    runOn ens [ "load saved tables T, V, W" ] >>= \case
      Left err -> err `shouldSatisfy` ("no saved tables `V`, `W`" `isInfixOf`)
      Right () -> expectationFailure "loaded a table that wasn't saved"

  it "load nothing, without complaint, from a file that saved none" $ do
    ens <- buildEnsembleFromScript twoTables >>= either (fail . snd) pure
    scenarioOn ens [ "load all saved tables", "select viewer \"Par\"", px "(0, 0, 0)" ]

  it "can't be loaded into a table of another type" $ do
    ens <- savedAfter twoTables fill
    bytes <- encodeEnsemble ens
    let retyped = replaceAll "U : Table 2 of R" "U : Table 2 of Z" bytes
    ens' <- parseEnsembleFromBytes "retyped.fs" retyped >>= either fail pure
    runOn ens' [ "load saved table U" ] >>= \case
      Left err -> err `shouldSatisfy` ("it was saved as a" `isInfixOf`)
      Right () -> expectationFailure "loaded a table saved with another type"

replaceAll :: String -> String -> BS.ByteString -> BS.ByteString
replaceAll from to bytes = case BS.breakSubstring (BS.pack from) bytes of
  (front, rest)
    | BS.null rest -> front
    | otherwise -> front <> BS.pack to <> replaceAll from to (BS.drop (length from) rest)
