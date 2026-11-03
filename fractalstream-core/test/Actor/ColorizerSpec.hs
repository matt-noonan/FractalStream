{-# language QuasiQuotes, OverloadedStrings #-}
module Actor.ColorizerSpec (spec) where

import Test.Hspec

import Actor.Colorizer
import Actor.Ensemble
import Actor.Layout (Field(..))
import Actor.SavedValues (noSavedValues)
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))
import Language.Code (pprintCode)
import Data.DynamicValue

import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.List (isInfixOf, sort)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson.KeyMap as KM
import Text.RawString.QQ

buildWith :: Map.Map String String -> String -> IO Ensemble
buildWith choices src = buildEnsembleWithColorizers choices src >>= \case
  Left (_, err) -> fail ("expected the script to build, got: " ++ err)
  Right ens -> pure ens

colorizers :: Ensemble -> Map.Map String (String, String)
colorizers ens = Map.map (\vc -> (colorizerInUse vc, colorizerDefault vc)) (ensembleViewerColorizers ens)

-- | Each viewer's code, pretty-printed, by title.
viewerCodes :: Ensemble -> IO [String]
viewerCodes ens = getDynamic (ensembleViewers ens) >>= mapM (\v ->
  getDynamic (cvCode v) >>= \case
    Left (_, err) -> fail ("expected the viewer's code to build, got: " ++ err)
    Right (SomeViewerWithContext _ _ code) -> pure (pprintCode code))

fieldNames :: Ensemble -> IO [String]
fieldNames ens = map (fieldName . snd) <$> ensembleFields ens

escapeOnly, setsItsColor :: String
escapeOnly = [r|
@viewer "V"
iterate z -> z^2 - 1 until z escapes
|]
setsItsColor = [r|
@viewer "V"
iterate z -> z^2 - 1 until z escapes
color <- if stuck then black else white
|]

spec :: Spec
spec = do

  describe "built-in colorizers" $ do
    it "are all well-formed, with distinct names" $ do
      forM_ builtinColorizers $ \c -> length (colorizerConfig c) `seq` pure ()
      let names = map colorizerName builtinColorizers
      sort names `shouldBe` sort (Map.keys (Map.fromList (map (,()) names)))
      map colorizerName builtinColorizers `shouldContain` [manualColorizerName, rainbowColorizerName]

    it "each build, on a Julia set's viewer and on a Mandelbrot set's" $
      forM_ builtinColorizers $ \c -> do
        ens <- buildWith (Map.fromList [("J", colorizerName c), ("M", colorizerName c)]) [r|
@config "P"
C : C as "C" <- -1

@viewer "J"
iterate z -> z^2 + C until z escapes

@viewer "M" var=C
z := 0
iterate z -> z^2 + C until z escapes
|]
        Map.map fst (colorizers ens) `shouldBe` Map.fromList [("J", colorizerName c), ("M", colorizerName c)]

  describe "a viewer's default colorizer" $ do
    it "is Manual for a viewer that sets its color" $ do
      ens <- buildWith Map.empty setsItsColor
      colorizers ens `shouldBe` Map.singleton "V" ("Manual", "Manual")
      [code] <- viewerCodes ens
      code `shouldNotSatisfy` isInfixOf "[colorizer]"
      fieldNames ens >>= (`shouldNotSatisfy` any ("[colorizer]" `isInfixOf`))

    it "is Rainbow for a viewer that doesn't" $ do
      ens <- buildWith Map.empty escapeOnly
      colorizers ens `shouldBe` Map.singleton "V" ("Rainbow", "Rainbow")
      [code] <- viewerCodes ens
      code `shouldSatisfy` isInfixOf "[colorizer] V: color_speed"
      fieldNames ens >>= (`shouldContain` ["[colorizer] V: color_speed"])

    it "counts a color set by a code block the viewer runs" $ do
      ens <- buildWith Map.empty [r|
@common
define shade:
  color <- white

@viewer "V"
iterate z -> z^2 - 1 until z escapes
run shade
|]
      colorizers ens `shouldBe` Map.singleton "V" ("Manual", "Manual")

    it "is the one named by the viewer's `colorizer=` attribute, if it has one" $ do
      ens <- buildWith Map.empty [r|
@viewer "V" colorizer=Rainbow
iterate z -> z^2 - 1 until z escapes
color <- white
|]
      colorizers ens `shouldBe` Map.singleton "V" ("Rainbow", "Rainbow")

    it "rejects a `colorizer=` attribute naming no colorizer" $
      buildEnsembleFromScript "@viewer \"V\" colorizer=Plaid\npass\n" >>= \case
        Left (_, err) -> err `shouldSatisfy` isInfixOf "no colorizer called \"Plaid\""
        Right _ -> expectationFailure "expected an unknown colorizer to be an error"

    it "rejects arguments for a colorizer without parameters" $
      checkScript "@viewer \"V\" colorizer=\"Glow(z)\"\npass\n" `shouldSatisfy` \case
        Left (_, err) -> "doesn't take any arguments" `isInfixOf` err
        Right () -> False

  describe "an interactive choice" $ do
    it "overrides the default" $ do
      ens <- buildWith (Map.singleton "V" "Manual") escapeOnly
      colorizers ens `shouldBe` Map.singleton "V" ("Manual", "Rainbow")
      getDynamic (ensembleColorizerChoices ens) >>= (`shouldBe` Map.singleton "V" "Manual")
      [code] <- viewerCodes ens
      code `shouldNotSatisfy` isInfixOf "[colorizer]"

    it "falls back to the default when it can't be used, but is kept" $ do
      ens <- buildWith (Map.singleton "V" "Plaid") escapeOnly
      colorizers ens `shouldBe` Map.singleton "V" ("Rainbow", "Rainbow")
      getDynamic (ensembleColorizerChoices ens) >>= (`shouldBe` Map.singleton "V" "Plaid")

    it "is dropped when the script has no such viewer" $ do
      ens <- buildWith (Map.singleton "Gone" "Manual") escapeOnly
      getDynamic (ensembleColorizerChoices ens) >>= (`shouldBe` Map.empty)

  describe "colorizer names" $ do
    it "never clash with a script's own" $ do
      ens <- buildWith Map.empty [r|
@config "Mine"
color_speed : Z as "Mine" <- 3

@viewer "V"
iterate z -> z^2 - 1 until z escapes
|]
      fieldNames ens >>= (`shouldContain` ["color_speed", "[colorizer] V: color_speed"])

    it "are separate for each viewer, each with a tab of its own" $ do
      ens <- buildWith Map.empty [r|
@viewer "A"
iterate z -> z^2 - 1 until z escapes

@viewer "B"
iterate z -> z^2 + 0.25 until z escapes
|]
      names <- fieldNames ens
      names `shouldContain` ["[colorizer] A: color_speed"]
      names `shouldContain` ["[colorizer] B: color_speed"]

  describe "the project file" $ do
    let file extra = BS8.pack (unlines (["script: |"] ++ map ("  " ++) (lines escapeOnly) ++ extra))

    it "reads and writes the interactive choices under `colorizers:`" $
      loadProjectBytes "c.fs" (file ["colorizers:", "  V: Manual"]) >>= \case
        Right (Built ens) -> do
          colorizers ens `shouldBe` Map.singleton "V" ("Manual", "Rainbow")
          KM.keys <$> getDynamic (ensembleFrontendProperties ens) >>= (`shouldBe` [])
          bytes <- encodeEnsemble ens
          BS8.unpack bytes `shouldSatisfy` isInfixOf "colorizers:\n  V: Manual\n"
        Right (BrokenScript _ err) -> expectationFailure err
        Left err -> expectationFailure err

    it "has no `colorizers:` key without interactive choices" $
      loadProjectBytes "c.fs" (file []) >>= \case
        Right (Built ens) -> do
          bytes <- encodeEnsemble ens
          BS8.unpack bytes `shouldNotSatisfy` isInfixOf "colorizers"
        _ -> expectationFailure "expected the project to build"

    it "keeps the choices of a project whose script doesn't build" $
      loadProjectBytes "c.fs" (BS8.pack "script: |\n  letz x = 1\ncolorizers:\n  V: Manual\n") >>= \case
        Right (BrokenScript ens _) -> do
          getDynamic (ensembleColorizerChoices ens) >>= (`shouldBe` Map.singleton "V" "Manual")
          bytes <- encodeEnsemble ens
          BS8.unpack bytes `shouldSatisfy` isInfixOf "colorizers:"
        _ -> expectationFailure "expected the script not to build"

  describe "@colorizer stanzas" $ do
    let smooth = [r|
@colorizer "Smooth" description="Smoothed escape time."
period : R as "Color period" <- 20
inside : Color as "Interior color" <- black

define colorize(v):
  if v escaped:
    m := iterations + 1 - log (log (|v|^2) / (2 log escape_radius)) / log 2
    color <- blend (cos (pi m / period)^2, dark blue, light yellow)
  else:
    color <- inside
|]

    it "define a colorizer a viewer can use, with its arguments" $ do
      ens <- buildWith Map.empty (smooth ++ [r|
@config "Parameters"
C : C as "C =" <- -0.12 + 0.75i

@viewer "Parameter plane" var=C colorizer="Smooth(z)"
z := 0
iterate z -> z^2 + C until z escapes

@viewer "Julia set" colorizer="Smooth(z)"
m := 1
iterate z -> z^2 + C until z escapes
|])
      colorizers ens `shouldBe` Map.fromList
        [ ("Parameter plane", ("Smooth(z)", "Smooth(z)"))
        , ("Julia set", ("Smooth(z)", "Smooth(z)")) ]
      names <- fieldNames ens
      names `shouldContain` ["[colorizer] Parameter plane: period", "[colorizer] Parameter plane: inside"]
      names `shouldContain` ["[colorizer] Julia set: period", "[colorizer] Julia set: inside"]
      -- the script's own first, then the built-in ones it doesn't replace
      map colorizerName (ensembleAvailableColorizers ens) `shouldBe`
        "Smooth" : filter (/= "Smooth") (map colorizerName builtinColorizers)
      codes <- viewerCodes ens
      forM_ codes (`shouldSatisfy` isInfixOf "[colorizer]")

    it "can be chosen interactively" $ do
      ens <- buildWith (Map.singleton "V" "Smooth(z)") (smooth ++ escapeOnly)
      colorizers ens `shouldBe` Map.singleton "V" ("Smooth(z)", "Rainbow")

    it "read their arguments knowing the script's functions" $ do
      _ <- buildWith Map.empty (smooth ++ [r|
@common
let f(w) = w^2 - 1

@viewer "V" colorizer="Smooth(f(z))"
iterate z -> f(z) until z escapes
|])
      pure ()

    it "can use @common definitions, whether or not the viewer does" $ do
      let common = [r|
@common
let g(w) = w^2
k := 3

@colorizer "Uses common"
define colorize:
  color <- if g(k) > 4 then white else black
|]
      _ <- buildWith Map.empty (common ++ "@viewer \"V\" colorizer=\"Uses common\"\npass\n")
      _ <- buildWith Map.empty (common ++ "@viewer \"V\" colorizer=\"Uses common\"\nw := g(k)\n")
      pure ()

    it "replace a built-in colorizer of the same name" $ do
      ens <- buildWith Map.empty ([r|
@colorizer "Rainbow"
define colorize:
  color <- white
|] ++ escapeOnly)
      colorizers ens `shouldBe` Map.singleton "V" ("Rainbow", "Rainbow")
      map colorizerName (ensembleAvailableColorizers ens) `shouldBe`
        "Rainbow" : filter (/= "Rainbow") (map colorizerName builtinColorizers)
      fieldNames ens >>= (`shouldNotSatisfy` any ("[colorizer]" `isInfixOf`))

    it "can't share a name" $
      checkScript "@colorizer \"A\"\ndefine colorize:\n  pass\n@colorizer \"A\"\ndefine colorize:\n  pass\n"
        `shouldSatisfy` \case
          Left (_, err) -> "two @colorizer stanzas called \"A\"" `isInfixOf` err
          Right () -> False

    it "need the right number of arguments" $
      checkScript (smooth ++ "@viewer \"V\" colorizer=Smooth\npass\n") `shouldSatisfy` \case
        Left (_, err) -> "needs a value for `v`" `isInfixOf` err
        Right () -> False

    it "report errors in their code where they are in the script" $ do
      let src = "@colorizer \"Broken\"\ndefine colorize:\n  color <- nope\n@viewer \"V\" colorizer=Broken\npass\n"
      case checkScript src of
        Left err -> formatScriptError src err `shouldSatisfy` isInfixOf "color <- nope"
        Right () -> expectationFailure "expected an error"

    it "without arguments, use the viewer's variables named after their parameters" $ do
      let byName = [r|
@colorizer "Escaped"
define colorize(z):
  color <- if z escaped then white else black
|]
      ens <- buildWith Map.empty (byName ++ [r|
@viewer "Julia" colorizer=Escaped
iterate z -> z^2 - 1 until z escapes

@viewer "Mandelbrot" var=C colorizer=Escaped
z := 0
iterate z -> z^2 + C until z escapes
|])
      Map.map fst (colorizers ens) `shouldBe` Map.fromList [("Julia", "Escaped"), ("Mandelbrot", "Escaped")]
      case checkScript (byName ++ "@viewer \"V\" var=w colorizer=Escaped\npass\n") of
        Left (_, err) -> err `shouldSatisfy` isInfixOf "the viewer has no `z` to use"
        Right () -> expectationFailure "expected a missing `z` to be an error"
      -- (and with an argument, any viewer can use it)
      _ <- buildWith Map.empty (byName ++ "@viewer \"V\" var=w colorizer=\"Escaped(w)\"\npass\n")
      pure ()

    it "with several parameters, take them all from the viewer, or all as arguments" $ do
      let two = [r|
@colorizer "Two"
define colorize(z, k):
  color <- if |z| > k then white else black

@config "C"
k : R as "k" <- 2
|]
      -- `z` is the viewer's coordinate, and `k` is in the configuration
      ens <- buildWith Map.empty (two ++ "@viewer \"V\" colorizer=Two\npass\n")
      Map.map fst (colorizers ens) `shouldBe` Map.singleton "V" "Two"
      _ <- buildWith Map.empty (two ++ "@viewer \"V\" var=w colorizer=\"Two(w, k + 1)\"\npass\n")
      let errorFor attr = case checkScript (two ++ "@viewer \"V\" var=w colorizer=" ++ attr ++ "\npass\n") of
            Left (_, err) -> err
            Right () -> ""
      errorFor "Two" `shouldSatisfy` isInfixOf "the viewer has no `z` to use"
      errorFor "\"Two(w, 1, 2)\"" `shouldSatisfy` isInfixOf "takes 2 arguments"
      -- (an argument for `z` alone is fine: `k` is in scope)
      errorFor "\"Two(w)\"" `shouldBe` ""

    it "use their parameters' default values when they aren't given arguments" $ do
      let defaulted = [r|
@colorizer "Shift"
offset : R as "Offset" <- 0.5
define colorize(t = iterations / 20, u = t + offset):
  color <- rainbow(u)
|]
          viewer = "@viewer \"V\" colorizer="
      -- no variable `t` or `u` needed, and a viewer's own `t` isn't used
      _ <- buildWith Map.empty (defaulted ++ viewer ++ "Shift\npass\n")
      ens <- buildWith Map.empty (defaulted ++ viewer ++ "Shift\nt := 1 + i\n")
      [code] <- viewerCodes ens
      code `shouldSatisfy` isInfixOf "[colorizer] V: offset"
      -- trailing defaults can be left out, and earlier arguments given
      _ <- buildWith Map.empty (defaulted ++ viewer ++ "\"Shift(re z)\"\npass\n")
      _ <- buildWith Map.empty (defaulted ++ viewer ++ "\"Shift(re z, 0.25)\"\npass\n")
      pure ()

    it "include Rainbow, which can color by any real value" $ do
      ens <- buildWith Map.empty [r|
@viewer "Newton" colorizer="Rainbow(arg z / (2 pi))"
lastZ := 0
repeat:
  lastZ <- z
  z <- z - (z^3 - 1) / (3 z^2)
until z = lastZ
|]
      Map.map fst (colorizers ens) `shouldBe` Map.singleton "Newton" "Rainbow(arg z / (2 pi))"

  describe "a viewer's coloring: block" $ do
    let viewer coloring = unlines $
          [ "@viewer \"V\" colorizer=Smooth"
          , "coloring:" ] ++ map ("  " ++) coloring ++
          [ "iterate z -> z^2 - 1 until z escapes" ]
        fieldText' ens name = do
          fs <- ensembleFields ens
          case [ f | (_, f) <- fs, fieldName f == "[colorizer] V: " ++ name ] of
            [f] -> fieldText f
            _ -> fail ("no field " ++ name)

    it "replaces its colorizer's defaults, choices by their labels" $ do
      ens <- buildWith Map.empty (viewer ["palette <- \"Two colors\"", "color1 <- red", "period <- 100"])
      fieldText' ens "period" >>= (`shouldBe` "100")
      fieldText' ens "palette" >>= (`shouldBe` "Two colors")
      fieldText' ens "color1" >>= (`shouldBe` "rgb(1.000, 0.000, 0.000)")
      -- they're the defaults, not changes
      changedValues ens >>= (`shouldBe` noSavedValues)

    it "reports settings and choices the colorizer doesn't have, where they are" $ do
      let errorFor src = case checkScript src of
            Left err -> formatScriptError src err
            Right () -> ""
      errorFor (viewer ["speed <- 3"]) `shouldSatisfy` \e ->
        "speed <- 3" `isInfixOf` e && "no setting called `speed`" `isInfixOf` e
      errorFor (viewer ["palette <- \"Plaid\""]) `shouldSatisfy` isInfixOf "no choice called \"Plaid\""
      errorFor (viewer ["period <- red"]) `shouldSatisfy` isInfixOf "period <- red"

    it "is ignored when another colorizer is chosen" $ do
      ens <- buildWith (Map.singleton "V" "Rainbow") (viewer ["palette <- \"Two colors\""])
      Map.map fst (colorizers ens) `shouldBe` Map.singleton "V" "Rainbow"
