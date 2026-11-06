module Language.LegacySpec (spec) where

-- | Translating original-FractalStream scripts ("Language.Legacy.Translate").
--
-- Every script in @test/legacy/@ (@*.txt@: the examples from the old
-- program's guides) is translated and compared with the translation
-- next to it (@*.fs@), and the translation must load. A missing @.fs@ is
-- written out (and the test passes); delete one to regenerate it, and
-- look over the result. @torus.txt@ is a real-dynamics script.

import Test.Hspec

import Actor.Ensemble (checkProjectBytes)
import Language.Legacy.Translate

import Control.Monad (forM_)
import Data.List (isInfixOf, isSuffixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), dropExtension)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8, hGetContents', hPutStr)

legacyDir :: FilePath
legacyDir = "test/legacy"

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h

-- | The translation of a script, failing the test if there's none.
translated :: String -> IO Translation
translated src = case translateLegacyScript "Test" src of
  Left err -> expectationFailure ("didn't translate:\n" ++ err) >> fail "untranslated"
  Right t -> pure t

-- | The lines of a script's translation, between @script: |@ and the metadata.
scriptOf :: Translation -> [String]
scriptOf = takeWhile (/= "metadata:") . drop 1 . dropWhile (/= "script: |") . lines . translatedProject

loads :: Translation -> Expectation
loads t = checkProjectBytes (T.encodeUtf8 (T.pack (translatedProject t))) `shouldBe` Right ()

spec :: Spec
spec = describe "translating original-FractalStream scripts" $ do

  scripts <- runIO $ sort . filter (".txt" `isSuffixOf`) <$> listDirectory legacyDir
  describe "the examples from the old guides" $
    forM_ scripts $ \file -> it file $ do
      src <- readUtf8 (legacyDir </> file)
      case (file, translateLegacyScript (dropExtension file) src) of
        (_, Left err) -> expectationFailure err
        (_, Right t) -> do
          loads t
          let expectedFile = legacyDir </> (dropExtension file ++ ".fs")
          exists <- doesFileExist expectedFile
          if exists
            then readUtf8 expectedFile >>= (translatedProject t `shouldBe`)
            else withFile expectedFile WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h (translatedProject t)

  it "ignores case, and keeps the first spelling of each name" $ do
    t <- translated "SET Count TO 0.\nITERATE Z^2 + 1 UNTIL z ESCAPES.\nset count to COUNT + 1."
    scriptOf t `shouldSatisfy` any ("Count := 0" `isInfixOf`)
    scriptOf t `shouldSatisfy` any ("Count <- Count + 1" `isInfixOf`)
    scriptOf t `shouldSatisfy` any ("iterate z -> z^2 + 1 until z escapes" `isInfixOf`)
    loads t

  it "keeps comments, and chains else-ifs" $ do
    t <- translated "(first) if re(z) > 1 then [A]. else if re(z) < -1 then [B]. else [C]."
    let s = scriptOf t
    s `shouldSatisfy` any ("# first" `isInfixOf`)
    s `shouldSatisfy` any ("else if re(z) < -1:" `isInfixOf`)
    s `shouldSatisfy` any ("  else:" `isInfixOf`)
    loads t

  it "renames variables whose names mean something else now" $ do
    t <- translated "set text to z. set color to text * 2. iterate color^2 on color until color escapes."
    scriptOf t `shouldSatisfy` any ("text_ := z" `isInfixOf`)
    scriptOf t `shouldSatisfy` any ("color_ := text_ * 2" `isInfixOf`)
    translationWarnings t `shouldSatisfy` any ("`text`" `isInfixOf`)
    loads t

  it "writes numbers the way FractalStream does" $ do
    t <- translated "set w to .5 + 2.5e-3 + 1.5i. iterate z^2 + w until z escapes."
    scriptOf t `shouldSatisfy` any ("w := 0.5 + 2.5 * 10^(-3) + 1.5i" `isInfixOf`)
    loads t

  it "compares complex numbers by modulus, and real ones as they are" $ do
    t <- translated "set r to re(z). if r < -1 then [Left]. if z < 2 then [Near]. if z > r then [Far]."
    let s = scriptOf t
    s `shouldSatisfy` any ("if r < -1:" `isInfixOf`)
    s `shouldSatisfy` any ("if |z| < 2:" `isInfixOf`)
    s `shouldSatisfy` any ("if |z| > |r|:" `isInfixOf`)
    loads t

  it "sets several variables at once" $ do
    t <- translated "set (a, b) to (b, a + b). iterate z^2 + a until z escapes."
    let s = scriptOf t
    s `shouldSatisfy` any ("a, b <- b, a + b" `isInfixOf`)
    loads t

  it "turns xor into a comparison of truth values" $ do
    t <- translated "if re(z) > 0 xor im(z) > 0 then [Checkered]."
    scriptOf t `shouldSatisfy` any ("if (re(z) > 0) != (im(z) > 0):" `isInfixOf`)
    loads t

  it "leaves special tools and data sources as TODOs" $ do
    t <- translated "load SaddleDrop.\niterate z^2 + c until z escapes."
    scriptOf t `shouldSatisfy` any ("# TODO: `load`" `isInfixOf`)
    translationWarnings t `shouldSatisfy` any ("SaddleDrop" `isInfixOf`)
    loads t

  it "warns about variables used without being set" $ do
    t <- translated "iterate z^2 + q until z escapes."
    translationWarnings t `shouldSatisfy` any ("`q`" `isInfixOf`)
    scriptOf t `shouldSatisfy` any ("q := 0" `isInfixOf`)
    loads t

  it "reports scripts it can't parse" $
    translateLegacyScript "Test" "iterate z^2 + until." `shouldSatisfy` either (const True) (const False)

  it "translates a real-dynamics script into real viewers, with a parameter plane for (a, b)" $ do
    t <- translated $ unlines
      [ "{real}"
      , "iterate (x^2 - y^2 + a, 2*x*y + b) until (x, y) escapes."
      , "probe real \"Distance\":"
      , "  report x^2 + y^2."
      , "end." ]
    let s = scriptOf t
    s `shouldSatisfy` any ("@viewer \"Dynamical plane\" xvar=x yvar=y" `isInfixOf`)
    s `shouldSatisfy` any ("@viewer \"Parameter plane\" xvar=a yvar=b" `isInfixOf`)
    s `shouldSatisfy` any ("a : R as \"a =\" := 0" `isInfixOf`)
    s `shouldSatisfy` any ("x, y := 0, 0" `isInfixOf`)
    s `shouldSatisfy` any ("iterate (x, y) -> (x^2 - y^2 + a, 2 * x * y + b) until (x, y) escapes" `isInfixOf`)
    -- the probe: the clicked point itself, in the dynamical plane, and
    -- the clicked parameters, starting from (0, 0), in the parameter plane
    s `shouldSatisfy` any ("on click at (x, y):" `isInfixOf`)
    s `shouldSatisfy` any ("on click at (clicked_a, clicked_b):" `isInfixOf`)
    loads t

  it "compares a real-dynamics script's values as real numbers" $ do
    t <- translated "{real}\nif x > y then [Right]. else [Left]."
    scriptOf t `shouldSatisfy` any ("if x > y:" `isInfixOf`)
    loads t

  it "accepts a flag without a period after it" $ do
    t <- translated "iterate z^2 + c until z escapes.\nif z escapes then [Out]\n[In]"
    scriptOf t `shouldSatisfy` any ("flag_color <- out_color" `isInfixOf`)
    scriptOf t `shouldSatisfy` any ("flag_color <- in_color" `isInfixOf`)
    loads t

  it "reads `then` without a colon as `then:`, with a warning, when that's the only way it parses" $ do
    t <- translated $ unlines
      [ "iterate z^2 + c until z escapes or z vanishes."
      , "if z vanishes then"
      , "  [Small]"
      , "  if re(z) > 0 then [Right] end."
      , "end." ]
    let s = scriptOf t
    s `shouldSatisfy` any ("if z vanishes:" `isInfixOf`)
    s `shouldSatisfy` any ("    if re(z) > 0:" `isInfixOf`)
    translationWarnings t `shouldSatisfy` any ("`then:`" `isInfixOf`)
    loads t

  it "reads `then` as usual when the script parses that way" $ do
    t <- translated "if re(z) > 0 then [A]. else [B]."
    translationWarnings t `shouldNotSatisfy` any ("`then:`" `isInfixOf`)
    loads t

  it "starts the vanishing radius at 0.01, as the old program's tolerance did" $ do
    t <- translated "iterate z^2 + c until z vanishes."
    let s = scriptOf t
    s `shouldSatisfy` any ("@config \"Iteration settings\"" `isInfixOf`)
    s `shouldSatisfy` any ("vanishing_radius : R as \"Vanishing radius: \" := 0.01" `isInfixOf`)
    loads t

  it "treats a real-dynamics script's pairs in arithmetic as complex numbers" $ do
    t <- translated $ unlines
      [ "{real}"
      , "set (x, y) to (x, y)*(0, 1) + (a, b)."
      , "set (x, y) to (x, y)/2."
      , "if (x, y) < (a, b) then [Inside]."
      , "iterate (x, y)^2 + (a, b) until (x, y) escapes." ]
    let s = scriptOf t
    s `shouldSatisfy` any ("new_point := (x + y * i) * i + (a + b * i)" `isInfixOf`)
    s `shouldSatisfy` any ("new_point_2 := (x + y * i) / 2" `isInfixOf`)
    s `shouldSatisfy` any ("if |x + y * i| < |a + b * i|:" `isInfixOf`)
    s `shouldSatisfy` any ("new_point_3 := (x + y * i)^2 + (a + b * i)" `isInfixOf`)
    s `shouldSatisfy` any ("until (x, y) escapes" `isInfixOf`)
    s `shouldNotSatisfy` any ("re((x, y)" `isInfixOf`)
    loads t

  it "keeps an `until` with no `do` as a comment, with a warning" $ do
    t <- translated "set w to z.\nset z to z^2 + c.\nuntil z escapes.\nif z escapes then [Out]."
    let s = scriptOf t
    s `shouldSatisfy` any ("#   until z escapes." `isInfixOf`)
    s `shouldSatisfy` any ("if z escapes:" `isInfixOf`)
    translationWarnings t `shouldSatisfy` any ("no `do`" `isInfixOf`)
    loads t

  it "keeps an `end.` that ends no block as a comment, with a warning" $ do
    -- (not after an `if ... then`, where it would end a block, as `then:`)
    t <- translated "iterate z^2 + c until z escapes.\n[Out].\nend."
    scriptOf t `shouldSatisfy` any ("#   end." `isInfixOf`)
    translationWarnings t `shouldSatisfy` any ("doesn't end any block" `isInfixOf`)
    loads t

  it "doesn't take a loop's own `until`, or a block's own `end.`, for a stray" $ do
    t <- translated "block\ndo\nset z to z^2 + c.\nuntil z escapes.\nend.\nend."
    let s = scriptOf t
    s `shouldSatisfy` any ("until z escapes" `isInfixOf`)
    s `shouldSatisfy` any ("repeat:" `isInfixOf`)
    s `shouldNotSatisfy` any ("#   until" `isInfixOf`)
    s `shouldSatisfy` any ("#   end." `isInfixOf`)
    loads t

  it "reads old Mac OS line endings" $ do
    t <- translated "set w to z.\riterate w^2 + c on w until w escapes.\r\nif w escapes then [Out]."
    let s = scriptOf t
    s `shouldSatisfy` any ("iterate w -> w^2 + C until w escapes" `isInfixOf`)
    lines (translatedProject t) `shouldSatisfy` any (== "#   iterate w^2 + c on w until w escapes.")
    translatedProject t `shouldNotSatisfy` elem '\r'
    loads t

  it "gives the dynamical plane a Trace tool, running the script one pass at a time" $ do
    t <- translated "set w to z.\ndo\nset z to z^2 + c.\nuntil z escapes or z = w.\nif z = w then [Back]."
    let s = scriptOf t
    s `shouldSatisfy` any ("@tool \"Trace\" for=\"Dynamical plane\" shortcut=t" `isInfixOf`)
    s `shouldSatisfy` any ("trace_color : Color as \"Trace color\" := white" `isInfixOf`)
    s `shouldSatisfy` any ("on click-or-drag at z:" `isInfixOf`)
    s `shouldSatisfy` any ("repeat up to 100 times:" `isInfixOf`)
    s `shouldSatisfy` any ("repeat up to 1 times:" `isInfixOf`)
    s `shouldSatisfy` any ("draw line from previous to z" `isInfixOf`)
    s `shouldSatisfy` any ("until z escapes" `isInfixOf`)
    -- (the flag's `if` has nothing to do in the tool, so it's left out)
    length (filter ("if z = w:" `isInfixOf`) s) `shouldBe` 2
    loads t

  it "caps an `iterate` in the Trace tool at one pass" $ do
    t <- translated "iterate z^2 + c until z escapes."
    scriptOf t `shouldSatisfy` any ("iterate z -> z^2 + C until z escapes up to 1 times" `isInfixOf`)
    loads t

  it "traces (x, y) in a real-dynamics script" $ do
    t <- translated "{real}\niterate (y, -x + a) until (x, y) escapes."
    let s = scriptOf t
    s `shouldSatisfy` any ("on click-or-drag at (x, y):" `isInfixOf`)
    s `shouldSatisfy` any ("previous_x, previous_y := x, y" `isInfixOf`)
    s `shouldSatisfy` any ("draw line from (previous_x, previous_y) to (x, y)" `isInfixOf`)
    s `shouldSatisfy` any ("until (x, y) escapes" `isInfixOf`)
    loads t

  it "names the Trace tool differently from a probe called Trace" $ do
    t <- translated "probe real \"Trace\":\n  report re(z).\nend.\niterate z^2 + c until z escapes."
    let s = scriptOf t
    s `shouldSatisfy` any ("@tool \"Trace\" for=\"Dynamical plane\"" `isInfixOf`)
    s `shouldSatisfy` any ("@tool \"Trace_2\" for=\"Dynamical plane\" shortcut=t" `isInfixOf`)
    loads t
