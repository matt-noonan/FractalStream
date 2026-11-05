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
