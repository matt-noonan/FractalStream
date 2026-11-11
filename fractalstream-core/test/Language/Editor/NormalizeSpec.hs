module Language.Editor.NormalizeSpec (spec) where

import Test.Hspec

import Control.Monad (forM, forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Language.Editor.Normalize
import Language.Parser.Tokenizer (SRToken(..), Token(..), tokenizeWithIndentation)

-- | Every project script that ships, by file.
projectScripts :: IO [(FilePath, String)]
projectScripts = do
  let library = "../fractalstream-library/projects"
  sections <- listDirectory library
  libraryFiles <- concat <$> forM (sort sections) (\d -> do
    isDir <- doesDirectoryExist (library </> d)
    if isDir then map ((library </> d) </>) . filter (".fs" `isSuffixOf`) <$> listDirectory (library </> d)
             else pure [])
  examples <- map ("../examples" </>) . filter (".fs" `isSuffixOf`) <$> listDirectory "../examples"
  concat <$> forM (libraryFiles ++ examples) (\f -> do
    bytes <- BS.readFile f
    pure $ case Y.decodeEither' bytes of
      Right (A.Object o) | Just (A.String s) <- KM.lookup (Key.fromString "script") o -> [(f, T.unpack s)]
      _ -> [])

spec :: Spec
spec = describe "normalizing scripts" $ do

  it "uses the mathematical symbols for operators" $ do
    normalize "x := 1\nx <- x + 1" `shouldBe` "x ≔ 1\nx ⟵ x + 1"
    normalize "iterate z -> z^2 + C until z escapes" `shouldBe` "iterate z ⟼ z² + C until z escapes"
    normalize "if a <= b and a >= c or a != d or a =/= e or a === f or a == g:"
      `shouldBe` "if a ≤ b and a ≥ c or a ≠ d or a ≠ e or a ≡ f or a = g:"
    normalize "y <- f'(z)" `shouldBe` "y ⟵ f′(z)"
    normalize "y <- 2 * x * z" `shouldBe` "y ⟵ 2 × x × z"

  it "writes integer powers as superscripts, and leaves others alone" $ do
    normalize "w <- z^10 + z^(3) + z^(-1)" `shouldBe` "w ⟵ z¹⁰ + z³ + z⁻¹"
    normalize "w <- 2^k + z^2.5 + e^(2 pi i t)" `shouldBe` "w ⟵ 2^k + z^2.5 + e^(2 π i t)"

  it "writes types with blackboard letters, but only types" $ do
    normalize "C : C := 0" `shouldBe` "C : ℂ ≔ 0"
    normalize "xs : List of Z := []" `shouldBe` "xs : List of ℤ ≔ []"
    normalize "T : Table 2 of (C x Z)" `shouldBe` "T : Table 2 of (ℂ × ℤ)"
    normalize "T : Table 2 of C x Z" `shouldBe` "T : Table 2 of ℂ × ℤ"
    normalize "p : R x C := (0, 0)" `shouldBe` "p : ℝ × ℂ ≔ (0, 0)"
    normalize "p : R × C x Z := (0, 0, 1)" `shouldBe` "p : ℝ × ℂ × ℤ ≔ (0, 0, 1)"
    normalize "q <- (k : Real)" `shouldBe` "q ⟵ (k : ℝ)"
    normalize "z <- C x R" `shouldBe` "z ⟵ C x R"

  it "leaves stanza headers, comments, and strings alone" $
    normalize "@viewer \"a -> b\" var=C\nwrite \"x <- 1\" at z  # z <- z^2"
      `shouldBe` "@viewer \"a -> b\" var=C\nwrite \"x <- 1\" at z  # z <- z^2"

  it "doesn't change what any project script means" $ do
    scripts <- projectScripts
    length scripts `shouldSatisfy` (> 10)
    forM_ scripts $ \(file, src) -> do
      -- (ℝ, ℂ and ℤ are other spellings of R, C and Z)
      let tokens = map (canonical . baseToken) . tokenizeWithIndentation
          canonical = \case
            Identifier "ℝ" -> Identifier "R"
            Identifier "ℂ" -> Identifier "C"
            Identifier "ℤ" -> Identifier "Z"
            -- and × of both `*` and a pair type's `x`
            Identifier "x" -> Times
            t -> t
          new = normalize src
      (file, tokens new) `shouldBe` (file, tokens src)
      (file, normalize new) `shouldBe` (file, new)
