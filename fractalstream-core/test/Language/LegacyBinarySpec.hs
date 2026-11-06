{-# language OverloadedStrings #-}
module Language.LegacyBinarySpec (spec) where

-- | Reading scripts out of FractalStream 1.0 save files
-- ("Language.Legacy.Binary"). In @test/legacy-binary/@,
-- @editor-layout.fs@ and @oldest-layout.fs@ are made by its
-- @make-fixtures.py@, in the two layouts real save files have; the
-- others are real save files.

import Test.Hspec

import Actor.Ensemble (checkProjectBytes)
import Language.Legacy.Binary
import Language.Legacy.Translate

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

fixture :: FilePath -> IO BS.ByteString
fixture name = BS.readFile ("test/legacy-binary/" ++ name)

spec :: Spec
spec = describe "reading FractalStream 1.0 save files" $ do

  it "finds the title and script in the usual layout, `editor: [title, script, ...]`" $ do
    bytes <- fixture "editor-layout.fs"
    mightBeAncientFS bytes `shouldBe` True
    decodeAncientFS bytes `shouldBe` Right ("Julia set for z\178 \8722 1", "iterate z^2 - 1 until z escapes.\n")

  it "finds the title and script in the oldest layout, `$1: [title, script, ...]`" $ do
    bytes <- fixture "oldest-layout.fs"
    fmap fst (decodeAncientFS bytes) `shouldBe` Right "Oldest"
    fmap (take 12 . snd) (decodeAncientFS bytes) `shouldBe` Right "set w to z.\r"

  it "translates the oldest layout's script, with its old line endings and a stray `end.`" $ do
    bytes <- fixture "oldest-layout.fs"
    case decodeAncientFS bytes >>= uncurry translateLegacyScript of
      Left err -> expectationFailure err
      Right t -> do
        translationWarnings t `shouldSatisfy` any ("`end.`" `isInfixOf`)
        checkProjectBytes (T.encodeUtf8 (T.pack (translatedProject t))) `shouldBe` Right ()

  it "reads the plist's values" $ do
    bytes <- fixture "editor-layout.fs"
    case decodeBPList bytes of
      Right (PDict kvs) -> do
        lookup "$archiver" kvs `shouldBe` Just (PString "NSKeyedArchiver")
        lookup "$version" kvs `shouldBe` Just (PInt 100000)
        lookup "$top" kvs `shouldBe` Just (PDict [("root", PUID 1)])
      other -> expectationFailure ("expected a dictionary, got " ++ show other)

  describe "real save files" $
    forM_ [ ("ArturoEx2fromLBforFractalStream.fs", "arturoex2", "iterate\n((-0.30178504906287279)*z^9")
          , ("per30cubic.fs", "Per30 in cubic", "par set z to (2/3)*")
          , ("Sarah3.fs", "Sarah 3", "set x to z.")
          , ("withLaura.fs", "withLaura", "par set z to (1/64)*") ] $ \(file, title, start) ->
      it (file ++ " translates into a project that loads") $ do
        bytes <- fixture file
        case decodeAncientFS bytes of
          Left err -> expectationFailure err
          Right (t, body) -> do
            t `shouldBe` title
            take (length start) body `shouldBe` start
            case translateLegacyScript t body of
              Left err -> expectationFailure err
              Right tr -> checkProjectBytes (T.encodeUtf8 (T.pack (translatedProject tr))) `shouldBe` Right ()

  it "explains, rather than crashing, when the bytes aren't a save file" $ do
    bytes <- fixture "editor-layout.fs"
    decodeAncientFS "iterate z^2 until z escapes." `shouldSatisfy` either (const True) (const False)
    decodeAncientFS (BS.take 100 bytes) `shouldSatisfy` either (const True) (const False)
    decodeAncientFS (BS.take 40 bytes <> BS.drop 60 bytes) `shouldSatisfy` either (const True) (const False)
