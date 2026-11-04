{-# language OverloadedStrings #-}
module Data.YamlUnicodeSpec (spec) where

import Test.Hspec

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as YAML
import qualified Data.Yaml.Pretty as YAMLPretty

import Data.YamlUnicode

-- | Read encoded YAML back, showing any error.
roundTrip :: ByteString -> Either String Value
roundTrip = either (Left . show) Right . YAML.decodeEither'

-- | The encoded text, as characters.
asText :: (Value -> a) -> (a -> String) -> Value -> String
asText enc shown = shown . enc

spec :: Spec
spec = describe "encoding YAML" $ do

  let doc = object
        [ "title" .= Text.pack "𝝎 and 𝑖"
        , "description" .= Text.pack "For each 𝝎_0;\nand θ.\n"
        , "𝑖-key" .= Text.pack "value" ]
      decoded = Text.unpack . TE.decodeUtf8

  it "writes characters outside the Basic Multilingual Plane as they are" $ do
    let out = decoded (encodeYaml doc)
    out `shouldSatisfy` isInfixOf "title: 𝝎 and 𝑖"
    out `shouldSatisfy` isInfixOf "𝑖-key: value"
    out `shouldSatisfy` (not . isInfixOf "\\U")

  it "keeps text of several lines as a block" $ do
    let out = decoded (encodePrettyYaml YAMLPretty.defConfig doc)
    out `shouldSatisfy` isInfixOf "description: |\n  For each 𝝎_0;\n  and θ.\n"

  it "reads back as the same value" $ do
    roundTrip (encodeYaml doc) `shouldBe` Right doc
    roundTrip (encodePrettyYaml YAMLPretty.defConfig doc) `shouldBe` Right doc

  it "leaves private-use characters already in the text alone" $ do
    let mixed = object [ "a" .= Text.pack "\xE000 𝝎 \xE001" ]
    roundTrip (encodeYaml mixed) `shouldBe` Right mixed

  it "is the plain encoder when there's nothing to keep" $ do
    let plain = object [ "a" .= Text.pack "θ, π, and ℝ" ]
    asText encodeYaml decoded plain `shouldBe` asText YAML.encode decoded plain
