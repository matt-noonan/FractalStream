-- | Writing YAML that keeps every character as itself.
--
-- libyaml, which the @yaml@ package's encoders use, doesn't count any
-- character outside Unicode's Basic Multilingual Plane (U+10000 and up,
-- like the mathematical letters 𝑖 and 𝝎 a script can use as names) as
-- printable. So it writes any string with one in it as a double-quoted
-- string, with that character escaped (@\\U0001D74E@) and, for text of
-- several lines, every newline escaped too: valid YAML, but unreadable.
--
-- These encoders get around that: each such character is swapped for a
-- character of the Private Use Area (from U+E000) that the document
-- doesn't already use, which libyaml writes as is, and swapped back in
-- the encoded text. Since the stand-ins appear nowhere else, the result
-- is exactly what libyaml would write if it counted those characters as
-- printable. (A document with more than 4096 such characters keeps the
-- rest escaped, which is still correct.)
module Data.YamlUnicode
  ( encodeYaml
  , encodePrettyYaml
  , keepingAllCharacters
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as YAML
import qualified Data.Yaml.Pretty as YAMLPretty

-- | 'YAML.encode', keeping every character as itself.
encodeYaml :: Value -> ByteString
encodeYaml = keepingAllCharacters YAML.encode

-- | 'YAMLPretty.encodePretty', keeping every character as itself.
encodePrettyYaml :: YAMLPretty.Config -> Value -> ByteString
encodePrettyYaml config = keepingAllCharacters (YAMLPretty.encodePretty config)

-- | Run a YAML encoder so that characters outside the Basic Multilingual
-- Plane come out as themselves, not escaped.
keepingAllCharacters :: (Value -> ByteString) -> Value -> ByteString
keepingAllCharacters encode v
  | Map.null standIns = encode v
  | otherwise = restore (encode (mapText (Text.map toStandIn) v))
  where
    texts = allText v
    inUse = Set.fromList (concatMap Text.unpack texts)
    astral = nub [ c | t <- texts, c <- Text.unpack t, c > '\xFFFF' ]
    free = [ c | c <- ['\xE000' .. '\xEFFF'], c `Set.notMember` inUse ]
    standIns = Map.fromList (zip astral free)
    originals = Map.fromList (zip free astral)
    toStandIn c = Map.findWithDefault c c standIns
    fromStandIn c = Map.findWithDefault c c originals
    restore = TE.encodeUtf8 . Text.map fromStandIn . TE.decodeUtf8

-- | Every piece of text in a value: its strings and its keys.
allText :: Value -> [Text]
allText = \case
  String t -> [t]
  Object o -> concat [ Key.toText k : allText x | (k, x) <- KM.toList o ]
  Array xs -> concatMap allText xs
  _ -> []

-- | Change every piece of text in a value (keys too).
mapText :: (Text -> Text) -> Value -> Value
mapText f = \case
  String t -> String (f t)
  Object o -> Object (KM.fromList [ (Key.fromText (f (Key.toText k)), mapText f x) | (k, x) <- KM.toList o ])
  Array xs -> Array (fmap (mapText f) xs)
  other -> other
