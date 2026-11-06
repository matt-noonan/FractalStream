{-# language OverloadedStrings #-}
-- | Reading the scripts out of old FractalStream 1.0 save files.
--
-- Those files are Apple binary property lists (@bplist00@) holding an
-- @NSKeyedArchiver@ archive of the document. The archive's root object
-- has an array of the script's title, its text, and some formatting;
-- it's either under the key @editor@, or (in the oldest files) @$1@:
--
-- > { type: "editor", editor: [title, script, rtfd data, ...] }
-- > { $0: "editor", $1: [title, script, rtfd data] }
--
-- The title and text may each be a plain string, or an @NSMutableString@
-- object holding one under @NS.string@.
module Language.Legacy.Binary
  ( decodeAncientFS
  , mightBeAncientFS
  -- * Binary property lists
  , PValue(..)
  , decodeBPList
  ) where

import FractalStream.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Data.Bits
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

-- | Peek at the start of the bytes to see if this looks like
-- Apple's binary property list data at all.
mightBeAncientFS :: ByteString -> Bool
mightBeAncientFS = (== "bplist00") . BS.take 8

-- | The title and text of the script in an old FractalStream 1.0 save
-- file, or why they couldn't be found.
decodeAncientFS :: ByteString -> Either String (String, String)
decodeAncientFS bytes = do
  top <- decodeBPList bytes
  let field k = \case
        PDict kvs -> lookup k kvs
        _ -> Nothing
      objects = case field "$objects" top of
        Just (PArray os) -> os
        _ -> []
      -- follow a reference into the archive's objects
      deref = \case
        PUID n | n >= 0, n < length objects -> deref (objects !! n)
        v -> v
      text v = case deref v of
        PString s -> Just s
        d -> field "NS.string" d >>= \case
          s | PString t <- deref s -> Just t
          _ -> Nothing
  when (null objects) $
    Left "This isn't an NSKeyedArchiver archive (it has no $objects)."
  root <- maybe (Left "The archive has no root object.") (Right . deref)
            (field "$top" top >>= field "root")
  let documents =
        [ (title, body)
        | k <- ["editor", "$1"]
        , Just arr <- [deref <$> field k root]
        , Just (PArray (t : b : _)) <- [deref <$> field "NS.objects" arr]
        , Just title <- [text t]
        , Just body <- [text b] ]
  case documents of
    doc : _ -> Right doc
    [] -> Left "The archive doesn't have a script's title and text where an old FractalStream file keeps them."

------------------------------------------------------------------------
-- Binary property lists

-- | A value in a binary property list. Arrays and dictionaries hold
-- their elements, already decoded.
data PValue
  = PNull
  | PBool Bool
  | PInt Integer
  | PReal Double
  | PDate Double
  | PData ByteString
  | PString String
  | PUID Int        -- ^ a reference to an object of an NSKeyedArchiver archive
  | PArray [PValue]
  | PSet [PValue]
  | PDict [(String, PValue)]  -- ^ (a key that isn't a string is left out)
  deriving (Eq, Show)

-- | Decode a binary property list (@bplist00@), described in Apple's
-- CoreFoundation source (@CFBinaryPList.c@): a header, the objects, a
-- table of where each object starts, and a 32-byte trailer saying how
-- to read the table.
decodeBPList :: ByteString -> Either String PValue
decodeBPList bytes = do
  unless (mightBeAncientFS bytes) $ Left "This isn't a binary property list."
  when (BS.length bytes < 8 + 32) $ Left "This binary property list is too short."
  let trailer = BS.drop (BS.length bytes - 32) bytes
      offsetSize = fromIntegral (BS.index trailer 6)
      refSize = fromIntegral (BS.index trailer 7)
      numObjects = beInt (BS.take 8 (BS.drop 8 trailer))
      topObject = beInt (BS.take 8 (BS.drop 16 trailer))
      tableStart = beInt (BS.take 8 (BS.drop 24 trailer))
  when (offsetSize < 1 || offsetSize > 8 || refSize < 1 || refSize > 8
        || tableStart + numObjects * offsetSize > BS.length bytes - 32
        || topObject >= numObjects) $
    Left "This binary property list's trailer doesn't make sense."
  let offsetOf n = beInt (slice (tableStart + n * offsetSize) offsetSize)

      slice at len = BS.take len (BS.drop at bytes)

      -- an object, by number, not following references more deeply
      -- than there are objects (so a cycle can't loop forever)
      object :: Int -> Int -> Either String PValue
      object depth n
        | n < 0 || n >= numObjects = Left ("A reference to object " ++ show n ++ ", which isn't there.")
        | depth > numObjects = Left "This binary property list's objects contain themselves."
        | otherwise = objectAt depth (offsetOf n)

      objectAt depth at = do
        when (at < 8 || at >= tableStart) $ Left "An object outside the list."
        let marker = BS.index bytes at
            kind = marker `shiftR` 4
            info = fromIntegral (marker .&. 0x0f) :: Int
            -- the length of a variable-sized object, and where its contents start
            sized = if info /= 0x0f then Right (info, at + 1) else do
              let m = BS.index bytes (at + 1)
              unless (m .&. 0xf0 == 0x10) $ Left "A length that isn't an integer."
              let w = 2 ^ (fromIntegral (m .&. 0x0f) :: Int)
              Right (beInt (slice (at + 2) w), at + 2 + w)
            -- (the references to an array's or dictionary's elements
            -- must fit before the offset table)
            refs start count
              | count < 0 || start + count * refSize > tableStart = Left "A collection longer than the list."
              | otherwise = Right [ beInt (slice (start + k * refSize) refSize) | k <- [0 .. count - 1] ]
            children = mapM (object (depth + 1)) <=< uncurry refs
        case kind of
          0x0 -> case info of
            0x0 -> Right PNull
            0x8 -> Right (PBool False)
            0x9 -> Right (PBool True)
            _ -> Left "An unknown kind of object."
          0x1 -> Right (PInt (signedInt (slice (at + 1) (2 ^ info))))
          0x2 -> PReal <$> float (slice (at + 1) (2 ^ info))
          0x3 -> PDate <$> float (slice (at + 1) 8)
          0x4 -> sized >>= \(len, start) -> Right (PData (slice start len))
          0x5 -> sized >>= \(len, start) ->
            Right (PString (map (toEnum . fromIntegral) (BS.unpack (slice start len))))
          0x6 -> sized >>= \(len, start) ->
            Right (PString (Text.unpack (TE.decodeUtf16BE (slice start (2 * len)))))
          0x8 -> Right (PUID (beInt (slice (at + 1) (info + 1))))
          0xA -> sized >>= \(len, start) -> PArray <$> children (start, len)
          0xC -> sized >>= \(len, start) -> PSet <$> children (start, len)
          0xD -> sized >>= \(len, start) -> do
            ks <- children (start, len)
            vs <- children (start + len * refSize, len)
            Right (PDict [ (k, v) | (PString k, v) <- zip ks vs ])
          _ -> Left "An unknown kind of object."

  object 0 topObject

-- | A big-endian unsigned integer.
beInt :: ByteString -> Int
beInt = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

-- | A big-endian integer, signed if it's 8 bytes or more (as the format
-- writes negative numbers).
signedInt :: ByteString -> Integer
signedInt bs =
  let n = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0 bs :: Integer
      bits = 8 * BS.length bs
  in if bits >= 64 && testBit n (bits - 1) then n - bit bits else n

-- | A big-endian IEEE float, 4 or 8 bytes.
float :: ByteString -> Either String Double
float bs = case BS.length bs of
  4 -> Right (realToFrac (castWord32ToFloat (fromIntegral (beInt bs))))
  8 -> Right (castWord64ToDouble (fromIntegral (beInt bs)))
  _ -> Left "A real number of an unusual size."
