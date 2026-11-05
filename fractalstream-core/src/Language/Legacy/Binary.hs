{-# language OverloadedStrings #-}
-- | A partial decoder for old FractalStream 1.0 save files.
-- These files were encoded using NSCoder, and we don't attempt
-- a full decode here. Instead, we look for a few landmark
-- strings in the save files and make a best-effort attempt at
-- extracting the script name and body.
module Language.Legacy.Binary
  ( decodeAncientFS
  , mightBeAncientFS
  ) where

import FractalStream.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Data.Bits
import Control.Monad.Trans.Maybe

-- | Peek at the start of the bytes to see if this looks like
-- Apple's binary property list data at all.
mightBeAncientFS :: ByteString -> Bool
mightBeAncientFS = (== "bplist00") . BS.take 8

-- | Try to extract the script title and body from an old
-- FractalStream 1.0 save file. This is not based on any
-- deep understanding of the format, it just uses some
-- patterns that were observed in the hexdump of several
-- save files. It likely can fail on some valid data.
decodeAncientFS :: ByteString -> Maybe (String, String)
decodeAncientFS = evalState . runMaybeT $ do
  -- Check for header
  get >>= (guard . mightBeAncientFS)

  -- Find the `NS.objects` string, followed soon after
  -- by the title
  fastForwardTo "NS.objects"
  skip 19
  title <- getString

  -- Find the `NS.string` string, followed soon after
  -- by the script
  fastForwardTo "NS.string"
  skip 2
  body <- getString

  pure (title, body)

 where
  fastForwardTo target = do
    s <- get
    guard (not $ BS.null s)
    if target `BS.isPrefixOf` s
      then modify (BS.drop (BS.length target))
      else modify (BS.drop 1) >> fastForwardTo target

  skip = modify . BS.drop

  pop = do
    (x, xs) <- MaybeT (gets BS.uncons)
    put xs
    pure x

  getInt = do
    tag <- pop
    guard (tag .&. 0x10 == 0x10)
    let log2bytes :: Int = fromIntegral (tag .&. 0x0f)
    extractInt ((2 :: Int)^log2bytes)

  extractInt = go (0 :: Int)
    where
      go acc 0 = pure acc
      go acc n = do
        x <- pop
        go (256 * acc + fromIntegral x) (n - 1)

  getFormatAndLength = do
    tag <- pop
    format <- case tag .&. 0xf0 of
      0x50 -> pure False  -- ascii
      0x60 -> pure True   -- utf16be
      _    -> fail "unhandled tag type"
    if tag .&. 0x0f == 0x0f
      then (format,) <$> getInt
      else pure (format, fromIntegral (tag .&. 0x0f))

  getString = do
    s <- get
    guard (not $ BS.null s)
    (isUtf16, len :: Int) <- getFormatAndLength
    let byteLen = if isUtf16 then 2 * len else len
    (bytes, rest) <- gets (BS.splitAt byteLen)
    put rest
    let decoder = if isUtf16 then TE.decodeUtf16BE else TE.decodeUtf8
    pure (Text.unpack . decoder $ bytes)
