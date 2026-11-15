-- | Reading and writing zip archives (the container of a @.fsz@ project:
-- see PROJECT_FILES.md), as much of the format as FractalStream needs:
-- entries stored or deflated, without zip64 (no entry, nor the archive,
-- past 4 GB). Written archives are reproducible: every entry has the
-- same timestamp (1980-01-01, zip's first day), so the same contents
-- always give the same bytes. Entries are read lazily: an entry's data is
-- inflated only as it's consumed, so a large one can go straight to where
-- it's needed.
module Data.Zip
  ( isZip
  , readZip
  , writeZip
  , crc32
  ) where

import qualified Codec.Compression.Zlib.Raw as Raw
import Control.Monad (forM, unless, when)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word16, Word32)
import Foreign.C.Types (CUInt(..), CULong(..))
import Foreign.Ptr (Ptr, castPtr)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.IO.Unsafe (unsafePerformIO)

-- | Whether some bytes are a zip archive (they start with a local file
-- header).
isZip :: BS.ByteString -> Bool
isZip = BS.isPrefixOf (BS.pack [0x50, 0x4b, 0x03, 0x04])

-- | An archive's entries: each one's path and its contents (inflated as
-- they're consumed), in the archive's order; or why it can't be read.
readZip :: BS.ByteString -> Either String [(FilePath, BL.ByteString)]
readZip bytes = do
  eocd <- findEnd
  let count = fromIntegral (u16 (eocd + 10))
      cdStart = fromIntegral (u32 (eocd + 16))
  when (u32 (eocd + 16) == 0xffffffff || u16 (eocd + 10) == 0xffff) $
    Left "it's a zip64 archive, which FractalStream can't read"
  go count cdStart
  where
    n = BS.length bytes
    byte i = fromIntegral (BS.index bytes i) :: Word32
    u16 i = fromIntegral (byte i .|. (byte (i + 1) `shiftL` 8)) :: Word16
    u32 i = byte i .|. (byte (i + 1) `shiftL` 8) .|. (byte (i + 2) `shiftL` 16) .|. (byte (i + 3) `shiftL` 24)
    inRange i len = i >= 0 && len >= 0 && i + len <= n
    -- the end of central directory record: 22 bytes, at most 64 KB of
    -- comment after it
    findEnd = case [ i | i <- [n - 22, n - 23 .. max 0 (n - 22 - 65535)], u32 i == 0x06054b50 ] of
      i : _ | n >= 22 -> Right i
      _ -> Left "it isn't a zip archive (it has no central directory)"
    go :: Int -> Int -> Either String [(FilePath, BL.ByteString)]
    go 0 _ = Right []
    go k at = do
      unless (inRange at 46 && u32 at == 0x02014b50) $ Left "its central directory is damaged"
      let method = u16 (at + 10)
          size = fromIntegral (u32 (at + 20))
          nameLen = fromIntegral (u16 (at + 28))
          extraLen = fromIntegral (u16 (at + 30))
          commentLen = fromIntegral (u16 (at + 32))
          local = fromIntegral (u32 (at + 42))
      unless (inRange (at + 46) nameLen) $ Left "its central directory is damaged"
      let name = Text.unpack (Text.decodeUtf8Lenient (BS.take nameLen (BS.drop (at + 46) bytes)))
      unless (inRange local 30 && u32 local == 0x04034b50) $ Left ("its entry " ++ show name ++ " is damaged")
      let dataStart = local + 30 + fromIntegral (u16 (local + 26)) + fromIntegral (u16 (local + 28))
      unless (inRange dataStart size) $ Left ("its entry " ++ show name ++ " is cut short")
      let raw = BL.fromStrict (BS.take size (BS.drop dataStart bytes))
      contents <- case method of
        0 -> Right raw
        8 -> Right (Raw.decompress raw)
        m -> Left ("its entry " ++ show name ++ " is compressed in a way FractalStream can't read (method " ++ show m ++ ")")
      rest <- go (k - 1) (at + 46 + nameLen + extraLen + commentLen)
      pure ((name, contents) : rest)

-- | An archive of these entries (each deflated), or why it can't be one
-- (an entry, or the archive, past 4 GB).
writeZip :: [(FilePath, BL.ByteString)] -> Either String BL.ByteString
writeZip entries = do
  locals <- forM entries $ \(path, contents) -> do
    let name = Text.encodeUtf8 (Text.pack path)
        packed = Raw.compressWith Raw.defaultCompressParams { Raw.compressLevel = Raw.defaultCompression } contents
        size = BL.length contents
        packedSize = BL.length packed
    when (size >= 0xffffffff || packedSize >= 0xffffffff) $
      Left ("its entry " ++ show path ++ " is too large for a zip archive (4 GB)")
    pure (name, crc32 contents, fromIntegral packedSize, fromIntegral size, packed)
  let header (name, crc, packedSize, size, _) =
        w16 20 <> w16 0x0800 <> w16 8 <> w16 0 <> w16 0x0021      -- version, UTF-8 names, deflate, 1980-01-01
        <> w32 crc <> w32 packedSize <> w32 size <> w16 (fromIntegral (BS.length name)) <> w16 0
      localOf e@(name, _, _, _, packed) = w32 0x04034b50 <> header e <> B.byteString name <> B.lazyByteString packed
      localSize (name, _, packedSize, _, _) = 30 + fromIntegral (BS.length name) + fromIntegral packedSize :: Integer
      offsets = scanl (+) 0 (map localSize locals)
      central = mconcat
        [ w32 0x02014b50 <> w16 20 <> header e <> w16 0 <> w16 0 <> w16 0 <> w32 0
          <> w32 (fromIntegral off) <> B.byteString name
        | (e@(name, _, _, _, _), off) <- zip locals offsets ]
      cdSize = sum [ 46 + fromIntegral (BS.length name) | (name, _, _, _, _) <- locals ] :: Integer
      cdStart = last offsets
  when (cdStart + cdSize >= 0xffffffff || length locals >= 0xffff) $
    Left "the archive would be too large for a zip archive (4 GB)"
  pure $ B.toLazyByteString $
    mconcat (map localOf locals) <> central
    <> w32 0x06054b50 <> w16 0 <> w16 0 <> w16 (fromIntegral (length locals)) <> w16 (fromIntegral (length locals))
    <> w32 (fromIntegral cdSize) <> w32 (fromIntegral cdStart) <> w16 0
  where
    w16 = B.word16LE
    w32 = B.word32LE

-- | The CRC-32 of some bytes (zlib's).
crc32 :: BL.ByteString -> Word32
crc32 = fromIntegral . BL.foldlChunks step 0
  where
    step crc chunk = unsafePerformIO $ BSU.unsafeUseAsCStringLen chunk $ \(p, len) ->
      c_crc32 crc (castPtr p) (fromIntegral len)

-- (zlib's own: the zlib package's, bundled, or the system's)
foreign import ccall unsafe "crc32"
  c_crc32 :: CULong -> Ptr () -> CUInt -> IO CULong
