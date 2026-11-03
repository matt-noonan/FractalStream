{-# language OverloadedStrings #-}
{- |
Module      : Data.Movie
Description : Writing Motion-JPEG movie files

A small, dependency-free movie writer: each frame is a JPEG image
(JuicyPixels' encoder), stored one after another in a movie container,
so a movie can be made anywhere without an external encoder like
@ffmpeg@. Two containers are supported, since no single one plays in
every operating system's own movie player:

* QuickTime (@.mov@, codec @jpeg@, "Photo - JPEG"), which QuickTime
  Player and the rest of macOS (AVFoundation) play;
* AVI (@.avi@, codec @MJPG@), which Windows' players and the usual Linux
  players play.

'defaultMovieFormat' picks the one that suits the platform. Frames are
written to the file as they come ('addMovieFrame'), so a long movie
never has to fit in memory; the indexes that describe them are written
once the movie is finished ('finishMovie'), along with the frame counts
the headers need. A file whose movie wasn't finished is no use, and is
removed by 'withMovieFile' when it fails.

Both containers here use 32-bit offsets, so a movie must stay under
4 GB (AVI players may already balk past 1 GB): at the sizes FractalStream
records, that's tens of thousands of frames.
-}
module Data.Movie
  ( MovieFormat(..)
  , defaultMovieFormat
  , movieExtension
  , describeMovieFormat
  , MovieFile
  , withMovieFile
  , addMovieFrame
  , encodeJpegFrame
  , movieFrameCount
  ) where

import Codec.Picture (Image, PixelRGB8)
import Codec.Picture.Jpg (encodeJpegAtQuality)
import Codec.Picture.Types (convertImage)
import Control.Exception (onException)
import Data.Bits (shiftL)
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Data.Word
import System.Directory (removeFile)
import System.IO
import qualified System.Info as System

-- | The kind of movie file to write.
data MovieFormat
  = QuickTimeMJPEG  -- ^ a QuickTime @.mov@ of JPEG frames
  | AviMJPEG        -- ^ an AVI of JPEG frames
  deriving (Eq, Show, Enum, Bounded)

-- | The format this platform's own movie player is sure to play:
-- QuickTime on macOS, and AVI elsewhere.
defaultMovieFormat :: MovieFormat
defaultMovieFormat = case System.os of
  "darwin" -> QuickTimeMJPEG
  _        -> AviMJPEG

-- | The file extension for a format, without the dot.
movieExtension :: MovieFormat -> String
movieExtension = \case
  QuickTimeMJPEG -> "mov"
  AviMJPEG       -> "avi"

-- | A name for a format, for a save dialog.
describeMovieFormat :: MovieFormat -> String
describeMovieFormat = \case
  QuickTimeMJPEG -> "QuickTime movie"
  AviMJPEG       -> "AVI movie"

-- | Encode a frame as a JPEG image, at the given quality (1 to 100).
encodeJpegFrame :: Int -> Image PixelRGB8 -> BS.ByteString
encodeJpegFrame quality =
  BSL.toStrict . encodeJpegAtQuality (fromIntegral (max 1 (min 100 quality))) . convertImage

-- | A movie being written.
data MovieFile = MovieFile
  { mfHandle :: Handle
  , mfFormat :: MovieFormat
  , mfSize   :: (Int, Int)
  , mfFps    :: Int
  , mfFrames :: IORef [(Word32, Word32)]
    -- ^ each frame written so far, newest first: where its data starts
    -- (as the format's index counts it) and how many bytes it has
  , mfOffset :: IORef Word32
    -- ^ how many bytes have been written to the file
  }

-- | How many frames have been added to a movie so far.
movieFrameCount :: MovieFile -> IO Int
movieFrameCount mf = length <$> readIORef (mfFrames mf)

-- | Write a movie file, of frames of the given size (in pixels), shown
-- at the given rate (in frames per second). The frames are added with
-- 'addMovieFrame'; once the action returns, the movie is finished. If
-- the action fails (or is cancelled), the partly written file is
-- removed.
withMovieFile :: MovieFormat -> FilePath -> (Int, Int) -> Int -> (MovieFile -> IO a) -> IO a
withMovieFile fmt path size fps action = do
  h <- openBinaryFile path WriteMode
  let cleanUp = hClose h >> removeFile path
  flip onException cleanUp $ do
    frames <- newIORef []
    offset <- newIORef 0
    let mf = MovieFile h fmt size (max 1 fps) frames offset
    writeHeader mf
    result <- action mf
    finishMovie mf
    hClose h
    pure result

-- | Add one frame (a JPEG image, as 'encodeJpegFrame' makes) to a movie.
addMovieFrame :: MovieFile -> BS.ByteString -> IO ()
addMovieFrame mf@MovieFile{..} jpeg = case mfFormat of
  QuickTimeMJPEG -> do
    start <- readIORef mfOffset
    emit mf (byteString jpeg)
    modifyIORef' mfFrames ((start, len) :)
  AviMJPEG -> do
    -- AVI's index counts from the `movi` list's own type tag.
    start <- subtract aviMoviTagOffset <$> readIORef mfOffset
    emit mf (word32BE (fourcc "00dc") <> word32LE len <> byteString jpeg
             <> (if odd len then word8 0 else mempty))
    modifyIORef' mfFrames ((start, len) :)
  where
    len = fromIntegral (BS.length jpeg)

emit :: MovieFile -> Builder -> IO ()
emit MovieFile{..} b = do
  let bytes = toLazyByteString b
  BSL.hPut mfHandle bytes
  modifyIORef' mfOffset (+ fromIntegral (BSL.length bytes))

---------------------------------------------------------------------------
-- Headers
---------------------------------------------------------------------------

-- | Write what goes before the first frame. Sizes and counts that
-- aren't known yet are written as zeros, and filled in by 'finishMovie'.
writeHeader :: MovieFile -> IO ()
writeHeader mf = case mfFormat mf of
  QuickTimeMJPEG -> emit mf (qtFileType <> word32BE 0 <> word32BE (fourcc "mdat"))
  AviMJPEG -> emit mf (aviHeader (mfSize mf) (mfFps mf) 0 0 0)

-- | Write the indexes after the last frame, and fill in the header.
finishMovie :: MovieFile -> IO ()
finishMovie mf@MovieFile{..} = do
  frames <- reverse <$> readIORef mfFrames
  case mfFormat of
    QuickTimeMJPEG -> do
      end <- readIORef mfOffset
      emit mf (qtMovie mf frames)
      -- the `mdat` atom runs from just after `ftyp` to the end of the frames
      let mdatStart = fromIntegral (BSL.length (toLazyByteString qtFileType))
      hSeek mfHandle AbsoluteSeek (fromIntegral mdatStart)
      BSL.hPut mfHandle (toLazyByteString (word32BE (end - mdatStart)))
    AviMJPEG -> do
      moviEnd <- readIORef mfOffset
      emit mf (atom LE "idx1" (mconcat
        [ word32BE (fourcc "00dc") <> word32LE 0x10 {- AVIIF_KEYFRAME -}
          <> word32LE start <> word32LE len
        | (start, len) <- frames ]))
      fileEnd <- readIORef mfOffset
      let moviSize = moviEnd - aviMoviTagOffset
      hSeek mfHandle AbsoluteSeek 0
      BSL.hPut mfHandle (toLazyByteString (aviHeader mfSize mfFps (fromIntegral (length frames))
                                                     (fileEnd - 8) moviSize))
      -- the header is the same size either way, so nothing else moves
      hSeek mfHandle SeekFromEnd 0

---------------------------------------------------------------------------
-- QuickTime
---------------------------------------------------------------------------

qtFileType :: Builder
qtFileType = atom BE "ftyp" (word32BE (fourcc "qt  ") <> word32BE 0x00000200 <> word32BE (fourcc "qt  "))

-- | The movie's description (its `moov` atom), for the given frames.
-- Every frame is a sample in a chunk of its own, shown for one tick of
-- a media clock that ticks once a frame.
qtMovie :: MovieFile -> [(Word32, Word32)] -> Builder
qtMovie MovieFile{..} frames = atom BE "moov" (mvhd <> trak)
  where
    (w, h) = mfSize
    n = fromIntegral (length frames) :: Word32
    fps = fromIntegral mfFps :: Word32
    movieScale = 1000 :: Word32
    movieDuration = (n * movieScale) `div` fps
    matrix = mconcat (map word32BE [0x00010000, 0, 0, 0, 0x00010000, 0, 0, 0, 0x40000000])
    fixed16 x = word32BE (fromIntegral x `shiftL` 16)
    zeros k = mconcat (replicate k (word8 0))

    mvhd = atom BE "mvhd" $ mconcat
      [ word32BE 0                 -- version, flags
      , word32BE 0, word32BE 0     -- creation, modification times
      , word32BE movieScale, word32BE movieDuration
      , word32BE 0x00010000        -- preferred rate 1.0
      , word16BE 0x0100            -- preferred volume 1.0
      , zeros 10
      , matrix
      , zeros 24                   -- preview, poster, selection, current time
      , word32BE 2                 -- next track ID
      ]

    trak = atom BE "trak" (tkhd <> mdia)

    tkhd = atom BE "tkhd" $ mconcat
      [ word32BE 0x0000000F        -- version 0; enabled, in movie, preview, poster
      , word32BE 0, word32BE 0
      , word32BE 1                 -- track ID
      , word32BE 0
      , word32BE movieDuration
      , zeros 8
      , word16BE 0, word16BE 0     -- layer, alternate group
      , word16BE 0                 -- volume (none, for video)
      , word16BE 0
      , matrix
      , fixed16 w, fixed16 h
      ]

    mdia = atom BE "mdia" (mdhd <> handler "mhlr" "vide" "Video Media Handler" <> minf)

    mdhd = atom BE "mdhd" $ mconcat
      [ word32BE 0, word32BE 0, word32BE 0
      , word32BE fps, word32BE n
      , word16BE 0x7FFF            -- language: unspecified
      , word16BE 0                 -- quality
      ]

    handler kind subtype name = atom BE "hdlr" $ mconcat
      [ word32BE 0
      , word32BE (fourcc kind), word32BE (fourcc subtype)
      , word32BE 0, word32BE 0, word32BE 0
      , pascalString name
      ]

    minf = atom BE "minf" $ mconcat
      [ atom BE "vmhd" (word32BE 0x00000001 <> word16BE 0x0040 <> zeros 6)
      , handler "dhlr" "alis" "Data Handler"
      , atom BE "dinf" (atom BE "dref" (word32BE 0 <> word32BE 1
                                         <> atom BE "alis" (word32BE 0x00000001)))
      , stbl
      ]

    stbl = atom BE "stbl" $ mconcat
      [ atom BE "stsd" (word32BE 0 <> word32BE 1 <> sampleDescription)
      , atom BE "stts" (word32BE 0 <> word32BE 1 <> word32BE n <> word32BE 1)
      , atom BE "stsc" (word32BE 0 <> word32BE 1 <> word32BE 1 <> word32BE 1 <> word32BE 1)
      , atom BE "stsz" (word32BE 0 <> word32BE 0 <> word32BE n <> foldMap (word32BE . snd) frames)
      , atom BE "stco" (word32BE 0 <> word32BE n <> foldMap (word32BE . fst) frames)
      ]

    sampleDescription = atom BE "jpeg" $ mconcat
      [ zeros 6, word16BE 1        -- reserved, data reference index
      , word16BE 0, word16BE 0     -- version, revision
      , word32BE (fourcc "appl")   -- vendor
      , word32BE 0                 -- temporal quality
      , word32BE 0x00000200        -- spatial quality: normal
      , word16BE (fromIntegral w), word16BE (fromIntegral h)
      , word32BE 0x00480000, word32BE 0x00480000   -- 72 dpi
      , word32BE 0                 -- data size
      , word16BE 1                 -- frames per sample
      , paddedPascalString 32 "Photo - JPEG"
      , word16BE 24                -- depth
      , word16BE 0xFFFF            -- no color table
      ]

---------------------------------------------------------------------------
-- AVI
---------------------------------------------------------------------------

-- | Everything up to the first frame, for a movie of the given frame
-- size and rate. It's always the same size, so it's written first with
-- zeros for what isn't known yet, and again, filled in, once the movie
-- is finished: the number of frames, the RIFF size, and the size of the
-- `movi` list (from its type tag on, as a list's size counts).
aviHeader :: (Int, Int) -> Int -> Word32 -> Word32 -> Word32 -> Builder
aviHeader (w0, h0) fps0 frameCount riffSize moviSize = mconcat
  [ word32BE (fourcc "RIFF"), word32LE riffSize, word32BE (fourcc "AVI ")
  , list "hdrl" (avih <> list "strl" (strh <> strf))
  , word32BE (fourcc "LIST"), word32LE moviSize, word32BE (fourcc "movi")
  ]
  where
    (w, h) = (fromIntegral w0, fromIntegral h0) :: (Word32, Word32)
    fps = fromIntegral fps0 :: Word32
    list tag body = atom LE "LIST" (word32BE (fourcc tag) <> body)
    avih = atom LE "avih" $ mconcat
      [ word32LE (1000000 `div` fps)  -- microseconds per frame
      , word32LE 0, word32LE 0
      , word32LE 0x10                 -- AVIF_HASINDEX
      , word32LE frameCount
      , word32LE 0, word32LE 1        -- initial frames, streams
      , word32LE 0                    -- suggested buffer size
      , word32LE w, word32LE h
      , word32LE 0, word32LE 0, word32LE 0, word32LE 0
      ]
    strh = atom LE "strh" $ mconcat
      [ word32BE (fourcc "vids"), word32BE (fourcc "MJPG")
      , word32LE 0, word16LE 0, word16LE 0, word32LE 0
      , word32LE 1, word32LE fps      -- scale, rate: fps frames a second
      , word32LE 0, word32LE frameCount
      , word32LE 0
      , word32LE 0xFFFFFFFF           -- quality: default
      , word32LE 0
      , word16LE 0, word16LE 0, word16LE (fromIntegral w), word16LE (fromIntegral h)
      ]
    strf = atom LE "strf" $ mconcat
      [ word32LE 40, word32LE w, word32LE h
      , word16LE 1, word16LE 24
      , word32BE (fourcc "MJPG")
      , word32LE (w * h * 3)
      , word32LE 0, word32LE 0, word32LE 0, word32LE 0
      ]

-- | Where the `movi` list's type tag is in the file: the last four
-- bytes of the header. Frame offsets in the index count from there.
aviMoviTagOffset :: Word32
aviMoviTagOffset = fromIntegral (BSL.length (toLazyByteString (aviHeader (1, 1) 1 0 0 0))) - 4

---------------------------------------------------------------------------
-- Building blocks
---------------------------------------------------------------------------

-- | Byte order of an atom's size: QuickTime's are big-endian and count
-- the 8-byte atom header too, while RIFF's are little-endian, count
-- only the body, and pad an odd-sized body to an even size.
data Endian = BE | LE

atom :: Endian -> String -> Builder -> Builder
atom endian tag body = case endian of
  BE -> word32BE (fromIntegral (len + 8)) <> word32BE (fourcc tag) <> lazyByteString bytes
  LE -> word32BE (fourcc tag) <> word32LE (fromIntegral len) <> lazyByteString bytes
        <> (if odd len then word8 0 else mempty)
  where
    bytes = toLazyByteString body
    len = BSL.length bytes

-- | A four-character code, as a big-endian word (so that writing it
-- big-endian gives its characters in order).
fourcc :: String -> Word32
fourcc = foldl (\acc c -> acc * 256 + fromIntegral (fromEnum c)) 0 . take 4 . (++ "    ")

pascalString :: String -> Builder
pascalString s = word8 (fromIntegral (length s)) <> string7 s

-- | A Pascal string in a fixed-size field (counting its length byte).
paddedPascalString :: Int -> String -> Builder
paddedPascalString size s0 =
  pascalString s <> mconcat (replicate (size - 1 - length s) (word8 0))
  where s = take (size - 1) s0
