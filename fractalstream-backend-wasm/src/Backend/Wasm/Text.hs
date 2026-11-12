-- | Text in compiled code, made in the kernel's arena (see 'Arena').
--
-- A text is the address of a node, or 0 for the empty text. Numbers are
-- kept as numbers, not formatted: the engine formats them when it reads a
-- text back (with 'Language.Type.showValue', as the interpreter does), so
-- they come out exactly as interpreted, without calling back into
-- Haskell from a worker. (The language has nothing that looks inside a
-- text, so nothing in compiled code needs its characters.) A node is, by
-- its kind (an i32 at 0):
--
-- * 'literalNode': its length in bytes (an i32 at 4), then its UTF-8
--   bytes from 8;
-- * 'numberNode': what the number is (an i32 at 4: 0 an integer, 1 a real
--   number, 2 a complex number), then its value as two f64s at 8 and 16
--   (an integer's as a real number, then 0; a real number's, then 0);
-- * 'concatNode': how many parts (an i32 at 4), then their addresses
--   (i32s from 8).
--
-- A node that doesn't fit in the arena sets the overflow flag, and gives
-- the empty text (as the LLVM backend does).
module Backend.Wasm.Text
  ( literalNode
  , numberNode
  , concatNode
  , textLiteral
  , textOfNumber
  , concatText
  , utf8Bytes
  , literalNodeBytes
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode
import Backend.Wasm.Gen
import Backend.Wasm.List (alloc)

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (ord)

literalNode, numberNode, concatNode :: Int
literalNode = 1
numberNode = 2
concatNode = 3

roundUp8 :: Int -> Int
roundUp8 n = (n + 7) `div` 8 * 8

-- | How many bytes a literal node for the given text takes.
literalNodeBytes :: String -> Int
literalNodeBytes s = roundUp8 (8 + length (utf8Bytes s))

-- | Allocate a node of the given size and kind; @fill@ writes the rest of
-- it (given its address), if it fits.
node :: Int -> Int -> (Int -> [Instr]) -> Gen Loc
node size kind fill = do
  p <- alloc size
  emit [ localGet p
       , IfElse ([ localGet p, i32Const (fromIntegral kind), i32Store 0 ] ++ fill p) [] ]
  pure (LText p)

-- | A text literal, copied into the arena (four bytes at a time).
textLiteral :: String -> Gen Loc
textLiteral s = do
  let bytes = utf8Bytes s
      n = length bytes
      word ws = foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Word32) ws
      chunks = chunksOf4 bytes
  node (roundUp8 (8 + n)) literalNode $ \p ->
    [ localGet p, i32Const (fromIntegral n), i32Store 4 ]
    ++ concat [ [ localGet p, i32Const (fromIntegral (fromIntegral (word ws) :: Int32)), i32Store (8 + 4 * k) ]
              | (k, ws) <- zip [0 ..] chunks ]
  where
    chunksOf4 [] = []
    chunksOf4 xs = let (a, b) = splitAt 4 xs in a : chunksOf4 b

-- | A number as text: kind 0 an integer (in an i32 local), 1 a real
-- number, 2 a complex number (in f64 locals).
textOfNumber :: Int -> [Int] -> Gen Loc
textOfNumber tag ls = node 24 numberNode $ \p ->
  [ localGet p, i32Const (fromIntegral tag), i32Store 4 ] ++ case (tag, ls) of
    (0, [i]) -> [ localGet p, localGet i, f64ConvertI32S, f64Store 8
                , localGet p, f64Const 0, f64Store 16 ]
    (1, [x]) -> [ localGet p, localGet x, f64Store 8, localGet p, f64Const 0, f64Store 16 ]
    (_, [x, y]) -> [ localGet p, localGet x, f64Store 8, localGet p, localGet y, f64Store 16 ]
    _ -> []

-- | Texts one after another.
concatText :: [Int] -> Gen Loc
concatText parts = node (roundUp8 (8 + 4 * length parts)) concatNode $ \p ->
  [ localGet p, i32Const (fromIntegral (length parts)), i32Store 4 ]
  ++ concat [ [ localGet p, localGet t, i32Store (8 + 4 * k) ] | (k, t) <- zip [0 ..] parts ]

-- | A string's UTF-8 encoding.
utf8Bytes :: String -> [Word8]
utf8Bytes = concatMap (encode . ord)
  where
    encode c
      | c < 0x80 = [fromIntegral c]
      | c < 0x800 = [0xc0 .|. hi 6, cont 0]
      | c < 0x10000 = [0xe0 .|. hi 12, cont 6, cont 0]
      | otherwise = [0xf0 .|. hi 18, cont 12, cont 6, cont 0]
      where
        hi n = fromIntegral (c `shiftR` n)
        cont n = 0x80 .|. fromIntegral ((c `shiftR` n) .&. 0x3f)
