-- | A small WebAssembly binary encoder: just what the code generator
-- needs. Instructions are structured (blocks, loops, and ifs hold their
-- bodies), and are encoded to bytes along with the rest of a module: its
-- types, imports, one exported function, and the function's locals.
module Backend.Wasm.Encode
  ( ValType(..)
  , Instr(..)
  , Import(..)
  , MemoryImport(..)
  , WasmModule(..)
  , encodeModule
    -- * Instructions
  , localGet, localSet, localTee
  , i32Const, f64Const
  , i32Load, i32Load8U, f64Load, i32Store8, i32Store, f64Store
  , call
  , br, brIf
  , op
    -- ** Opcodes
  , i32Eqz, i32Eq, i32Ne, i32LtS, i32GtS, i32LeS, i32GeS, i32LtU, i32GtU, i32GeU
  , i32Add, i32Sub, i32Mul, i32DivS, i32DivU, i32RemS, i32And, i32Or, i32Xor, i32Shl
  , f64Eq, f64Ne, f64Lt, f64Gt, f64Le, f64Ge
  , f64Abs, f64Neg, f64Ceil, f64Floor, f64Trunc, f64Sqrt
  , f64Add, f64Sub, f64Mul, f64Div
  , f64ConvertI32S, f64ConvertI32U
  , i32TruncSatF64S, i32TruncSatF64U
  , select
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64)

data ValType = I32 | F64
  deriving (Eq, Show)

valType :: ValType -> Word8
valType = \case
  I32 -> 0x7f
  F64 -> 0x7c

data Instr
  = Op [Word8]                -- ^ a plain instruction, already encoded
  | Block [Instr]
  | Loop [Instr]
  | IfElse [Instr] [Instr]    -- ^ (consumes an i32 condition)
  deriving Show

-- | An imported function, by module and name, with its parameter and
-- result types.
data Import = Import
  { importModule :: String
  , importName   :: String
  , importParams :: [ValType]
  , importResults :: [ValType]
  }

-- | How the module imports @env.memory@: the engine's own (unshared)
-- memory, or a shared one (for workers; a shared memory must give a
-- maximum).
data MemoryImport = UnsharedMemory | SharedMemory

data WasmModule = WasmModule
  { wmMemory  :: MemoryImport
  , wmImports :: [Import]        -- ^ functions 0, 1, ...
  , wmName    :: String          -- ^ the exported function's name
  , wmParams  :: [ValType]
  , wmResults :: [ValType]
  , wmLocals  :: [ValType]       -- ^ after the parameters
  , wmBody    :: [Instr]
  }

encodeModule :: WasmModule -> [Word8]
encodeModule WasmModule{..} =
  [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]
  ++ section 1 (vec (map funcType types))
  ++ section 2 (vec (memory : zipWith importFunc [0 ..] wmImports))
  ++ section 3 (vec [uleb (fromIntegral (length wmImports))])
  ++ section 7 (vec [name wmName ++ [0x00] ++ uleb (fromIntegral (length wmImports))])
  ++ section 10 (vec [let b = code in uleb (fromIntegral (length b)) ++ b])
  where
    -- type k is import k's; the last is the exported function's
    types = [ (importParams i, importResults i) | i <- wmImports ] ++ [(wmParams, wmResults)]
    funcType (ps, rs) = 0x60 : vec (map (pure . valType) ps) ++ vec (map (pure . valType) rs)
    memory = name "env" ++ name "memory" ++ [0x02] ++ case wmMemory of
      UnsharedMemory -> 0x00 : uleb 1
      SharedMemory   -> 0x03 : uleb 1 ++ uleb 65536
    importFunc :: Integer -> Import -> [Word8]
    importFunc k Import{..} = name importModule ++ name importName ++ [0x00] ++ uleb k
    code = vec [ uleb 1 ++ [valType t] | t <- wmLocals ] ++ concatMap encodeInstr wmBody ++ [0x0b]

encodeInstr :: Instr -> [Word8]
encodeInstr = \case
  Op bs -> bs
  Block is -> [0x02, 0x40] ++ concatMap encodeInstr is ++ [0x0b]
  Loop is -> [0x03, 0x40] ++ concatMap encodeInstr is ++ [0x0b]
  IfElse yes no -> [0x04, 0x40] ++ concatMap encodeInstr yes
                   ++ (if null no then [] else 0x05 : concatMap encodeInstr no)
                   ++ [0x0b]

section :: Word8 -> [Word8] -> [Word8]
section k body = k : uleb (fromIntegral (length body)) ++ body

vec :: [[Word8]] -> [Word8]
vec xs = uleb (fromIntegral (length xs)) ++ concat xs

name :: String -> [Word8]
name s = uleb (fromIntegral (length s)) ++ map (fromIntegral . fromEnum) s

uleb :: Integer -> [Word8]
uleb n | n < 0x80 = [fromIntegral n]
       | otherwise = (fromIntegral (n .&. 0x7f) .|. 0x80) : uleb (n `shiftR` 7)

sleb :: Integer -> [Word8]
sleb n | n >= -64 && n < 64 = [fromIntegral (n .&. 0x7f)]
       | otherwise = (fromIntegral (n .&. 0x7f) .|. 0x80) : sleb (n `shiftR` 7)

------------------------------------------------------------------------
-- Instructions

op :: [Word8] -> Instr
op = Op

localGet, localSet, localTee :: Int -> Instr
localGet n = Op (0x20 : uleb (fromIntegral n))
localSet n = Op (0x21 : uleb (fromIntegral n))
localTee n = Op (0x22 : uleb (fromIntegral n))

i32Const :: Integer -> Instr
i32Const n = Op (0x41 : sleb n)

f64Const :: Double -> Instr
f64Const d = Op (0x44 : [ fromIntegral (castDoubleToWord64 d `shiftR` (8 * k)) | k <- [0 .. 7 :: Int] ])

-- | Loads and stores, at the address on the stack plus an offset.
i32Load, i32Load8U, f64Load, i32Store8 :: Int -> Instr
i32Load off   = Op ([0x28, 0x02] ++ uleb (fromIntegral off))
i32Load8U off = Op ([0x2d, 0x00] ++ uleb (fromIntegral off))
f64Load off   = Op ([0x2b, 0x03] ++ uleb (fromIntegral off))
i32Store8 off = Op ([0x3a, 0x00] ++ uleb (fromIntegral off))

-- | Stores of an i32 or f64 (the address, then the value, on the stack).
i32Store, f64Store :: Int -> Instr
i32Store off = Op ([0x36, 0x02] ++ uleb (fromIntegral off))
f64Store off = Op ([0x39, 0x03] ++ uleb (fromIntegral off))

call :: Int -> Instr
call f = Op (0x10 : uleb (fromIntegral f))

br, brIf :: Int -> Instr
br d   = Op (0x0c : uleb (fromIntegral d))
brIf d = Op (0x0d : uleb (fromIntegral d))

i32Eqz, i32Eq, i32Ne, i32LtS, i32GtS, i32LeS, i32GeS, i32LtU, i32GtU, i32GeU :: Instr
i32Eqz = Op [0x45]; i32Eq = Op [0x46]; i32Ne = Op [0x47]
i32LtS = Op [0x48]; i32LtU = Op [0x49]; i32GtS = Op [0x4a]
i32GtU = Op [0x4b]; i32LeS = Op [0x4c]; i32GeS = Op [0x4e]; i32GeU = Op [0x4f]

i32Add, i32Sub, i32Mul, i32DivS, i32DivU, i32RemS, i32And, i32Or, i32Xor, i32Shl :: Instr
i32Add = Op [0x6a]; i32Sub = Op [0x6b]; i32Mul = Op [0x6c]
i32DivS = Op [0x6d]; i32DivU = Op [0x6e]; i32RemS = Op [0x6f]
i32And = Op [0x71]; i32Or = Op [0x72]; i32Xor = Op [0x73]; i32Shl = Op [0x74]

f64Eq, f64Ne, f64Lt, f64Gt, f64Le, f64Ge :: Instr
f64Eq = Op [0x61]; f64Ne = Op [0x62]; f64Lt = Op [0x63]
f64Gt = Op [0x64]; f64Le = Op [0x65]; f64Ge = Op [0x66]

f64Abs, f64Neg, f64Ceil, f64Floor, f64Trunc, f64Sqrt :: Instr
f64Abs = Op [0x99]; f64Neg = Op [0x9a]; f64Ceil = Op [0x9b]
f64Floor = Op [0x9c]; f64Trunc = Op [0x9d]; f64Sqrt = Op [0x9f]

f64Add, f64Sub, f64Mul, f64Div :: Instr
f64Add = Op [0xa0]; f64Sub = Op [0xa1]; f64Mul = Op [0xa2]; f64Div = Op [0xa3]

f64ConvertI32S, f64ConvertI32U :: Instr
f64ConvertI32S = Op [0xb7]; f64ConvertI32U = Op [0xb8]

-- | Float to integer conversions that saturate instead of trapping.
i32TruncSatF64S, i32TruncSatF64U :: Instr
i32TruncSatF64S = Op [0xfc, 0x02]; i32TruncSatF64U = Op [0xfc, 0x03]

-- | @select@: the first of two values if the i32 on top is nonzero.
select :: Instr
select = Op [0x1b]
