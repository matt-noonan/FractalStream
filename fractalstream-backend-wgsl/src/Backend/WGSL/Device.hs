-- | What the GPU backend needs of a GPU (GPU_PLAN.md, "The device
-- layer"): the only part that differs from platform to platform (the
-- web's WebGPU, through the JavaScript FFI; later the desktop's, through
-- webgpu.h). Compute only: a shader is compiled once a viewer, given its
-- arguments once a render, and run once a block.
module Backend.WGSL.Device
  ( GpuDevice(..)
  , GpuKernel(..)
  , GpuArgs(..)
  , GpuTable(..)
  , GpuAutocolor(..)
  , paramsSize
  , missCapacity
  , missesBytes
  ) where

import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)

-- | A GPU, ready to use.
data GpuDevice = GpuDevice
  { gdCompile :: String -> IO (Either String GpuKernel)
    -- ^ a WGSL module (see "Backend.WGSL.Code": its bindings, and its
    --   entry point, @main@), or why the GPU wouldn't compile it
  , gdF64 :: Bool
    -- ^ whether its shaders can have @f64@s (wgpu's native-only
    --   @SHADER_F64@)
  }

-- | A compiled shader.
newtype GpuKernel = GpuKernel
  { gkBind :: [Word32] -> [GpuTable] -> IO GpuArgs
    -- ^ the arguments (binding 1), and the tables, one after another
    --   (binding 3), on the GPU until they're dropped (a table's
    --   elements are kept by its version, and copied only once)
  }

-- | A table's elements, as the CPU lays them out ("Language.Table").
data GpuTable = GpuTable
  { gtVersion :: Int              -- ^ which contents: a version never changes
  , gtOffset  :: Int              -- ^ where they go in binding 3, in bytes
  , gtBytes   :: ForeignPtr Word8
  , gtSize    :: Int              -- ^ in bytes
  }

-- | A shader with its arguments.
newtype GpuArgs = GpuArgs
  { gaRun :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> Maybe GpuAutocolor -> IO (Either String Word32)
    -- ^ run it over a block of this width and height, with these
    --   parameters ('paramsSize' bytes: binding 0), writing its pixels
    --   into the buffer, as RGB rows (binding 2 holds a pixel a u32,
    --   @0xIIBBGGRR@, for the device to unpack), and giving its pixels'
    --   issues (II, or'ed); or say why it couldn't (the device was lost,
    --   say, or the tables didn't fit). A kernel that looks autocolors up
    --   gets the autocolor groups (bindings 4 and 5); one that doesn't,
    --   'Nothing' (and empty bindings).
  }

-- | The autocolor groups, for a block whose kernel looks colors up in
-- them ("Backend.WGSL.Runtime"'s @ac_lookup@): the palette (binding 4,
-- as @Actor.Autocolor.autocolorPalette@ lays it out), and where the
-- device copies the points the block noted to add (binding 5,
-- 'missesBytes' of it: a count, which can be more than 'missCapacity' if
-- the rest didn't fit, a word of padding, then each point's group, mode,
-- and x and y, each as two floats' bits, hi and lo).
data GpuAutocolor = GpuAutocolor
  { gacPalette :: Ptr Word32
  , gacWords   :: Int
  , gacMisses  :: Ptr Word8
  }

-- | How many points a block can note to add (as the wasm backend's).
missCapacity :: Int
missCapacity = 256

-- | The misses' size, in bytes: the count and padding, then 24 bytes a point.
missesBytes :: Int
missesBytes = 8 + 24 * missCapacity

-- | How many bytes a block's parameters take (see "Backend.WGSL.Code").
paramsSize :: Int
paramsSize = 80
