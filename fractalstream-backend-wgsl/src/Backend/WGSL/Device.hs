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
  , paramsSize
  ) where

import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)

-- | A GPU, ready to use.
newtype GpuDevice = GpuDevice
  { gdCompile :: String -> IO (Either String GpuKernel)
    -- ^ a WGSL module (see "Backend.WGSL.Code": its bindings, and its
    --   entry point, @main@), or why the GPU wouldn't compile it
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
  { gaRun :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> IO (Either String Word32)
    -- ^ run it over a block of this width and height, with these
    --   parameters ('paramsSize' bytes: binding 0), writing its pixels
    --   into the buffer, as RGB rows (binding 2 holds a pixel a u32,
    --   @0xIIBBGGRR@, for the device to unpack), and giving its pixels'
    --   issues (II, or'ed); or say why it couldn't (the device was lost,
    --   say, or the tables didn't fit)
  }

-- | How many bytes a block's parameters take (see "Backend.WGSL.Code").
paramsSize :: Int
paramsSize = 80
