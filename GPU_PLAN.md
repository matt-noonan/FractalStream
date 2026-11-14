# FractalStream on the GPU (Phase 5)

The living plan for rendering viewers on the GPU with WebGPU: first in the
web app (where Per_n is slow), then on the desktop. It replaces the sketch
of `Backend.WGSL` in WEB_PLAN.md ("Backend.WGSL", "Precision", Phase 5).
The evidence is the spike (WEB_PLAN.md findings, 2026-11-14, "The GPU
spike"; `dev-tools/gpu-spike`): df64 on the GPU is about 13x the web
app's CPU path and 5x native LLVM on every core, at about 1e-14, once its
error-free transforms are guarded.

Status: **agreed 2026-11-14**; 5a in progress (branch `gpu-plumbing`).

## Decisions

| # | Decision | Notes |
|---|---|---|
| G1 | **Backends are an ordered preference list** (WEB_PLAN D20): GPU df64, then the CPU (LLVM, or Backend.Wasm on the web), then GPU f32, unless the user reorders it | Precision first, then speed. |
| G2 | **A backend declines what it can't do as well as doubles**, at three points: compiling a script, starting a render of a view, and finishing one | So nothing renders worse than doubles would; see "Declining". |
| G3 | **One code generator for desktop and web**: `Code` to WGSL, in a package that builds with both GHCs | Only a small device layer differs per platform. |
| G4 | **Web first, then the desktop** | The desktop GPU is wanted (5e), on whichever of wgpu-native and Dawn is the lighter dependency (likely wgpu-native's prebuilt binaries); the df64 harness decides whether its compiler keeps the guard. |
| G5 | **The block API stays** (`ViewerFunction`): a GPU backend renders the blocks it's given, and says which size it prefers | No separate frame-level API (WEB_PLAN's R3) unless the block API proves too slow. |
| G6 | **Tools stay on the CPU** | A handler runs once an event; no gain on the GPU. |
| G7 | **The df64 guard is part of the backend's contract**, tested in every browser we support | Without it both browsers' compilers silently give f32 accuracy (the spike). |
| G8 | **The app-wide list is the default; each viewer can override it** | From the viewer's menu; the override is per viewer window, not saved in the project. |
| G9 | **Per_n runs on the GPU in df64 and in f32** | Its `10^(-300)` thresholds are rewritten (the atlas scripts are ours); f32 is reached by a viewer's override, since the default order puts it after the CPU. |
| G10 | **GPU checks are run by hand for now** | Before a release, in Chromium and WebKit: the df64 harness and the golden comparisons. No GPU in CI. |

## Architecture

```
fractalstream-core            Backend record; the chain (R4); preferences
fractalstream-backend-wgsl    Code -> WGSL (f32 / df64); the df64 runtime
  (pure Haskell, both GHCs)   (WGSL text); argument and table layouts;
                              the GpuDevice interface; the GPU backend
                              built on any GpuDevice
fractalstream-backend-wasm    + a GpuDevice over navigator.gpu (JS FFI),
                              in the engine's worker
(later) desktop               + a GpuDevice over webgpu.h (FFI to Dawn or
                              wgpu-native), compute only, read back to wx
```

### The device layer

The only platform-specific part, deliberately small (compute only: no
windows, no render passes):

```haskell
data GpuDevice = GpuDevice
  { gdLimits   :: GpuLimits                  -- buffer sizes, workgroup sizes
  , gdCompile  :: Text -> IO (Either String GpuPipeline)   -- WGSL, one entry point
  , gdBuffer   :: GpuUsage -> Int -> IO GpuBuffer
  , gdWrite    :: GpuBuffer -> Int -> Ptr Word8 -> Int -> IO ()
  , gdDispatch :: GpuPipeline -> [GpuBuffer] -> (Int, Int) -> IO ()
  , gdRead     :: GpuBuffer -> Int -> Ptr Word8 -> Int -> IO ()  -- waits for the GPU
  , gdLost     :: IO Bool                    -- the device was lost (fall back)
  }
```

On the web, `gdRead` awaits `mapAsync` through an asynchronous JS FFI
import (the engine's thread waits; the page doesn't). Validation errors
are caught with error scopes (the spike's `checked`), and become a decline.

### The chain (WEB_PLAN's R4; built in 5a)

`Backend` lists the platform's compilers (`bViewerCompilers`), each of
which can decline:

```haskell
newtype Decline = Decline String   -- why, for the viewer's menu

newtype ViewerCompiler = ViewerCompiler
  { compileViewer :: ... -> IO (Either Decline (CompiledViewer env)) }

newtype CompiledViewer env = CompiledViewer (Context HaskellValue env -> IO BlockFunction)

data BlockFunction = BlockFunction
  { bfView  :: ViewGeometry -> Maybe Decline                 -- before a render
  , bfBlock :: ViewerArgs -> IO (Either Decline ViewerIssues) }  -- Left: gave up
```

Each viewer is compiled by a backend the first time it's tried (the first
in the app's order that accepts it, at load); each render goes to the
first, in the viewer's order (the app's, with the viewer's "Draw with"
choice first), that accepts the script and the view; a render one of them
gives up on is done again, from scratch, by the next. The viewer's menu
shows which backend drew the last picture, and why the ones before it
declined; when none will, the viewer says why, as it would an error.

## Declining

**Compiling.** The GPU backend declines a script with:

- text (as WEB_PLAN planned), or autocolor (until 5c);
- a literal outside df64's range (about 1e-38 to 1e38, nonzero), such as
  Per_n's `10^(-300)` thresholds (constant-folded first, so `10^(-300)` is
  seen as the number it is);
- tables or lists of configuration values larger than the device's
  storage limits;
- anything not yet generated (each phase widens this).

**Viewing.** df64 has about 48 bits; the GPU backend declines a view whose
pixel size is less than about 2^-40 times the largest coordinate in it
(about 1e-12 relative: a pixel then still spans about 256 df64 ulps). f32
declines below about 2^-16 (1e-5). Hyperbolic and sphere views are judged
in the coordinates the code computes in.

**Rendering.** Each invocation sets an issue bit if a value left df64's
range (infinite or NaN where the script didn't divide by zero, or a
magnitude past 1e30), or a local list outgrew its fixed space; any such
bit makes the render go to the next backend, which draws it from scratch.
(The CPU's own issues, a bad index, stay magenta pixels, as now.)

## Code generation

An `indexedFold` over `ValueF`/`CodeF`, as `Backend.LLVM.Value`/`Code`
emit IR, producing one WGSL compute shader a viewer (one per precision).

- **Types:** `R` is `vec2f` (df64) or `f32`; `C` is two `R`s (a struct);
  `Z` is `i32` (as LLVM); `Boolean` is `bool`; `Color` is a packed `u32`;
  tuples are structs.
- **Code:** the viewer's code is already expanded (functions inlined,
  procedures run): `DoWhile`, `IfThenElse`, `ITE`, loops with limits and
  `stuck` map to structured WGSL. The disk, sphere and group wrappers are
  ordinary generated code, so they come for free.
- **Pixels:** the kernel computes its pixel's point as the CPU kernels do
  (`vaPoint` plus index times step, in df64), loops over subsamples
  (smoothing), and writes RGB8 into the block's output buffer.
- **The hoisted prefix** runs on the CPU (`bRunOnce`), as now; its results
  are bound like configuration values.
- **Math:** `+ - * /`, `sqrt`, `abs`, `floor`, `round`, `mod` and
  comparisons first; then df64 `exp`, `log`, `sin`, `cos`, `tan`,
  `atan2`, `sinh`, `cosh`, `tanh`, their inverses and complex `pow` (range
  reduction plus series or Newton steps, tested against doubles to about
  1e-13). Colors (`rgb`, `hsv`, `blend`, the color wheels) are f32: a color
  is 8 bits a channel anyway.

## Data

- **Configuration values and hidden values** (a group's sides, say) go into
  one storage buffer when the block function is bound, laid out as
  Backend.Wasm lays out its arguments, with WGSL's alignment; reals as
  df64 pairs (8 bytes, the same as f64) or f32.
- **Tables** go into storage buffers, converted from f64 to df64 pairs or
  f32 once a table version, and kept on the GPU while the version lives
  (as D19 keeps them on the workers). Per_n's are large: the backend asks
  the adapter for its largest `maxStorageBufferBindingSize`, and declines
  past it.
- **Lists computed in the kernel** (`append` in a loop) live in a fixed
  amount of private memory per invocation, sized from the code where it
  can be bounded, else a default (1 KB); overflowing it is a render
  decline (above), not a magenta pixel.

## Rendering

- The GPU backend prefers large blocks (up to 256 x 256: big enough that
  dispatch costs vanish, small enough to stay under GPU watchdogs and to
  be stopped between blocks) and needs no coarse passes when it's fast:
  `renderProgressivelyInto` takes the backend's preferences.
- Blocks are read back into the buffer they were asked for, so the rest of
  the pipeline (patches to the page, the wx tiles, captures, movies) is
  unchanged. Drawing straight to the page's canvas, without a read-back,
  is a later optimization.
- A lost device makes the GPU backends decline until it's back.

## Preferences

- A "Rendering" list in Preferences (desktop and web), reorderable, with
  each backend's availability ("no WebGPU in this browser"); saved as
  `rendering: [gpu-df64, cpu, gpu-f32]` (settings.yaml on the desktop,
  local storage on the web). No saved list: that default, the backends the
  platform lacks skipped.
- The viewer's menu says which backend drew it ("GPU, double-float",
  "CPU") and, under it, why earlier ones declined.

## Testing

- **The df64 harness** (the spike's primitive check, then the math
  functions) as an automated check in every browser we test, failing if
  any operation is worse than its bound.
- **Golden comparisons with a tolerance** (WEB_PLAN D4): every library
  project rendered by the GPU and the CPU backends, compared pixel by
  pixel; the share of pixels within a color tolerance must be at least
  what doubles give against themselves under a 1e-15 nudge (the spike's
  measure of a picture's own sensitivity).
- **Declining:** scripts and views on each side of each rule.
- CI has no GPU; the checks are run by hand (G10). CI covers the code
  generator's text and the declining rules, which need no GPU.

## Phases

- [x] **5a. Plumbing, no GPU yet.** The decline-aware compiler, the chain,
      the preference list and the viewer menu's report, with only the CPU
      backends: no change in what's drawn. (Branch `gpu-plumbing`; see
      CLAUDE_NOTES.md, "Backends as a chain". A backend's preferred block
      size is left for 5b, which has a backend that wants one.)
- [ ] **5b. The web device and the first kernels.** `GpuDevice` over
      `navigator.gpu`; WGSL for reals, complex numbers, integers, booleans,
      colors, control flow and the first math; df64 and f32; view
      declines. Target: the library's "Getting started" and "Quadratic
      family" projects on the GPU, within tolerance.
- [ ] **5c. Data and the rest of the math.** Configuration lists and
      tables in storage buffers; local lists; groups and the hyperbolic
      and sphere wrappers; df64 transcendental functions; render declines.
      Target: Per_n (after rewriting its `10^(-300)` thresholds, or with
      them declining to the CPU).
- [ ] **5d. Autocolor** (snapshot buffer, misses appended, re-rendered).
- [ ] **5e. The desktop**: a `GpuDevice` over webgpu.h, and
      GPU f64 where the driver has it (Vulkan, DirectX 12; not Metal).

## Settled questions (2026-11-14)

- Desktop: yes (G4), the lighter of wgpu-native and Dawn.
- Per-viewer override: yes (G8).
- Per_n: on the GPU in df64 and f32 (G9).
- CI: checks by hand (G10).
