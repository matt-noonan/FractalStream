# FractalStream on the GPU (Phase 5)

The living plan for rendering viewers on the GPU with WebGPU: first in the
web app (where Per_n is slow), then on the desktop. It replaces the sketch
of `Backend.WGSL` in WEB_PLAN.md ("Backend.WGSL", "Precision", Phase 5).
The evidence is the spike (WEB_PLAN.md findings, 2026-11-14, "The GPU
spike"; `dev-tools/gpu-spike`): df64 on the GPU is about 13x the web
app's CPU path and 5x native LLVM on every core, at about 1e-14, once its
error-free transforms are guarded.

Status: **agreed 2026-11-14**; 5a merged (#186); 5b merged (#187); 5c
built (branch `gpu-data`).

## Decisions

| # | Decision | Notes |
|---|---|---|
| G1 | **Backends are an ordered preference list** (WEB_PLAN D20): GPU df64, then the CPU (LLVM, or Backend.Wasm on the web), then GPU f32, unless the user reorders it | Precision first, then speed. |
| G2 | **A backend declines what it can't do as well as doubles**, at three points: compiling a script, starting a render of a view, and finishing one | So nothing renders worse than doubles would; see "Declining". For now (2026-11-14) only what it can't compile: declining views (by pixel size, which varies in the disk, half-plane and sphere views) and renders (by values leaving df64's range) are deferred, to be reconsidered. |
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
windows, no render passes). As built in 5b (`Backend.WGSL.Device`), a
shader is compiled once a viewer, given its arguments once a render, and
run once a block; the device owns its buffers:

```haskell
newtype GpuDevice = GpuDevice { gdCompile :: String -> IO (Either String GpuKernel) }
newtype GpuKernel = GpuKernel { gkBind :: [Word32] -> IO GpuArgs }
newtype GpuArgs   = GpuArgs   { gaRun :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> IO (Either String ()) }
                              -- parameters (80 bytes), width, height, RGB out
```

On the web (`Backend.Wasm.Gpu`, over `fractalstream-backend-wasm/js/gpu.mjs`,
which the engine's worker makes before the engine starts), a run awaits
`mapAsync` through an asynchronous JS FFI import (the engine's thread
waits; the page doesn't). Validation and out-of-memory errors are caught
with error scopes, and a compile's become a decline, a run's a render the
CPU draws instead. With no WebGPU (or no adapter), both GPU backends
decline every viewer, saying why; the page is told too (the `gpu`
message), for Preferences and the viewer's menu.

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

**Compiling** (built in 5b). The GPU backends decline a script with:

- text, or autocolor ("not on the GPU yet: text"; 5d takes autocolor on;
  text stays on the CPU);
- a literal outside f32's range (about 1e-38 to 3e38, nonzero), such as
  Per_n's `10^(-300)` thresholds;
- a shader the GPU won't compile (its messages say why);
- every script, with no GPU ("No WebGPU in this browser").

**Viewing and rendering** (deferred, G2). As first planned: decline a view
whose pixel size is below about 2^-40 of the largest coordinate in it (f32:
2^-16), and a render in which a value left df64's range, or a local list
outgrew its space. The pixel-size rule doesn't fit the views whose pixel
size varies (the disk, the half-plane, the sphere), so it waits for a
better rule; until then, a deep zoom in f32 draws what f32 can (the user
chose it), and df64 past its depth draws what df64 can. The same goes for
values past f32's range (about 3e38, where doubles go to 1e308).

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
- **Math** (all of it, in 5b): `+ - * /`, `sqrt`, `abs`, `floor`,
  `round`, `mod` and comparisons; df64 `exp` (range reduction by ln 2, a
  Taylor series of r/512, squared 9 times), `log` (two Newton steps on
  `exp`, from f32's), `sin` and `cos` (reduction by pi/2 in three parts,
  then Taylor), `atan2` (two Newton steps on `sin` and `cos`), the
  hyperbolic functions (a series near 0), and from those the inverses,
  real and complex powers, and the complex functions. In f32, WGSL's own.
  Colors are f32: a color is 8 bits a channel anyway.
- **The browsers' compilers use fast math** (on Metal at least), so the
  runtime can't trust ordinary float comparisons with NaN or infinity, nor
  division near f32's limits (it's a multiplication by the reciprocal,
  which flushes to 0 past about 8e37). What must handle them asks through
  the bits (`fisnan`, `fisinf`, and NaN-safe `rlt`/`req`), and `|z|` is
  scaled by a power of 2 (`ldexp`), not divided. (Found by the internal
  rays project, whose `w` runs at twice `z`'s speed and passes 3e38: `|z
  - w|` came out 0, and its orbits "vanished".)

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
- **Lists** (built in 5c) are word addresses: a block is a length, then
  the elements (a truth value, integer, color or list a word; a real two,
  as the arguments hold them; a complex number four). A configuration's
  lists are in the arguments, after the slots (the address's top bit
  set); a kernel's are in a private arena of 4 KB a pixel, afresh for
  each subsample, as the CPU's (which has 1 MB). Running out is as on
  the CPU (decided 2026-11-14): magenta pixels, and the viewer says so,
  adding that the GPU's space is smaller ("Draw with" CPU). A loop whose
  body sets no variable to a list gives back its iteration's arena (so
  `for each k in range(1, 10)` in a loop of thousands doesn't run out).
- **Tables** (built in 5c) go to the GPU as their bytes, laid out as on
  the CPU, and the shader decodes each f64 into df64 itself (`rf64`): no
  conversion. A render's tables are one buffer (binding 3), uploaded once
  for each set of versions (the last two sets kept); their descriptors
  are in the arguments. Tables past the GPU's largest binding are drawn by
  the CPU, saying why.
- **Issues**: each pixel's (out of room, bad index) ride in its output
  word's top byte, or'ed by the device into the block's `ViewerIssues`.
  The web viewer now shows a finished render's issues (any backend's),
  as the desktop's status line does.

## Rendering

- The GPU backend has no coarse passes (`bfCoarse`: it starts at full
  resolution, then smooths), and draws in pieces of 4 x 4 of the usual
  (256 x 256 on the web), so that fewer dispatches and read-backs start.
- Blocks are read back into the buffer they were asked for, so the rest of
  the pipeline (patches to the page, the wx tiles, captures, movies) is
  unchanged. Drawing straight to the page's canvas, without a read-back,
  is a later optimization.
- A lost device makes the GPU backends decline until it's back.

## Preferences

- A "Rendering" list in Preferences (desktop and web), reorderable, with
  each backend's availability ("No WebGPU in this browser"); saved as
  `rendering: [gpu-df64, cpu, gpu-f32]` (settings.yaml on the desktop,
  local storage on the web). No saved list: that default, the backends the
  platform lacks skipped.
- The viewer's menu says which backend drew it ("GPU, double-float",
  "CPU") and, under it, why earlier ones declined.

## Testing

- **Golden comparisons with a tolerance** (WEB_PLAN D4), by hand:
  `dev-tools/web/check-gpu.mjs [--images DIR] [section | file.fs ...]`
  opens projects in Chromium with WebGPU and draws each viewer with each
  backend ("Draw with"), saying which drew it (or why it declined), how
  long it took, and how much of each GPU picture matches the CPU's (no
  channel off by more than 8), next to the yardstick: how much of the
  CPU's picture matches itself with the view nudged by 1e-15.
- **The df64 harness** (the spike's primitive check, then the math
  functions): still to do, as a check of its own.
- **Without a GPU** (CI): `fractalstream-backend-wgsl`'s spec, through a
  device that keeps the shaders and fails every block: what's compiled and
  what's declined, the arguments' layout, and a failed render drawn by the
  CPU instead. `check-tools.mjs backends` runs with WebGPU or without
  (`FS_WEBGPU=1`: Chromium, the full browser).
- CI has no GPU; the GPU checks are run by hand (G10).

## Phases

- [x] **5a. Plumbing, no GPU yet.** The decline-aware compiler, the chain,
      the preference list and the viewer menu's report, with only the CPU
      backends: no change in what's drawn. (Branch `gpu-plumbing`; see
      CLAUDE_NOTES.md, "Backends as a chain". A backend's preferred block
      size is left for 5b, which has a backend that wants one.)
- [x] **5b. The web device and the first kernels.** `GpuDevice` over
      `navigator.gpu`; WGSL for reals, complex numbers, integers, booleans,
      colors and control flow, and all the math (df64's transcendental
      functions came forward from 5c: the target needed them); df64 and
      f32; declining what it can't compile. Target met (2026-11-14, Apple
      M5, Chromium): every viewer of "Getting started" and "Quadratic
      family" draws in df64, matching the CPU's picture as closely as the
      CPU's matches itself nudged by 1e-15 (to within 0.2%); f32 within a
      few percent where its precision shows. A deep zoom (1e-10 a pixel,
      5000 iterations, smoothed): CPU 1131 ms, df64 346 ms, f32 156 ms (and
      wrong: 5% matching, as expected).
- [x] **5c. Data.** Configuration lists and tables in storage buffers;
      local lists. Target met (2026-11-14): Per_1 to Per_6 draw on the GPU
      in df64 as closely to the CPU's pictures as the CPU's to themselves
      nudged by 1e-15 (Per_n's thresholds needed no rewriting: the atlas
      uses 1e-30); df64 1.4 to 3.3 times as fast as the CPU, f32 4 to 15.
      Every library viewer but autocolor's draws on the GPU (the forced
      pendulum's parameter plane: CPU 9.3 s, df64 1.7 s, f32 0.1 s).
      df64's transcendental functions are slow (the pendulum's dynamical
      plane, a `sin` a step, is no faster than the CPU): a follow-up.
- [ ] **5d. Autocolor** (snapshot buffer, misses appended, re-rendered).
- [ ] **5e. The desktop**: a `GpuDevice` over webgpu.h, and
      GPU f64 where the driver has it (Vulkan, DirectX 12; not Metal).

## Settled questions (2026-11-14)

- Desktop: yes (G4), the lighter of wgpu-native and Dawn.
- Per-viewer override: yes (G8).
- Per_n: on the GPU in df64 and f32 (G9).
- CI: checks by hand (G10).
