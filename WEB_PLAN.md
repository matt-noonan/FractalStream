# FractalStream on the web

A living plan for a browser version of FractalStream. The core and the
backends stay the same Haskell codebase, compiled with GHC's WebAssembly
backend; only the UI is TypeScript. Update this file as work proceeds:
tick off steps, record findings from the spikes, and move answered
questions into **Decisions**.

## Decisions

| # | Decision | Notes |
|---|---|---|
| D1 | Compile Haskell with **GHC's wasm backend**, not the JS backend | Better numeric performance, cleaner C support (wasi-sdk clang), JSFFI, TH via external interpreter. |
| D2 | **WebGPU only**, no WebGL2 fallback | WebGL has no compute shaders; all major browsers now ship WebGPU. |
| D3 | Precision is a **user toggle**, not chosen automatically | Single precision is rarely useful for the systems people study, so double precision (df64 on the GPU or f64 on the CPU) is the main path; see "Precision". |
| D4 | GPU output must be **visually identical within tolerance**, not pixel-identical | Golden comparisons for the GPU path use a tolerance. |
| D5 | The **script editor is deferred** but in scope | Phase 6. |
| D6 | **Static site** first; a server only if that hits a roadblock | |
| D7 | User projects are **local files** | File System Access API where available, upload/download elsewhere, IndexedDB for recents and settings. |
| D8 | **Shared refactors** of core and the wx app are welcome | |
| D9 | UI is **TypeScript**, using Solid or plain DOM | Mostly forms built from `Layout`, plus canvases. |
| D10 | **No ordering guarantees** for autocolor | GPU render order may assign hues differently; saved or locked groups stay stable. |
| D11 | **cabal** for the wasm build; migrating everything off stack is open | See "Build". |
| D12 | The precision toggle is an **app-level preference** | Stored per browser with the other preferences (IndexedDB), not in project files. |

## Architecture

```
┌──────────────── Main thread (TypeScript UI) ────────────────┐
│  Layout renderer (config panels from Layout JSON)           │
│  Viewer windows: <canvas>, pointer/scroll → events          │
│  Overlay painting (Canvas2D, from DrawCommand lists)        │
│  Script editor, later (CodeMirror 6; highlighting from HS)  │
└───────────────▲──────────────────────────┬──────────────────┘
                │ postMessage (JSON RPC)   │ OffscreenCanvas transfer
┌───────────────┴──────────────────────────▼──────────────────┐
│  Engine worker: fractalstream-core built with GHC-wasm      │
│   • parse/typecheck/build Ensemble, Dynamic graph           │
│   • fractalstream-ui-web: a UI record that is RPC to the UI │
│   • Backend.Wasm: Code → wasm module bytes (f64, CPU)       │
│   • Backend.WGSL: Code → WGSL compute shader (df64 / f32)   │
│   • Tools: pure interpreter at first, Backend.Wasm later    │
└───────────────────────────────┬─────────────────────────────┘
                                │ SharedArrayBuffer tiles
                    ┌───────────▼───────────┐
                    │ N kernel workers      │ (Backend.Wasm kernels)
                    └───────────────────────┘
```

- **The engine runs in a dedicated worker.** The wasm RTS is
  single-threaded (green threads work; `getNumCapabilities` is 1), so
  project builds and interpreted tools must stay off the main thread.
  WebGPU and OffscreenCanvas both work in workers, so the engine owns the
  GPU device and draws straight to the visible canvas, with no readback.
- **RPC protocol (JSON):**
  - Engine to UI: make layout, make viewer, set field text, draw commands
    for layer *n*, viewer issues for the status bar, errors.
  - UI to engine: field edited, pointer/scroll/key event, view changed,
    open project (bytes), save project, choose precision.
  - `Layout` and `DrawCommand` are already plain data.
- **Static hosting needs cross-origin isolation.** `SharedArrayBuffer`
  needs COOP/COEP headers. GitHub Pages can't set headers, so either host
  somewhere that can (Netlify, Cloudflare Pages) or use the
  `coi-serviceworker` shim. Check this in Phase 0.

## Backends

The `Backend` record and `ViewerCompiler` are already the right seam. Both
new backends sit beside `Backend.LLVM` and `Backend.Pure`, and the
desktop app keeps its own backends.

### Backend.Wasm (exact CPU path)

The browser analogue of the LLVM JIT, and the reference for correctness:

- Haskell emits a wasm binary module from `Code (ViewerEnv env)`, using a
  small handwritten encoder (no LLVM).
- The engine instantiates the module in N kernel workers that share one
  `WebAssembly.Memory` (a `SharedArrayBuffer`) for tiles, configuration
  data, tables and per-worker arenas.
- Real f64, with the same arena, overflow and bad-index semantics as
  `Backend.LLVM`, so output should match desktop closely.
- Transcendental functions (`sin`, `exp`, `log`, ...) aren't wasm
  instructions. Import them from JS `Math` (simple; results may differ
  slightly between browsers), or compile a libm to wasm and link it into
  the kernels (deterministic). Decide in Phase 3.
- Autocolor: the kernels call a lookup over the published snapshot in
  shared memory, as `fs_ac_lookup` does. A miss is queued for the engine
  rather than calling back into Haskell, since the kernel runs in another
  worker. The engine inserts and publishes, and the existing
  `pointAdded` redraw finishes the job.
- Later, tools can run through it too, replacing the interpreter.

### Backend.WGSL (fast GPU path)

An `indexedFold` over `ValueF`/`CodeF` that emits WGSL text, as
`Backend.LLVM.Value`/`Code` emit IR.

- **Scalars:** `Z` becomes `i32`, which matches `Backend.LLVM.Operand`.
  `R` is a df64 pair (or `f32` in single mode), `C` two of those, `Color`
  a packed `u32`, and `Boolean` a `bool`.
- **Control flow:** `DoWhile`, `IfThenElse` and `ITE` become structured
  WGSL loops and branches (no uniformity requirement in compute shaders).
- **Hoisted prefix:** it runs once on the CPU through `bRunOnce`, and its
  results go into a uniform buffer, so the shader only sees the per-pixel
  part.
- **Configuration values** go in uniforms. Lists and tables go in
  read-only storage buffers, reusing `Language.Table`'s byte layout after
  checking it against WGSL's alignment rules.
- **Lists and text inside a kernel** use a fixed-size scratch region per
  invocation, with the same `ViewerIssues` bits (magenta pixels). Text in
  viewer kernels is refused at compile time, so the viewer falls back to
  the CPU.
- **Autocolor:** the snapshot is a storage buffer, and misses go to an
  append buffer with an atomic counter. The CPU inserts the misses,
  publishes and re-renders.
- **Unsupported code** makes the backend decline (see refactor R4), and
  that viewer runs on `Backend.Wasm`.

### Precision

The user picks the mode per viewer or app-wide (D3). How much the GPU path
is worth depends almost entirely on df64.

| Mode | Backend | Usable to pixel size of about | Status |
|---|---|---|---|
| f64 | Backend.Wasm (CPU) | as on desktop | the **default** and reference |
| df64 (two f32s) | Backend.WGSL | about 1e-13 | must pass the accuracy harness first |
| f32 | Backend.WGSL | about 1e-6 | cheap once df64 exists |

df64 risks:

- WGSL doesn't promise to preserve the rounding that two-float arithmetic
  needs; compilers may fuse operations into FMA or reassociate them.
  Known countermeasures exist (opaque values, explicit `fma`), but they
  must be verified on each browser's compiler: Dawn (Chrome), Metal
  (Safari) and wgpu/naga (Firefox).
- `exp`, `log`, `sin`, `cos`, `atan2` and complex `pow` need df64
  implementations of our own.
- Expect roughly 10–20× the cost of f32. That is still likely much faster
  than the CPU on a decent GPU, but Phase 0 must measure it.

## Shared refactors

| # | Refactor | Why |
|---|---|---|
| R1 | A **platform record** for effects: project files, `settings.yaml`, saved tables, the user projects folder, clock | Desktop uses `directory`/`filepath`; web uses File System Access, IndexedDB and downloads. |
| R2 | **Move view logic out of `UI.ProjectViewer`** into core: history, pan/zoom, reprojected seeds, trackball, coalescing of parameter changes | `UI.ViewModel` and `UI.Tile` already import nothing from wx; the web UI shouldn't re-derive behaviour that's already been debugged. |
| R3 | A **frame-level render API** next to `ViewerFunction`'s block API | GPU wants "render this frame (pass *k*) into this texture, async". CPU backends keep blocks. |
| R4 | **Backends can decline, and a dispatcher chains them** | e.g. WGSL first, else Wasm; also lets the precision toggle pick. |
| R5 | ✅ **Git metadata on wasm** | Done: `dev-tools/wasm/build.sh` writes `fractalstream-gitinfo`'s one module (gitignored) from git, and the core depends on it `if arch(wasm32)` (else on `githash`). A file read by TH instead would need an `extra-source-files` entry, which can't be conditional, so every desktop build would warn that it's missing. |
| R6 | **C pieces:** optionally simplify `autocolor_cache.c` for one thread | It compiles unchanged for wasm (0a); the epoch scheme is just unnecessary there. `find_jemalloc.c` is gone (#149). |

## Build

Build the wasm targets with `dev-tools/wasm/build.sh` (it runs
`wasm32-wasi-cabal build --project-file=cabal.project.wasm`, after writing
the git metadata; see R5).

- Use **`ghc-wasm-meta`** with the GHC 9.10/9.12 line, through a root
  `cabal.project` (plus `cabal.project.wasm` for the cross build).
- **Open:** whether to migrate the desktop build from stack to cabal.
  - In favour: one build system and one set of version bounds, and the
    wasm build forces cabal anyway.
  - Against: the macOS/Windows/Ubuntu stack files, the wxc patches in
    `patches/`, and CI all need porting.
  - Suggestion: get the wasm build working first, then migrate desktop as
    a separate change once it's clear how much the two builds share.

## Phases

### Phase 0: Spikes (each can reshape the plan)

- [x] **0a.** Build `fractalstream-core` and `fractalstream-backend-pure`
  with GHC-wasm, and list everything that breaks: C dependencies (`yaml`,
  `zlib`), TH (R5), `async`, `monad-par`, `network`. (Branch
  `spike/wasm-core`; see the findings log.)
- [ ] **0b.** Run one `fsrun` scenario under Node with the wasm build and
  compare its PNG with the desktop output.
- [ ] **0c. df64 accuracy harness.** Compare `+ − × ÷ sqrt exp log sin
  cos atan2` against f64 on Chrome, Safari and Firefox. **This decides how
  much of Backend.WGSL is worth building.**
- [ ] **0d.** A hand-written df64 Mandelbrot and a Newton's-method kernel
  in WGSL, drawn from a worker to an OffscreenCanvas; time them against
  `fsrun` with LLVM.
- [ ] **0e.** A hand-written wasm kernel run in N workers over a
  `SharedArrayBuffer`; check COOP/COEP on the intended host.
- [ ] **0f.** Measure the engine's binary size and startup time, before
  and after `wasm-opt` (Earley, megaparsec, aeson, yaml, Rasterific and
  the embedded library are all in it).

### Phase 1: Headless engine on the web

- [ ] R5 and R6; root `cabal.project`.
- [ ] The scenario runner under Node (wasm, pure backend).
- [ ] The `end-to-end` suite under Node, giving a parity harness before
  any UI exists.

### Phase 2: Minimal web app

- [ ] R1 (platform record), with browser implementations.
- [ ] Engine worker and RPC; `fractalstream-ui-web` implements `UI`.
- [ ] TypeScript UI renders `Layout`s as config panels.
- [ ] R2 (view logic in core); one viewer with pan, zoom and resize.
- [ ] The built-in library; open and save local `.fs` files.
- [ ] Deployed as a static site (`site/`, or a separate app directory).

### Phase 3: Backend.Wasm (default, f64)

- [ ] wasm binary encoder; `Value`/`Code` codegen with arithmetic and
  control flow.
- [ ] Kernel worker pool; tiles and progressive passes (R3).
- [ ] Lists, text and tables in a per-worker arena; issue flags.
- [ ] Autocolor snapshot lookup and the miss queue.
- [ ] Decide on libm (JS `Math` or compiled libm).
- [ ] Golden comparison against LLVM output across the library projects
  (tolerance per D4).

### Phase 4: Backend.WGSL (df64, then f32)

Only if 0c/0d come out well.

- [ ] R4 (declining and chaining backends); the precision toggle in the UI.
- [ ] Arithmetic and control-flow subset, df64 runtime library.
- [ ] Uniforms for hoisted values; storage buffers for tables and lists;
  scratch arena; issue flags.
- [ ] Autocolor append buffer.
- [ ] f32 mode (the same codegen with a different scalar module).
- [ ] Golden comparisons through headless Chrome, or Node with Dawn.

### Phase 5: Tools and overlays

- [ ] Tool events (click, drag, motion, buttons, timers) through the pure
  interpreter; Backend.Wasm for tools later.
- [ ] Overlay layer: Canvas2D painting of `DrawCommand`s, matching
  `Actor.Viewer.Overlay`.
- [ ] Preferences (IndexedDB).

### Phase 6: Script editor

- [ ] CodeMirror 6, with highlighting, symbol entry, normalizing and
  error ranges from `Language.Editor.*` over RPC.
- [ ] Rebuild the project when the script is edited, as the wx app does.

### Phase 7: Polish

- [ ] Sphere view (trackball).
- [ ] Autocolor editor.
- [ ] Movies (WebCodecs).
- [ ] Saved tables.
- [ ] Open a project from a URL; shareable links.

## Open questions

- **stack → cabal for desktop** (see "Build"): decide after Phase 1.
- **libm for Backend.Wasm:** decide in Phase 3.
- **Hosting:** which static host can set COOP/COEP (or do we use the
  service-worker shim)? Settle in 0e.

## Findings log

Record spike results and surprises here, newest first.

### 2026-11-11: spike 0b (running under wasmtime), in progress

- **Parity:** the Julia set project's three scenarios (`simple`,
  `circle`, `trace`, the last with a tool click and overlay drawing) give
  PNGs **byte-identical** to the desktop LLVM and pure builds.
- **Speed (the big finding):** one 600×600 capture takes 0.1 s with LLVM,
  263 s with the desktop pure interpreter on one thread, and 628 s under
  wasmtime (about 2.4× the native interpreter). The interpreter can't be
  even a stopgap renderer, so Backend.Wasm (Phase 3) has to come before
  any usable web app, or the interpreter needs a much faster design (for
  example compiling `Code` to closures once, rather than a state monad
  over freshly allocated `IORef`s for every pixel).
- **Tests:** all four suites build for wasm. That needs a *native*
  `hspec-discover` on `PATH` (cabal builds the tool for wasm otherwise),
  and cabal caches the search path when it first configures a suite, so
  suites configured without it must be reconfigured. Still to go into
  `dev-tools/wasm/build.sh`.
  - Headless `spec`: 43 examples, 0 failures.
  - Core and library `spec`: crash with `ffi_alloc_prep_closure failed`
    in `createAdjustor`. `foreign import ccall "wrapper"` (making a C
    function pointer from a Haskell function) doesn't work on wasm. The
    only one outside the LLVM backend and wx is `Actor.Autocolor`'s
    callback for the C cache's "add a point". Fix to discuss: on wasm,
    drop the callback (misses queued and inserted from Haskell, much as
    planned for Backend.Wasm anyway) or skip the C cache entirely.
  - End-to-end suite: results not recorded yet.

### 2026-11-11: spike 0a (build with GHC-wasm)

Toolchain: `ghc-wasm-meta` with `FLAVOUR=9.10` on aarch64-darwin, which
gives `wasm32-wasi-ghc` 9.10.3.20260731 and cabal 3.14.2, installed in
`~/.ghc-wasm`. Build: `cabal.project.wasm` (core, pure backend, library,
headless with fsrun), pinned to Stackage `lts-24.12`
(`cabal/lts-24.12.config`). Test suites weren't built.

What worked unchanged:
- **Every dependency** in the snapshot, including the C ones
  (`libyaml-clib`, `zlib-clib`) and `async`, `monad-par`, Earley,
  megaparsec, aeson, Rasterific, FontyFruity and JuicyPixels.
- **`autocolor_cache.c`**, with its `__atomic_*` built-ins and
  `sched_yield`.
- **`file-embed`**: the overlay font, `CONTRIBUTORS`, and the library's
  `embedDir`.
- **The library's splice, which loads the core into the TH interpreter**
  (C object included). This was the biggest unknown.

What broke (two things, both small):
1. **`githash`**: `tGitInfoCwd` fails with "runInteractiveProcess:
   unsupported operation", since the wasm TH interpreter can't spawn
   `git`. Stopgap on the spike branch: on wasm the metadata is
   "unknown" (CPP on `wasm32_HOST_ARCH`). Proper fix: R5.
2. **`-threaded`** in fsrun's `ghc-options`: the link fails because
   there's no threaded RTS (`-lHSrts-1.0.2_thr`). Fix: pass `-threaded`
   and `-with-rtsopts=-N` only `if !arch(wasm32)`. This is a proper fix,
   not a stopgap. The end-to-end suite has the same `-threaded` and will
   need it too.

Result: `fsrun.wasm` links, 25.5 MB before `wasm-opt`, and
`wasmtime run fsrun.wasm --help` prints the usage (about 3 s wall time,
most of it wasmtime compiling the module). Nothing past `--help` has
run yet; that is spike 0b.

## References

- GHC users' guide, WebAssembly backend:
  https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html
- Tweag, wasm backend TH and ghci (Nov 2024):
  https://tweag.io/blog/2024-11-21-ghc-wasm-th-ghci/
- jsaddle-wasm: https://hackage-content.haskell.org/package/jsaddle-wasm
- WebGPU in major browsers: https://web.dev/blog/webgpu-supported-major-browsers
- GPU floating-point precision (double-single on WebGPU):
  https://luma.gl/docs/api-guide/shaders/gpu-floating-point-precision
- gpuweb implementation status:
  https://github.com/gpuweb/gpuweb/wiki/implementation-status
