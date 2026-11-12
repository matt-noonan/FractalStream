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
| D14 | Kernels' math functions come from **musl's libm, compiled to wasm** (built each time) | Not JavaScript's `Math`: calling it from wasm on several worker threads crashed V8; and this way every host computes alike. |
| D13 | **Backend.Wasm before the web app;** test wasm under Node from 0g on | The interpreter is about 2,500× slower than LLVM (0b), too slow even as a stopgap. |
| D16 | Tools' handlers (and overlays) are **compiled by Backend.Wasm and run on the workers**; what it can't compile is a tool compilation error, not interpreted | Interpreted, external rays took 7.5 s, and the engine's one thread was stuck meanwhile. On a worker the engine stays free, and Stop can come later by stopping the worker. (Without a worker pool, tools are interpreted.) |
| D17 | Text in Backend.Wasm is a **tree of nodes in the arena** (literals, numbers, concatenations), formatted by the engine when it reads a text back | Numbers then print exactly as interpreted (`showValue`), without calling back into Haskell or reimplementing `show` for `Double`. Nothing in the language looks inside a text; if that changes (comparisons, `length`), revisit. |
| D18 | A slow handler (a tool's, or an overlay's refresh) shows a **bar in its viewer's window** after a quarter of a second, with Stop (and, for a tool, Stop and switch to Navigate); **Stop kills the worker** running it, which the pool replaces; a stopped overlay is off until the project's reopened (or, later, recompiled) | Not a modal window, which is the wrong idiom on the web: the rest of the app stays usable. Killing the worker needs no shared memory (so no cross-origin isolation), and stops even an endless loop. |
| D19 | **Tables on the workers: per-worker copies**, by version (`Table`'s new `tableVersion`): the pool keeps a copy of each version (up to 512 MB), sends it once to each worker that needs it, and a worker keeps up to 256 MB of them in its memory, above its jobs' scratch memory; only the tables a kernel reads are sent | Works without shared memory (so without cross-origin isolation); revisit if we get COOP/COEP. A tool's operations on tables will be C compiled to wasm, as autocolor is, since calling JavaScript from wasm on workers crashed V8. |
| D15 | Tools' drawings are drawn **by the page, with Canvas2D**, from shapes the engine sends; fsrun keeps Rasterific | Rasterific in wasm took 1.5 s for a 2,000-segment orbit; Canvas2D takes well under a millisecond, and drawings move with the view at once. Far-reaching shapes are clipped as `Actor.Viewer.Clip` does. |

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
│   • Tools: Backend.Wasm, on the workers (D16)               │
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
- Tools' handlers compile to a `tool(args, arena, arenaSize, result)`
  function and run on a worker, once per event: lists are made from the
  arena's bottom, draw commands put in 40-byte records from its top, and
  the arguments written back in place; the engine reads back what changed
  (setting the changed values) and replays the draw commands, so nothing
  calls back into Haskell (D16).

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

Build the web app with `dev-tools/web/build.sh`, and check it in headless
Chromium with:

- `node dev-tools/web/check.mjs`: every library project opens and draws.
- `node dev-tools/web/check-tools.mjs [SECTION ...]`: the tools. Its
  sections are `drawing` (shapes far from the view), `tools`, `refresh`,
  `sweep` (every library tool compiles), and `stop` (handlers that never
  finish, from `dev-tools/web/endless.fs`).
- `dev-tools/wasm/compare-tools.sh`: every library tool's results against
  the native LLVM build's.

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

Reordered after spike 0b (D13): the pure interpreter is far too slow to
render with in the browser, so the compiled CPU backend comes before
the web app. Generated wasm can only be loaded by a JavaScript host, so
from spike 0g on, wasm builds are tested under Node rather than
wasmtime.

### Phase 0: Spikes

- [x] **0a.** Build the core, pure backend, library and fsrun with
  GHC-wasm, and list what breaks (see the findings log).
- [x] **0b.** Run fsrun scenarios and the test suites under wasmtime and
  compare with desktop. The end-to-end suite is skipped on wasm until
  Backend.Wasm exists, because its full-size renders take too long with
  the interpreter.
- [x] **0g.** The heart of Backend.Wasm, under Node: Haskell (in the wasm
  engine) emits the bytes of a small Mandelbrot kernel module, loads it
  through the JavaScript FFI (`WebAssembly.instantiate`), runs it over a
  block of pixels, and reads the result. Time it against LLVM, on one
  thread and then with a few workers over shared memory.
- [x] **0f.** The engine's size and startup: see the findings for
  2026-11-12 ("Shipping").
- [ ] **0c / 0d** (before Phase 5): the df64 accuracy harness, and
  hand-written WGSL kernels timed against the CPU.

### Phase 1: Headless engine (mostly done)

- [x] R5, R6 (git metadata; autocolor without a callback); `cabal.project.wasm`.
- [x] The scenario runner and the core, library and headless `spec`
  suites under wasmtime.
- [x] Run the wasm tests in CI (`.github/workflows/web.yml`, with the app's checks).

### Phase 2: Backend.Wasm (default, f64)

- [x] wasm binary encoder; `Value`/`Code` codegen for arithmetic and
  control flow (`fractalstream-backend-wasm`).
- [x] Loading generated modules through JSFFI; a Node harness for fsrun
  (`dev-tools/wasm/fsrun.sh`).
- [x] Milestone 1: `basic_mandelbrot` and the Julia set project through
  Backend.Wasm under Node, compared with LLVM at a small size.
- [x] Kernel worker pool (`fractalstream-backend-wasm/js/`).
- [ ] Tiles and progressive passes (R3).
- [x] Lists in a per-block arena; issue flags.
- [x] Text (D17).
- [x] Tables: viewers reading them, and tools writing them (D19).
- [x] Autocolor: snapshot lookup (shared C, `autocolor_lookup.h`), misses added by the engine, block redrawn.
- [x] libm: musl's, compiled to `js/libm.wasm` (D14).
- [x] Turn the end-to-end suite back on for wasm (under Node: `dev-tools/wasm/node.sh`; its table tests wait for tables).
- [x] Golden comparison against LLVM output across the library projects
  (tolerance per D4): `dev-tools/wasm/compare.sh`.

### Phase 3: Minimal web app (done, apart from R1 and R2)

- [ ] R1 (platform record), with browser implementations.
- [x] Engine worker and RPC; `fractalstream-ui-web` implements `UI`.
- [x] TypeScript UI renders `Layout`s as config panels (colors as text so far).
- [x] Viewers with pan and zoom (in the page for now; R2 still to do).
- [x] The built-in library.
- [x] Open and save local `.fs` files (with `values:` and `tables:`, as the desktop app; Save writes back where the browser allows it, else downloads).
- [x] Basic window management: viewers as windows to rearrange and
  resize; the configuration as a panel that pops in and out; hiding and
  showing the library.
- [x] High-DPI canvases (rendered at the screen's density, up to 2×).
- [x] Smoothing: a toolbar switch, remembered; off by default on high-DPI screens, on otherwise. Smoothed frames are byte-identical to fsrun's LLVM captures.
- [x] Saving the view state in project files, under `web-properties:`
  (each window's place and size, its center, and a pixel's size per CSS
  pixel).
- [x] Tidy-ups: color pickers; render errors on the viewer, not the
  status line; globe dragging for sphere viewers; faster pinch zoom; a
  chooser for the configuration's tabs.
- [x] Try it in Safari and Firefox: every project works, no console errors; panning and zooming smooth after reprojecting frames onto the page's own view.
- [x] Deployed as a static site, at `/app/` on the project's GitHub Pages (from `main`, by `pages.yml`, once `web.yml`'s checks pass), linked from the landing page as a preview.
- [ ] **Progressive rendering** (deferred). Today the page shows the last
  frame, moved and scaled, until the whole new render arrives, so a slow
  viewer (the forced pendulum takes about 8 s) shows nothing new until
  it's done. The desktop renders coarse passes first, sharpening to full
  resolution (`Task.Block.progressivelyOver`), keeping the moved old
  picture where it has one. For the web, the engine would post partial
  frames as passes finish, and the page would keep reprojecting the
  newest. It must fit with reprojection (which pieces count as already
  shown), smoothing (the subsampled pass comes last), and later the GPU
  backend's frame-level API (R3).

### Phase 4: Tools and overlays

- [x] Tool events through the interpreter, as the desktop app sends them:
  click, double click, drag and drag finished (events a tool doesn't
  handle navigate), buttons, timers, Activated/Deactivated/Refresh; the
  default Select tool; a tool's own panel; fields a handler sets update
  in the panels.
- [x] A tool palette in the toolbar, for the active viewer (the last one
  clicked), with each tool's shortcut key, and n for Navigate.
- [x] Drawings and overlays: Canvas2D painting of `DrawCommand`s (D15),
  matching `Actor.Viewer.Overlay`, on the plane and the sphere.
- [x] Progress and Stop for slow handlers and overlays (D18).
- [x] Backend.Wasm for tools' handlers, run on the workers (D16);
  `dev-tools/wasm/compare-tools.sh` compares every library tool's results
  with LLVM's.
- [x] Text in Backend.Wasm, wherever the LLVM backend has it (D17).
- [x] Tables and autocolor in tools.
- [ ] Preferences (IndexedDB), including the precision toggle (D12).

### Phase 5: Backend.WGSL (df64, then f32)

Only if 0c/0d come out well.

- [ ] R4 (declining and chaining backends).
- [ ] Arithmetic and control-flow subset, df64 runtime library.
- [ ] Uniforms for hoisted values; storage buffers for tables and lists;
  scratch arena; issue flags.
- [ ] Autocolor append buffer.
- [ ] f32 mode.
- [ ] Golden comparisons through headless Chrome, or Node with Dawn.

### Phase 6: Script editor

- [x] CodeMirror 6 (bundled with esbuild), with the engine's coloring
  (`Language.Editor.Highlight`), live error marks (`checkScript`),
  normalizing (`Language.Editor.Normalize`), and symbol entry (a
  TypeScript port of `Language.Editor.Symbols`, with the engine's
  tables), over RPC; over the whole workspace, from the toolbar's Script
  button.
- [x] Rebuild the project from the edited script, as the wx app does:
  windows, views, minimized windows and tools kept, values and tables
  carried over (`carryOverProjectState`).
- [x] Windows minimize to their title bar (double-click it, or its
  button), and stay so when saved.
- [ ] Folding, line numbers, font: with preferences.

### Phase 7: Polish

- [x] Sphere view (trackball): done in Phase 3.
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

### 2026-11-12: pixel_size on high-DPI screens, and smoothed

On a Retina screen the page draws a CSS pixel with two canvas pixels,
and a viewer's pixel size was a canvas pixel's: so whatever a script
sized in `pixel_size` came out half as big as on the desktop, whose
pixels are the screen's logical ones (markers, label offsets, a
crosshair's width). Smoothed renders halved it again, in fsrun's
captures too, since they sample at twice the size.

Now a viewer's code sees a pixel's size apart from its sampling step
(`ViewerArgs`'s new `vaPixel`, which every backend binds `pixel_size`,
`pixel_width` and `pixel_height` to; `renderViewerBufferAt` takes how
many of the viewer's pixels make the screen's), and tools and overlays
are given it too: a CSS pixel's, in the web app (`fs_density`), whatever
the density or smoothing; the desktop's tiles pass their step, so
nothing changes there. New checks: `PixelSizeSpec` (a smoothed render
sees the unsmoothed pixel; it fails with the old binding), and the
`density` and `refresh` sections of `check-tools.mjs` (with `FS_DPR=2`).

### 2026-11-12: The script editor

The editor is a desk window (the toolbar's Script button) holding
CodeMirror 6. The page is now bundled by esbuild (TypeScript only type
checks), which also takes in the WASI shim. The engine answers three new
calls: `fs_analyze` (colors, the check's error, and normalizations, in
UTF-16 offsets: a character outside the BMP, like 𝑖, is two), `fs_symbols`
(the names' tables), and `fs_rebuild` (the project built again, through
the same path as opening one: windows, views and tools stay).

**Symbol entry is a port**, since it reacts to every keystroke; a
cross-check of `web/src/symbols.ts` against `Language.Editor.Symbols`
(names, accents with braces, spaces and nested names, double
backslashes, previews, Tab completions) agreed on all 26 cases.

**Error positions:** a `SourceRange` counts rows and columns from 0, and
includes its last column (as `indicateError` draws it); a first guess of
1-based rows marked the wrong text, which the editor check now catches.

**The editor pops over the workspace**, not as a window on the desk: in
Safari, after a rebuild, the editor's text showed on top of a raised
viewer window; and editing wants the room. A rebuild that works closes
it (one that doesn't leaves it open, saying why), and it keeps what's
typed in it while it's closed.

**A garbling race, found in Firefox but in every browser:** an analysis
that came back after more had been typed (above what it would rewrite)
was used with the text as it then was, so its normalizations landed off
by what had been typed (`≔=`, `⟵-`, `<⟵`). Analyzing a long script takes
150–350 ms, so this happens at a natural typing pace. Now an analysis is
used only if the text is still what it was of. The editor check makes
analyses 100 ms late and types above them, which garbled the script in
Chromium, Firefox and WebKit before the fix. (The desktop app's editor
can't do this: it works out its normalizations from the editor's text
as it is, when it applies them.)

**Other browsers in the checks:** `FS_BROWSER=firefox` or `webkit` runs
the checks in Playwright's Firefox or WebKit (install them first); the
editor checks pass in all three.

**Found on the way:** the tool shortcuts (one key each) took keys typed
into the editor, which is a `contenteditable`, not an input; and clicking
a viewer left the keyboard with the editor. Shortcuts now skip editable
elements, and a viewer takes focus when it's clicked.

### 2026-11-12: Shipping

**Size and startup (0f).** The engine is 16.7 MB as GHC links it (2.9 MB
gzipped, 2.1 MB with brotli). `wasm-opt -Oz` (binaryen, from the
toolchain) brings it to 7.6 MB (2.35 MB gzipped, 1.8 MB brotli), with
every check passing; `dev-tools/web/build.sh` now does it, after making
the JavaScript glue from the module as GHC left it. (`-O2` gives 10.2 MB.)
Served locally, the app shows the library about 0.3 s after the page
loads, and draws a first project 0.3 s later; online, the download
dominates.

**CI.** `.github/workflows/web.yml` builds the app with the toolchain
pinned (ghc-wasm-meta `8079eb96`, FLAVOUR 9.10, cached), runs the wasm
end-to-end suite under Node and both browser checks, and uploads the app
as an artifact; on pushes and pull requests touching the web side. The
LLVM comparisons stay manual.

**Deploying.** `pages.yml` runs `web.yml` on `main` as a job, and puts its
artifact at `/app/` beside the landing page, which links to it as a
preview. Everything in the app is relative, so it works under the
project's Pages path (checked by serving it at `/FractalStream/app/`).

### 2026-11-12: Tools write tables, and use autocolor, in Backend.Wasm

**Tables.** A tool's `reset`, cell and point writes, slice writes from
lists, slice copies, and `resize` compile. Making and copying tables is
`libm/fstables.c`, compiled to `js/tables.wasm` (no data or stack of its
own, and bulk memory for its copies, all checked by the build), which
makes them in the tool's arena; the engine reads back any table a tool
replaced, as a new version. A frozen table (a configuration's, or one
another variable also holds) is copied before it's written, and nothing
is written unless that copy worked, so a worker's cached tables are never
written. The table tests (`TableSpec`), skipped under wasm until now,
all pass there.

**Autocolor.** A tool gets an autocolor snapshot and a misses list, as a
viewer's kernel does; a run that meets points to add is run again once
the engine has added them. New test: `ToolAutocolorSpec`.

**SaddleDrop** (copies of `../saddledrop-layers.2.fs`, with its 6 MB of
saved tables): the saved tables' layer, Add (a resize), and the Critical
point tool capture identically with wasm and LLVM, and as fast. The flood
fill needs the file's saved values (fsrun starts from the script's
defaults; `zoom = 0.01` matters, as do `c`, `crit`, `crit_c` and
`crit_found`), set with `set parameter`. With them, Fill over a 200x200
view is identical; over the full 600x600 view, 10 pixels differ, on the
filled region's edge: 7 escape-time colors, and 3 cells where Newton's
method converged in one build and not the other (musl's libm against
macOS's, as elsewhere).

**A race in Stop's "switch to Navigate"**, which the new timings showed:
when a stopped handler let go of the viewer's lock, another handler
waiting for it (a refresh after a new frame) could start, clearing the
shared "navigate" flag, and run with the old tool. Now each run has its
own flag, the switch happens before the lock is let go, and a waiting
handler checks that its tool is still selected.

### 2026-11-12: Viewers read tables in Backend.Wasm

A viewer's code reading tables compiles: by index, by point (with
`else`), slices as lists, sizes, `in`, `table_cell`, `table_point`. A
table is the address of a descriptor laid out as the LLVM backend's (with
a 32-bit pointer), written into the job's arguments; on a worker, the
worker fills in where its copy of the table is (D19).

**Only what a kernel reads is sent.** The core hands a viewer every
configuration value, tables included (it uses the variables a viewer
reads only to decide when to re-render it). The wasm backend now leaves
out of a job's arguments the lists, texts and tables its code doesn't
read (or, for a tool, set: `usedVarsInCode` doesn't count assignments).
In the fixture, a 960 KB table the viewer never reads isn't sent, and nor
is one it reads only in the part run once per render.

**Checked against LLVM:** `dev-tools/wasm/tables/compare.sh` renders
`tables-read.fs` (saved tables, made by `MakeFixture.hs` through the
app's own save path, since fsrun can't save tables) with both builds:
identical, including a view mostly off the tables. The library's tools
still compare as before (`compare-tools.sh`: 34 identical, the rest the
same few render-only pixels).

**A parser oddity:** a variable named `on` followed by a line starting
with `(` fails with "`point` is a built-in function, so it can't also
name a variable" (it parses alone). Not looked into.

**Beware stale binaries** when comparing: `compare-tools.sh` uses the
installed native `fsrun` and the last wasm `exe:fsrun` build; rebuild
both (`stack build`, `dev-tools/wasm/build.sh exe:fsrun`) first.

### 2026-11-12: Progress and Stop

A handler still running after a quarter of a second gets a bar along the
bottom of its viewer: a spinner, the seconds so far, Stop, and (for a
tool) Stop and switch to Navigate. Handlers run one at a time per viewer,
but different viewers' at once, and the rest of the page stays usable.
The engine runs each with a `CancelFlag`, as the desktop app does;
Backend.Wasm watches the flag, and stops a set one's job with
`pool.stop(tag)`, which kills its worker and starts another (that one
compiles kernels again as it needs them). The core's
`vRefreshOverlay` now takes a `CancelFlag`, and `vStopOverlay` turns an
overlay off for the viewer's build (so a recompile will bring it back).

Tested with a project whose handlers never finish (a click, an `on
refresh:`, and an overlay): the bar comes after about 280 ms; another
viewer renders meanwhile; Stop drops the run's changes and leaves the
tool selected, and clicks work afterwards; Stop and switch to Navigate
ends a slow refresh for good; a stopped overlay's drawing goes, and it
stays off. (A `while` loop stops at the iteration limit unless it says
`up to N times`.)

### 2026-11-11: Text in Backend.Wasm

Text compiles wherever the LLVM backend has it (D17): literals,
`text(...)`, numbers as text, text variables and configuration fields
(in and out), lists and pairs of text, and `write`. A new end-to-end test
(`TextSpec`) runs a tool that builds texts (with non-ASCII characters,
and numbers like `1.0e-2` and `1.0e10`), reads a text field and a list of
texts, and writes them back: it passes with LLVM natively and with wasm
under Node. All 51 of the library's tools now compile; the Böttcher
coordinate tool's captures match LLVM's.

### 2026-11-11: Compiled tools

Tools' handlers now compile with Backend.Wasm and run on the workers
(D16). Selecting External rays' dynamical-plane tool, which draws its
rays when it's refreshed, took 7.5 s interpreted, during which nothing
rendered; compiled, it takes about 110 ms, including compiling it.

Of the library's 51 tools, 50 compile; the Böttcher coordinate tool
writes text, which Backend.Wasm can't compile yet, so it fails to build
(as decided: no interpreter fallback).

`dev-tools/wasm/compare-tools.sh` drives every library tool the same way
in the wasm and native LLVM builds (select it, click, drag) and compares
captures of every viewer: 34 of 48 identical, 10 differing slightly, 0
failed. Each differing case differs as much without the drawings (base
captures), so it's the viewers' renders (libm against the native math
library, in chaotic regions), not the tools; a chaotic orbit (period 5)
can move a pixel too, its tool using the same math.

The fix for overlays and Refresh (as the desktop app sends them) came
just before: overlays are refreshed when any configuration value changes,
and the selected tool after each render.

### 2026-11-11: Tools and drawings

**Rasterific is too slow in wasm for live drawings.** Timing
`Actor.Viewer.Overlay` on typical drawings at 600 by 600:

| Drawing | Native | Wasm (Node) |
|---|---|---|
| Orbit, 2,000 segments | 290 ms | 1,490 ms |
| 200 circles and points | 13 ms | 70 ms |
| 20 rectangles with labels | 3 ms | 18 ms |

Stroking a run of lines as one shape barely helped (295 to 261 ms
natively): the cost is in outlining each segment. So the page draws them
(D15): the same orbit takes 0.3 ms in Chromium at 1200 by 1200.

**Far-away shapes.** The page places shapes in pixels in double precision,
and cuts any that reach more than 10^4 pixels outside the view down to
the part near it, porting `Actor.Viewer.Clip`. A line from 0 to 10^10 (or
from -10^300 to 10^300), a circle of radius 10^10, and a rectangle out to
10^12 each draw in under half a millisecond, and the line to 10^10 covers
exactly the pixels a short line in the same direction does.

**A drag's points are the wrong way round in fsrun.** A drag handler's
first point is where the pointer is, and its second where the drag
started (`parseDragScript`), and the desktop app sends them that way
(`Drag now start`); the web app does too. But fsrun's `drag from A to B`
sends `Drag A B`, so a script sees the drag going from B to A; and the
comment on `Event`'s `Drag` says "from / to". Not fixed yet.

### 2026-11-11: A first web app

To try it:
```
cd web && npm install   # once
dev-tools/web/build.sh
node dev-tools/web/serve.mjs   # http://localhost:8000
```

How it fits together:
- **The engine:** `fractalstream-ui-web`, a 14.6 MB reactor module, runs
  in a Web Worker. It implements `UI`, sets fields by name through the
  core's `Field`s, and runs a coalescing render loop per viewer. It posts
  JSON and RGB frames that carry the view they show.
- **The page:** TypeScript, plain DOM, in `web/src`. It shows the library
  with previews, the configuration panel built from the layout, and a
  canvas per viewer: drag to move, wheel to zoom about the pointer, with
  the old frame moving at once until the new one arrives.
- **The browser's WASI:** `@bjorn3/browser_wasi_shim` 0.4.2.

**Results:**
- **Every project opens:** `dev-tools/web/check.mjs` opens all 32 library
  projects in headless Chromium (Playwright from the toolchain). Every
  viewer draws with no errors, most in 55–300 ms; the forced pendulum
  takes 2.0 s and period 5 takes 1.0 s.
- **Edits, zooms and drags** each give a new frame in about 150 ms.
- **A bad input** marks its field; the parse error also shows in the
  status line, because that render fails.
- **Against `fsrun`:** the Julia set's canvas differs from `fsrun`'s
  capture only along edges, because captures are smoothed (rendered at 2×
  and averaged) and the web app renders at 1×.

### 2026-11-11: Autocolor in Backend.Wasm: every library viewer compiles

The search and blend in a snapshot moved from `autocolor_cache.c` into
`autocolor_lookup.h`. It's shared by desktop's compiled code (behaviour
unchanged, LLVM tests pass) and `libm/fsautocolor.c`, which builds to a
1 KB `js/autocolor.wasm` that works in the kernel's memory (the build
checks it has no data and no stack).

How a kernel handles autocolor:
- It gets a copy of the current snapshot (a worker gets it with the job).
- A point to add goes on a list of misses, and the pixel gets what a
  locked group would give.
- After the block, the engine adds the missed points through the
  project's `AutocolorLookup`, then **redraws the block until it misses
  nothing** (at most 8 times). So blocks come out as LLVM's do, with
  nothing flashing, which beats the miss color we'd planned.

**Library sweep (100×100): 53 of 53 viewers compile, no fallbacks.** 42
are byte-identical to LLVM, including the autocolor ones, with and
without saved tables and workers. The other 11 differ by a few chaotic
pixels (musl versus Apple libm).

### 2026-11-11: Lists in Backend.Wasm; a V8 crash with workers

**Lists** are compiled (`Backend.Wasm.List`) with the LLVM backend's
semantics: an arena per block, reset for each subsample; overflow and bad
indices make the pixel magenta and set the issue bits. Every list test
script and library project compiles. At 100×100, every one renders
byte-identical to LLVM except a few pixels in chaotic regions (math
functions). Only the autocolor viewers still fall back.

**A V8 crash.** Node (26.11.1, the toolchain's, on macOS/ARM) dies now
and then with SIGILL in a worker thread running a kernel, when two or
more workers run kernels at once. It happens in about 1 run in 8 of
twisted-mandelbrot or test-scripts/list-transform-config.fs with 4
workers, and never with 0 or 1 worker. What I found:
- It reproduces without the engine, replaying the captured jobs on the
  pool (`FS_WASM_DUMP`), so it isn't the engine.
- The same jobs on the main thread never fail and never trap, so the
  kernels themselves are valid.
- It isn't V8 sharing compiled code between threads: distinct module
  bytes per worker still crash.
- V8 flags only make it rarer: no lazy compilation 2/160, no dynamic
  tiering 0/260 on one project but 2/3 on another.
- **It's the kernels' calls into JavaScript** (the imported `Math`
  functions): with wasm stub imports instead, 0/60 against ~15%.
  Wrapping `Math` in arrow functions helped on replay but not for real.
  The Julia kernel, with 1 import call, never crashed; twisted-mandelbrot
  has 13.

**Fixed (D14):** kernels import their math functions from `libm.wasm`
(musl's, compiled by `dev-tools/wasm/build.sh` from
`fractalstream-backend-wasm/libm/fsmath.c`). It has no imports of its
own, and workers and the engine each instantiate it once. The result: 0
crashes in 100 runs of each project, the same speed, and the library
sweep slightly closer to LLVM (39 of 53 identical; Hénon's differences
fell from about 200 pixels to 71 and 26). The rest are a few pixels in
chaotic regions, from musl versus Apple's libm.

### 2026-11-11: Backend.Wasm on a pool of workers

The host makes a pool of kernel workers (`fractalstream-backend-wasm/js/`;
`run-node.mjs` uses one per core, or `FS_WASM_WORKERS`) and puts it at
`globalThis.fractalstreamWorkers`. Each worker has its own private memory
and instance of a kernel; a block's arguments go to it, and its pixels
come back in a transferred buffer. So **no shared memory is needed, and
the static site doesn't need COOP/COEP headers** (this settles 0e's
hosting question).

The engine has one capability, so the core's new `renderConcurrency`
(`Task.Concurrent`) says how many blocks a render keeps in flight. It
defaults to the capabilities count, the wasm backend sets it to the
pool's size, and fsrun's `-j` sets it. Each block is an async JSFFI call
on its own green thread. The pixels are copied into the block's buffer
after the result is forced, so a cancelled render never writes into a
freed buffer.

A 1200² Julia set at 2,000 iterations, total wall time under Node,
identical to LLVM at every worker count:

| Workers | 1 | 2 | 4 | 8 | 10 |
|---|---|---|---|---|---|
| wasm | 2.35 s | 1.35 s | 0.84 s | 0.66 s | 0.61 s |

LLVM takes 1.58 s with `-j 1` and 0.29 s with `-j 10`. The machine has 4
performance and 6 efficiency cores. What's left of the gap:
- single-threaded fixed costs (startup and PNG encoding in the engine),
- one wasm thread being about 1.5× LLVM here, probably from calls out to
  JavaScript's `Math` (Glow uses `log`).

### 2026-11-11: Backend.Wasm, milestone 1

`fractalstream-backend-wasm` (branch `wasm-backend`) compiles viewers to
wasm kernels: arithmetic, complex numbers, colors, pairs, comparisons,
control flow, and every math function, case by case as the LLVM backend
does. Math functions come from JavaScript's `Math` (and `%` for `fmod`).
Configuration values go into one buffer per render. Lists, text, tables
and autocolor fall back to the interpreter (with a note on stderr), and
tools run interpreted. On wasm, fsrun uses this backend, is a reactor
module, and runs under Node with `dev-tools/wasm/fsrun.sh`.

- **`basic_mandelbrot` (200²) and all three Julia set scenarios (600²):
  byte-identical to LLVM.** Each Julia scenario takes about 0.3 s under
  Node, startup included; the interpreter took 628 s or more. Rendering is
  still on one thread.
- **Library sweep, each viewer at 100×100, against LLVM:**
  - 44 of 53 viewers compile.
  - 38 of those are byte-identical.
  - 6 differ in a few chaotic pixels: Newton 0, Böttcher parameter plane
    7, twisted Mandelbrot about 50 each, Hénon planes about 200 (2%)
    each. These look the same and are scattered like round-off noise.
    Likely cause: JavaScript's `Math` versus the system math library in
    the last bit. Hénon's only transcendental call is `hypot` (from
    complex `sqrt`), and V8's `Math.hypot` isn't correctly rounded. Not
    yet confirmed; part of the math-library decision.
  - 9 viewers fall back: autocolor (newton-cubic-parameter-plane,
    newton-quintic), lists (slow-mating, pendulum-poincare-map,
    period-5). pendulum and period-5 take more than 2 minutes at 100×100
    with the interpreter.
- **Integer division:** where wasm would trap (division by 0, or the
  smallest integer by -1), the kernel gives what the LLVM backend's code
  gives on 64-bit ARM.

### 2026-11-11: spike 0g (a generated kernel, under Node)

Code: `dev-tools/wasm/kernel-spike/` on branch `spike/wasm-kernel`
(`run.sh [size...]`). The engine (a reactor module using JSFFI) encodes
a 321-byte Mandelbrot kernel itself (basic_mandelbrot's region, 100
iterations, escape radius 10), loads it with `new WebAssembly.Module`
plus `new WebAssembly.Instance` through a synchronous JSFFI import
(0.07 ms), and runs it with the engine's own memory imported as
`env.memory`, so the kernel writes pixels straight into a Haskell
buffer.

- **Single thread: native speed.** Warm times:

  | Size | Generated wasm | Native C `-O2` | Compiled Haskell in wasm |
  |---|---|---|---|
  | 600² | 6.5 ms | 12–16 ms | 6.3 ms |
  | 2400² | 104 ms | 101 ms | 100 ms |
  | 4800² | 414 ms | 401 ms | 397 ms |

  The pixels are identical to the Haskell version's, and to C's once C's
  fused multiply-adds are off (`-ffp-contract=off`). We'll want FMA off
  in the generated code too, to match the LLVM backend.
- **First call about 3× slower** (2400²: 328 ms, then 104). V8 starts a
  new module on its baseline compiler and switches to the optimizing
  one. Only the first render with a new kernel pays this; whether it can
  be avoided (say, by warming up on a tiny block) is a Phase 2 question.
- **Workers scale well.** One shared `WebAssembly.Memory` and an
  instance of the kernel per Node worker, rendering 16-row bands from a
  queue, then copied into the engine's memory. 4800²: 420 ms on 1
  worker, 217 on 2, 116 on 4, 78 on 8, 67 on 10, on 4 performance and
  6 efficiency cores. The engine's own memory can't be shared, so the
  workers' kernels import a separate shared memory (the module declares
  it `shared`, with a maximum).
- **Bands need the image's origin.** Starting each band at
  `y0 - row * step` changed 2 of 5,760,000 pixels by rounding. The
  backend should compute every pixel's coordinate from the image origin
  and its index, not by stepping from the band's start. (The current
  block API passes each block its own start point, so R3 should address
  this.)
- **Async JSFFI results are lazy:** a render's result has to be forced
  (`evaluate`) before the next one starts, or they overlap.
- **JSFFI setup notes:** link with `-no-hs-main
  -optl-mexec-model=reactor -optl-Wl,--export=...`, then run
  `post-link.mjs`. Node's WASI needs a non-empty `args` (a program name),
  or the RTS exits during `_initialize`.

**Conclusion:** Backend.Wasm is viable. Its kernels run at native speed
and scale across workers, so Phase 2 can go ahead as planned.

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
- **Tests:** all four suites build for wasm (`dev-tools/wasm/build.sh
  --enable-tests ...`; it builds a native `hspec-discover` once, since
  cabal would build that tool for wasm). Run each from its package's
  folder: `wasmtime run --dir=.. --dir=. --dir=/tmp <suite>.wasm`.
  - Core `spec`: 576 examples, 6 failures, in about 10 s. The 6 are
    golden files that #146 left stale (the desktop fails the same six),
    and #146's Böttcher project has no golden yet. The golden the wasm run
    writes for it is identical to the desktop's.
  - Library `spec`: 45 examples, 0 failures. Headless `spec`: 43, 0.
  - The autocolor stress test passes on the single-threaded RTS.
  - What fixed the core and library crashes: `foreign import ccall
    "wrapper"` doesn't work on wasm, and `Actor.Autocolor` used one for
    the C cache's "add a point" callback. On wasm the cache now has no
    callback, and `fs_ac_lookup` gives magenta rather than call it.
    Nothing on wasm uses the cache yet.
  - End-to-end suite: its renders are full size, so with the interpreter
    the run took over 50 minutes and I stopped it (it had reached
    `tables`, all passing). It needs reduced resolutions first.

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
