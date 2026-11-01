# FractalStream - Developer Notes for Claude

## Project Overview

FractalStream is a Haskell application for studying real and complex dynamical systems.
Users author scripts in a custom language to define dynamical systems, and can build
customizable UIs around them with configuration panels and interactive tools.
Projects are saved as YAML files.

## Package Structure

Five Cabal packages, built with Stack (GHC 9.10.x):

```
fractalstream-core/           # Core library: ASTs, parsing, type system, actors, dynamic values
fractalstream-backend-pure/   # Pure Haskell interpreter backend
fractalstream-backend-llvm/   # LLVM JIT compiler backend (default)
fractalstream-ui-wx/          # wxWidgets UI (the main executable: FractalStream)
fractalstream-ui-headless/    # Headless frontend (`fsrun`), no GUI dependencies
```

`fsrun project.fs --scenario NAME` runs a project's scripted scenario
(clicks, parameter changes, viewer captures to PNG) without a GUI; see
`SCENARIO_LANGUAGE.md`. It is the easiest way to check a change end to end,
e.g. by rendering a viewer before and after and comparing the PNGs.

`fractalstream-ui-wx` depends on `fractalstream-core` and exactly one backend
(controlled by the `use-llvm` Cabal flag, default True). The `Backend` module in
each backend package exposes a `withBackend` function that `Main.hs` calls.

## Core Architecture Concepts

### Type System (`Language.Type`)

FractalStream values have types described by the `FSType` kind:

```haskell
data FSType = VoidT | BooleanT | IntegerT | RealT | ComplexT | RationalT
            | Pair FSType FSType | ColorT | ImageT | ListT FSType | TextT
```

The `HaskellType` type family maps `FSType` to Haskell runtime types
(e.g. `HaskellType 'ComplexT = Complex Double`, `HaskellType 'IntegerT = Int64`).

`TypeProxy t` is a singleton GADT for `FSType`, used to carry type info at runtime.
`KnownType t` is the typeclass equivalent.

### Environments (`Language.Environment`)

An `Environment` is a type-level list of `(Symbol, FSType)` pairs:

```haskell
type MyEnv = '[ '("z", 'ComplexT), '("C", 'ComplexT) ]
```

`EnvironmentProxy env` is the value-level proxy. `KnownEnvironment env` is the
typeclass. Key operations: `NameIsPresent`, `NameIsAbsent`, `bindingEvidence`.

A `Context f env` holds values for each binding in `env` using mapping `f`:

```haskell
myCtx :: Context HaskellValue MyEnv
myCtx = Bind (Proxy @"z") ComplexType (1:+0) $ Bind (Proxy @"C") ComplexType (0:+0) EmptyContext
```

### Value AST (`Language.Value`)

`Value '(env, t)` is the typed expression AST. It is the fixed point of `ValueF`:

```haskell
type Value = ValueF (FIX ValueF)
```

`ValueF` is an *indexed functor* (see `Data.Indexed.Functor`) indexed by
`(Environment, FSType)`. It covers: constants, variables, arithmetic (real/complex/integer),
trig functions, boolean ops, comparisons, if-then-else, color ops, list ops, text ops.

Key constructors: `Const`, `Var`, `LocalLet`, `ITE`, `AddF/AddC/AddI`, `RGB`, `Blend`, etc.

`pprint :: Value et -> String` pretty-prints values.

### Code AST (`Language.Code`)

`Code env` is the typed statement/command AST. Fixed point of `CodeF`:

```haskell
type Code = CodeF (FIX CodeF)
```

`CodeF` is indexed by `Environment`. Key constructors:
- `Let` - introduce a new local variable (extends env)
- `Set` - assign to an existing variable
- `Block` - sequence of statements
- `DoWhile` - do-while loop (all loops compile to this)
- `IfThenElse` - conditional
- `DrawCommand` - draw to the viewer canvas
- `Lookup` / `ForEach` - list iteration commands

### Indexed Functors (`Data.Indexed.Functor`)

The codebase uses an indexed functor pattern for both `ValueF` and `CodeF`.
Key typeclasses: `IFunctor` (with `imap`, `toIndex`) and `ITraversable` (with `isequence`).
`FIX f` is the fixed point of indexed functor `f`.
`indexedFold` and `indexedFoldM` enable catamorphisms over these ASTs.

Uses the `first-class-families` package for type-level computation (`Exp Type`, `Eval`).

### Dynamic Values (`Data.DynamicValue`)

`Variable a` is a mutable cell backed by an `MVar` with a listener map.
`Dynamic a` is an applicative functor over `Variable`:

```haskell
data Dynamic a = Dynamic (Variable a) | Ap (Dynamic (a->b)) (Dynamic a) | Pure a | Join (Dynamic (Dynamic a))
```

`watchDynamic :: Dynamic a -> (a -> IO ()) -> IO ()` registers a listener.
`getDynamic`, `setValue`, `modifyValue` are the main operations.

This is the reactive system: UI widgets and viewer scripts are wired together via
`Dynamic` values so changes propagate automatically.

### Codec (`Data.Codec`)

A combinator library for bidirectional serialization (YAML/JSON <-> Haskell).
`Codec a` describes how to serialize/deserialize `a`.
`CodecWith dep a` is a codec that depends on some dynamic context `dep`.

It still backs a few small pieces (scenarios, labels, dimensions, coordinates,
tool info), but projects themselves are not encoded through a `Codec`: see
"Project File Format" below.

## Actors and UI Model

### Layout (`Actor.Layout`)

`Layout` is a recursive ADT for UI panel contents:
- `Vertical`, `Horizontal` - layout containers
- `Panel` - a titled subpanel
- `Tabbed` - tabbed interface
- `TextBox` - text entry for a named variable
- `Button`, `CheckBox`, `Selection`, `ColorPicker`

`layoutContext` converts a `Layout` into a `SomeContext DynamicValue` (live values for
each named variable in the layout).

### Configuration (`Actor.Configuration`)

`Configuration` bundles a title, size (`Dimensions`), and `Layout` into a panel.
Each `Ensemble` has an optional `Configuration` (from its `@config` stanzas):
a `Tabbed` layout with one tab per stanza title (stanzas sharing a title are
merged), ending with the implicit "Iteration settings" tab. A tool's own
declarations instead become a single titled `Panel`
(`buildMaybeConfiguration AsTabs`/`AsPanels`).

### Ensemble (`Actor.Ensemble`)

An `Ensemble` is a loaded project:
- `ensembleConfiguration` - optional configuration panel
- `ensembleViewers` - list of `ComplexViewer`s
- `ensembleScenarios` - named scenarios (for `fsrun`)
- `ensembleSourceScript` - the raw `script:` text, written back verbatim on save

`buildEnsembleFromScript` builds one from a unified script (grammar in
`Actor.Script`); `parseEnsembleFromFile :: FilePath -> IO (Either String Ensemble)`
loads a `.fs` file, and `encodeEnsemble` saves one.
`runEnsemble` launches the UI for an ensemble.

### Viewer (`Actor.Viewer`, `Actor.Viewer.Complex`)

`Viewer` holds the state needed to render one dynamical system view:
- `vCodeWithArgs` - compiled/interpreted code + context
- `vTools` - the `Tool`s available in this viewer
- `vTitle`, `vSize`, `vPosition`, `vCenter`, `vPixelSize` - view parameters
- `vDrawCmds` - drawing overlay commands

`ViewerCompiler` wraps a backend's compilation function. `withCompiledViewer` compiles
a `Code` into a `ViewerFunction` using the active backend.

`ComplexViewer` is the YAML-serializable viewer config (extends to complex-plane viewers).

### Tools (`Actor.Tool`)

A `Tool` has a name, keyboard shortcut, and a list of event-handler actions.
Each action maps an `Event` to a `Code` block that runs in response.

### Events (`Event`)

`Event args` is parameterized by the environment the event carries:
- `Click`, `MouseDown`, `MouseUp`, `Motion` carry `PosEnv` (posX, posY)
- `Pinch` carries `PinchEnv` (posX, posY, posZ)
- `Drag` carries `DragEnv` (posX, posY, oldX, oldY)
- `Resize` carries `ResizeEnv` (viewWidth, viewHeight)
- `Button` carries `ButtonEnv` (button)
- `Timer`, `Refresh`, `Update` carry no args

## Parsing and typechecking

Source text goes through three stages before it becomes a typed AST:

1. **Parsing** (`Text.Earley`): produces *untyped* surface syntax --
   `Language.Value.Syntax.Expr` for values and `Language.Code.Syntax.Stmt` for
   code (aliased as `ParsedValue` / `ParsedCode`). These are plain trees
   annotated with `SourceRange`s; blocks are flat lists of statements.
2. **Typing**: `Language.Value.Typing` decorates an `Expr` with an `FSType` on
   every node (a `TExpr`). It is bidirectional: `synth` computes a node's
   smallest type, `check` accepts implicit conversions (Z → R → C, numbers →
   Text) or pushes the expected type into pairs/lists/if-then-else/list ops.
   For code, `Language.Code.Infer` first infers the types of `x := e`
   declarations: every assignment to `x` is a lower bound, solved by fixpoint
   iteration re-running `synth` in *lenient* mode (`TypeEnv{tyLenient}`); the
   solved types are written back as if annotated (`x : T <- e`).
3. **Elaboration**: `Language.Value.Elaborate` builds `Value '(env, t)` from a
   `TExpr` (no backtracking; conversions inserted at node boundaries), and
   `Language.Code.Typecheck` (`runParsedCode`) builds `Code env`, resolving
   scoping (a declaration or function definition scopes over the rest of its
   block) and inlining user functions.

User functions (`let f(x, y) = e`, `Language.Code.Typecheck.Functions`) are
inlined at each call as `ELet`s (typed `LocalLet`s) binding freshly renamed
parameters, so each call is typechecked at its own argument types. `f'(a)` /
`f''(a)` (`EDerivCall`) expands to `let t = a in diff(t, f(t))` with `t` fresh
and widened to at least real (`EWiden`). The
grammar must know function names to parse `f(z)` as a call rather than an
implicit multiplication, so each stanza's tokens are pre-scanned for
`let NAME(` (`withDefinedFunctions`, stored in `CommonDefs{funNames}`).

`diff(x, e)` is differentiated symbolically on the surface AST
(`Language.Value.Differentiate`), during typing: the typing pass replaces the
`EDiff` node by the derivative expression and types that. Local bindings
(`ELet`, e.g. from inlined calls) are handled by the chain rule, and
subexpressions that don't mention `x` have derivative 0.

Code blocks (`define name(x, y):` + block, `SProc`; `run name(a, b)`, `SRun`)
are expanded by `Language.Code.Procedures.expandRuns`, the first step of
`runParsedCode` (before inference): each `run` becomes a block declaring
fresh `[internal] name.x #k := arg` variables, then the renamed body. Other
names in the body resolve at the run site; a definition sees only the code
blocks defined before it (no recursion).

`Language.Value.Typecheck.atType` (typing + elaboration at a requested type)
is the entry point used throughout the actors.

Golden tests (`fractalstream-core/test/Actor/GoldenSpec.hs`, files in
`test/golden/`) pin the pretty-printed typed code of every example and
project under `examples/templates/`; delete a golden file to regenerate it,
and review the diff.

Key modules:
- `Language.Parser.Tokenizer` - tokenizer
- `Language.Parser.SourceRange` - source location tracking
- `Language.Value.Parser`, `Language.Code.Parser` - grammars
- `Language.Value.Syntax`, `Language.Code.Syntax` - untyped surface ASTs
- `Language.Value.Builtins` - builtin function/color tables, internal names
- `Language.Value.Typing` - typing pass for values
- `Language.Value.Elaborate` - typed surface AST -> `Value`
- `Language.Value.Differentiate` - symbolic differentiation (`diff`)
- `Language.Value.Typecheck` - `atType` entry point
- `Language.Code.Infer` - inference for `:=` declarations
- `Language.Code.Procedures` - `define` / `run` expansion
- `Language.Code.Typecheck` (+ `.Functions`) - code elaboration, functions
- `Language.Typecheck` - `TC` monad, `TCError`, shared helpers

## Backends

### Pure backend (`fractalstream-backend-pure`)

`Backend.Pure.interpretViewer` runs the code AST directly using `Language.Code.InterpretIO`
(a state-monad interpreter over `IORef`-backed contexts). Slow but portable.

### LLVM backend (`fractalstream-backend-llvm`)

`Backend.LLVM` compiles `Code` to LLVM IR via `llvm-hs`, then JIT-compiles and runs it.
Key modules:
- `Backend.LLVM.Value` - compiles `Value` AST to LLVM operands
- `Backend.LLVM.Code` - compiles `Code` AST to LLVM instructions
- `Backend.LLVM.Operand` - LLVM operand helpers

The JIT kernel signature (`KernelFun`) takes a pixel buffer, dimensions, step size,
start coordinates, and an opaque context pointer.

Text (`TextOp`) is a NUL-terminated UTF-8 `i8*`, built in the per-call arena
(literals and `text(...)` concatenation via strlen/memcpy; arena overflow
gives the empty string plus the overflow flag). Only tool handlers can
convert numbers to text: they call back into the host's `showValue` (the
`text.format` parameter), so output matches the interpreter exactly. `write`
calls the `draw.write` callback, which forwards to `dsWrite`. Text and list
config variables are in/out for tools (a cell holding the string/head-node
pointer; compiled list ops never mutate nodes in place, so a changed pointer
means a changed list, read back with `peekList`).

Tools don't use the viewers' arena pool: each call gets its own arena,
starting at 1 MB and doubling on overflow (rerunning the handler) up to
256 MB. Draw commands are buffered and replayed only after a run that didn't
overflow, and write-back is skipped on overflow, so a rerun has no
duplicated effects. (Repeated `append` is quadratic in arena use.)

A compiled list index out of range sets a separate `asIndexError` flag: a
viewer renders the pixel magenta (as for an overflow), a tool fails with
"A list index was out of range" (no retry, unless the run also overflowed,
since truncated lists can cause bad indices). The tool's status byte has
bit 0 = overflow, bit 1 = bad index.

Viewer kernels report the same two problems through an `#issuesOut` byte
(same bits), sticky across the block: `ViewerFunction` returns
`Actor.Viewer.ViewerIssues`, the wx viewer collects them per render (by
render id, in `renderTile'`) and shows `describeViewerIssues` in the
window's status bar. The bar is hidden when there's nothing to report; it
is cleared only once a render finishes clean (`tileFinished`), and showing
or hiding it resizes the frame to keep the image's client size, with the
resize timer skipping the re-render when the size is unchanged. Also,
headless `capture` prints it as a warning on stderr.

The wx viewer runs every tool handler through `guardTool`
(`UI/ProjectViewer.hs`): on an exception it switches to Navigate first
(skipping the failed tool's Deactivated handler, hiding its config, and
clearing any click/drag in progress), then shows an error dialog, so a tool
that fails on every event can't produce an endless stream of dialogs. `test/Backend/LLVMSpec.hs` checks compiled
tools against `Actor.Event.interpretedToolExec` on the same scripts.

### Headless overlay text

`Actor.Viewer.Overlay` draws `write` with FontyFruity/Rasterific, using a
copy of DejaVu Sans embedded via `file-embed` in `Actor.Viewer.Overlay.Font`
(`fractalstream-core/fonts/`). (It used `include-file`, which took ~15 minutes to
compile the font module on Windows and failed outright at -O0.) It approximates the wx viewer: 13px text in
the stroke color, centered on the point (vertically on the line height).

## UI (`fractalstream-ui-wx`)

Built on `wxWidgets` via the `wx`/`wxcore`/`wxHaskell` Haskell bindings.

Key modules:
- `Main` - entry point; opens welcome screen, handles open/new
- `UI.Welcome` - welcome screen (new project / open project)
- `UI.ProjectViewer` - renders an `Ensemble`'s viewers as wx windows
- `UI.ProjectScriptEditor` - source editor for a project's unified script
- `UI.ProjectActions` - open/save/new-project actions
- `UI.ConfigEditor` - renders a `Configuration` / `Layout` as wx widgets
- `UI.Widgets` - custom wx widgets
- `UI.CodeEditor` - script code editor (uses `StyledTextCtrl`)
- `UI.Layout` - wx layout helpers
- `UI.Tile` - tiled rendering of viewer pixels (parallelized)
- `UI.Menu` - menu bar construction
- `UI.Session` - session management (open windows)

A frontend supplies a `UI` record (`Actor.UI`: `newEnsemble`, `makeLayout`,
`makeViewer`); the wx and headless frontends each provide one.

wxHaskell layout rules that are easy to get wrong (see `UI.ProjectInfo` for a
worked example):

- Each layout has two independent properties: *stretch* (may its cell be given
  more room than it needs?) and *fill mode* (does it grow to fill its cell, or
  sit in it at its own size, aligned?). `expand` sets fill; `hstretch`/
  `vstretch`/`stretch` set stretch; `hfill = hstretch . expand`, `fill =
  stretch . expand`, `hfloatCentre = hstretch . alignCentre`.
- The stretch setters *replace* both directions: `hstretch` also turns
  vertical stretch off.
- `row` and `column` are grids, and a grid column widens only if *every* item
  in it stretches horizontally (rows likewise, vertically). One plain `label`
  in a `column` pins the whole column's width. Give items that shouldn't grow
  `hstretch` (or `hfloatCentre` etc.) so they get the room without filling it.
- Grids and `container` work out their own stretch from their children, so
  wrapping them in `fill` is unnecessary (and overrides that).

## Project File Format

Projects are YAML files with a single top-level `script:` key holding a unified
script made of `@config`, `@common`, `@viewer`, and `@tool` stanzas, plus an
optional `scenarios:` key for `fsrun`. (The older multi-key `setup:` /
`configuration:` / `viewer(s):` format is no longer supported.)

A `metadata:` key (`Actor.Metadata`, `ensembleMetadata`) holds a `title`, a
`description`, `preview-commands` (scenario commands, run by
`fsrun --update-preview`, the only `fsrun` mode that writes the project file),
and `preview` (a base64 PNG, from `Actor.Viewer.Preview.renderPreview`, also
used by the wx viewer menu's "Use this as preview"). Unknown keys under it
are kept. `encodeEnsemble` writes keys in a fixed, readable order (script,
metadata, scenarios, then the rest); see `projectYamlConfig`. A rebuild from
an edited script uses `carryOverProjectState` to keep scenarios, metadata,
and frontend keys.

The built-in project library is the top-level `library/` directory, one
subdirectory per section (an optional leading number orders the sections and
isn't shown). `FractalStream.Library` embeds it into the core at compile time
(Template Haskell has to run in a package without the LLVM backend, whose
static libraries GHC's loader can't handle); `LibrarySpec` checks every entry
has a title, description, preview, and preview commands, and no saved view.
After *adding or removing* a library file, run `stack build --force-dirty`
in each project you build (e.g. `fractalstream-ui-wx`; each has its own
work-dir, so its own copy of core's build): neither Stack nor GHC notices
otherwise, and `LibrarySpec` fails if the embedded copy is stale. The wx
welcome window (`UI.Welcome`) is the library browser;
opening an entry gives an untitled copy whose first save suggests its
filename.

Any other top-level key belongs to a frontend: the core keeps it verbatim in
`ensembleFrontendProperties` and `encodeEnsemble` writes it back out, but never
interprets it. The wx UI saves its window layout under `wx-properties:` (see
`UI.WxProperties`): for each viewer, by title, its window's `x`/`y`, image
`width`/`height`, `resizable`, `visible`, and its *current* `center`/
`pixel-size`; and the configuration window's `x`/`y`. A viewer's own
`center=`/`pixelSize=` attributes give only its *original* view, which "Original
view" returns to. Window size, position, and resizability are not part of the
script at all: `fsrun` renders at 600x600 unless a scenario says
`resize viewer to`. A rebuild from the script editor keeps every window's
layout and view (`rememberLayout`). Positions read from the project file are clamped onto the
primary display (`keepOnPrimaryDisplay`), since wxHaskell can't tell us about
any other display: a layout saved with a projector attached must still open
visibly elsewhere. Positions carried over within a session (rebuilds) aren't
clamped, so a window on a second display stays there.

```yaml
script: |
  @config "Configuration"
  C : C as "C =" <- -0.11 + 0.78i

  @viewer "Julia set" var=z
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else rainbow(iterations / 20)

  @tool "Trace" for="Julia set" shortcut=t
  on click-or-drag:
    erase
    draw point at z
scenarios:
  - name: simple
    commands: |
      capture viewer "Julia set" to "julia.png"
wx-properties:
  viewers:
    Julia set:
      width: 512
      height: 512
      center: [0, 0]
      pixel-size: 0.0078125
```


`@config` declarations always need a type (it determines the UI widget); code
in `@viewer` / `@tool` bodies can use `x := e` to have a type inferred.

Config statements (in `@config`, and at the top of `@tool`) can be grouped
with indented `row:` blocks (side by side) and `tab "Label":` blocks. Each
`tab` parses as its own one-page `ConfigTabs`, and `configStmts` merges runs
of consecutive ones, so the grammar stays unambiguous; a group becomes a
`Tabbed` layout (a wx notebook). Color fields re-parse their text like text
boxes do; they used to be frozen `constParsed` values, which silently
ignored the color picker and `set parameter`. A `button` is only allowed in
a `@tool` (`noButtons` rejects it in `@config`, since only a tool's own
panel delivers `ButtonPressed`).

Tool behavior in the wx viewer: `on timer` handlers run while the tool is
selected (`toolTimers`, from `toolBlockTimers` on the parsed blocks; the
`runningTimers` are stopped on deactivation and when `guardTool` gives up
on a failing tool). `on refresh` also runs when the selected tool's own
field values change (the 50 ms refresh timer compares `toolFieldValues`
with a snapshot taken after the last refresh); with `refreshCanUpdate=true`
the snapshot is taken before the refresh instead, so a refresh handler that
changes its own fields keeps running (an animation).

Tool handlers are type-checked at load: `buildEnsembleFromScript` runs
`checkViewerTools` (the same pure check `checkScript` uses) on every tool
before building, since `buildTool` only stores a tool's parsed handlers.

`for each x in <expr>` / `with first ... in <expr>` accept any list
expression: the parser (`withListVar`) stores a non-variable list in a
hidden variable named after its source position, since the typed AST needs
a list variable. `@`/`@@` parse at multiplication precedence.

`@common` stanzas (no title or attributes) hold declarations, function
definitions, and code definitions (`define`) that are prepended to every viewer body, prep script, and tool
handler (`withCommon`, via `CommonDefs{commonCode}`), and so are typechecked
separately in each. Only the definitions a script mentions (transitively,
by name; `stmtNames`) are prepended, so a definition using e.g. the viewer
coordinate doesn't break prep scripts or refresh handlers, which lack it.
`parseScript` parses `@common` stanzas first, then the rest, since the
others' grammars need the common statements and function names.

The older `@setup` stanzas, `«name»` splices, `using` formula fields, and
`code` blocks were removed in favor of `@common`; the Welcome screen's
template flow went with them (the old templates are now ordinary example
projects under `examples/templates/`).

## Scripting Language

See `LANGUAGE.md` for full docs. Key points:
- Assignment: `z <- z^2 + C`; new vars: `z : C <- 0`, or `z := 0` (type inferred)
- Functions: `let f(z) = z^2 + C` (inlined at each call); `f'(z)` = `diff(z, f(z))`
- Types: `Z` (integer), `R` (real), `C` (complex), `Color`, `Boolean`, `Text`, `List of T`
- Loops: `while`, `until`, `repeat...while`, `iterate z -> expr until/while cond`
- Special vars: `iterations` (loop count), `stuck` (hit iteration limit)
- Drawing commands: `draw point/line/circle/rectangle at ...`, `write "text" at ...`
- List ops: `@` (index), `@@` (cyclic index), `join`, `append`, `prepend`,
  `remove`, `transform`, `find`, `for each ... in`
- Unicode support: `←`, `⭠`, `→`, `⟼`, `π`, `𝑒`, `𝑖`, `ℤ`, `ℝ`, `ℂ`

## Key Files Quick Reference

| File | Purpose |
|------|---------|
| `fractalstream-core/src/Language/Type.hs` | `FSType`, `TypeProxy`, `HaskellType` |
| `fractalstream-core/src/Language/Environment.hs` | `Environment`, `Context`, `EnvironmentProxy` |
| `fractalstream-core/src/Language/Value.hs` | `Value`/`ValueF` AST, `IFunctor` instance |
| `fractalstream-core/src/Language/Code.hs` | `Code`/`CodeF` AST, `IFunctor` instance |
| `fractalstream-core/src/Language/Value/Typing.hs` | Bidirectional typing of surface values |
| `fractalstream-core/src/Language/Code/Infer.hs` | Type inference for `:=` declarations |
| `fractalstream-core/src/Data/Indexed/Functor.hs` | `IFunctor`, `ITraversable`, `FIX`, catamorphisms |
| `fractalstream-core/src/Data/DynamicValue.hs` | `Variable`, `Dynamic`, reactive system |
| `fractalstream-core/src/Data/Codec.hs` | Bidirectional YAML/JSON serialization |
| `fractalstream-core/src/Actor/Script.hs` | Unified-script (`@config`/`@common`/`@viewer`/`@tool`) grammar |
| `fractalstream-core/src/Actor/Ensemble.hs` | `Ensemble`, project loading, viewer setup |
| `fractalstream-core/src/Actor/Layout.hs` | `Layout` ADT, UI panel structure |
| `fractalstream-core/src/Actor/Viewer.hs` | `Viewer`, `ViewerCompiler`, rendering types |
| `fractalstream-core/src/Actor/Tool.hs` | `Tool`, event handler actions |
| `fractalstream-core/src/Event.hs` | `Event` type with typed environment args |
| `fractalstream-core/src/Language/Draw.hs` | `Draw` commands (DrawPoint, DrawLine, etc.) |
| `fractalstream-core/src/Language/Code/InterpretIO.hs` | Pure interpreter (IORef-based) |
| `fractalstream-backend-llvm/src/Backend/LLVM.hs` | LLVM JIT entry points |
| `fractalstream-backend-llvm/src/Backend/LLVM/Value.hs` | Value -> LLVM codegen |
| `fractalstream-backend-llvm/src/Backend/LLVM/Code.hs` | Code -> LLVM codegen |
| `fractalstream-ui-wx/src/Main.hs` | Application entry point |
| `fractalstream-ui-wx/src/UI/ProjectViewer.hs` | Renders ensemble as wx windows |
| `fractalstream-ui-wx/src/UI/Tile.hs` | Parallel tiled pixel rendering |

## Build Notes

- Each package has its own `stack.yaml` (resolver `lts-24.12`); build and test
  from a package directory, e.g. `cd fractalstream-core && stack test`.
  `cd fractalstream-ui-headless && stack test` runs the core, LLVM backend, and
  headless suites together. There is no top-level `stack.yaml`; CI builds the
  app from `fractalstream-ui-wx` (see `.github/workflows/`).
- wxHaskell is a local path dependency: `../wxHaskell/wx`, `../wxHaskell/wxcore`, etc.
- LLVM backend requires shared LLVM (`shared-llvm: true` flag on `llvm-hs`)
- `-threaded -rtsopts -with-rtsopts=-N` for parallel rendering
- GHC extensions used heavily: `DataKinds`, `GADTs`, `TypeFamilies`, `RankNTypes`,
  `KindSignatures`, `TypeOperators`, `ScopedTypeVariables`, `TypeApplications`
- `-Wall -Werror` enforced in all packages

## Current Branch

`define-functions-and-code` - user-defined functions, `:=` type inference, and the
surface-AST / typing / elaboration pipeline (main branch is `main`)
