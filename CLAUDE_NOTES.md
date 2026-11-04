# FractalStream - Developer Notes for Claude

## Project Overview

FractalStream is a Haskell application for studying real and complex dynamical systems.
Users author scripts in a custom language to define dynamical systems, and can build
customizable UIs around them with configuration panels and interactive tools.
Projects are saved as YAML files.

## Package Structure

Six Cabal packages, built with Stack (GHC 9.10.x):

```
fractalstream-core/           # Core library: ASTs, parsing, type system, actors, dynamic values
fractalstream-backend-pure/   # Pure Haskell interpreter backend
fractalstream-backend-llvm/   # LLVM JIT compiler backend (default)
fractalstream-library/        # The built-in project library (projects/), compiled in
fractalstream-ui-wx/          # wxWidgets UI (the main executable: FractalStream)
fractalstream-ui-headless/    # Headless frontend (`fsrun`), no GUI dependencies
```

`fsrun scenario --project project.fs --name NAME` runs a project's scripted scenario
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

Some color syntax has no node of its own: elaboration (`Language.Value.Elaborate`)
rewrites it into simpler values, binding its arguments with `LocalLet` under
`#`-prefixed names no script can write. `cycle(t, c1, c2)` (`cycleColors`) and the
wheels like `rainbow(t)` (`wheel`) become `Blend`s; `hsv(h, s, v)` and `hsl(h, s, l)`
(surface `EColor3 OpHSV/OpHSL`; `hueColor`) become an `RGB` of real arithmetic,
binding the arguments, then the hue (×6, mod 6) and the clamped saturation and
value/lightness, so nothing is computed twice. `hsl` matches `Data.Color.hslToColor`
exactly; `hsv` agrees with `hsvToColor` to within 1 per channel (a different, but
equal, formula; `hsvToColor` is left alone since autocolor picks its hues with it).

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
- `ActionButton` - a button that does something in the app (an `AppAction`, such as
  `OpenAutocolorSettings`) rather than run a tool's `on button` handler; it comes
  from a `ConfigAction` statement, which no script can write (the built-in Autocolor
  colorizer adds one), and `UI.Layout.generateWxLayout` is given its handler

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

### Colorizers (`Actor.Colorizer`)

A colorizer is a named piece of viewer code that sets `color`, run at the
end of a viewer's body, so a viewer script can just say what to compute.
Built-ins (`builtinColorizers`): Manual (no code), Rainbow, Escape time,
Smooth, Binary decomposition, Glow, Autocolor (always group 0, since a
colorizer parameter would be a variable, not the literal group number
`autocolor` needs; its `allow_new` setting switches to `get_autocolor`'s blend,
for a parameter plane, or `nearest_autocolor` with `blend_misses` off --
not `blend`, which is a built-in function's name); each is written as `@colorizer` source
text, parsed by `parseScript` (the test suite builds every one). A script
adds its own with `@colorizer "Name"` stanzas (`ColorizerStanza`: config
declarations, then `define colorize(params):`); one with a built-in's name
replaces it, with a warning on stderr.

`applyColorizers` is a stanza-to-stanza pass that runs right after
`parseScript` in both `buildEnsembleWithColorizers` and
`checkScriptWithColorizers` (`buildEnsembleFromScript`/`checkScript` are
these with no interactive choices). For each viewer it picks the colorizer
(interactive choice by title, from `colorizers:`; else `colorizer=`; else
Manual if the body assigns `color` (`setsColor`, after `define`/`run`
expansion) and Rainbow if not), appends its code to the viewer's code, and
adds its settings as a `ConfigStanza` (tab "Coloring", or "<title>
coloring" with several viewers). Every name the colorizer declares
(settings, parameters, locals) is renamed `[colorizer] <title>: <name>`,
so it can't clash with the script's names and each viewer has its own
settings. Its `@common` dependencies (`czCommon`) are added unless the
viewer already has them. A parameter without an argument gets its
default (`t = iterations / period`, read in the colorizer's renamed
scope), else the viewer's variable of the same name (read in the
viewer's scope; `ViewerColorizer.colorizerScope` approximates what's in
scope, for the wx menu). Arguments are split at top-level commas
(`splitArguments`) and parsed with `argumentGrammarWithFunctions`, which
excludes top-level pairs (`a, b`), with the viewer's function names. A
viewer's `coloring:` block (`vsColoring`) replaces the default colorizer's
setting defaults (`configure`; a choice by its quoted label), only when
that colorizer is in use. Built-ins' source ranges are stripped; a script
colorizer keeps its own, so its errors point into its stanza.

The ensemble keeps the interactive choices (`ensembleColorizerChoices`),
how each viewer is colorized (`ensembleViewerColorizers`: in use, default,
scope), and the available colorizers (`ensembleAvailableColorizers`).
Changing a choice means building the project again.

### Autocolor (`Actor.Autocolor`)

`autocolor(n, p)`, `nearest_autocolor(n, p)`, and `get_autocolor(n, p)`
(surface `EAutocolor`, value `Autocolor AutocolorMode Int`; the mode is
in `Language.Type`, since the language can't depend on `Actor`) look point
`p` up in autocolor group `n` (a literal; typing rejects anything else,
via `integerLiteral`), giving the color of the nearest entry within the
group's tolerance. Otherwise: `autocolor` adds a new entry, with the next
golden-angle hue (unless the group is locked or has
`maxAutocolorEntries` = 256 entries, when it gives `get_autocolor`'s blend,
or the miss color if the group's `agBlendMisses` is off -- saved as
`blend-misses: false`, published in the C snapshot's group record);
`nearest_autocolor` gives the group's `agMissColor`; `get_autocolor`
gives a blend of every entry's color weighted by 1/distance² (the miss
color for an empty group), computed identically in C (`fs_ac_lookup`)
and Haskell (`blendAutocolor`), rounding half up. Lookups read the table without
the lock; a miss takes `agTableWriteMutex` and looks again before
inserting, so concurrent misses can't insert near-duplicates.

**Not deterministic:** which point gets which hue depends on the order
points are first seen, which varies with tile scheduling and threads, so
an unsaved project's colors (and so `fsrun` output from a project
without an `autocolor:` key) can differ from run to run. Saved tables
(and locked groups) make them stable.

The `Ensemble` owns an `AutocolorState` (`ensembleAutocolor`, carried
over by `carryOverProjectState`, saved under a core-owned `autocolor:`
key that `fsrun` honors). Backends get an `AutocolorLookup`: the Haskell
lookup function, and `autocolorCache`, a snapshot directory for compiled
code. `withCompiledViewer` and `runTool` take one.

Compiled code doesn't call back into Haskell for each lookup (that
roughly doubled render times): viewer kernels (`#autocolor`,
`#autocolorDir`, after `#issuesOut`) and tool handlers (`autocolor`,
`autocolor.dir`, after `text.format`) get a pointer to the C function
`fs_ac_lookup` (`src/Actor/autocolor_cache.c`) and the directory, reached
through `ArenaState.asAutocolor`. It reads an immutable snapshot of every
group (points with their colors, squared tolerance, miss color, and
whether the group can add a point) without any lock, and calls back into
Haskell only to add a point it doesn't have. The Haskell side publishes a
new snapshot (`publishAutocolor`, under `acPublishLock` so the newest
state always wins) after every change: a point added, a group added, any
edit, a project file read. A replaced snapshot is freed once no lookup can
still be reading it, by a small epoch scheme (see the comment at the top
of `autocolor_cache.c`): a lookup registers with the current epoch's
parity before loading the snapshot and leaves before any callback, and
`fs_ac_publish` advances the epoch and waits for the old parity's lookups
(each a single, non-blocking lookup) before freeing. The directory, with
its last snapshot and the callback, is freed by its `ForeignPtr`'s
finalizer. `fs_ac_debug_quarantine` (for the stress test in
`AutocolorSpec`, which runs lookups on several threads, so the core's test
suite is built `-threaded`) poisons replaced snapshots and keeps them,
so that a lookup reading one too late is caught instead of masked by
malloc reusing the memory. A group's "full" warning is printed when it fills up, since afterwards
compiled code no longer asks.

The pure evaluator can't compute autocolor (it's pure); it raises an
error. Viewers redraw when `autocolorEdits` changes: at once for an edit
(an entry recolored, moved, added by hand, or deleted; a group cleared,
locked or unlocked, or its tolerance or miss color changed), and 200 ms
after a render adds points, once for the whole burst (`pointAdded`). That
redraws a viewer that only reads (a parameter plane with
`allow_new <- false`) once another viewer has found the points it needs;
the viewer that added them finds nothing new the second time.

In the `autocolor:` key, points are complex literals in ASCII
(`showAutocolorPoint`: 10 decimals, |x| < 1e-9 written as 0; a plain
YAML number is accepted too) and colors are written by `toUserString`,
both read back with `parseConstant`. The wx autocolor window
(`UI.AutocolorEditor`, one per project, from "Autocolor settings..." in
the viewer's Colorizer menu, or the Autocolor colorizer's "Open autocolor
settings..." button: a `ConfigAction OpenAutocolorSettings` statement,
which no script can write, that becomes a `Layout` `ActionButton`, run by
`generateWxLayout`'s `AppAction` handler) shows a group
selector, each entry's point (edited in place, committed on Enter or
leaving the field) and color picker with a Delete button, an add-a-point
field, "Clear all", a lock checkbox, the group's tolerance (a positive
real, committed like a point), and its miss color. It watches the list of
groups and each group's table with `wxWatchDynamic` (tables change on
render threads; the watches run on the GUI thread; each table is watched
once, from when its group appears, for the window's life), and rebuilds
its list only when the selected group's set of points changes, so a field
being edited isn't wiped by a render that adds points elsewhere.

The LLVM backend compiles an expression `if c then x else y` (`ITE`) as a
branch, with each side in its own block and a `phi` per operand
(`mergeBranches`), not as `select` over both sides computed: `autocolor`
has a side effect, so the side not taken mustn't run. (With `select`,
`if stuck then black else autocolor(0, z)` added every stuck point.)
LLVM's optimizer turns branches between cheap, pure values back into
`select`s.

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

`x stops` in a loop's condition is rewritten away next, by
`Language.Code.Stops.expandStops`, so typing never sees it: the loop gets
a fresh `[internal] stops.prev #k := x` (and, for loops that check first,
an `[internal] stops.started #k := false`), set at the start of each pass,
and the condition compares `x = prev` (`started and x = prev`). A `stops`
left anywhere else reaches typing, which reports it.

`Language.Value.Typecheck.atType` (typing + elaboration at a requested type)
is the entry point used throughout the actors.

Golden tests (`fractalstream-core/test/Actor/GoldenSpec.hs`, files in
`test/golden/`) pin the pretty-printed typed code of every example and every
library project (`fractalstream-library/projects/`); delete a golden file to
regenerate it, and review the diff.

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
- `Language.Code.Stops` - `x stops` in loop conditions, rewritten away
- `Language.Legacy.Syntax`, `.Parser`, `.Translate` - scripts for the
  original (2009, Mac OS X) FractalStream, and their translation into a
  project (`fsrun translate`); see `Language.Legacy.Translate`'s header
  for how each part maps. Tested by `test/Language/LegacySpec.hs` against
  `test/legacy/` (each guide example's `.txt` and its expected `.fs`:
  delete a `.fs` to regenerate it). `{real}` scripts aren't translated yet.
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
- `Language.Editor.Highlight` - classifies a script's spans for coloring
  (keywords, literals, and identifiers by what they name: parameter,
  variable, function, code block, or undefined), from tokens and the
  definitions it can see, so it works on scripts that don't parse.
  `highlightsWith` takes names defined elsewhere (a movie's script).
  `UI.EditorStyle` gives each class its own Scintilla style (some share
  colors by default); `UI.Widgets.scriptWidget` applies them.
- `UI.EditorKeys` - code editor keys: Return indents the new line
  (`Language.Editor.newLineIndent`; the indent unit is 2), paste
  re-indents (`pasteIndented`), Cmd/Ctrl+/ toggles comments
  (`toggleComments`), and emacs movement keys, always on macOS and the
  `emacs-keys` setting elsewhere. Return, paste and comment keys are
  handled on key-down (Scintilla acts on Return then, with no character
  event after); on macOS the slash key goes down as `KeyDivide`. On macOS, Control-letter reaches
  the key handler as its control character (Control-E is ENQ, 5) with no
  modifiers set, so control codes 1-26 are read as Control-letters.
- Errors in code editors are underlined with indicator 8, a red squiggle
  (`UI.CodeEditor.markError`/`clearError`). wxHaskell's own indicator
  functions are for Scintilla's old style-bit indicators, which it no
  longer draws; the modern calls go through `styledTextCtrlSendMsg`,
  added to our wxHaskell fork (any Scintilla message with integer
  arguments, by its number from Scintilla.h). wxcore's bindings are
  generated from the *installed* wxc headers when wxcore is configured,
  and stack caches wxcore by its own sources: a new wxc function needs
  wxc reinstalled, and a change under `wxcore/` for stack to rebuild it.
- `UI.AppSettings` - the app-wide `Variable Settings` (`appSettings`),
  set by `Main` at startup and changed by the Preferences window
- Code editor folds (`UI.CodeEditor.updateFoldLevels`): stanza headers
  and tool handlers' `on ...:` lines always fold; other blocks only with
  the `fold-code-blocks` setting. `scriptWidget` watches `appSettings` and
  calls `refreshFolds` when it changes, opening folded blocks first (a
  fold point that goes away would otherwise leave its lines hidden).
- `UI.Layout` - wx layout helpers
- `UI.Tile` - tiled rendering of viewer pixels (parallelized)
- `UI.Menu` - menu bar construction
- `UI.Session` - session management (open windows)

A frontend supplies a `UI` record (`Actor.UI`: `newEnsemble`, `makeLayout`,
`makeViewer`); the wx and headless frontends each provide one.

Cancellation-safe cleanup: a redraw cancels the tile's render (every
worker in its `forPool_`), so render code must give back what it holds
however it's interrupted. Cleanup that can block (a `writeChan`, a
`takeMVar`) is an interruptible point even under `bracket`'s mask, where
an async exception abandons it: the LLVM viewer's arena pool returns each
arena under `uninterruptibleMask_` (before, cancelled renders slowly lost
arenas, and quitting hung in `withJIT`'s teardown, which collects them all
back), and `Task.Concurrent`'s `with` / `synchedWith` (the tile buffer's
lock) register and unregister uninterruptibly, with the unregistering in
`finally` (before, a cancelled worker could leave the buffer locked for
good). `LLVMSpec` ("gets every arena back") and `Task.ConcurrentSpec`
cancel users at random, hundreds of times, and fail (by timeout) without
these fixes.

Destroy handlers: `wxWindowDestroyEvent` is a command event, so it
propagates up from child windows (stopping at a top-level window), and a
plain `windowOnDestroy` handler runs whenever *any* child of the window is
destroyed, not just the window itself. Use `UI.Widgets.onOwnDestroy`,
which checks the event's source and chains the handlers installed before
it; every destroy hook in the app does (including those in
`wxWatchDynamic` and `wxTimer`). Before it, a window that rebuilt some of
its widgets (the autocolor window's list) silently stopped every watch and
timer on it, since their cleanup ran on the first child destroyed. Mixing
in a plain handler would bring that back: it would run on a child's
destruction and call the chained cleanups unconditionally.

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

### Movies

A movie of a viewer is made by stepping an *animation* (`Actor.Movie`,
`Animation = Double -> IO ()`: put the project's values where they
should be at time `t` in [0, 1]) and rendering a frame after each step.
`scriptAnimation` takes a script with a `define animate(t):` block
(any parameter name), and appends `run animate([movie] time)`, binding
the time under that unwritable name, so `define`/`run` expansion gives
the parameter a fresh name and it can't clash with the project's (a
config variable named `t`, say). Otherwise it treats the code as a
tool's `on refresh` handler with one more variable: it parses it with the project's
`CommonDefs` (`ensembleCommonDefs`, from the script's `@common` stanzas
via `Actor.Script.scriptCommonDefs`), prepends the common code it uses
(`withCommon`), and typechecks it with `Actor.Event.parseUnitScript`
against the configuration context plus a real `t` (bound with
`constArg`; an existing variable of that name is an error), which adds
the bookkeeping `iterate` needs, with limits from `max_iters`,
`escape_radius`, and `vanishing_radius`. It runs with
`interpretedToolExec` and a no-op `DrawSink`, so it assigns through the
arguments' setters as a tool does. `checkMovieScript` is the same check,
pure, so the movie dialog's editor is `UI.Widgets.scriptWidget` (the
project script editor's), with live error display, driven by a `Mapped`
over it, as the script editor's is by `checkScriptWithColorizers`. `pathAnimation` moves one complex variable along a polyline by
arclength (`pathPointAt`). `recordMovie` renders from a `cloneViewer`
copy (so a window resized mid-recording can't change the frame size) with
`renderViewerImage`, which adds tool drawings (if asked for) to
`Actor.Viewer.renderViewerBuffer`'s render. That renders in horizontal
strips over every capability with `forPool_`, downsampling each strip as
it goes; `withRenderedViewerBuffer` (snapshots, `fsrun` captures,
previews) uses it too, so `fsrun` renders on every core unless `-j N`
says otherwise (`-j 1` for repeatable colors from unlocked autocolor
groups, which depend on render order). `compositeDrawLayers` rasterizes
drawings onto a transparent canvas and lays it over the base image (not
Rasterific's `drawImage`, which resamples it half a pixel off), and
skips Rasterific entirely when there are no drawings. `withFieldsRestored` puts every field's *text* back afterwards
(so `1/3` stays `1/3`).

`Data.Movie` writes the frames as Motion-JPEG (JuicyPixels' encoder) into
QuickTime `.mov` (macOS's default, `defaultMovieFormat`) or AVI (`.avi`,
elsewhere), with no external encoder; frames stream to disk and the
indexes are written at the end (32-bit offsets, so under 4 GB). The `.mov`
was checked with AVFoundation (`avconvert`, `AVAssetImageGenerator`); the
AVI only structurally, not yet in a Windows or Linux player.

In the wx app (`UI.MovieMaker`), "Capture > Movie..." in a viewer's menu
(next to "Capture > Image...", the old "Take a picture...")
asks for a script, and the built-in "Animate along path" tool
(`MoviePathTool`, next to Navigate and Debug, in viewers whose coordinate
is a complex configuration variable, i.e. those with a "Select ..."
tool) records a dragged path in model coordinates and then asks how to
make a movie moving that variable along it (defaulting to recording
another viewer; "Loop back to start" closes the path with a straight
line and leaves out the last frame; "Show ... as an inset" passes a
`MovieInset` of the dragged viewer to `recordMovie`, which renders it
once, at its current view (`prepareInset`, like a preview), and puts it
in the chosen corner of each frame (`InsetCorner`, a choice beside the
dialog's checkbox) with an inverted-color crosshair at
`pathPoint`, the same point `pathAnimation` sets: `withInset`). Any of
the project's viewers can be recorded: windows register themselves in
the session's `MovieSupport` (`pmMovies`), which also remembers the last
dialog's settings. Recording runs on a background thread (setting values
from there is safe: widgets pick changes up on GUI-thread timers), into a
temporary folder, and the result opens in the system's player
(`openWithDefaultApp`); the progress window then offers Play/Save as.
Cancelling marks the recording finished *before* cancelling it: wx
destroys the window only after the current event, so its progress timer
can fire once more in between, and used to report the worker's
`AsyncCancelled` as a failure (a cancelled worker is never reported).
It's an ordinary frame, not a tool window, since macOS hides tool windows
while another app (the movie player) is in front. Recording doesn't count
as an unsaved change (`pmUnsaved` is put back). The Animate along path tool's
mouse handlers must `propagateEvent`, like Navigate's: without it, wx
on macOS never delivers the drag.

`fsrun` has `record viewer ...` (see `SCENARIO_LANGUAGE.md`).

FractalStream's own settings (`FractalStream.Settings`) are a plain YAML file,
not wx's config: `~/Library/Application Support/FractalStream/settings.yaml`
on macOS, `%APPDATA%\FractalStream\settings.yaml` on Windows, and
`~/.config/fractalstream/settings.yaml` elsewhere. Every setting has a default
and is left out of the file when it's the default; anything unreadable is a
warning and the default; unknown keys are kept. So far there's one setting,
`projects-folder` (the library browser's "My projects"; default
`~/Documents/FractalStream`, see `FractalStream.Library.projectsDirectory`).
The wx app's Preferences window (`UI.Preferences`, under the standard
preferences menu id, so in the app menu on macOS) saves each change at once.

## Project File Format

Projects are YAML files with a single top-level `script:` key holding a unified
script made of `@config`, `@common`, `@viewer`, and `@tool` stanzas, plus an
optional `scenarios:` key for `fsrun`. (The older multi-key `setup:` /
`configuration:` / `viewer(s):` format is no longer supported.)

A `metadata:` key (`Actor.Metadata`, `ensembleMetadata`) holds a `title`, a
`description`, `preview-commands` (scenario commands, run by
`fsrun update-preview`, the only `fsrun` mode that writes the project file),
and `preview` (a base64 PNG, from `Actor.Viewer.Preview.renderPreview`, also
used by the wx viewer menu's "Use this as preview"). Unknown keys under it
are kept. `encodeEnsemble` writes keys in a fixed, readable order (script,
metadata, colorizers, autocolor, scenarios, then the rest); see `projectYamlConfig`. A rebuild from
an edited script uses `carryOverProjectState` to keep scenarios, metadata,
and frontend keys.

The built-in project library is `fractalstream-library/projects/`, one
subdirectory per section (an optional leading number orders the sections and
isn't shown). `FractalStream.Library` embeds it at compile time; it's a package
of its own because Template Haskell has to run in a package without the LLVM
backend (whose static libraries GHC's loader can't handle), and so that `fsrun`
doesn't carry it. Its cabal file lists `projects/**/*.fs` as extra source
files, so adding, removing, or changing a project rebuilds it automatically.
`LibrarySpec` (in that package: `cd fractalstream-library && stack test`)
checks every entry has a title, description, preview, and preview commands
(except a hand-drawn preview listed in `handMadePreviews`, like the Blank
project's icon), and no saved view, and that the embedded copy matches the
directory. Previews may be transparent; the wx UI draws them over its
background color.

The wx welcome window (`UI.Welcome`) is the library browser. Opening an entry
gives an untitled copy whose first save suggests its filename.

A project whose script doesn't build still opens (`loadProjectBytes` /
`loadProjectFile` give a `BrokenScript` with an ensemble that has the script's
text and the file's other keys, but `ensembleBuilt = False`): the wx app opens
it straight into the script editor, and applies its saved layout and values
when the fixed script is rebuilt (`carryOverProjectState`). Until then, saving
writes the edited script and keeps the layout and values as the file had them.
Only a file that can't be read as a project at all fails to open.

The browser also shows the user's own projects, from `~/Documents/FractalStream`
(`myProjectsDirectory`, via `getUserDocumentsDirectory`), laid out like the
library: files directly in it under "My projects", each subfolder a section.
`scanProjectsIn` reads it at run time: lazily, when one of those sections is
first selected (reading Documents can make macOS ask permission, which should
happen in context), and again when the window is brought to the front. These
open in place; one without metadata is titled by its filename and shown with
`placeholderPreview`; one that can't be read, or whose script has an error
(`checkProjectBytes`, which type-checks without building), is listed with the
reason. A
project without a file is first saved into that folder by default. The section
called "Starting points" is special (`lsStartingPoints`): its projects are
templates, so opening one gives a new "Untitled" project without the
template's metadata, with the script editor open; "New project" shows that
section.

A `values:` key (`Actor.SavedValues`) holds the configuration values a
project was left with, where they differ (by value) from its script's
defaults: `config:` by variable name, and `tool:` by viewer title, then tool
name, then variable name (a tool without `for=` has separate values in each
viewer). Values are the text entered in each field (`Actor.Layout.Field`).
Like views, these are where a project was left, so the wx app restores them
(`applySavedValues`, before the first render) and saves them (`changedValues`),
while `fsrun` ignores them; the core keeps the key with the frontend keys. An
ensemble remembers its defaults when built (`ensembleValueDefaults`);
`carryOverProjectState` carries changed values through a rebuild, and
`resetValues` (the config window's "Reset to defaults") goes back to them.
Library projects may not carry `values:` (`LibrarySpec`).

A `colorizers:` key maps viewer titles to the colorizers chosen
interactively (`ensembleColorizerChoices`; see "Colorizers" above),
written as chosen: `Smooth`, `Rainbow(arg z / (2 pi))`. Unlike `values:`,
this is part of what the project draws, so the core reads it and `fsrun`
honors it. Only choices that differ from a viewer's default are saved
(choosing the default removes the entry); an unreadable or unusable entry
is a warning and the viewer's default, and is kept. Entries for viewers
the script no longer has are dropped on build. A library project should
use `colorizer=` and `coloring:` in its script instead, so that "Reset to
defaults" and the preview agree. Scenarios set a colorizer's settings with
`set colorizer parameter <name> to <value or "choice">`, for the selected
viewer (the hidden name is `[colorizer] <title>: <name>`).

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

Colorizers in the wx viewer: the hamburger menu's "Colorizer" submenu
lists `ensembleAvailableColorizers`, checking the one in use. Picking one
whose parameters all have defaults or same-named variables in scope
switches at once; otherwise its item says "..." and it asks for the
arguments (`askColorizerArguments`: one field per parameter, an empty
field meaning its default or same-named variable). "Arguments..." edits
the one in use. Switching calls `pmSetColorizer`, which rebuilds the
project with `UI.ProjectScriptEditor.rebuildColorizers`: it builds at once
(an error is a dialog and leaves the project alone), then replaces the
windows from a one-shot timer, since the menu's own viewer is among them,
leaving an open script editor (`sehEditor`) and its draft in place. The
config window's "Reset to defaults" also clears the interactive choices
(rebuilding first, then resetting values). The script editor's Rebuild
and live check keep the current choices.

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
template flow went with them (the old templates are now the library's
"Starting points", in `fractalstream-library/projects/0 Starting points/`).

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
- Colors: named colors, `rgb`/`hsv`/`hsl`, `blend`, `cycle`, wheels (`rainbow(t)`, ...),
  and `autocolor`/`nearest_autocolor`/`get_autocolor` (see "Autocolor" above)
- A viewer that doesn't set `color` is colored by a colorizer (see "Colorizers"
  above): `colorizer=` in its header, `coloring:` blocks, `@colorizer` stanzas

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
| `fractalstream-core/src/Actor/Script.hs` | Unified-script (`@config`/`@common`/`@viewer`/`@tool`/`@colorizer`) grammar |
| `fractalstream-core/src/Actor/Colorizer.hs` | Built-in colorizers, `applyColorizers` |
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
| `fractalstream-core/src/Actor/Movie.hs` | Movie animations and recording |
| `fractalstream-core/src/Data/Movie.hs` | Motion-JPEG `.mov`/`.avi` writer |
| `fractalstream-ui-wx/src/UI/MovieMaker.hs` | Movie dialog and progress window |

## Build Notes

- Each package has its own `stack.yaml` (resolver `lts-24.12`); build and test
  from a package directory, e.g. `cd fractalstream-core && stack test`.
  `cd fractalstream-ui-headless && stack test` runs the core, LLVM backend, and
  headless suites together. There is no top-level `stack.yaml`; CI builds the
  app from `fractalstream-ui-wx` (see `.github/workflows/`).
- wxHaskell is a local path dependency: `../wxHaskell/wx`, `../wxHaskell/wxcore`, etc.
- LLVM backend requires shared LLVM (`shared-llvm: true` flag on `llvm-hs`)
- `-threaded -rtsopts -with-rtsopts=-N` for parallel rendering (the wx app and `fsrun`)
- GHC extensions used heavily: `DataKinds`, `GADTs`, `TypeFamilies`, `RankNTypes`,
  `KindSignatures`, `TypeOperators`, `ScopedTypeVariables`, `TypeApplications`
- `-Wall -Werror` enforced in all packages

## Deferred Work

Things looked into and deliberately put off, with what was found, for whoever
picks them up:

- **Light / dark / follow-the-system appearance setting.** Wanted as a
  Preferences choice (the settings file, `FractalStream.Settings`, is ready
  for another key). Every platform's build uses wxWidgets 3.2, which can't
  set an app's appearance: `wxApp::SetAppearance` arrived in 3.3, and 3.2 on
  Windows has no dark mode at all (wxHaskell exposes nothing for it either).
  Options found: on 3.2, set the app's `NSAppearance` on macOS through a
  little Objective-C runtime FFI (applies immediately), and `GTK_THEME` on
  Linux before wx starts (next launch, and only for themes with a dark
  variant), with no setting offered on Windows; or move every build to
  wxWidgets 3.3 (CI and the macOS static build; check wxHaskell against it)
  and use `SetAppearance` everywhere. Either way, the viewer's on-screen
  overlay hard-codes dark (`isDarkMode = True` in `UI.ProjectViewer`) and
  must follow the chosen appearance too.
- **Asking to save when macOS quits the app by other means.** The Quit
  menu item (Cmd+Q) offers to save each project with unsaved changes
  (`quitApp` in `Main.hs`), but quitting from the Dock, or at logout or
  shutdown, doesn't go through it: wx closes the windows directly
  (`wxApp::OSXOnShouldTerminate`), and nothing asks. Covering those needs
  a hook into that application-level request, which wxHaskell doesn't
  expose (likely a binding to add to the wxHaskell fork).
- **Better parse error messages.** Errors like "I got stuck and couldn't
  parse the input here, where I was expecting :" (for a typo like `letz`)
  say little; improving them probably needs changes to the Earley package.

## Current Branch

`movie-tool` (main branch is `main`):
- movies of a viewer (see "Movies" under the UI): `Actor.Movie`, the `Data.Movie`
  Motion-JPEG writer, the wx "Capture > Movie..." dialog and "Animate along path" tool, and
  `fsrun`'s `record viewer` command
