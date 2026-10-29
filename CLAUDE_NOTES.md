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

`fsrun project.yaml --scenario NAME` runs a project's scripted scenario
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
- `ScriptBox` - embedded script editor

`layoutContext` converts a `Layout` into a `SomeContext DynamicValue` (live values for
each named variable in the layout).

### Configuration (`Actor.Configuration`)

`Configuration` bundles a title, size (`Dimensions`), and `Layout` into a panel.
Each `Ensemble` has an optional setup `Configuration` and an optional main `Configuration`.

### Ensemble (`Actor.Ensemble`)

An `Ensemble` is a loaded project:
- `ensembleSetup` - optional setup panel shown before the main UI
- `ensembleSplices` - values/code that `@setup` stanzas splice into the rest
- `ensembleConfiguration` - optional configuration panel
- `ensembleViewers` - list of `ComplexViewer`s
- `ensembleScenarios` - named scenarios (for `fsrun`)
- `ensembleSourceScript` - the raw `script:` text, written back verbatim on save

`buildEnsembleFromScript` builds one from a unified script (grammar in
`Actor.Script`); `parseEnsembleFromFile :: FilePath -> IO (Either String Ensemble)`
loads a `.yaml` file, and `encodeEnsemble` saves one.
`runEnsemble` / `runEnsembleFromSetup` launch the UI for an ensemble.

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
parameters, so each call is typechecked at its own argument types. The
grammar must know function names to parse `f(z)` as a call rather than an
implicit multiplication, so each stanza's tokens are pre-scanned for
`let NAME(` (`withDefinedFunctions`, stored in `Splices{funNames}`).

`Language.Value.Typecheck.atType` (typing + elaboration at a requested type)
is the entry point used throughout the actors.

Golden tests (`fractalstream-core/test/Actor/GoldenSpec.hs`, files in
`test/golden/`) pin the pretty-printed typed code of every example and
template; delete a golden file to regenerate it, and review the diff.

Key modules:
- `Language.Parser.Tokenizer` - tokenizer
- `Language.Parser.SourceRange` - source location tracking
- `Language.Value.Parser`, `Language.Code.Parser` - grammars
- `Language.Value.Syntax`, `Language.Code.Syntax` - untyped surface ASTs
- `Language.Value.Builtins` - builtin function/color tables, internal names
- `Language.Value.Typing` - typing pass for values
- `Language.Value.Elaborate` - typed surface AST -> `Value`
- `Language.Value.Typecheck` - `atType` entry point
- `Language.Code.Infer` - inference for `:=` declarations
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

## UI (`fractalstream-ui-wx`)

Built on `wxWidgets` via the `wx`/`wxcore`/`wxHaskell` Haskell bindings.

Key modules:
- `Main` - entry point; opens welcome screen, handles open/new/template
- `UI.Welcome` - welcome screen with recent files and templates
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

A frontend supplies a `UI` record (`Actor.UI`: `newEnsemble`, `runSetup`,
`makeLayout`, `makeViewer`, ...); the wx and headless frontends each provide one.

## Project File Format

Projects are YAML files with a single top-level `script:` key holding a unified
script made of `@setup`, `@config`, `@viewer`, and `@tool` stanzas, plus an
optional `scenarios:` key for `fsrun`. (The older multi-key `setup:` /
`configuration:` / `viewer(s):` format is no longer supported.)

```yaml
script: |
  @config "Configuration"
  C : C as "C =" <- -0.11 + 0.78i

  @viewer "Julia set" var=z size=512x512
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
```

`@config` declarations always need a type (it determines the UI widget); code
in `@viewer` / `@tool` bodies can use `x := e` to have a type inferred.

## Scripting Language

See `LANGUAGE.md` for full docs. Key points:
- Assignment: `z <- z^2 + C`; new vars: `z : C <- 0`, or `z := 0` (type inferred)
- Functions: `let f(z) = z^2 + C` (inlined at each call)
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
| `fractalstream-core/src/Actor/Script.hs` | Unified-script (`@setup`/`@config`/`@viewer`/`@tool`) grammar |
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
