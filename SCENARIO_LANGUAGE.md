# FractalStream scenario language

A scenario is a small, human-writable script that drives
`fsrun` through a project the same way a person would
drive the interactive app: pick a viewer, pick a tool, click and drag,
change parameters, and check what got drawn. It exists to let headless
runs do more than render one static picture -- to set up a scene with
tool-driven annotations and assert on the result, for use in tests or
batch image generation.

A scenario file does not name a project; the project is always a
separate CLI argument, so the same scenario vocabulary is meaningful
across any project. Commands can come from a standalone file:

```
fsrun project.fs --commands scenario.txt
```

or from a scenario embedded directly in the project file itself (see
"Embedding scenarios in a project file" below), selected by name:

```
fsrun project.fs --scenario "basic trace"
```

(A third, unrelated mode exists for the simple case of just rendering
one viewer with no tool interaction at all:
`fsrun project.fs --snapshot "<viewer name>" --output
file.png`.)

## Design principles

- **Current-context commands vs. explicitly-addressed commands.** Every
  command that *changes state or injects input* (selecting, configuring,
  resizing, clicking, dragging, ...) acts on the *currently selected*
  viewer and tool, set by `select viewer`/`select tool`. Every command
  that *observes or produces output* (`capture`, `assert`, `wait`) names
  its target viewer explicitly, since it isn't really "session state" --
  it's a one-off query. This mirrors how the two groups of commands
  actually map onto the implementation (`select` sets which `Viewer`/
  `Tool` subsequent calls use; `capture`/`assert` are independent calls
  that happen to take a `Viewer` as an argument).

- **Two coordinate spaces, used consistently by command, not by syntax.**
  `click`/`drag` coordinates are *model space* -- the same `(Double,
  Double)` coordinates a tool's event handler actually receives, and the
  same space `draw` commands use. `assert`'s pixel references are *pixel
  space* -- you're indexing into a rendered raster image. There's no
  wrapper syntax (`model (x,y)` vs `pixel (x,y)`) to mark this; each
  command's coordinate space is fixed by what the command means.

- **A model-space point can be written either way.** `(<x>, <y>)` is a
  Cartesian pair of real-valued expressions; a bare complex-valued
  expression like `1 + 2i` or `e^(2 pi i / 5)` is also accepted, and is
  the more natural spelling for a complex-plane viewer. Both forms
  resolve to the same underlying `(Double, Double)`. The one thing the
  complex form can't do is be *exactly* a two-item parenthesized pair
  with nothing else around it (`(1, 2)`) -- that specific shape always
  means Cartesian coordinates, since otherwise it would be genuinely
  ambiguous which reading was meant. Parentheses are fine anywhere else,
  including wrapping the whole thing (`(1 + 2i)`) or a sub-expression
  (`e^(2 pi i / 5)`).

- **Value expressions reuse the real scripting language**, not a
  separate ad hoc syntax. The right-hand side of `set parameter ... to
  ...` is parsed with the same value grammar `1 + 2i` already works in
  (`Language.Value.Parser`), including arithmetic and type coercion, and
  is resolved against the actual configuration variable's declared type
  only once a project is loaded (the same deferred-parse-then-typecheck
  split used throughout the scripting language).

- **RGB, not RGBA**, in scenario-facing color values. The renderer's
  internal buffers are RGBA (`Actor.Viewer.Overlay` needs a packeable
  pixel type for Rasterific), but nothing a scenario draws is ever
  partially transparent, so `assert ... is (r,g,b)` stays a 3-tuple and
  the alpha channel is dropped when reading a pixel back for comparison.

## Commands

### Selection (stateful)

| Command | Effect |
|---|---|
| `select viewer "<name>"` | Set the current viewer for all commands below. |
| `select tool "<name>"` | Set the current tool, within the current viewer. Fires `Deactivated` on whatever tool was previously active and `Activated` on this one, matching the live UI's tool-switch behavior. |
| `deselect tool` | Fire `Deactivated` on the current tool (if any) and return to no tool selected. |

### Configuration (current viewer/tool)

| Command | Effect |
|---|---|
| `set parameter <var> to <value-expr>` | Set an ensemble-level `configuration:` variable (e.g. `max_iters`, `C`). There is no separate "viewer parameter" concept: a `ComplexViewer` doesn't carry its own configuration panel distinct from the ensemble's -- `z-coord:`, `iteration-limit:`, etc. just name ensemble-level variables. |
| `set tool parameter <var> to <value-expr>` | Set a variable in the *current tool's own* `configuration:` panel. |

### Viewport (current viewer)

| Command | Effect |
|---|---|
| `resize viewer to <W>x<H>` | Change the viewer's pixel dimensions (`WxH`, e.g. `800x600`; viewers start at `600x600`). |
| `set viewer center to <point>` | Change the model-space point the view is centered on. |
| `set viewer pixel size to <value-expr>` | Change the model-space size of one pixel (zoom level). |

### Synthetic input (current viewer/tool)

| Command | `Actor.Event.Event` |
|---|---|
| `click at <point>` | `Click` |
| `double-click at <point>` | `DoubleClick` |
| `drag from <point> to <point>` | one `Drag`, then `DragDone` |
| `press button "<label>"` | `ButtonPressed` |
| `trigger refresh` | `Refresh` |
| `trigger timer "<name>"` | `Timer` |

`drag` synthesizes a single intermediate `Drag` position plus a final
`DragDone`, rather than a full path of incremental positions -- a real
drag in the live UI sends many `Drag` events as the mouse moves. That's
fine for tools that only look at start/end, but a tool accumulating
state across drag steps would see a jump. A multi-step form (`drag from
(...) to (...) via (...), (...)`) is a plausible later extension, not
built now.

No commands exist for `Motion`/`Resize`/`Pinch`/raw `MouseDown`/
`MouseUp` -- those event kinds aren't addressable from a tool script's
`on ...:` headers at all, so there's nothing for a scenario to target.

### Synchronization

| Command | Effect |
|---|---|
| `wait for viewer "<name>"` | Block until rendering settles. |

Reserved syntax, currently a no-op: today's headless rendering path
(`withRenderedViewerBuffer`/`runEnsemble`) is fully synchronous, with no
background tile-rendering threads the way the live UI's `UI.Tile` has.
Parsed and accepted so scenarios don't need to change if that becomes
untrue later.

### Output

| Command | Effect |
|---|---|
| `capture viewer "<name>" to "<file.png>"` | Render the named viewer, composite every tool's drawn layers on top (`Actor.Viewer.Overlay.compositeDrawLayersToFile`), and write a PNG. |
| `capture viewer "<name>" base to "<file.png>"` | Same, but the per-pixel script only -- no tool overlays. |

### Assertions

| Command | Effect |
|---|---|
| `assert viewer "<name>" pixel (<x>, <y>) is (<r>, <g>, <b>)` | Exact pixel-color match. |
| `assert viewer "<name>" pixel (<x>, <y>) is (<r>, <g>, <b>) within <tolerance>` | Match within a per-channel absolute tolerance -- the form to actually reach for, given anti-aliasing and pure-vs-LLVM backend rendering differences will produce off-by-ones. |

### Structure

- `#` line comments, matching the main scripting language's convention.
- Not in scope for v1, deliberately: control flow (loops, conditionals)
  within a single command sequence. A sequence is always just a flat
  list of commands, run in order.
- Multiple *named* sequences aren't a construct in the language itself
  (there's no way to define more than one inside a single standalone
  commands file) -- that's handled one layer up, by a project file's own
  `scenarios:` list (below), with the CLI selecting one by name.

## Example

```
select viewer "Mandelbrot set"
set parameter max_iters to 200

select tool "Trace"
click at -0.75 + 0.1i
drag from (-0.75, 0.1) to (-0.7, 0.15)
capture viewer "Mandelbrot set" to "trace.png"
assert viewer "Mandelbrot set" pixel (256, 256) is (0, 0, 0) within 4

deselect tool
resize viewer to 800x600
set viewer pixel size to 1/512
capture viewer "Mandelbrot set" base to "zoomed.png"
```

## Embedding scenarios in a project file

A project file can carry its own named scenarios directly, as a
top-level `scenarios:` list, each entry a `name:` and a `commands:`
block (the same command sequence syntax as a standalone file, as a YAML
block-literal string):

```yaml
viewer:
  title: Mandelbrot set
  ...

scenarios:
  - name: "basic trace"
    commands: |
      select viewer "Mandelbrot set"
      select tool "Trace"
      click at (-0.75, 0.1)
      capture viewer "Mandelbrot set" to "trace.png"
  - name: "zoomed view"
    commands: |
      select viewer "Mandelbrot set"
      resize viewer to 800x600
      set viewer pixel size to 1/512
      capture viewer "Mandelbrot set" base to "zoomed.png"
```

run with `fsrun project.fs --scenario "basic trace"`.
This is the natural home for a project's own regression\/smoke-test
scenarios -- checked in alongside the project file itself, rather than
kept as separate files that can drift out of sync with it. A project
with no `scenarios:` field just has none (`--scenario` reports which
names, if any, are available); a standalone commands file
(`--commands`) remains the way to run a one-off sequence that isn't
part of the project at all.

**`commands:` must use the `|` block-literal marker**, as above --
never a bare `commands:` followed by indented lines. Without `|`, YAML
folds a multi-line plain scalar onto one line (newlines become
spaces), silently merging every command in the scenario into a single
line of tokens. Depending on what's in it, the scenario language's
value-expression grammar can sometimes still parse that merged line as
one syntactically valid (but nonsensical) command -- e.g. `set
parameter C to 0` followed by a folded-in `capture viewer ...` has
been observed to parse as one `set parameter` command whose value
expression swallows the rest, running "successfully" while silently
never executing the capture at all. This is worse than it sounds: the
scenario reports success and gives no indication anything is wrong.
Always use `|`.

The project-file-level `scenarios:` field is parsed by
`fractalstream-core` (`Actor.Ensemble.NamedScenario`) as raw text --
the scenario language's own parser lives in
`fractalstream-ui-headless` (`Scenario.parseScenario`), specific to
that frontend, not something every consumer of a project file needs to
know about.
