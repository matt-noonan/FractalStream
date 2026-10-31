---
name: fractalstream-scripts
description: Write, fix, or explain FractalStream project scripts (the @config/@common/@viewer/@tool language for exploring real and complex dynamical systems), including implementing an algorithm from a paper as a FractalStream project and checking it headlessly. Use whenever a task involves a FractalStream .fs project or its scripting language.
---

# Writing FractalStream scripts

FractalStream is an app for exploring dynamical systems. A *project* is a
YAML file whose `script:` holds stanzas:

- `@config` declares user-adjustable parameters (with UI widgets),
- `@common` holds shared definitions,
- `@viewer` colors every pixel of a window over the complex plane,
- `@tool` reacts to clicks and drags on a viewer, draws overlays, and can
  change parameters.

The language is small, typed, indentation-based, and math-flavored
(`z^2 + C`, `2 pi i`, `|z|`, `f'(z)`). Read `reference.md` (next to this
file) before writing anything non-trivial; it is the complete, verified
language reference. `examples/` holds tested projects to copy from, and the
repo's `examples/*.fs` has ~35 more (spider, external rays, Newton,
Hénon, bifurcation diagrams...).

## Workflow

1. **Understand the math first.** Identify the map(s), the parameter
   space, which planes are worth viewing (parameter plane? dynamical plane?
   both?), what each pixel's color should mean, and what interaction would
   help (trace an orbit, pick a parameter, draw rays, mark periodic points).
2. **Map it onto FractalStream** (see "From a paper to a project" below).
3. **Write the project** in one `.fs` file. Start from the skeleton below
   or from the closest example.
4. **Check it headlessly**, every time you change it:

   ```
   python3 .claude/skills/fractalstream-scripts/check.py project.fs [outdir]
   ```

   This renders every viewer, selects every tool and exercises it with
   clicks, drags and refreshes (catching runtime errors such as a bad list
   index), and prints errors, magenta-pixel warnings, and the PNGs it
   wrote. **Look at the PNGs**
   (read them as images). A script that runs is not necessarily right:
   compare against what the paper's figures or the math predict. For
   finer checks, write a `--commands` file with `set parameter`, `click`,
   `capture`, and `assert viewer ... pixel (x, y) is (r, g, b) within 8`
   (see reference.md §5).
5. **Report** what you implemented, any deviation from the source (e.g.
   approximations, iteration limits, simplifications), and how to use the
   tools. If it helps, add a `scenarios:` entry so the user can re-run your
   check.

If `fsrun` isn't built: `cd fractalstream-ui-headless && stack build`
(don't pass `--fast`; it fails on the embedded font).

## Skeleton

```yaml
script: |
  @config "Parameters"
  C : C as "C =" <- -0.12 + 0.75i

  @common
  let f(z) = z^2 + C

  @viewer "Dynamical plane" center=0 pixelSize=1/150
  iterate z -> f(z) until z escapes
  color <- if stuck then black else cycle(iterations / 20, dark blue, white)

  @tool "Trace" for="Dynamical plane" shortcut=t
  on click-or-drag at w:
    erase
    use yellow for stroke
    use yellow for fill
    prev := w
    repeat up to 100 times:
      draw point at w
      prev <- w
      w <- f(w)
      draw line from prev to w
    until w escapes
  on deactivated:
    erase
```

## The mistakes that actually happen

- `i` and `e` are the imaginary unit and Euler's number: **never name a
  variable `i` or `e`** (loop counters: `k`, `j`, `n`). Other reserved
  words include `text`, `length`, `range`, `times`, `mod`, `diff`, color
  names, and function names (full list in reference.md).
- **No scientific notation.** `1e-6` silently means `1·e − 6`. Write
  `0.000001` or `1/1000000`.
- `=` and `!=` on reals/complex numbers are approximate (within
  `vanishing_radius`, default 0.0001); use `===` for exact.
- `x := ...` declares and `x <- ...` assigns. Redeclaring an existing name
  (including the viewer's `z` or a config variable) is an error. A
  variable declared inside an `if` or loop isn't visible after it.
- `/` always gives a real; integer division is `//`. A declared `Z`
  variable can't receive a real (`round`, `floor`).
- Every loop stops after `max_iters` (default 100) unless it has `up to N
  times`. Check `stuck` to tell "hit the limit" from "condition met".
  Deep zooms need a larger `max_iters` (users can raise it in the
  "Iteration settings" tab).
- `iterate z -> f(z) until c` updates one variable. For systems with
  several variables (derivatives, real 2-D maps), write an `until c:` loop
  with a body.
- In tools, without `at w` a click binds the viewer's own coordinate
  name; on a parameter-plane viewer (`var=C` where `C` is in `@config`)
  that **sets the parameter** to the clicked point. Use `at w` unless
  that's the point of the tool.
- Lists are 1-based. `xs @ k` binds like multiplication: `xs @ k + 1` is
  `(xs @ k) + 1`, so write `xs @ (k + 1)` for the next item.
- Tool persistent state must be a tool declaration (it shows in the tool's
  panel); handler locals are reset on each event. `button "Label"` belongs
  in a tool's declarations (a `@config` button is an error).
- `text(...)` needs at least two arguments: `text("n = ", n)`.

## From a paper to a project

- **Complex polynomial / rational map.** `@config` for the parameters, the
  map as `let f(z) = ...` in `@common`, a dynamical-plane viewer, and if
  the map has a parameter, a parameter-plane viewer with `var=<param>`
  starting at the critical point (`z := 0` for z^2 + C; in general, the
  critical point as a function of the parameter). Add a "Pick C" tool on
  the parameter plane (`on click-or-drag: pass`) so the dynamical plane
  follows the chosen parameter, and a Trace tool on the dynamical plane.
  See `examples/mandelbrot-julia.fs`.
- **Derivatives.** `f'(z)` and `diff(z, expr)` are exact and symbolic; use
  them for Newton's method, multipliers (`|f'(z)|` along a cycle), and
  critical points. Track an orbit derivative alongside the orbit in a loop
  body (`dz <- 2 z dz + 1` for d/dC of z^2 + C; `dz <- 2 z dz` for d/dz).
- **Distance estimation, potentials, smooth coloring.** See
  `examples/distance-estimate.fs` and the smooth-coloring `define shade`
  in `examples/mandelbrot-julia.fs`. Use a large escape radius
  (`escape=` attribute) when the formula assumes one, and `pixel_size` for
  pixel-relative thresholds.
- **Basins of attraction / root finding.** Iterate until two successive
  iterates agree (`|z - old| < tol`), then color by which attractor was
  reached (`rainbow(arg z / (2 pi))` or by an index), shaded by
  `iterations`. See `examples/newton-roots.fs` (roots in a configurable
  list, edited by clicking).
- **Real maps and real 2-D systems.** Viewers are complex; use `x :=
  re(z)`, `y := im(z)` and loop over both. Tools can bind `at (x, y)` and
  draw at `(x, y)`. See `examples/real-map.fs`. A 1-D real family can be
  a bifurcation-style picture with the parameter on one axis (repo example
  `logistic.fs`).
- **Things computed once, not per pixel** (periodic points, rays,
  landing points, orbit diagrams): do them in a tool's `on refresh`
  handler and draw the result, with the tool's declarations as its
  inputs (repo example `external_rays.fs`). Tools can use lists freely
  (they get up to 256 MB of scratch space; viewers get 1 MB per pixel).
- **Shared logic** between viewers and tools goes in `@common` (`let` for
  formulas, `define`/`run` for multi-statement code like coloring).
- **What the language can't express**: recursion, user-defined types,
  arrays with mutation in place (lists are values: `xs <- append(xs, x)`),
  file I/O, randomness, and viewers over anything but a rectangle of the
  complex plane. If the algorithm needs one of these, say so and propose
  the closest faithful alternative (e.g. unroll a recursion into a loop with
  an explicit list as a stack, inside a tool).

Performance: viewer code runs for every pixel, several times (progressive
passes and supersampling), compiled to native code. Loops of a few
thousand iterations per pixel are fine; avoid building long lists per
pixel. Work whose result doesn't depend on the pixel (e.g. finding a
periodic point for the current parameter) still runs for every pixel if
it's in a viewer or `@common`, so do it in a tool instead and draw it.
