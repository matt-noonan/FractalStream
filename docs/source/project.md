# Project structure

A FractalStream project's script is made up of several *stanzas* that name and define the components
making up a FractalStream project. Each stanza starts with a *stanza definition*, indicated by
a `@` at the start of the line. The different kinds of stanzas are described in the subsections here.

## Viewers

A *viewer* is defined by a stanza of the form `@viewer "VIEWER NAME"`, where `VIEWER NAME` is the
quoted title of the viewer window. The job of the viewer's script is to run once for each pixel in the view, and tell FractalStream how to color that pixel. This communication back and forth between FractalStream and the script works as follows:
- FractalStream tells the script which pixel it is drawing by setting a variable. For complex viewers, this variable is called `z` by default.
- The script tells FractalStream what color to use for this pixel by setting a variable called `color`, of `Color` type. If the script does not set `color`, FractalStream will fall back to a default colorizer (**TODO** link for more info)

For example, the viewer script

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bY9NTsMwFIT3OcWoe1v9Se00K9bcIg0DsbASy3YJVNyFOyCx66pH6Un6RJWUBbv53nszmpfa6EKu8akK4OHNcWTE4vHgXYPEvJCpy4xNJo64fJ1wPH9DYaXttqystcbg0GfnZcvUNoFJHO3ghyjXP3DP8-IJuWOPsZM80Cdi75v2tRi5VyEOgTE7plrstxa_Epir3FDC32ssJ_1x10HSOB0pWL2j2sxotCmp1jMv9WZnqj9o19WEkS9u6O9RpTaV3f6PqZPXaqyKKw)
---
@viewer "Julia set"
iterate z -> z^2 - 1.754877766 until z escapes
color <- if z escaped then white else black
```

draws the "airplane" Julia set in black, with white for the rest of the plane.

If a viewer's script does not explicitly set the `color` variable, FractalStream will implicitly insert some "colorizer" code at the of the script. For example, the default "Rainbow" colorizer is roughly equivalent to this script fragment:

``` fractalstream
if stuck:
  color <- black
else:
  color <- rainbow(iterations / 20)
```

FractalStream currently supports three different kinds of viewers:

| Kind | Its points | Header | See |
| ---- | ---------- | ------ | --- |
| The complex plane | complex numbers, `z` | (nothing, or `over z`) | |
| The real plane | pairs of real numbers, `(x, y)` | `over (x, y)` | {ref}`real-viewers` |
| The Riemann sphere | points of `Ĉ` | `on the sphere` | {ref}`sphere-viewers` |
| The hyperbolic plane | points of the unit disk, or of the upper half-plane | `on the disk`, `on the half-plane` | {ref}`hyperbolic-viewers` |

To use a different variable name for your viewer, say so at the end of your `@viewer` line with
`over NAME`. For example, here is a Mandelbrot viewer script that uses `C` instead of `z` for the
variable name:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bY89DoJAEIV7TvFCayAIyF9lQu0hACe6CYHN7gpKvIC9vXewtvIonsQVAmhi930vM_MyshCMqwRnA1g3jFoSMDdZtaUyF7WCJGWibnSa6okOr8sVjiamSGSKPsntge55xwIpDpVipc5IFhknabSUW1zUnIRiJBO9N1T0CPz2DBlQHJO-oufTzLzMChqHLIR2TJY3aWAHPlnu5I7txUH0paEbjSpox-pqPuXbQRSu_qvc608SLI03)
---
@viewer "Mandelbrot set" over C
z := 0
iterate z -> z^2 + C until z escapes
```

For a real plane viewer, name both of its variables: `over (x, y)`.

### A viewer's header, and its `setup:`

After its title, a viewer's header line says what kind of viewer it is, in a few phrases that can
come in any order:

| Phrase | Means |
| ------ | ----- |
| `over z`, or `over (x, y)` | its variable (or a real viewer's two), `z` if it doesn't say |
| `on the sphere`, `on the disk`, `on the half-plane` | what it shows, the plane if it doesn't say |
| `modulo the lattice τ` | the plane modulo a lattice (see {ref}`lattices` below) |
| `modulo the modular group`, `modulo the group T` | the hyperbolic plane modulo a group |
| `centered at c` | the center of that lattice's or group's fundamental domain |

A value in a phrase is a name, a number, or something in parentheses, like
`modulo the lattice (1, τ)`, so that it's clear where it ends.

Everything else about a viewer goes in a `setup:` block right after the header, with one
`name: value` line for each setting:

```{code-block} fs
@viewer "Parameter plane" over C
setup:
  center: -0.5
  pixel size: 1/200
  iteration limit: 200
  coloring:
    color_speed <- 3
z := 0
iterate z -> z^2 + C until z escapes
```

| Setting | Is |
| ------- | -- |
| `center:`, `pixel size:` | where the view starts, and its zoom |
| `aspect:` | a real viewer's pixel height over its width |
| `colorizer:` | the colorizer it starts with; a `coloring:` block inside `setup:` changes its settings |
| `show if:` | when it's shown (see below) |
| `tiled:` | `false` to start showing only the fundamental domain (see below) |
| `iteration limit:`, `escape radius:`, `vanishing radius:` | what its loops use instead of `max_iters`, `escape_radius` and `vanishing_radius`, so that two viewers can differ |

(real-viewers)=
### Viewers of the real plane

A viewer `over (x, y)` is a *real viewer*: each pixel is a pair of real numbers, the first the
horizontal coordinate and the second the vertical one, so there's no need to take a complex number
apart into `re(z)` and `im(z)`:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=NY5LbsMwDET3PsXAKwd1EtntSkiCLLsseoACskC3RGTJkOTEKXKhnKMXK93PhhjOkIPXhziYrNEUyUYeRd0K4GiD7_kd5YuJZqBMMZViG2i8Qu_RbJ5k7f5XtXkslq8z04Uiyuevuw8egxlLhLM41VzjupKTRHkatQjAkpdejUrVUKsfa-SZHBJ_kgBtW6XEnXxmh_mtxQOuMg9olPptWEqxW6PBWtD-TmrBmiW2wYW4pNwj5cmekD_Io3NGJLlEiIZ9Fy4VC4bJHHzCFq2gfAM)
---
@config "Parameters"
a : R := 1.4
b : R := 0.3

@viewer "Hénon map" over (x, y)
setup:
  center: (0, 0)
  pixel size: 1/200
until x^2 + y^2 > 100:
  x, y <- 1 - a x^2 + y, b x
color <- if stuck then black else rainbow(iterations / 20)
```

Either coordinate can be a real configuration value's name, as in `over (a, b)`, for a plane of
parameters: inside the viewer, it then means the pixel's coordinate. With both coordinates
configuration values, the viewer has a "Select" tool that sets them both to the point clicked.

A real viewer's `center:` is a point, like `center: (1, -1/2)`. Its pixels needn't be square: an
`aspect:` setting gives a pixel's height over its width (`aspect: 2` fits twice as much of the y
axis in the window), and the viewer's menu has an "Aspect ratio…" item to change it. Besides
`pixel_size` (a pixel's width), a real viewer's code and its tools can read `pixel_width` (the same)
and `pixel_height`. A complex viewer's pixels are always square.

### A viewer's size and position

A viewer's code and its tools can read where the viewer is looking: `viewer_width` and
`viewer_height` (the image's size, in pixels), and the middle of the view, `viewer_center` in a
complex viewer, or `viewer_center_x` and `viewer_center_y` in a real one. With `pixel_size`, that's
enough to work out which pixel a point is in. They change as the window is resized, panned and
zoomed, and a viewer whose code uses them is drawn again when they do.

### Showing a viewer only some of the time

A viewer with a `show if:` setting is only shown while its condition holds. The condition is a
truth value worked out from the configuration's values, and it's worked out again whenever one of
them changes. It can also use the values and one-line functions defined in `@common`, and the
standard library's one-line functions, like `max`. Because it's worked out apart from any viewer
or tool code, it can't call a function with a body. For example, this project
shows the Julia set only once a point of the Mandelbrot set has been picked with the "Pick C" tool:

```{code-block} fs
@config "Parameters"
C : C := 0
selected_C : Bool := false

@viewer "Mandelbrot set" over C
z := 0
iterate z -> z^2 + C until z escapes

@viewer "Julia set"
setup:
  show if: selected_C
iterate z -> z^2 + C until z escapes

@tool "Pick C" for "Mandelbrot set"
on click:
  C <- click
  selected_C <- true
```

The condition is the rest of its line, as in `show if: selected_C and not compare_mode`. It can
only use the configuration's values (and `@common`'s), not a
viewer's own variables (like `z`), which only have values pixel by pixel.

While a viewer is hidden, its window is hidden, and it isn't drawn, even as the configuration
changes; its tools and its overlay don't run. When it's shown again, its window comes back where
it was, with the same view, and it's drawn with the configuration as it is then. Whether a viewer
is shown isn't saved with the project, since it follows from the configuration.

(lattices)=
### Lattices: viewers of a torus

A viewer of the plane can have a *lattice* acting on it. The viewer then *folds* each pixel's
point into the lattice's fundamental parallelogram before its script sees it, so the script works
on the torus (the plane modulo the lattice), and the picture repeats over the plane, one copy per
tile. For example, this project draws the Weierstrass ℘ function of the lattice ℤ + τℤ (see
{ref}`elliptic`), with the tiles' edges in black:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=JY29DsIwEIP3PoXViYrfgFiqIiGxMrJXIb3CibSpkhRQxcj78UpcYbNsf3btfKNjDpUE47kT9UqAvXFtzRekRx0jG0rF-7yR44B8h9VigynUQoGTsXxnepBHenK-DykaV_XWIV4J9o8LK71Ase9yEUDHT7IIPJA8L9V2Ja5x1nkUc3AtSUVlxSHqVugC6z9RjsQ43OJstbmBbCBUrtHclj9-8ugmwwxqJpdZlnwB)
---
@config "Lattice"
τ : C := 0.3 + 1.1 i

@viewer "Torus" modulo the lattice τ
setup:
  pixel size: 1/150
color <- if side_distance < 2 pixel_size then black else domain_color(wp(z, 1, τ))
```

In a complex viewer, `modulo the lattice τ` is the lattice ℤ + τℤ, and `modulo the lattice (ω1, ω2)` is the lattice
ω1ℤ + ω2ℤ. In a real viewer, the lattice is given by a pair of vectors: `modulo the lattice ((1, 0), (0, 1))`
is ℤ². The periods are worked out from the configuration's values (and the standard library's
functions), and again whenever one of them changes, so changing a configuration value (by hand or
from a tool) changes the lattice, and the viewer is drawn again.

The fundamental parallelogram is the one centered at 0: the points `s ω1 + t ω2` with `s` and `t`
between -1/2 and 1/2. A `centered at c` phrase centers it at another point instead (a complex
number, or a pair of real numbers in a real viewer).

Besides its folded point, a viewer with a lattice has two more variables:

| Variable | Is |
| -------- | -- |
| `cell` | The pair of integers `(m, n)` of the tile the pixel's point was in, the one that was moved back by `m ω1 + n ω2` |
| `side_distance` | The distance from the folded point to the nearest side of the parallelogram |

The points a tool is given (`click`, `drag start` and `drag end`) are folded too, and a tool's
drawings are drawn in each tile on the screen, unless the tiles are too small to see.

#### Showing only the fundamental domain

Instead of all of its tiles, a viewer with a lattice can show only its fundamental domain, with the
rest of the plane greyed out: the picture outside the domain is still drawn, but faded toward
grey. A `tiled: false` setting in its `setup:` starts it that way, and its menu's "Show fundamental
domain" switches between the two. Which one it shows is saved with the project. While it shows
only its domain, a tool's drawings are only drawn in the domain, not copied into the other tiles.

The same works for a viewer of the hyperbolic plane with a group, whose fundamental domain is a
polygon (see {ref}`groups`).

(sphere-viewers)=
### Viewers of the Riemann sphere

A complex viewer can show the Riemann sphere instead of the plane, as a globe, with `on the sphere`:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=VY3BDoIwEETvfMWEiyURBbkZJH6Bn0CCsEiT0pJ2sQnh42315GVmd19mdjR27viKMnG9lUuY9gS4vyV5skgf5Nnog8NMPJkBRoMnglsmspT-ryEnmWzHhA15EwVia6tgZYYzRIWtvWRYNUsVqGOzuBDyuN7QjaPUJMojtizceqOMRZ1Djth9bNhRozgVZXyoYWkAKUeRy1n4DA2KH3pZCvqFT7VS8gE)
---
@viewer "Newton's method on the sphere" on the sphere
iterate z -> z - (z^3 - 1) / (3 z^2) until z stops
w := affine(1, z)
color <- if |w - 1| < 0.01 then red else if im(w) > 0 then green else blue
```

On the sphere, `z` is the point of the globe under each pixel, a point of `Ĉ` (see
{ref}`riemann-sphere`): 0 is the globe's north pole, ∞ its south pole, and the unit circle its
equator. It's exact everywhere, ∞ included, so a rational map like Newton's method above is right
all over the globe. For the complex number a point is, use `affine(1, z)`. A viewer over a
configuration value, like a parameter space `over c`, needs that value to be a point of the sphere
too: `c : Ĉ := 0`.

The globe always fills the window. `center:` is the point facing you to start with (0, if it doesn't
say), and `pixel size:` is the size of a pixel in the middle of the globe, measured on the sphere,
whose radius is 1. Scrolling zooms in or out about the point under the pointer, and dragging with
the Navigate tool turns the globe; zoomed in, that pans. Tools get points of `Ĉ` too (a click on ∞
is ∞ itself), and draw on the globe: a line is an arc of a great circle.

(hyperbolic-viewers)=
### Viewers of the hyperbolic plane

A complex viewer can show the hyperbolic plane, as the Poincaré disk, with `on the disk`:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=RY3BCoMwEETvfsVgLwotWI9Socf-Rho3JhizsoYWix_frZfOaQbezDiW2eQO12K1EhZ1ewHcX4HeJCgf20Ly5BgslmgSleCE7AlDWCcFBV2PFiab5Kv9s9dQnQ7C_6sKK2AJTnhGoz3LkQW3C4LDzEPlIrNULaQ-o63Ro_ltJMQw-oxRaAPFVW-NTEcsvg)
---
@viewer "Hyperbolic plane" on the disk
r := 2 atanh(|z|)    # the hyperbolic distance from 0
color <- if mod(floor(2 r), 2) = 0 then light grey else dark grey
```

`z` is a point of the unit disk under each pixel. What the window shows is the disk moved by the
view, an isometry of the hyperbolic plane: dragging with the Navigate tool moves the view by a
hyperbolic translation, keeping the point under the pointer there, so the whole plane can be
visited. Scrolling zooms in on the disk as it's drawn, to see the small details near its edge.
`center:` is the point of the disk shown in the middle to start with, and `pixel size:` is measured
on the disk, whose radius is 1.

With `on the half-plane` instead, `z` is a point of the upper half-plane, drawn with its real axis
along the window's bottom edge. Either way, the viewer's menu can switch between showing the
hyperbolic plane as the disk and as the half-plane, keeping the view (the code still sees points
of its own model).

`pixel_size` is the width of a pixel in the model, which in the hyperbolic plane depends on the
pixel. A pixel's size in the hyperbolic metric is `2 pixel_size / (1 - |z|²)` in the disk, and
`pixel_size / im(z)` in the half-plane. Tools get model points, and what they draw follows the
view; for a marker that's the same size anywhere, give a circle's radius in pixels (see
{ref}`draw-commands`).

(groups)=
#### Groups: tilings and surfaces

A viewer of the hyperbolic plane can have a *group* acting on it, as a lattice acts on the plane:
the viewer folds each pixel's point into the group's fundamental polygon before its code sees it,
so the code works on the quotient (a surface, or an orbifold), and the picture is repeated over the
plane, one copy per tile.

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bYy7DoJAFER7vmJCpQUqhQ2BxI-wJ-vuBTYue8ld8EH8eFewMbGbnDkzDUuvxgJ5ErTYIaZXApxulu4kSM8doWczOSVohachBXuMkXbKNdnglP8KvNAfNx6RaQlFhWAN1caGUXlNKJHvjhjsg1wd7EzYw_abeRsHmh0Lygy2Wcfx1ePilL6CXKAPb9iZWvPkR1Q4rIaQ-d_na8-ifHxblFbombwB)
---
@viewer "The modular group" on the half-plane modulo the modular group
edge := side_distance < 1.5 pixel_size / im(z)
color <- if edge then black else if fold_count = 0 then red else if fold_count = 1 then orange else grey
```

`modulo the modular group` is the modular group PSL(2, ℤ), whose fundamental polygon is the
familiar one, |Re z| ≤ 1/2 and |z| ≥ 1 in the half-plane. Any other group is given by a
`Table 2 of C` of the configuration, `modulo the group T`: column `k` of the table is the polygon's
side `k`, and its first four rows are the `a`, `b`, `c` and `d` of that side's *pairing*, the map
`s ⟼ (a s + b)/(c s + d)` that takes a point beyond the side back across it. The side is halfway
between the polygon's center and the point the pairing takes the center to. The center is the
middle of the model (0, or `i`), or `centered at c` in the header. A tool can fill in or change the
table, and the viewer is drawn again; a table that can't be used (a pairing that isn't invertible,
for example) pauses the viewer, which says what's wrong.

Besides its folded point, a viewer with a group has:

| Variable | Is |
| -------- | -- |
| `side_distance` | the hyperbolic distance from the point to the nearest side of the polygon |
| `side_index` | which side that is (its column of the table) |
| `fold_count` | how many times the point was moved across a side to get into the polygon |

As with a lattice, a tool's points are folded too, and its drawings are drawn in each tile on the
screen. "Show fundamental domain" works the same way, too.

## Shared definitions

Shared definitions are grouped into one or more *common* stanzas, delineated by a `@common` header.

``` fractalstream
@common

f(z) := z^2 + C

my_special_constant := 1234

define my_coloring_script(p):
    color <- if p then red else blue
```

`@common` stanzas can contain definitions for constants, functions, and script fragments, but
nothing else. These definitions are made visible to every other component of your project.

## Configuration panels

Configuration panels hold one or more variable definitions, giving the user a way to view and update those variables while the viewers are running and to see the results in real time. They  are
defined by stanzas of the form `@config "GROUP NAME"`, where `GROUP NAME` is the name that the
variables will be grouped under in the "Configuration" window.

The variables defined in a `@config` block are visible to every other script in your project,
and their values can be updated by the user or by tool scripts.

`@config` stanzas can hold variable definitions, but every variable defined in a `@config` stanza
must be given a type annotation; type inference is not enabled here.

``` fractalstream
@config "My parameters"

a : C := 0
b : R := 1
```

You can use `text` to add explanatory text to your configuration panel:

``` fractalstream
@config "My parameters"

text "`a` is complex, but `b` is real"
a : C := 0
b : R := 1
```

Different types of variables turn into different widgets in the configuration panel:
numeric variables get a text entry field, `Boolean` variables become a checkbox, and
`Color` variables become a color picker.

By default, the label on a variable is just the variable's name. If you would like to
use a different label, add an `as "LABEL"` clause after the variable's type but before
the `:=` operator:

``` fractalstream
@config "My parameters"

C : C as "f_C(z) = z^2 + " := 0
```

The widgets for variables can be placed side-by-side by collecting the variable definitions
under a `row:` block:

``` fractalstream
@config "My parameters"

text "`a` is complex, but `b` is real"
row:
  a : C := 0
  b : R := 1
```

A table (see {ref}`language-tables`) is also declared in a `@config` stanza, with its type but
without a default value, since every table starts out empty:

``` fractalstream
@config "Parameters"
saddle : Table 2 of C
```

A table can hold integers, real numbers, complex numbers, truth values, colors, or tuples of
these, in any number of dimensions (`Table 1 of R`, `Table 3 of Z`, `Table 2 of C x Z`, and so
on). In the configuration panel, a table appears as a line showing its name, its type, and its
current size, with buttons to export it to a file and import it from one (see {ref}`table-files`).

When you edit a project's script and rebuild it, each table keeps its contents as long as the new
script still declares it with the same name and type.

## Tools

Tools are additional scripts attached to a viewer that provide some user interaction customized
to that viewer. A tool is defined by a header like `@tool NAME for "VIEWER"` where `NAME` is
the name of the tool, and `VIEWER` is the name of the viewer that the tool should attach to.
With only one viewer, `for` can be left out; a keyboard shortcut goes at the end too:
`@tool "Trace" for "Julia set" with shortcut t`.

Tools work by running scripts in response to actions taken by the user. A tool can draw on top
of the viewer's window, and can also change the value of configuration variables. Through these
simple mechanisms, fairly sophisticated tools can be built.

| Event name | Meaning |
| ---------- | ------- |
| `activated` | The user switched to this tool |
| `deactivated` | The user switched away from this tool |
| `refresh` | The tool's viewer redrew itself, often because some parameter changed value |
| `click` | The user clicked at a point; and, if the tool has no `drag` block, the user is dragging |
| `double-click` | The user double-clicked at a point |
| `drag` | The user continued a drag (this runs each time the pointer moves) |
| `drag-finished` | The user let go at the end of a drag |
| `button TEXT` | The tool's button labeled with `TEXT` was pressed |
| `timer TEXT every N ms` | Runs every `N` milliseconds while the tool is selected |

A block for a click or a drag is given the point the user clicked or dragged to, by a keyword
that only means that point inside the blocks it belongs to:

| Keyword | Available in | Is |
| ------- | ------------ | -- |
| `click` | `on click:`, `on double-click:` | The point clicked (or, in `on click:` while dragging, where the pointer is now) |
| `drag start` | `on drag:`, `on drag-finished:` | Where the drag started |
| `drag end` | `on drag:`, `on drag-finished:` | Where the pointer is now, or where the drag ended |

The point is the same kind of point as the viewer's: a complex number in a complex viewer, a pair
of real numbers in a real viewer (so `(x, y) := click` takes it apart), and a point of `Ĉ` in a
viewer of the Riemann sphere, ∞ included. To give it a name of your own, declare a variable:
`w := click`.

A tool with an `on click:` block but no `on drag:` block runs its `on click:` block while the user
drags, too, with `click` wherever the pointer is. That's what you want for a tool that follows the
pointer, like the "Orbits" tool in the {doc}`scripting tutorial <scripting>`. A tool with neither
block leaves dragging to move the view.

A tool doesn't see its viewer's own variable: in the tool for a viewer of `z`, there is no `z`
unless the tool declares one. In a parameter plane (a viewer like `@viewer "Parameter plane" over C`
with `C` a configuration value), `C` in the viewer's code is each pixel's own value, but `C` in
its tools is the configuration's value. So a tool that sets the parameter to the point clicked
says so: `C <- click`.

A tool can also have its own configuration panel, which only appears when the tool is activated.
Any `@config`-style variable definitions immediately following the `@tool` header will go
into the tool's configuration panel, and those variables will be visible to the tool's event
scripts.

After the optional variable definitions, the tool's script defines any number of blocks of the form

``` fractalstream
on EVENT:
  SCRIPT
```

As a simple example, let's build a blank viewer and a tool that draws the unit circle when it is
activated, and erases it when a button is pressed:

``` {code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bZBNTsMwEIX3OcWT94lCW5I0qwrEniu4yUAsrDgaT5oicRdOwYU4Cc5fAxK7-ea9eeOxr9h0UuIjAk4XQwMx1IPV7ZsKncpZx_j-_MLQGKFoNIlzFurRcGVJ4cVt_qAKXQXqmcl7SGM8zr2IayEOxNpTaBKqeTbYF1U9jdoUEEhXYi5aqC4DA32YYqqnTda0NDVr1sOSAy1IMRhpwLo2vcfdEvQ3fU6bXhENdI47dh2xGPKjMp_uZ9N0z1yGP7iWSNf6fas7qytaTTHy5Ejx_oZZkh0o3t04TfbHrPiF-a5YkenVuHaLOiRZkd__j77RHZXhxB8)
---
@viewer "Blank"
color <- white

@tool "Circle" for "Blank"

text "Press this button to erase the circle"
button "Erase"

on activated:
  use red for line
  draw circle at 0 with radius 1

on button "Erase":
  erase
```

(colorizers)=
## Colorizers

A viewer's script often only says what to compute, like `iterate z -> z^2 + C until z escapes`,
and leaves the coloring to a *colorizer*: a small piece of script that runs at the end of the
viewer's code and sets `color`. Each viewer has one, which can be changed from its menu (under
"Colorizer"), and which is saved with the project. When nothing says which colorizer a viewer
uses, it gets `Manual` (which does nothing) if its script sets `color` itself, and `Rainbow` if it
doesn't. A viewer names its own with a `colorizer:` setting, and changes the colorizer's settings
from their defaults with a `coloring:` block, both in its `setup:`:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=TY9Bi8JADIXv_RWPuS7VVvBSVATPgrB7FsY26sB0UjKjLsUfb9qieAgkX15ekjNLa1OFMou1uE6zZwZsaw5nd4E5WLEtJZJoFO9QDbFGkQ2iu6MHCczehob8STghUjLgu9KdKrS6dZUmQE1BXSrkxWw5gs79k0d0Peny-aIoJhl7FmWq_G2Z0_WLhstkpbPWU0qEVQ7z9-CpP174UZdDU6h5T5A4bgZWjpv66QvA6VVWrXrkG_THBX70w1tIziuiWNuOYvYC)
---
@config "Parameters"
C : C := 0

@viewer "Mandelbrot set" over C
setup:
  center: -0.5
  pixel size: 1/200
  colorizer: Smooth
  coloring:
    palette <- "Two colors"
    color1 <- red
    period <- 100
z := 0
iterate z -> z^2 + C until z escapes
```

FractalStream's own colorizers are:

| Colorizer | Arguments | What it does |
| --------- | --------- | ------------ |
| `Manual` | | Nothing: the viewer's script sets `color`. |
| `Rainbow` | `t = iterations / color_speed` | Points that escape cycle through the rainbow; the rest are black. |
| `Escape time` | `t = iterations / period` | Like `Rainbow`, with a choice of palettes (or two colors of your own). |
| `Smooth` | `z` | Continuous color by escape time, without bands, from a choice of palettes. |
| `Binary decomposition` | `z` | Color by escape time, different where `z` ends up in the upper and lower half-planes, which outlines the external rays. |
| `Glow` | | Blends between two colors by how close the iteration count came to the iteration limit. |
| `Autocolor` | `z` | A color for each point `z` ends up at, by {ref}`autocolor`: the basins of Newton's method, without knowing its roots. |

Each colorizer's settings (like `Rainbow`'s "Iterations per color cycle") appear on the viewer's
page of the configuration window's "Color settings", and each viewer has its own copy of them. In
a `coloring:` block, a setting with a list of choices (like `palette` above) is given its choice's
name in quotes.

### Arguments

Some colorizers have parameters, given as arguments in parentheses after the name. A parameter
without an argument uses its default value, if it has one, and otherwise the viewer's variable of
the same name: so `Smooth` uses the viewer's `z`, and `Smooth(w)` uses `w` instead. An argument can
be any expression, using the viewer's variables and functions. For example, `Rainbow` colors by
iteration count, and `Rainbow(arg z / (2π))` colors by the angle of `z`, which shows the basins of
Newton's method:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=FYoxDoJAEEV7TvFDI1sYA3QUxhNYcACSFUfdBBgyO7jJxsIbeiWH5v-f_96DZfbaoS7iKGG19SmAyztQIkF5paS8HCJm0hffS2ORdFs7G8DIE0vIJB16H5Ybp8rLExknVM3v65xZQUm8kp3H8x6o8tBa1W63WuShcdgWDZPRqLzG4g8)
---
@viewer "Newton's method"
setup:
  colorizer: Rainbow(arg z / (2π))
iterate z -> z - (z^3 - 1) / (3 z^2) until z stops
```

### Writing a colorizer

A project can define colorizers of its own, with `@colorizer` stanzas. A `@colorizer` stanza has
its settings, declared like a `@config` stanza's, then its code, as a block called `colorize`:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=XVHLasMwELz7KwafbJqnoRREWwI9tcf2nqLY62SJIhlJrqnJoX_YX6pkpTnkIqTdmWFm1Bp7kl5gnbnachdu5wzY1EYZyyNZ5B8nY_wBeysbJu3zsHbk-06EC9BQ4rHR4oqd2NiRH4g0_GDSxC0iuSPLpoHAO6RD_jJh0zCHeEK1CiDWjhsKoLSOwFftI8gmrQm6U7I-Zll00bIm_LsuxjK54xYjgkPZUZMmwCkyOWjJaNrhDmvMocweRTzO4xnL6Zl4nzF378rLsLqopIiP8-CBdFPUxhW_P0F7eYlSbqsZFO8PHt-klBlmaKQ9BnhP5aRBypG4VUu5Y6TNF9MQ-3_rFctY-U3z1y8SuPmi2N-Uj0L6-TPGbRUSrhbrKoRdLR7uwei1Z3Utx2V_)
---
@colorizer "Smooth gradient"
setup:
  description: "Smooth color between two colors."
period : R as "Color period" := 20
inside : Color as "Interior color" := black

define colorize(z):
  if z escaped:
    m := iterations + 1 - log (log |z| / log escape_radius) / log 2
    color <- blend(cos(π m / period)^2, light yellow, dark blue)
  else:
    color <- inside

@viewer "Julia set"
setup:
  colorizer: Smooth gradient
iterate z -> z^2 - 0.12 + 0.75 i until z escapes
```

Its code runs at the end of the viewer's code, so it can use the viewer's `iterations` and `stuck`,
the configuration, and `@common`'s definitions; the viewer's own variables come to it as
arguments. Its parameters can have default values, which can use its settings and its earlier
parameters: `define colorize(t = iterations / period):`. The names a colorizer declares are its
own, so they can't clash with the viewer's. A `@colorizer` with the same name as one of
FractalStream's own is used instead of it.

