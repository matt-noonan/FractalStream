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

| Kind | Default variables | Viewer modes |
| ---- | ----------------- | ------------ |
| Complex line | `z : C` | Plane, Riemann sphere |
| Real plane | `x : R`, `y : R` | Plane |
| Hyperbolic plane | `z : C` | Disk, upper halfplane; coordinate is bounded in norm by 1 |

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

The same works for a viewer of the hyperbolic plane with a group (`modulo the group`), whose fundamental
domain is a polygon.

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

### Timers, and a tool's setup

A tool's `on timer "NAME" every N ms:` block runs every `N` milliseconds while the tool is selected:
an easy way to animate a parameter. This tool moves `C` around a circle:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=VVDBasMwDL3nKx4-rXQtWUdZKd0Y9LbT6A67FYyrNIIkDpbSbmUfPzntDjsYS0_vPfm5iqn1usZDISFxb9VPAbyG2FV8hHv3ybeklMQZvMU6n2eU80fQ_q6cL8GTIgtOTGdKcG9Dwx5CmvlsQq-EC2YvuOwXmJp86JQbg0iC70lGtcbYwH303DlU8Z8Nzqw1pI5Jw6AQoyt9KdxnzQ1BaxaMcruFGgpKh3tbc4wk8CkO3QEegVNoaJ4flfyBB7EkO3iB242tu6Z6Wq2WRvHd0awzI6MGxA7Kbc4nSr0DnSh9Y1milbWN_xSb2a2Ymle5GCfbjN522pfxlTEpfgE)
---
@config "Parameters"
C : C := 0.3 e^(0.5 i)

@viewer "Julia set"
iterate z -> z^2 + C until z escapes

@tool "Spin" for "Julia set" with shortcut s
text "While this tool is selected, C goes around a circle."
radius : R as "Radius" := 0.7885
angle : R := 0
on timer "step" every 50 ms:
  angle <- angle + 0.02
  C <- radius e^(i angle)
```

A tool can have a `setup:` block right after its header, like a viewer's, with a `draw layer:` (a
whole number: tools with higher layers draw over those with lower ones) and an `icon:`.

### Overlays

A viewer can have one *overlay*: drawings that are always shown over the viewer, whichever tool is
selected (or none), below every tool's own drawings. It's written as a tool named with the bare
word `overlay`, with only an `on refresh:` block:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=ZZBBa8MwDIXv-RUPnza2dmkhDAIdg5wH-wUDz1FWgRsH2WnasB8_2TnuYHj60JOePAS52NTiUEUnPKn6rYB3F8aBf2A-rdgLJZJoFHdo8zthV-8PRzyh3r82XGXDlWkhgfmwY0_-W0JCpGQQrko77dBqnloVgKNRJ7Z5SlPAxDfyiLySBnk51rXSNe_JgrXXJlKwe8P6ldd2mMfEXhFFZyeKJUIKwZd93t4xhP9htCmMEBqE4nmLoqMjFTVHwp28D0vxDux94b3YpVTUw7E4T7BJEyyczhDb8xzRbBfEYlhEEyPRLT2YDieYZ3SPmyd_2KEBV38)
---
@config "Parameters"
C : C := -0.12 + 0.75i

@viewer "Mandelbrot set" over C
setup:
  center: -0.5
  pixel size: 1/200
z := 0
iterate z -> z^2 + C until z escapes

@tool overlay for "Mandelbrot set"
on refresh:
  erase
  use yellow for fill
  draw filled circle at C with radius 5 pixels
  write text("C = ", C) at C + 0.15 i
```

Its `on refresh:` block runs when the project opens, whenever the viewer's view changes, and whenever
any configuration value changes, so what it draws follows the parameters, wherever they're changed
from. An overlay's header only says which viewer it's for (it's never selected, so it has no
shortcut), and it has no settings of its own.

## Colorizers

**TODO**
