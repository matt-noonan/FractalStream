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

To use a different variable name for your viewer, add a `var=NAME` attribute to the end of your
`@viewer` line. For example, here is a Mandelbrot viewer script that uses `C` instead of `z` for
the variable name:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bY9NCsIwEIX3PcWjW2mptvYnIAiuPURaBw2UNiSx_uAF3Lv3Dq5deRRPYmyxVXD3fY-ZeYwulJCG4eQA80bQjhTcJa9WVOaqNtBkXDRczRZ24Ijn-YLAkjCkuKF3cr3j-LhhhAW2lRGlzUgXXJJ2dpR7UtWSlBGkmd3rGloEfmu6DCj2rK1o-TCwLHlBnyEPiZ-RF_Ya-3FE3qT3wA-zOP3SZJJ-VNFa1NVwKvLjNJn-V72xnzCMnRc)
---
@viewer "Mandelbrot set" var=C
z := 0
iterate z -> z^2 + C until z escapes
```

For real plane viewers, use `xvar=` and `yvar=` instead of `var=` to change the variable names.

### Showing a viewer only some of the time

A viewer with a `show_if=` attribute is only shown while its condition holds. The condition is a
truth value worked out from the configuration's values (and the standard library's functions,
like `max`), and it's worked out again whenever one of them changes. For example, this project
shows the Julia set only once a point of the Mandelbrot set has been picked with the "Pick C" tool:

```{code-block} fs
@config "Parameters"
C : C := 0
selected_C : Bool := false

@viewer "Mandelbrot set" var=C
z := 0
iterate z -> z^2 + C until z escapes

@viewer "Julia set" show_if=selected_C
iterate z -> z^2 + C until z escapes

@tool "Pick C" for="Mandelbrot set"
on click:
  C <- click
  selected_C <- true
```

A condition with spaces in it needs quotes, as other attribute values do:
`show_if="selected_C and not compare_mode"`. It can only use the configuration's values, not a
viewer's own variables (like `z`), which only have values pixel by pixel.

While a viewer is hidden, its window is hidden, and it isn't drawn, even as the configuration
changes; its tools and its overlay don't run. When it's shown again, its window comes back where
it was, with the same view, and it's drawn with the configuration as it is then. Whether a viewer
is shown isn't saved with the project, since it follows from the configuration.

### Lattices: viewers of a torus

A viewer of the plane can have a *lattice* acting on it. The viewer then *folds* each pixel's
point into the lattice's fundamental parallelogram before its script sees it, so the script works
on the torus (the plane modulo the lattice), and the picture repeats over the plane, one copy per
tile. For example, this project draws the Weierstrass ℘ function of the lattice ℤ + τℤ (see
{ref}`elliptic`), with the tiles' edges in black:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=Jc67CgIxEIXhPk9xSKV4jWKz7IJga6f9ErOzOphNliReEEvfz1cyajcc_g-m9aHTqYAS0QTu8_UUwNp41_IRcqtTYkMyb-8XCmxQVJhPlxhBTRVYfOMr040C5N6HS5Swf1Nl0POd7I4fVKmZWs1zbLz1AeUE3CJyQ3XDMWlnCCUW_76OGSCdyOFgtTmDbCQ0vtPs6p8f3PrBYww1zk8Nh-ID)
---
@config "Lattice"
τ : C := 0.3 + 1.1 i

@viewer "Torus" lattice=τ pixelSize=1/150
color <- if side_distance < 2 pixel_size then black else domain_color(wp(z, 1, τ))
```

In a complex viewer, `lattice=τ` is the lattice ℤ + τℤ, and `lattice="(ω1, ω2)"` is the lattice
ω1ℤ + ω2ℤ. In a real viewer, the lattice is given by a pair of vectors: `lattice="((1, 0), (0, 1))"`
is ℤ². The periods are worked out from the configuration's values (and the standard library's
functions), and again whenever one of them changes, so changing a configuration value (by hand or
from a tool) changes the lattice, and the viewer is drawn again.

The fundamental parallelogram is the one centered at 0: the points `s ω1 + t ω2` with `s` and `t`
between -1/2 and 1/2. A `latticeCenter=` attribute centers it at another point instead (a complex
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
grey. Adding `tiled=false` to its header starts it that way, and its menu's "Show fundamental
domain" switches between the two. Which one it shows is saved with the project. While it shows
only its domain, a tool's drawings are only drawn in the domain, not copied into the other tiles.

The same works for a viewer of the hyperbolic plane with a group (`group=`), whose fundamental
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
to that viewer. A tool is defined by a header like `@tool NAME for="VIEWER"` where `NAME` is
the name of the tool, and `VIEWER` is the name of the viewer that the tool should attach to.

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
unless the tool declares one. In a parameter plane (a viewer like `@viewer "Parameter plane" var=C`
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
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bZBNTsMwEIX3OcWT94lCW5I2EhICsecKrjMQCyuOxpOmSNyFU3AhToLz14LEbr55b954HAzbTip8JMD9ydJADPXgdPumYsd45xnfn18YGiuUjCbx3kE9WjaOFF48363-qAqdBeqZKQRIYwOOvYhvIR7EOlBsEsw8G-2Lqp5GbQqIpI3Ykxaqq8hAH6eY6nETnG1patashyUHWpBjsNKAdW37gJsl6G_6nDa9IhnomHbsO2KxFEZlPj3MpumeuYx_cK6Qr_X7te6cNrSaUpTZgdLtBYus2FG6uXCebQ_F_heWm_2KTK_Wt9eoXVbsy9v_MTS6oyqe-AM)
---
@viewer "Blank"
color <- white

@tool "Circle" for="Blank"

text "Press this button to erase the circle"
button "Erase"

on activated:
  use red for line
  draw circle at 0 with radius 1

on button "Erase":
  erase
```

## Colorizers

**TODO**
