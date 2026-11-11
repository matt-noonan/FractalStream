# FractalStream script language reference

Everything here was checked against the parser, type checker and the
headless runner (October 2026). Where this disagrees with the repo's
`LANGUAGE.md`, this file is newer: `LANGUAGE.md` still describes tools in an
old YAML format, and says lists can't be used in viewers (they can).

## 1. Project files

A project is a YAML file with one key, `script:`, holding a block literal
(use `|`). The script is a sequence of *stanzas*, each starting with an
`@` header at the left margin; a stanza's body is the following lines, at
the same indentation as its header.

```yaml
script: |
  @config "Title"          # configuration fields, shown in a panel
  ...
  @common                  # definitions shared by every viewer and tool
  ...
  @viewer "Title" attrs    # a window that colors each pixel
  ...
  @tool "Name" attrs       # mouse-driven interaction with one viewer
  ...
  @colorizer "Name"        # a project's own colorizer (rarely needed)
  ...
scenarios:                 # optional: named headless test runs
  - name: smoke
    commands: |
      capture viewer "Title" to "out.png"
```

Stanzas can come in any order, and each kind can appear more than once.
`#` starts a comment (anywhere on a line). Indentation is significant, as in
Python: a line ending in `:` opens an indented block.

### `@config "Title"`

Declares the project's user-adjustable variables. No attributes. In the
app, each `@config` title becomes one tab of the configuration window
(stanzas with the same title share a tab). Every project also gets an
automatic last tab, "Iteration settings", declaring:

| Variable | Type | Default | Meaning |
|---|---|---|---|
| `max_iters` | Z | 100 | loop iteration limit |
| `escape_radius` | R | 10 | `z escapes` means abs(z) > escape_radius |
| `vanishing_radius` | R | 0.0001 | `z vanishes` means abs(z) < this; also the tolerance of `=` on reals |

Declaring any of these names yourself is an error (duplicate variable).
Body statements:

```
name : Type := default                      # a field; the type is required
name : Type as "Label" := default           # with a display label
mode : Z as "Mode" choice { "A", "B", "C" } := 0   # dropdown; value is 0-based index
text "Some explanatory text"
button "Label"                              # @tool declarations only (an error in @config)
row:                                        # lay out the indented fields side by side
  a : R := 1
  b : R := 2
tab "Label":                                # consecutive tab blocks form one tab group
  ...
```

Widgets by type: `Boolean` is a checkbox, `Color` a color picker, `Z` with
`choice` a dropdown, everything else a text box that accepts any expression
of the declared type (`List of C` shows as `1, 2, i` without brackets; `Text`
shows the raw text). Defaults are expressions (`2 pi / 3`, `-0.12 + 0.75i`,
`[0, 1, i]`, `dark blue`) and can't refer to other variables. A table,
`T : Table 2 of C`, is declared with *no* default (it starts empty, size
0), has no widget, and is kept when the project is rebuilt if the new
script declares it the same way (see Tables in §3). The wx app can save
tables' contents in the .fs file (`tables:` key, asked on save); fsrun
ignores saved tables (and `values:`), so headless runs start empty, unless
a scenario runs `load saved table T[, U...]` or `load all saved tables`.

### `@common`

Holds only definitions: variable declarations (`x := ...`, `x : T := ...`),
functions (`f(z) := ...`), and code blocks (`define name: ...`). No
attributes. Each viewer and tool handler gets the common definitions it
uses (and whatever those use), as if written at its top, and each is
type-checked separately in that script. So a common definition can mention a
viewer's coordinate variable or any config variable, and is only an error in
scripts that actually use it where those names don't exist.

### `@viewer "Title" attrs`

A window over the complex plane, or (with `xvar=` and `yvar=`) over the
real plane. Its body runs once per pixel (several times per pixel, in
fact: the app renders progressively and supersamples), with the pixel's
point bound to a complex variable (or a pair of real ones), and produces
`color`: either the body sets it, or the viewer's *colorizer* does (see
below).

| Attribute | Default | Meaning |
|---|---|---|
| `var=` | `z` | name of the pixel's complex coordinate. Can be a `@config` variable's name (e.g. `var=C` with `C` in `@config`): inside this viewer, it then means the pixel instead of the config value. This is how parameter-plane viewers are made. |
| `xvar=`, `yvar=` | none | instead of `var=`: a *real viewer*, whose pixel is the pair of real coordinates `(x, y)` with these names. Both are needed, and they can't be used with `var=`. Either can be a real `@config` variable's name, for a plane of real parameters (`xvar=a yvar=b`). |
| `center=` | `0` | original center ("Original view" returns here); quote anything but a plain number: `center="-0.75 + 0.1i"`. A real viewer's is a point, `center=(1, -1/2)`, or a complex number. |
| `pixelSize=` | `1/128` | model width of one pixel (zoom) in the original view; `1/200`, `0.005` |
| `view=` | `plane` | `sphere`: show a complex viewer's points on the Riemann sphere, as a globe. `z` is still a complex number (stereographic: 0 is the north pole, ∞ the south pole); `center=` is the point facing you, and `pixelSize=` is measured on the unit sphere (default: the globe fills most of the window). Navigate's drag turns the globe; the viewer's menu switches between sphere and plane. Not for real viewers. |
| `aspect=` | `1` | real viewers only: a pixel's height over its width in the original view (`aspect=2` fits twice as much of the y axis). A complex viewer's pixels are always square. The wx viewer's menu has "Aspect ratio..." to change it. |
| `limit=`, `escape=`, `vanish=` | `max_iters`, `escape_radius`, `vanishing_radius` | name a different (config) variable to use as the loop limit / escape radius / vanishing radius in this viewer |
| `pixelVar=` | `pixel_size` | name under which the current pixel size is visible |
| `colorizer=` | automatic | the viewer's default colorizer, e.g. `colorizer=Smooth`, `colorizer="Rainbow(arg z / (2 pi))"` (quote anything but a bare name) |

There are no `size=`, `position=`, or `resizable=` attributes (they're
rejected): window geometry isn't part of the script. Viewers open at 600x600;
the wx app saves each window's size, position, and current view in the project
file under a top-level `wx-properties:` key, e.g.

```yaml
wx-properties:
  viewers:
    "Julia set":          # viewer title
      width: 512          # image size in pixels
      height: 512
      x: 100              # window position
      y: 50
      resizable: false    # default true
      visible: false      # window closed; default true
      center: [-0.75, 0.1]   # current view (the script's center= is the original)
      pixel-size: 0.005
      aspect: 2              # a real viewer's current aspect, when it isn't 1
      view: sphere           # a complex viewer showing the other view than its script's
      rotation: [1, 0, 0, 0] # on the sphere, the globe's orientation (a unit quaternion)
  configuration: { x: 40, y: 80 }
```

Every field is optional, and the app moves windows back onto the primary display
if `x`/`y` would put them off it. `fsrun` ignores all of it: a scenario that needs a
particular size or view says `resize viewer to 512x512`, `set viewer center
to ...`, `set viewer pixel size to ...`, `set viewer aspect to ...`.


Inside a viewer:
- in scope: the coordinate (`z` or `var=`; or `xvar=` and `yvar=`), all
  `@config` variables (read-only), `pixel_size` (model width of a pixel;
  use it for zoom-independent thickness), in a real viewer also
  `pixel_width` (the same) and `pixel_height`, the window's geometry
  (`viewer_width`, `viewer_height`: Z, the image size in pixels;
  `viewer_center`: C, the current view's center, or in a real viewer
  `viewer_center_x`, `viewer_center_y`: R; on the sphere the center is 0),
  `color` (the output, starts grey),
  `iterations` and `stuck` (see Loops).
- viewers can't change the configuration: assigning a config variable
  only changes that pixel's local copy. Only `color` is output.
- a viewer that never assigns `color` gets the `Rainbow` colorizer (see
  below), unless its `colorizer=` says otherwise. With `colorizer=Manual`
  it renders grey (128, 128, 128).
- a pixel whose script indexes a list out of range, or runs out of its
  1 MB of scratch memory for lists/text, is drawn magenta (255, 0, 255);
  the app shows why in a status bar, and `fsrun` prints a warning.
- The coordinate is always complex. For a real 2-D system use
  `x := re(z)` and `y := im(z)`.

Code that doesn't depend on the pixel (values derived only from the
configuration, a list built from a critical orbit, `@common` values) is
split off automatically and run once per render, compiled, before the
pixels; nothing needs marking. Only statements every pixel runs are taken
(not ones inside a pixel-dependent `if` or loop), and code reading
`pixel_size` stays per-pixel, since it changes between the renderer's
passes.

#### Colorizers

A colorizer is code that runs after the viewer's body and sets `color`.
Which one a viewer uses: the one chosen in the app's viewer menu (saved
under a top-level `colorizers:` key, by viewer title; `fsrun` honors it
too), else the `colorizer=` attribute, else `Manual` if the body assigns
`color` anywhere (including in code it `run`s) and `Rainbow` if it
doesn't. Prefer letting a colorizer do the coloring when one fits; write
`color <- ...` yourself only for coloring a colorizer can't express.

| Colorizer | Parameters | Settings (in a "Coloring" config tab, one copy per viewer) |
|---|---|---|
| `Manual` | | none: the body sets `color` |
| `Rainbow` | `t = iterations / color_speed` | `color_speed` (20), `use_arenberg` (false). Stuck points are black. |
| `Escape time` | `t = iterations / period` | `palette` (choice: "Rainbow", "Arenberg", "Romao", "Bamo", "Broco", "Corko", "Viko", "Fire", "Ice", "Ocean", "Forest", "Rose", "Wheat", "Two colors"), `period` (20), `shade` (false), `color1`/`color2` (for "Two colors"), `gives_up` (black) |
| `Smooth` | `z` | `palette`, `period` (20), `degree` (2), `color1`/`color2`, `gives_up` |
| `Binary decomposition` | `z` | `period` (10), `degree` (2), `upper1`/`upper2`, `lower1`/`lower2`, `smooth` (true), `gives_up` |
| `Glow` | | `brightness` (0.5), `near`/`far` colors, `gives_up`. Uses `iterations / max_iters`. |
| `Autocolor` | `z` | `allow_new` (true), `blend_misses` (true), `shade` (true), `period` (20), `gives_up`. `autocolor(0, z)`, or with `allow_new` off `get_autocolor(0, z)` (the blend; `nearest_autocolor` with `blend_misses` off) (group 0, shared by all its viewers): Newton basins without known roots. Set `allow_new <- false` in a parameter plane, so it reads the dynamical plane's colors instead of adding a point per pixel. |

A parameter without an argument uses its default, else the viewer's
variable of the same name (so `Smooth` needs a `z` in scope at the end of
the body, or an argument: `Smooth(w)`). Arguments are expressions in the
viewer's scope: `colorizer="Rainbow(arg z / (2 pi))"` colors Newton basins
by the angle of the root.

Change a colorizer's setting defaults with a `coloring:` block, first in
the body; choices are given by their quoted label:

```
@viewer "M" var=C colorizer=Smooth
coloring:
  palette <- "Two colors"
  color1 <- red
  period <- 100
z := 0
iterate z -> z^2 + C until z escapes
```

It applies to the viewer's default colorizer only; an unknown setting or
choice is an error on that line.

#### Autocolor

`autocolor(n, p)` (n: a literal ℤ group number, p: ℂ) returns a color for
point `p`, adding it to group `n`'s table with a new hue if no entry is
within the group's tolerance (0.001). Never adding: `nearest_autocolor(n, p)`
(a miss gives the group's miss color, grey) and `get_autocolor(n, p)` (a
miss gives a blend of all the group's colors, weighted by 1/distance²). A
locked (or full, 256-point) group's `autocolor` blends too, unless the group's
`blend-misses: false` (Autocolor window: "Blend when no matching point"). For Newton basins without
known roots: `color <- if stuck then black else autocolor(0, z)`.
Viewers naming the same group share colors. Usable in viewers and tools;
LLVM backend only. Colors depend on the order points are first seen, so
they aren't reproducible between runs of an unsaved project (the project
file's `autocolor:` key saves them; a locked group, or one with 256
entries, adds no more). Don't use it on points that never repeat (every
pixel's escaping `z`): the group fills up and the rest get grey.

### `@colorizer "Name" attrs`

A project's own colorizer: settings declared like `@config` fields, then
`define colorize(params):` and its block. Optional `description="..."`.

```
@colorizer "Escaped"
inside : Color as "Inside" := black
define colorize(z, t = iterations / 20):
  color <- if z escaped then rainbow(t) else inside
```

Its code runs at the end of the viewer's code (sees `iterations`,
`stuck`, config, `@common`). Everything it declares (settings, parameters,
locals) is renamed privately per viewer, so it never clashes with the
viewer's names. Parameter defaults can use the colorizer's settings and
earlier parameters. A `@colorizer` with a built-in's name replaces it.
Its code is only type-checked in viewers that use it.

### `@tool "Name" attrs`

Interactive behavior for one viewer: responds to clicks, drags, and
refreshes, can draw overlays, and can change configuration variables (the
viewers then re-render).

| Attribute | Default | Meaning |
|---|---|---|
| `for=` | the only viewer | target viewer title; required if there are several viewers. Quote titles with spaces: `for="Julia set"` |
| `shortcut=` | none | single-key shortcut, e.g. `shortcut=t` |
| `refreshOnActivate=` | `true` | run `on refresh` right after `on activated` |
| `drawLayer=` | `100` | stacking order of this tool's drawings among tools |
| `refreshCanUpdate=` | `false` | whether changes that `on refresh` makes to the tool's own fields trigger another refresh. `true` makes a refresh handler that changes its fields run repeatedly (about 20 times a second): an animation. |
| `icon=` | | accepted, currently unused |

Body: first any config-style declarations (the tool's own panel, shown while
the tool is selected; these variables persist between events and can be
changed by the handlers), then event blocks:

| Header | Fires |
|---|---|
| `on click:` / `on click at w:` / `on click at (x, y):` | single click |
| `on double-click [at ...]:` | double click |
| `on drag [from s to w]:` | repeatedly while dragging |
| `on drag-finished [from s to w]:` | when a drag ends |
| `on click-or-drag [at ...]:` | one body for click and drag |
| `on refresh:` | after activation (see `refreshOnActivate`), every time the viewer re-renders (pan, zoom, resize, or a change to a config variable the viewer uses), and when the tool's own fields change, whether edited in its panel or by another handler (see `refreshCanUpdate`). |
| `on activated:` / `on deactivated:` | tool selected / deselected (use `on deactivated: erase` to clean up) |
| `on button "Label":` | a `button "Label"` in this tool's own declarations was pressed |
| `on timer "name" every N ms:` | repeatedly while the tool is selected (from `fsrun`: `trigger timer "name"`) |

At most one block per event kind, except `button` (one per label).

Coordinates: `at w` binds the point as a complex `w`; `at (x, y)` binds two
reals (in either kind of viewer). Without `at`, the point is bound to the
viewer's own coordinate name(s): `var=`, or a real viewer's `xvar=`/`yvar=`. **Caution**: if the viewer's coordinate is a `@config` variable
(`var=C`), a handler using the default coordinate writes the clicked point
into that config variable even if the body never assigns it. That is the
idiomatic "click to pick a parameter" tool (`on click-or-drag: pass`), and
a nasty surprise otherwise; use `on click at w:` when you don't want it.
`on refresh`, `activated`, `deactivated`, `button` and `timer` have no
point and can't see the viewer coordinate.

In scope in a handler: all config variables (ensemble and the tool's own;
assigning them updates the UI and re-renders viewers that use them), the
bound point, `pixel_size` (and a real viewer's `pixel_width`/`pixel_height`),
the viewer's `viewer_width`, `viewer_height` and `viewer_center` (or
`viewer_center_x`/`_y`), `max_iters` etc. Local variables are fresh on
every event; persistent tool state must be a declared tool variable.
Loops in handlers obey the same limits as viewers.

Drawing (tools only); points are complex numbers or `(x, y)` pairs in model
coordinates:

```
erase                                   # clear this tool's drawings
use red for stroke                      # (or `for line`): lines, outlines, points, text
use blue for fill                       # filled shapes
draw point at p
draw line from p to q
draw circle at p with radius r          # r in model units; use k pixel_size for screen units
draw filled circle at p with radius r
draw rectangle from p to q
draw filled rectangle from p to q
write "label" at p                      # text centered on p, in the stroke color
write text("z = ", z, ", n = ", n) at p # text(...) needs 2+ arguments
write 3.5 at p                          # numbers are converted automatically
```

A viewer can also have one **overlay**, `@tool overlay for="Title"` (the
bare word, not a quoted name): only an `on refresh:` block, no
declarations, no attributes but `for=`. It's never selected; its drawings
are always shown, above the viewer and below every tool's (whatever their
`drawLayer=`), and its refresh runs when the project opens, when the view
changes, and when any configuration value changes. Use it to mark
parameters (e.g. the current `C` on a parameter plane) that tools or other
viewers change. In `fsrun`, it's refreshed before each capture or assert.

Drawings accumulate until `erase`; most handlers start with `erase`. The pen
starts white and keeps the last `use` color from earlier events, so set the
colors explicitly in each handler that draws. Points, lines, outlines and
text use the stroke color; filled shapes use the fill color.

## 2. Types

| Type | Spellings | Literals |
|---|---|---|
| integer | `Z`, `ℤ`, `Int`, `Integer` | `3`, `-7` |
| real | `R`, `ℝ`, `Real` | `0.5`, `2.0` (a digit is required before the `.`) |
| complex | `C`, `ℂ`, `Complex` | `1 + 2i`, `2 𝑖`, `e^(i theta)` |
| boolean | `Boolean`, `Bool` | `true`, `false` |
| color | `Color` | `red`, `rgb(0.2, 0.4, 1)`, ... |
| text | `Text` | `"quoted"`, with `\"`, `\\`, `\n`, `\t` escapes |
| list | `List of T` | `[1, 2, 3]`, `[]` |
| pair | `T x U`, `T × U` | `(1.0, 2.0)` |
| table | `Table N of T` (T one of Z, R, C, Boolean, Color, or a tuple of them: `Table 2 of C x Z`) | none: declared in `@config`, filled by tools; dims 1-2 in the plane, more are layers |

Numbers widen automatically Z → R → C wherever needed (`k := 0` then
`k <- 0.5` makes `k` real). Nothing narrows automatically: use `round`,
`floor`, `ceiling`/`ceil` (real → Z), `re`, `im`, `abs |…|`, `arg`.
`/` always produces a real (`7 / 2` is 3.5); `//` is integer division;
`mod(a, b)` works on integers and reals. A cast is written `(e : T)`.

**No scientific notation**: `1e-6` (or `3.5e-2`, `2e5`, `1E-6`) is an error,
since `e` is Euler's number; the message says what to write instead. Write
`0.000001` or `10^(-6)`. (`2e`, `2e - 1`, and `2e^x` still mean Euler's
number.) A power of integers used as a real is computed as a real:
`3.5 * 10^(-2)` is 0.035, while `2^(-1)` as an integer is 0.


**Reserved words** that can't be variable or function names: `e`, `i`,
`pi`, `if`, `then`, `else`, `and`, `or`, `not`, `true`, `false`,
`times`, `text`, `escapes`, `vanishes`, `stops`, `iterations`, `stuck`, `append`,
`prepend`, `join`, `remove`, `find`, `transform`, `range`, `length`, `diff`,
`mod`, `point`, `table_size`, `table_cell`, `table_point`, `in`, `filled`, `viewer_width`, `viewer_height`, `viewer_center` (and `_x`/`_y`; as
names in `@config`), `rgb`, `blend`, `cycle`, `dark`, `light`, `invert`, the color names,
the color-wheel names, and the built-in function names (including `bar`, the
complex conjugate; using one for a variable, function, or code block is
reported as such). In particular,
**loop counters can't be called `i`** (use `k`, `j`, `n`) and **no variable
can be called `e`**. Also avoid `C` as a type-vs-variable confusion: `C : C
:= 0` is legal and common (a complex variable named C).

## 3. Expressions

Arithmetic: `+ - * / // ^`, unary `-` (`×` is also `*`). Implicit multiplication by
juxtaposition: `2 z`, `x y`, `2 pi i theta`, `r x (1 - x)`. Functions apply
to a simple following argument without parentheses (`cos z`, `log |z|`),
but `cos x + y` is `(cos x) + y`, and `cos 2z` is rejected:
write `cos(2 z)`. When juxtaposing, a function application must be the
last factor: `2 cos z` and `z cos z` are fine, `cos z 2` and `cos(z) 2` are
not (write `2 cos z`).

Powers: `z^2`, `z²` (superscript digits, `z⁻¹`), `2^(-m / S)`, `z^(1/3)`
(principal branch). `e^(...)` and `e^{...}` for exponentials. Any of
`()`, `[]`, `{}` group, but `[...]` with commas is a list.

Comparisons: `<`, `>`, `<=`/`≤`, `>=`/`≥` (integers and reals only).
`=` (or `==`) and `!=` (`≠`, `=/=`) are **approximate** for reals and
complex numbers: `x = y` means `x - y vanishes`, i.e. within
`vanishing_radius`. They're exact on integers and booleans. Use `===`
(`≡`) for exact numeric equality. `z escapes` / `z escaped` means
abs(z) > escape radius, and `z vanishes` / `z vanished` means abs(z) <
vanishing radius. In a loop's condition (only there), `x stops` / `x
stopped` means `x` (any expression) is `=` to its value at the start of
the latest pass through the body: `iterate z -> newton(z) until z stops`.
It's false at the first check of a `while`/`until`/`iterate` loop.

Boolean: `and`, `or`, `not`. Chains of one (`p and q and r`) are fine,
but mixing `and` with `or` without parentheses is a parse error ("`and`
and `or` can't be mixed without parentheses"): write `(p and q) or r`.
`and` and `or` short-circuit (in every backend): `p and q` evaluates `q`
only if `p` is true, `p or q` only if `p` is false, so
`length(xs) > 5 and xs @ 6 = 0` is safe on a short list.

Conditional expression: `if c then a else b` (chains: `if ... then ... else
if ... then ... else ...`).

Functions:
- real and complex: `exp log sqrt sin cos tan sinh cosh tanh`
- real only: `arcsin arccos arctan arcsinh arccosh arctanh` (also
  `asin`, `acos`, `atan`, `asinh`, `acosh`, `atanh`)
- complex → real: `re im arg` (also `Re Im Arg`); complex → complex: `conj`/`bar`
- real → integer: `round floor ceiling ceil`
- absolute value / modulus: `|x|` (integer, real, or complex → real)
- `mod(a, b)`
- derivatives: `diff(z, expr)` (symbolic, exact); for a one-variable
  user function `f`, `f'(z)` and `f''(z)`

Constants: `pi`/`π`, `e`/`𝑒`, `i`/`𝑖`, `true`, `false`.

Colors:
- names: `red green blue black white grey gray orange yellow purple violet pink`
- `rgb(r, g, b)` with components in [0, 1] (clamped to it; NaN counts as 0)
- `hsv(h, s, v)`, `hsl(h, s, l)`: hue h mod 1 (0 red, 1/3 green, 2/3 blue),
  s/v/l clamped to [0, 1]; hsl's l = 0.5 is the full color, 1 white
- `dark c`, `light c`, `invert c` (stackable: `light light red`)
- `blend(t, c1, c2)`: t·c1 + (1 − t)·c2 (so t = 1 gives c1)
- `cycle(t, c1, c2)`: period 1 in t, c2 at integers, c1 at half-integers
- smooth color wheels, period 1: `rainbow(t)`, `arenberg(t)`, `romao(t)`,
  `bamo(t)`, `broco(t)`, `corko(t)`, `viko(t)`; gradient wheels: `ice fire
  wheat rose forest ocean winter spring summer fall`. Typical use:
  `rainbow(arg z / (2 pi))`, `cycle(iterations / 20, dark c, light c)`.

Text: `text(a, b, ...)` concatenates two or more values of any scalar type
into Text. It can't take pairs.

Lists (1-based):
- `length(xs)`, `xs @ k` (error/magenta if out of range), `xs @@ k`
  (cyclic, any integer, 0 is the last item)
- `range(a, b)`: the integers a..b inclusive
- `join(xs, ys, ...)`, `append(xs, x)`, `prepend(xs, x)`
- `remove(xs, x -> pred)`, `transform(xs, x -> expr)` (same element type),
  `find(xs, x -> pred, fallback)`
- `@` and `@@` bind like multiplication: `xs @ k = 3` compares the item,
  and `xs @ k + 1` is `(xs @ k) + 1`; write `xs @ (k + 1)` for the next one.
- An empty list needs a declared type: `xs : List of C := []`.

Tables (`T : Table 2 of C` in `@config`; contiguous storage, O(1) access,
for per-pixel data a tool works out ahead of time, e.g. by carrying values
from pixel to neighboring pixel, which a viewer can't):
- `T @ (j, k)`: column j, row k, 1-based, (1, 1) at the top left, rows go
  down; n integers for n dimensions (`T @ k` for 1-D). Out of range is an
  error (magenta / discarded tool event). No `@@`.
- Elements can be tuples (`T : Table 2 of C x Z`), read and written whole:
  `(z, k) := T @ point c else (0, 0)`, `T @ (j, k) <- (w, 1)`; no way to
  write one part of a cell's tuple (read it, write it back).
- The first two dimensions are in the plane; more are *layers* over it
  (`T : Table 3 of C`, `T @ (j, k, m)` is layer m).
- `T @ point z` (2-D), `T @ (point z, m)` (layers): the cell z is in
  (cells are small rectangles; a table laid over a window has one cell
  per pixel). `point z` stands for the first two indices and must come
  first. Off the table is an error; `T @ point z else v` /
  `T @ (point z, m) else v` gives `v` there instead (it's
  `if z in T then ... else v`; `v` is a single operand, like the
  right side of `*`, so `T @ point z else 0 + 1` adds 1 to the result).
- Slices: `*` for every index of a dimension. With exactly one `*` it's a
  List: `T @ (point z, *)` (z in each layer), `T @ (j, *)` (a column),
  `T @ *` (all of a 1-D table). Two or more `*`s only as a copy (below).
- `z in T` (2-D or more): whether the point z is on the table.
- `table_cell(T, z)` (2-D or more): the cell z is in, as `(j, k)`, even off
  the table (indices may then be < 1 or > the size): `(j, k) := table_cell(T, z)`.
- `table_point(T, (j, k))` (2-D or more): the point at the center of cell
  (j, k), for any j and k.
- `table_size(T)`: Z for 1-D, else a tuple: `(w, h) := table_size(T)`.
  0 until a tool fills the table in.
- Tools only (a viewer that does these is an error):
  `reset T to (w, h)` lays a zero-filled table over the tool's
  viewer window (at `(viewer_width, viewer_height)`, cell (j, k) is pixel
  (j - 1, k - 1)); `reset T to (w, h) from p to q` covers the
  rectangle with top left p and bottom right q (the size must then be in
  parentheses, or a single name or number). Either form can end with
  `filled with v` to start every cell at v instead of 0. `T @ (j, k) <- v`,
  `T @ point z <- v` and `T @ (point z, m) <- v` write a cell.
  `T @ (point z, *) <- xs` writes a one-`*` slice from a list of exactly
  its length. `T @ (*, *, 1) <- U @ (*, *, 2)` copies a slice onto one of
  the same shape (same table or another; U must be a table variable).
  `resize T to (w, h, n) [filled with v]` changes the size keeping cell
  (1, 1)'s place and the cell size: kept cells keep their values, the
  rectangle grows/shrinks at the right/bottom, new cells are 0 (or v).
  Drop layer k: copy layers k+1..n down one, then `resize` to n - 1.
- A tool's table changes take effect when its handler succeeds, and
  viewers that read the table redraw. Writing a table viewers may be
  reading (or that another variable holds: `U := T`) writes a copy, so
  renders in flight and `U` are unaffected. Filling a 600x600 table cell
  by cell in a compiled tool is fast.

## 4. Statements

```
x := expr                 # declare, type inferred from all later uses
x : R := expr             # declare with a type
x <- expr                 # assign an existing variable
x, y <- e1, e2            # assign several at once (right side computed first;
                          # `x, y <- y, x` swaps); also (x, y) <- p for a pair p
u, v := 0, 1              # declare several, each type inferred (no typed form)
pass                      # do nothing
if cond:                  # also `else if cond:` and `else:`
  ...
while cond:               # also: until cond:
  ...
while cond up to N times: # explicit iteration limit
  ...
repeat:                   # body runs at least once; ends with while/until
  ...
until cond                # (or `while cond`); `repeat up to N times:` too
iterate z -> f(z) until cond          # shorthand loop on one variable
iterate z -> f(z) while cond up to N times
iterate (x, y) -> (1 - a x^2 + y, b x) until (x, y) escapes   # tuples too
for each x in xs:         # any list expression: [1, 2, 3], range(1, n), ...
  ...
with first x matching pred in xs:
  ...
else:
  ...
f(z) := expr           # function; no recursion
g(a, b) := a f(b)
define shade(c):          # named code block, parameters optional
  ...
run shade(blue)           # `run name` with no arguments, or run name(a, b)
reset T to (w, h)   # tools only: a table over the viewer's window
reset T to (w, h) from p to q   # ... or over a rectangle (corners)
reset T to (w, h) filled with v # ... starting every cell at v, not 0
T @ (j, k) <- v           # tools only: write a table's cell
T @ point z <- v          # ... the cell the point z is in
T @ (point z, m) <- v     # ... in layer m of a table with layers
T @ (point z, *) <- xs    # ... a slice with one `*`, from a list as long as it
T @ (*, *, 1) <- T @ (*, *, 2)   # ... copy a slice onto one of the same shape
resize T to (w, h, n)     # tools only: new size, cells kept in place
```

A long line can be continued with `...` (or `…`) at the end.

Scope: a variable is visible from its declaration to the end of its block.
Declaring a name that already exists (including the viewer coordinate or a
config variable) is an error ("already defined"); assign it instead.
Variables declared inside an `if` or loop body aren't visible after it.

Loops: every `while`/`until`/`repeat`/`iterate` stops after `max_iters`
iterations (or the viewer's `limit=` variable) unless given `up to N
times`. After a loop, `iterations` is how many times its body ran, and
`stuck` is true if it stopped because of the limit rather than its
condition. `for each` is not limited and doesn't set them. Use `while
true up to N times:` for a fixed number of iterations.

Functions (`f(z) := ...`): the body is one expression; parameters are local; any
other names refer to what's in scope where the function is *used* (so a
common `f(z) := z^2 + C` works in a viewer whose `var=C`). A function
works at whatever numeric type it's called with. No recursion.

`define`/`run`: the block runs as if pasted at the `run` site; parameters
become local variables (copies: assigning a parameter doesn't change the
argument); it reads and assigns the caller's other variables (e.g.
`color`, `z`), so `define step:` updating `x` and `y` directly is how to
share a multi-variable step. Variables it declares are local to each run.

Tuples: `a, b, c` is `(a, (b, c))`. Tuples of numbers compare with `=`
by distance (within `vanishing_radius`), and `escapes`/`vanishes`/`stops`
measure them by length (L2), so `until (x, y) escapes` means
x² + y² > escape_radius². There is no way to take a pair apart except a
tuple assignment.

## 5. Headless runner (`fsrun`)

Built from `fractalstream-ui-headless` (`stack build` there; locate it with
`stack exec which fsrun`).

```
fsrun snapshot -p project.fs --viewer "Viewer title" -o out.png
fsrun commands --project project.fs --commands commands.txt
fsrun scenario --project project.fs --name "name"   # from the project's scenarios:
fsrun update-preview --project project.fs           # runs metadata: preview-commands:
fsrun normalize -p project.fs                       # ASCII spellings to symbols: <- to ⟵, R to ℝ, z^2 to z²
fsrun translate -i old-script.txt -o project.fs     # a script for the original FractalStream
fsrun COMMAND --help                                # options for one subcommand
```

Each subcommand also takes `-j N` / `--jobs N` (default: every core).
Pass `-j 1` when asserting on a viewer that adds points to an unlocked
autocolor group, whose colors otherwise depend on render order.

Command language (one per line, `#` comments): `select viewer "T"`,
`select tool "N"`, `deselect tool`, `set parameter x to <expr>`,
`set tool parameter x to <expr>`,
`set colorizer parameter x to <expr or "choice">` (the selected viewer's
colorizer's setting), `resize viewer to 800x600`,
`set viewer center to <point>`, `set viewer pixel size to <expr>`,
`click at <point>`, `double-click at <point>`, `drag from <p> to <q>`,
`press button "L"`, `trigger refresh`, `trigger timer "name"`,
`capture viewer "T" to "f.png"` (with tool drawings),
`capture viewer "T" base to "f.png"` (without),
`assert viewer "T" pixel (px, py) is (r, g, b) [within tol]`.

A project can also carry a `metadata:` key, for FractalStream's library of
projects: a `title`, a plain-text `description`, and `preview-commands`
that make its preview image. Give a new project all three:

```yaml
metadata:
  title: Newton's method for z^3 - 1
  description: |
    Basins of attraction of Newton's method, colored by root.
  preview-commands: |
    select viewer "Newton basins"
    capture viewer "Newton basins" as preview
```

then run `fsrun update-preview --project project.fs`, which renders the preview
(128 px on the long side) and writes it into the file as `preview:`
(base64 PNG; never write that by hand). `capture viewer "T" as preview`
(or `... base as preview`) is only allowed in `preview-commands`.
Points are `x + y i` or `(x, y)`. Pixel (0, 0) is the top-left corner; with
the defaults, model point 0 is pixel (300, 300) and one unit is 128 pixels.

Errors: a parse or type error anywhere, including in tool handlers, stops
loading with a message pointing at the line (shown with a caret). Runtime
errors in a tool (e.g. a list index out of range) only show up when the
handler runs, so exercise each tool. A failed assert
prints the actual color, which is a handy way to probe values: make the
viewer color a pixel by a condition and assert on it.

## 6. Error messages you'll see

| Message | Usual cause |
|---|---|
| `I got stuck and couldn't parse the input here, where I was expecting ...` | a reserved word as a variable (`i := 0`), a statement where an expression should be, bad indentation, `text(x)` with one argument |
| `To avoid ambiguity when using implicit multiplication, functions must go to the left of other values` | `cos 2z` (write `cos(2 z)`) |
| `` `and` and `or` can't be mixed without parentheses ... `` | `p and q or r`; write `(p and q) or r` or `p and (q or r)` |
| `A @config stanza can't have a button ...` | move the button into the `@tool` whose `on button` handles it |
| `The variable z is already defined, and would be re-defined here.` | `:=` on an existing name; use `<-` |
| `No variable named w is defined here.` | out of scope (declared inside an `if`/loop), typo, or a refresh handler using the viewer coordinate |
| `I expected a real number here, but z is a complex number.` | assigning a wider type to a declared variable; use `re`, `|…|`, or declare it wider |
| `I expected an integer here, but the result of an arithmetic operation is a real number.` | `/` on integers; use `//` or `round(...)` |
| `No function named f is defined here.` | recursion, or using a function before its definition |
| `A list or table index was out of range, so this tool's changes were discarded.` (from fsrun, or a dialog in the app) | a tool handler indexed a list or table out of range (or wrote off a table); the app switches back to Navigate |
| `warning: viewer "V": Magenta pixels: ...` | list or table index out of range (or `T @ point z` off the table), or list/text scratch memory exhausted, in some pixels |
