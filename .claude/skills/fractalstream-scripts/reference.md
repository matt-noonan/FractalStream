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
name : Type <- default                      # a field; the type is required
name : Type as "Label" <- default           # with a display label
mode : Z as "Mode" choice { "A", "B", "C" } <- 0   # dropdown; value is 0-based index
text "Some explanatory text"
button "Label"                              # @tool declarations only (an error in @config)
row:                                        # lay out the indented fields side by side
  a : R <- 1
  b : R <- 2
tab "Label":                                # consecutive tab blocks form one tab group
  ...
```

Widgets by type: `Boolean` is a checkbox, `Color` a color picker, `Z` with
`choice` a dropdown, everything else a text box that accepts any expression
of the declared type (`List of C` shows as `1, 2, i` without brackets; `Text`
shows the raw text). Defaults are expressions (`2 pi / 3`, `-0.12 + 0.75i`,
`[0, 1, i]`, `dark blue`) and can't refer to other variables.

### `@common`

Holds only definitions: variable declarations (`x := ...`, `x : T <- ...`),
functions (`let f(z) = ...`), and code blocks (`define name: ...`). No
attributes. Each viewer and tool handler gets the common definitions it
uses (and whatever those use), as if written at its top, and each is
type-checked separately in that script. So a common definition can mention a
viewer's coordinate variable or any config variable, and is only an error in
scripts that actually use it where those names don't exist.

### `@viewer "Title" attrs`

A window over the complex plane. Its body runs once per pixel (several
times per pixel, in fact: the app renders progressively and supersamples),
with the pixel's point bound to a complex variable, and must set `color`.

| Attribute | Default | Meaning |
|---|---|---|
| `var=` | `z` | name of the pixel's complex coordinate. Can be a `@config` variable's name (e.g. `var=C` with `C` in `@config`): inside this viewer, it then means the pixel instead of the config value. This is how parameter-plane viewers are made. |
| `center=` | `0` | initial center; quote anything but a plain number: `center="-0.75 + 0.1i"` |
| `pixelSize=` | `1/128` | model width of one pixel (zoom); `1/200`, `0.005` |
| `size=` | `600x600` | window size in pixels |
| `limit=`, `escape=`, `vanish=` | `max_iters`, `escape_radius`, `vanishing_radius` | name a different (config) variable to use as the loop limit / escape radius / vanishing radius in this viewer |
| `pixelVar=` | `pixel_size` | name under which the current pixel size is visible |
| `resizable=` | `true` | |
| `position=` | staggered | window position, `100x50` |

Inside a viewer:
- in scope: the coordinate (`z` or `var=`), all `@config` variables
  (read-only), `pixel_size` (model size of a pixel; use it for
  zoom-independent thickness), `color` (the output, starts grey),
  `iterations` and `stuck` (see Loops).
- viewers can't change the configuration: assigning a config variable
  only changes that pixel's local copy. Only `color` is output.
- a viewer that never assigns `color` renders grey (128, 128, 128).
- a pixel whose script indexes a list out of range, or runs out of its
  1 MB of scratch memory for lists/text, is drawn magenta (255, 0, 255);
  the app shows why in a status bar, and `fsrun` prints a warning.
- The coordinate is always complex. For a real 2-D system use
  `x := re(z)` and `y := im(z)`.

Optional `prepare` block, first thing in the body:

```
@viewer "V"
prepare scale : R, center : C:
  scale <- a / 4
  center <- i
color <- if |z - center| < scale then red else blue
```

It declares outputs with types, assigns them, and the main body reads them.
It can't see the pixel coordinate; use it for values derived only from the
configuration. Rarely needed.

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
reals. Without `at`, the point is bound to the viewer's own coordinate
name. **Caution**: if the viewer's coordinate is a `@config` variable
(`var=C`), a handler using the default coordinate writes the clicked point
into that config variable even if the body never assigns it. That is the
idiomatic "click to pick a parameter" tool (`on click-or-drag: pass`), and
a nasty surprise otherwise; use `on click at w:` when you don't want it.
`on refresh`, `activated`, `deactivated`, `button` and `timer` have no
point and can't see the viewer coordinate.

In scope in a handler: all config variables (ensemble and the tool's own;
assigning them updates the UI and re-renders viewers that use them), the
bound point, `pixel_size`, `max_iters` etc. Local variables are fresh on
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
| pair | `T x U` | `(1.0, 2.0)` |

Numbers widen automatically Z → R → C wherever needed (`k := 0` then
`k <- 0.5` makes `k` real). Nothing narrows automatically: use `round`,
`floor`, `ceiling`/`ceil` (real → Z), `re`, `im`, `abs |…|`, `arg`.
`/` always produces a real (`7 / 2` is 3.5); `//` is integer division;
`mod(a, b)` works on integers and reals. A cast is written `(e : T)`.

**No scientific notation**: `1e-6` parses as `1·e − 6` (e is Euler's
number) without any error. Write `0.000001` or `1/1000000`.

**Reserved words** that can't be variable or function names: `e`, `i`,
`pi`, `if`, `then`, `else`, `and`, `or`, `not`, `true`, `false`, `let`,
`times`, `text`, `escapes`, `vanishes`, `iterations`, `stuck`, `append`,
`prepend`, `join`, `remove`, `find`, `transform`, `range`, `length`, `diff`,
`mod`, `rgb`, `blend`, `cycle`, `dark`, `light`, `invert`, the color names,
the color-wheel names, and the built-in function names. In particular,
**loop counters can't be called `i`** (use `k`, `j`, `n`) and **no variable
can be called `e`**. Also avoid `C` as a type-vs-variable confusion: `C : C
<- 0` is legal and common (a complex variable named C).

## 3. Expressions

Arithmetic: `+ - * / // ^`, unary `-`. Implicit multiplication by
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
vanishing radius.

Boolean: `and`, `or`, `not`. Parenthesize mixtures.

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
- `rgb(r, g, b)` with components in [0, 1]
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
- An empty list needs a declared type: `xs : List of C <- []`.

## 4. Statements

```
x := expr                 # declare, type inferred from all later uses
x : R <- expr             # declare with a type
x <- expr                 # assign an existing variable
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
for each x in xs:         # any list expression: [1, 2, 3], range(1, n), ...
  ...
with first x matching pred in xs:
  ...
else:
  ...
let f(z) = expr           # function; no recursion
let g(a, b) = a f(b)
define shade(c):          # named code block, parameters optional
  ...
run shade(blue)           # `run name` with no arguments, or run name(a, b)
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

`let` functions: the body is one expression; parameters are local; any
other names refer to what's in scope where the function is *used* (so a
common `let f(z) = z^2 + C` works in a viewer whose `var=C`). A function
works at whatever numeric type it's called with. No recursion.

`define`/`run`: the block runs as if pasted at the `run` site; parameters
become local variables; it reads and assigns the caller's variables (e.g.
`color`, `z`). Variables it declares are local to each run.

## 5. Headless runner (`fsrun`)

Built from `fractalstream-ui-headless` (`stack build` there; locate it with
`stack exec which fsrun`).

```
fsrun project.yaml --snapshot "Viewer title" --output out.png
fsrun project.yaml --commands commands.txt
fsrun project.yaml --scenario "name"      # from the project's scenarios:
```

Command language (one per line, `#` comments): `select viewer "T"`,
`select tool "N"`, `deselect tool`, `set parameter x to <expr>`,
`set tool parameter x to <expr>`, `resize viewer to 800x600`,
`set viewer center to <point>`, `set viewer pixel size to <expr>`,
`click at <point>`, `double-click at <point>`, `drag from <p> to <q>`,
`press button "L"`, `trigger refresh`, `trigger timer "name"`,
`capture viewer "T" to "f.png"` (with tool drawings),
`capture viewer "T" base to "f.png"` (without),
`assert viewer "T" pixel (px, py) is (r, g, b) [within tol]`.
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
| `A @config stanza can't have a button ...` | move the button into the `@tool` whose `on button` handles it |
| `The variable z is already defined, and would be re-defined here.` | `:=` on an existing name; use `<-` |
| `No variable named w is defined here.` | out of scope (declared inside an `if`/loop), typo, or a refresh handler using the viewer coordinate |
| `I expected a real number here, but z is a complex number.` | assigning a wider type to a declared variable; use `re`, `|…|`, or declare it wider |
| `I expected an integer here, but the result of an arithmetic operation is a real number.` | `/` on integers; use `//` or `round(...)` |
| `No function named f is defined here.` | recursion, or using a function before its `let` |
| `A list index was out of range, so this tool's changes were discarded.` (from fsrun, or a dialog in the app) | a tool handler indexed a list out of range; the app switches back to Navigate |
| `warning: viewer "V": Magenta pixels: ...` | list index out of range, or list/text scratch memory exhausted, in some pixels |
