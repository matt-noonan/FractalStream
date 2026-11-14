# FractalStream scripting language

FractalStream accepts both *expressions* and *scripts* in different
parts of the program. *Expressions* represent values of various types,
while *scripts* represent code that FractalStream should execute
step-by-step. For example, `z² + C` is an expression, but

```
z ⭠ z² + C
```

is a very simple script that tells FractalStream to update the value of
`z` to `z² + C`.

## Expressions

### Arithmetic expressions

The usual arithmetic operators `+`, `-`, `*` (multiplication, also written `×`), `/`, and `^` (exponentiation) are available for real and complex types. With the exception of
`/`, these are also available for integer types. For integer division,
use `//` instead.

A power of integers is an integer (`2^10` is 1024), except that a negative
power is a fraction, so it's a real number: `10^(-2)` is 0.01, and
`3.5 * 10^(-2)` is 0.035. That's decided by the exponent as it's written:
a negative constant, like `-2` or `1 - 3`, makes the power real. With a
variable exponent, like `2^k`, the power is an integer, unless it's used
as a real number (`x : R := 2^k`, or `2^k / 3`), where it's computed as a
real power. If `k` turns out to be negative where the power has to stay
an integer, it's rounded toward zero: 0, except that `1^k` is 1 and
`(-1)^k` is 1 or -1.

Numbers aren't written in scientific notation: `e` is Euler's number, so
`2e` means 2·e. Something like `3.5e-2`, which could be read either way, is
an error that suggests how to write what you meant: `0.035` (or
`3.5 * 10^(-2)`), or `3.5 e - 2` with spaces to multiply by e.


### Comparisons

Values can be compared for equality with `=` (alternate spelling: `==`),
and for inequality with `!=` (alternate spellings `=/=` or `≠`). Real
and integer values can be compared with `<`, `>`, `<=` (alternate spelling
`≤`), and `>=` (alternate spelling `≥`).

Real and complex numbers are equal by `=` when they're within
`vanishing_radius` of each other. Tuples of numbers, like a pair of reals
`(x, y)`, are equal when the distance between them is within
`vanishing_radius`: `(x, y) = (u, v)` when √((x − u)² + (y − v)²) is.
Likewise, a tuple of numbers `escapes` or `vanishes` by its length, as a
point of the plane (or of space, for a longer tuple): `(x, y) escapes`
when √(x² + y²) is more than `escape_radius`.

### Boolean operations

The boolean operators `and`, `or`, and `not` can be used. `not` applies
to what follows it directly, so `not p and q` means `(not p) and q`.
`and`s can be chained (`p and q and r`), and so can `or`s, but mixing
them needs parentheses to say which goes first: `(p and q) or r`, or
`p and (q or r)`. Without them, `p and q or r` is an error.

`and` and `or` only evaluate their right side when the left side doesn't
already decide the answer: in `p and q`, `q` is evaluated only if `p` is
true, and in `p or q`, only if `p` is false. So the right side can rely on
the left one, as in `length(xs) > 5 and xs @ 6 = 0`, which doesn't index
`xs` out of range when it's too short.

### Constants

FractalStream knows about the real constants `pi` and `e` (also spelled
`π` and `𝑒`), the imaginary unit `i` (also spelled `𝑖`), and
the boolean constants `true` and `false`. Numeric constants are
assumed to be integers if they don't include a decimal point, or
real numbers otherwise.

### Parentheses

Any of `(`/`)`, `[`/`]`, and `{`/`}` can be used to parenthesize subexpressions,
as in `𝑒^{π 𝑖 t}`.

### Functions

FractalStream knows a number of different functions.

Functions that can be used on both real and complex values are
`exp`, `log`, `sqrt`, `cos`, `sin`, `tan`, `tanh`, `cosh`, `sinh`, and `tanh`.

Functions that can be used only on real values are
`arccos`, `arcsin`, `arctan`, `arccosh`, `arcsinh`, and `arctanh`. Each of
these can also be spelled in its abbreviated form (e.g. `arccos` -> `acos`)

Functions that can be used only on complex values are
`re`, `im`, `arg`, and `conj`. These can also be spelled in capitalized form
(e.g. `re` -> `Re`)

### Derivatives

`diff(x, expr)` is the derivative of `expr` with respect to the real or
complex variable `x`. For example, `diff(z, z^3 - 1)` is `3 z^2`.
Derivatives are computed symbolically, so they are exact.

### Syntax quirks

FractalStream supports some syntactical shortcuts common in mathematics.
If a function is applied to a simple argument, the application can be
written without parentheses. So instead of writing `cos(z)`, you could write
`cos z` instead. But `cos x + y` would mean `(cos x) + y`, not `cos (x + y)`.

FractalStream also supports implicit multiplication when arguments are
concatenated, so `x y` is the same as `x * y` and `2 cos z` is the same as
`2 * cos(z)`. To avoid ambiguity, FractalStream will not allow you to
write expressions like `cos 2z` since it could be interpreted as
`(cos 2) * z` or as `cos (2 * z)`.

### Colors

Values can also describe colors, not just plain scalar values.
FractalStream knows about a number of predefined colors:
`red`, `green`, `blue`, `black`, `white`, `grey` (`gray`),
`orange`, `yellow`, `purple`, and `violet`. You can lighten or darken a
color with the `light` and `dark` operators, so `dark blue` is a kind of
navy blue, and `light light red` is a shade of pink.

Colors can be inverted with the `invert` operator, so `invert green`
is a shade of purple.

Custom colors can be made with the `rgb(r,g,b)` operation, where
`r`, `g`, and `b` are values representing real numbers from `0` to `1`
(a value below `0` counts as `0`, and one above `1` as `1`).
Colors can also be made from a hue, with `hsv(h, s, v)` (hue, saturation,
and value) or `hsl(h, s, l)` (hue, saturation, and lightness). The hue goes
around the color wheel once as `h` goes from `0` to `1`: `0` is red, `1/3`
green, `2/3` blue, and `1` red again (so `hsv(arg z / (2 pi), 1, 1)` colors
by the angle of `z`). The saturation, value, and lightness run from `0` to
`1` (anything outside that is treated as `0` or `1`). With `hsv`, value `1`
is the brightest the color gets, and `0` is black. With `hsl`, lightness
`0.5` is the full color, `0` is black, and `1` is white; saturation `0`
gives a grey, in either.
Two different colors can be blended by `blend(t, c1, c2)` where `t`
is a real number from `0` to `1` representing the percentage to take
from `c1`, with `(1 - t)` taken from `c2`.

#### Automatic colors

`autocolor(n, p)` gives each distinct point `p` a color of its own, the
first time it sees that point. It's for coloring by points you don't
know in advance, such as the root a Newton's method converges to:

```
lastZ ≔ 0
repeat:
  lastZ ⭠ z
  z ⭠ z - p(z) / p'(z)
until z = lastZ
color ⭠ if stuck then black else autocolor(0, z)
```

Points are kept in numbered *groups*; `n` is the group's number, and has
to be written as a number (`autocolor(0, z)`, not `autocolor(k, z)`).
Viewers that use the same group share its colors, so a parameter plane
and a dynamical plane can agree on what color each root is. A point gets
the color of the nearest point in its group within the group's tolerance
(0.001 to begin with), or else a new color.
Two other functions look a point up the same way, but never add one:

- `nearest_autocolor(n, p)` gives a point that isn't near any point in
  the group the group's "miss" color (grey to begin with).
- `get_autocolor(n, p)` gives it a blend of all the group's colors
  instead, each weighted by how near its point is (by one over the
  squared distance): the more useful picture in a viewer that only reads
  colors other viewers found, like a parameter plane, or with a locked
  group. (A group with no points gives the miss color.)

A group stops adding points once it's locked, or once it has 256 of them;
`autocolor` then works like `get_autocolor`, or, if the group's "Blend when
no matching point" setting (in the Autocolor window) is off, like
`nearest_autocolor`.

Which point gets which color depends on the order the points are first
seen in, and the app draws in pieces, on several threads at once: so
**the same script can give the same points different colors each time it
starts from scratch**. Saving a project saves its groups' colors, and
then they stay put.

"Autocolor settings..." in a viewer's Colorizer menu (or the "Open
autocolor settings..." button in the Autocolor colorizer's settings) opens
a window for a project's groups:
pick a group to see its points and their colors, and change a point or
its color, add or delete points, clear the group, lock it, change its
tolerance or the color it gives points it doesn't have, or choose whether
it blends its colors for those points.

Only the LLVM backend (the default) can compute `autocolor`,
`nearest_autocolor`, and `get_autocolor`. They can be used in viewers and
in tools.

## Script commands

### Variable assignment

To assign a value to a pre-existing variable, use the `<-` operator.

```
z <- z^2 + C
```

### Assigning several variables at once

A tuple of variables can be assigned all at once, from a tuple of values:

```
x, y ⭠ 1 - a x² + y, b x
```

(or with parentheses, `(x, y) ⭠ (1 - a x² + y, b x)`). Every value on
the right is worked out before any variable is set, so `x, y ⭠ y, x`
swaps `x` and `y`. Longer tuples work the same way, `a, b, c ⭠ b, c, a`,
and so do nested ones, `(a, b), c ⭠ p, 0` (where `p` is a pair); the
value on the right doesn't have to be written out as a tuple, as long as
its parts match.

You can also use the unicode versions `⟵`, `⭠`, or `←`. The script
editor writes `⟵` for `<-` once the script checks out, along with the
other symbols the language accepts (`⟼` for `->`, `≔` for `:=`, `≤`,
`≥`, `≠`, `ℝ`, `ℂ`, `ℤ`, `×`, `z²` for `z^2`, `π`, ...), unless that's
turned off in Preferences; `fsrun normalize` does the same to a
project file.

You can type symbols in the script editor by their LaTeX names:
`\theta` gives `θ`, `\le` gives `≤`, `\R` gives `ℝ`, and so on. A
preview shows what a name will become as you type it, and Tab completes
the name. Accents work as in LaTeX: `\hat x`, `\hat{x}` and
`\hat\theta` give `x̂`, `x̂` and `θ̂`. The other accents are `\tilde`,
`\bar`, `\overline`, `\dot`, `\ddot`, `\dddot`, `\vec`, `\check`,
`\breve`, `\acute`, `\grave` and `\mathring`. A variable's name can
have accents anywhere after its first character (`ẑ`, `x̄₁`). A name
with an accent is the same name however its letters were entered: a
keyboard's `â` and `\hat a` give the same variable.

### Variable initialization

To create a new variable in your script, use `:=` (or `≔`). Everything
that defines something new uses `:=`: variables, as here, and functions
(see "Defining functions"). The arrow `<-` only changes a variable that
already exists.

You can give the new variable a type:

```
k : Z ≔ 0
```

Common types you might want to use include `Z` for the integers, `R` for the
real numbers, `C` for the complex numbers, `Color` for a color, and
`Boolean` for a truth value, and `Text` for plain text. You can also use
the unicode blackboard bold variants `ℤ`, `ℝ`, and `ℂ`, or the full names `Integer`, `Real`, and `Complex`.

Lists of values of type `x` have the type `List of x`. Tables have types
like `Table 2 of C` (see "Tables"), and are only declared in `@config`.

Or you can leave the type out, and let FractalStream work it out:

```
z := 0
iterate z -> z^2 + C until z escapes
```

The variable's type is the smallest one that fits both its initial value
*and* everything that is later assigned to it, so here `z` is complex
(assuming `C` is), even though its initial value `0` is an integer.
Similarly, a list starting out as `xs := [1, 2]` becomes a list of reals if
you later `append(xs, 0.5)` to it. An empty list `[]` gives no clue about
what kind of items it will hold, so a variable starting out empty needs a
type annotation, like `xs : List of R := []`.

Several new variables can be made at once, each with its own type worked
out, with a tuple: `u, v := 0, 1` (or `(u, v) := (0, 1)`).

A new variable can be used in the rest of the block it is declared in.

### Defining functions

To define your own function, write it as you would on paper, with `:=`
(or `≔`):

```
f(z) := z^2 + C
g(a, b) := a * f(b) - b
iterate z -> g(2, z) until z escapes
```

The body of a function is a single expression, and it can use any variables
and functions that are defined where the function is defined. A function's
parameters are separate from any variables with the same name, so `f`'s
parameter `z` above has nothing to do with the viewer's own variable `z`.
A function can be used in the rest of the block it is defined in, and its
name can't also be used as a variable.

Functions don't need type annotations: each use of a function works with
whatever kinds of numbers it is given, so `f(2)` and `f(z)` are both fine.

The derivative of a function of one variable can be written with a prime:
`f'(z)` means the same as `diff(z, f(z))`, and `f''(z)` is the second
derivative. (You can also use `′`.) Derivatives of your own functions also
work inside `diff`, so Newton's method can be written as

```
f(z) := z^3 - 1
iterate z -> z - f(z) / f'(z) until f(z) vanishes
```

### Defining code to run

A block of code can be given a name with `define`, and then run (as many
times as you like) with `run`:

```
define shade(c):
  if stuck:
    color <- black
  else:
    color <- blend(iterations / 20, light c, dark c)

iterate z -> z^2 + C until z escapes
run shade(blue)
```

The parameters, like `c` here, are optional (`define shade:` ... `run
shade`). Each one acts like a new variable holding the value it is given,
with its type worked out from that value, so changing it inside the code
doesn't change anything outside. Everything else the code mentions, like
`color` and `iterations` above, refers to whatever is in scope where it is
run, and any variables the code declares are local to each run. A `define`
can be used in the rest of the block it appears in, and code can run
other code defined before it.

### Sharing definitions between scripts

In a project, an `@common` stanza holds definitions that every viewer and
tool script can use, as if they were written at the top of each one:

```
@config "Configuration"
a : C as "a =" := 1

@common
f(z) := z^3 - a
newton(z) := z - f(z) / f'(z)

@viewer "Newton's method" var=z
iterate z -> newton(z) until f(z) vanishes

@tool "Trace" for="Newton's method"
on click:
  erase
  repeat up to 20 times:
    draw point at z
    z <- newton(z)
  until f(z) vanishes
```

An `@common` stanza can only contain function definitions, code definitions
(`define`), and variable declarations. Each script gets just the common definitions it uses (and
whatever those use), checked against its own variables, so they can use
anything in scope there: here, the configuration variable `a`, and each
script's own `z`. A definition that mentions a variable some scripts don't
have is only a problem in scripts that use it. A project can have several
`@common` stanzas, anywhere in the script.

### Line continuations

If you have a very long command, it might be convenient to split it across
several lines. You can do this with the `...` or `…` line continuation markers. For
example,

```
z <- z^2 + 0.123456789 ...
         + 0.987654321 i
```

means the same thing as

```
z <- z^2 + 0.123456789 + 0.987654321 i
```

### Loops

There are six looping constructs: `while...`, `until...`, `repeat...while`, and
`repeat...until`, `iterate...while`, and `iterate...until`. For the first four kinds of loops, the body of the loop is given by an indented block of statements:

```
while |z| < escape_radius:
  z ⭠ z^2 + C
```


The `until` forms are just like the `while` forms but with
the condition logically negated. So

```
while n < 10:
  n ⭠ n + k
```

is the same as

```
until n ≥ 10:
  n ⭠ n + k
```

The `repeat...` forms of the loop always executes the loop body at least once. After running this code:

```
x ⭠ 0
repeat:
  x ⭠ x + 1
while 1 = 2
```

the value of `x` will be `1`. But after running this code:

```
x ⭠ 0
while 1 = 2:
  x ⭠ x + 1
```

the value of `x` will still be `0`.

The `iterate` forms are a special shorthand for `while` and `until` loops.
They look like `iterate v -> e until c` or `iterate v -> e while c`,
where `v` is a variable, `e` is an expression, and `until c` or `while c`
determines the exit condition.

You can use the unicode symbols `→`, `⭢`, `⟼`, or `↦` as synonyms for `->`.

The code

```
iterate z ⟼ z² + C until |z| > 2
```

is equivalent to

```
until |z| > 2:
  z ⭠ z² + C
```

Similarly, `iterate z ⟼ z² + C while |z| < 2` is equivalent to

```
while |z| < 2:
  z ⭠ z² + C
```

A tuple of variables can be iterated too, as a tuple assignment:

```
iterate (x, y) ⟼ (1 - a x² + y, b x) until (x, y) escapes
```

### More about loops

FractalStream enforces a limit on the number of times each loop will run.
This limit is configured in the script and often is exposed to the script's
user, so they can easily change the iteration limit while exploring.

Every project automatically gets an "Iteration settings" tab with three
settings: `max_iters` (the iteration limit, an integer, 100 to start),
`escape_radius` (for `escapes`, 10), and `vanishing_radius` (for
`vanishes`, `=`, and `stops`, 0.0001). To start one of them at a different
value, or give it a different label, declare it yourself in any `@config`
stanza, with the same type; the others stay in the "Iteration settings"
tab, which your declaration joins if it is in a stanza by that name:

```
@config "Iteration settings"
vanishing_radius : R as "Vanishing radius: " := 0.01
```

The special variable `iterations` always holds the number of iterations
performed by the last loop that finished. There is also a special predicate
`stuck` which is `true` if the last loop halted because it reached the
iteration limit, or `false` if the last loop halted because its exit condition
was successfully met.

Here are two simple scripts that draw a Julia set for `z² - 1`. The first
one re-checks the loop exit condition when deciding what color to use:

```
iterate z ⟼ z² - 1 until |z| > 10

# NOTE: this recomputes the loop exit condition
color ⭠ if |z| > 10 then white else black
```

The second one is equivalent, but just checks if the loop was "stuck" (hit
the iteration limit) or not:
```
iterate z ⟼ z² - 1 until |z| > 10

# NOTE: no need to recompute the exit condition,
#       just check how the loop terminated
color ⭠ if stuck then black else white
```

### Waiting for a value to settle down

In a loop's condition, `v stops` (or `v stopped`) is true when `v` is
the same as it was at the start of the latest pass through the loop, so
the loop just made no further change to it. "The same" means equal by
`=`: for real and complex numbers, within `vanishing_radius`, and for a
tuple of numbers, like `(x, y) stops`, by the distance it moved. `v` can
be any expression, not just a variable. This is handy for iterations that
converge to something you don't know in advance, like Newton's method:

```
iterate z ⟼ z - (z³ - 1) / (3 z²) until z stops
```

In a loop that checks its condition before running its body (`while`,
`until`, and `iterate`), `v stops` is `false` the first time it's
checked, since the loop hasn't made a pass yet. `stops` can only be used
in a loop's condition; anywhere else it's an error.

### The value at the start of a pass

Inside a loop, `previous v` is the value the variable `v` had at the
start of the loop's latest pass. In the loop's body, that's the pass
running now, so a tool can draw each step of an orbit without keeping
track of the last point itself:

```
repeat up to 50 times:
  z ⭠ z² + C
  draw line from previous z to z
until z escapes
```

In the loop's condition, it's the pass that just finished:
`repeat: ... until previous z = z` stops once a pass leaves `z` as it
was. In a loop that checks its condition before running its body
(`while`, `until`, and `iterate`), there's been no pass at the first
check, and `previous v` is just `v` there. (To wait for a value to stop
changing, use `stops`, above, which is `false` at that first check.)

`previous` belongs to the innermost loop around it, including a
`for each` loop, and `iterate`'s step and condition. A `previous` in a
block of code that a loop `run`s belongs to that loop. `v` must be a
variable that exists when the loop starts, not one declared inside the
loop; and `previous` can only be used inside a loop.

### Conditionals

You can conditionally execute code with `if` and `if/else` statements.

After running this code:

```
x ⭠ 0
if 1 = 2:
  x ⭠ 1
```

`x` will still be `0`. But after this code:


```
x ⭠ 0
if 1 = 2:
  x ⭠ 1
else:
  x ⭠ 2
```

`x` will be `2`. You can chain multiple conditions together with `else if`s,
like so:

```
x ⭠ 0
if 1 = 2:
  x ⭠ 1
else if 3 = 4:
  x ⭠ 2
else:
  x ⭠ 3
```

### Output a value from a script

Some scripts will need to produce one or more values as a result. For
example, scripts that are used to draw pictures output a value to
determine which color to draw. This is done using normal assignment
operators:

```
color ⭠ black
```

Putting everything together so far, we can make a basic script for
drawing the Douady rabbit. This script is meant to be run in a context
where there is an input variable `z` of complex type, and an output
variable `color` of color type.

```
iterate z ⟼ z² - 0.12256 + 0.74486 𝑖 while |z| < 10
color ⭠ if stuck then black else white
```

### Inputs and outputs to scripts

Different scripts may have different expected outputs.

- Viewer scripts produce the `color` variable. Changes to other
variables are ignored. A viewer script doesn't have to set `color`
itself: if it doesn't, a *colorizer* sets it (see below).

- Event handlers in tool scripts can set any configuration value. This
is useful for building tools that directly manipulate the configuration,
such as selecting a parameter value.

### Viewers over the real plane

A viewer's pixels are usually points of the complex plane, named by its
`var=` attribute (`z` if there isn't one). A viewer with `xvar=` and
`yvar=` instead is a *real viewer*: each pixel is a pair of real
numbers, the first the horizontal coordinate and the second the
vertical one, so there's no need to split a complex number into
`re(z)` and `im(z)`:

```
@viewer "Hénon map" xvar=x yvar=y center=(0, 0) pixelSize=1/200
until x² + y² > 100:
  t := x
  x ⭠ 1 - a x² + y
  y ⭠ b t
color ⭠ if stuck then black else rainbow(iterations / 20)
```

Either coordinate can be the name of a real configuration variable
(`xvar=a yvar=b`, say), for a plane of parameters; inside the viewer,
it then means the pixel's coordinate, just as `var=C` does in a complex
viewer. With both coordinates configuration variables, the viewer gets a
"Select" tool that sets them both to a clicked point.

A real viewer's `center=` is a point, `center=(1, -1/2)` (or a complex
number, `center="1 - 0.5i"`). Its pixels needn't be square: `aspect=`
gives the height of a pixel over its width (`aspect=2` stretches the
view vertically, so twice as much of the `y` axis fits), and the
viewer's menu has an "Aspect ratio..." item to change it. Besides
`pixel_size` (the width of a pixel), a real viewer's script and its
tools can read `pixel_width` (the same) and `pixel_height`. A complex
viewer's pixels are always square.

### A viewer's geometry

A viewer's script and its tools can read the viewer's current geometry:
`viewer_width` and `viewer_height` (integers: the image's size in
pixels), and the center of the current view, `viewer_center` (complex)
in a complex viewer or `viewer_center_x` and `viewer_center_y` (reals)
in a real one. With `pixel_size` (and a real viewer's `pixel_height`),
that's enough to find the pixel under a point. They follow the view as
it's resized, panned and zoomed, and a viewer whose script reads them is
drawn again when they change. On the sphere, `viewer_center` is the
globe's center on the screen, 0; in the disk, it's the screen's center on
the disk's drawing (0 until it's zoomed somewhere else). These names can't be declared in
`@config`.

### Viewers on the Riemann sphere

A complex viewer can show its points on the Riemann sphere instead of
the plane: as a globe, with `view=sphere`.

```
@viewer "Newton's method" var=z view=sphere
iterate z ⟼ z - (z³ - 1) / (3 z²) until z stops
```

The script is the same as on the plane: `z` is the point of the globe
under each pixel, by stereographic projection, so 0 is the globe's north
pole, ∞ its south pole, and the unit circle its equator. With no
rotation, 0 faces you, with the real axis to the right and the imaginary
axis up, as on the plane; `center=` is the point that faces you to start
with, and `pixelSize=` is measured on the sphere, whose radius is 1 (by
default, the globe fills most of the window). Dragging with the Navigate
tool turns the globe, and scrolling zooms in or out. The viewer's menu
switches between the sphere and the plane ("Show on the sphere"), keeping
the point in the middle of the view. Tools work the same way on the
sphere, with what they draw placed on the globe; a click off the globe
does nothing.

Near ∞, a complex number loses precision, so a script whose interesting
part is near ∞ may look rough there for now.

### Viewers of the hyperbolic plane (the Poincaré disk, the half-plane)

A complex viewer can show the hyperbolic plane, as the Poincaré disk,
with `view=disk`:

```
@viewer "Hyperbolic plane" var=z view=disk center=0.5
r := 2 atanh(|z|)      -- the hyperbolic distance from 0
color <- if mod(floor(2 r), 2) = 0 then light grey else dark grey
```

`z` is a point of the unit disk, the *model point* under each pixel. What
the window shows is the disk moved by the view, an isometry of the
hyperbolic plane: dragging with the Navigate tool moves the view by the
hyperbolic translation that keeps the point under the pointer there, and
a click moves the clicked point to the middle of the window, so the whole
plane can be visited. Scrolling (and pinching) zooms in on the disk as it
is drawn, anywhere on it, to see small details near its edge. `center=` is
the point of the disk shown in the middle to start with (it must be in
the disk), and `pixelSize=` is measured on the disk, whose radius is 1 (by
default, the disk fills most of the window). Off the disk, there's
nothing to draw.

`pixel_size` is, as always, the width of a pixel in the model: in the disk
it depends on the pixel, since the view stretches the disk. A pixel's
size in the hyperbolic metric is `2 pixel_size / (1 - |z|²)`, the same
anywhere the view puts it.

Tools work in model points, as everywhere: a click gives the model point
clicked, and what a tool draws at model points is placed where the view
puts them (a straight segment between two model points may look curved).
Since the view stretches the disk, a tool's `pixel_size` (the width of a
pixel where the disk's drawing is centered) isn't the right size for a
marker elsewhere: `pixel_size_at(p)` is the model width of a screen pixel
at the model point `p`, so

```
draw filled circle at q with radius 3 pixel_size_at(q)
```

is a dot 3 pixels in radius, wherever `q` is shown. (`pixel_size_at` works
in every tool: on the plane it's just `pixel_size`, and on the sphere it's
the size of a pixel facing you.) A disk viewer can't be shown on the plane
or the sphere.

A script can work in the upper half-plane instead, with `view=halfplane`:
`z` is then a point with a positive imaginary part. The half-plane is drawn
with its real axis along the window's bottom edge, and `center=` (by
default `i`) shown 1/`pixelSize` pixels above it (by default 150), halfway
across, or in the middle of the window if that's lower. So for
`center=i`, `pixelSize=` is the width of a pixel at `i`. In the
half-plane, a pixel's hyperbolic size is `pixel_size / im(z)`.

Either way, the viewer's menu can show the hyperbolic plane as the disk or
as the half-plane ("Show as the half-plane"), keeping the view: the script
still sees points of its own model, and the view's center goes from the
middle of the disk to its place above the half-plane's axis (and back).
`pixelSize=` is for the drawing the script's `view=` names; the other
starts at its own zoom (1/150 for the half-plane; the disk filling most of
the window for the disk). On the half-plane drawing, zooming (with the
scroll wheel or a pinch) is about the point of the real axis below the
pointer, and resizing the window keeps the axis at the bottom, so there's
no empty space below it; double-clicking a point, or jumping to one, puts
it in the middle of the window.

### Colorizers

A viewer script often only needs to say what to compute, like

```
iterate z ⟼ z² + C until z escapes
```

and leave the coloring to a *colorizer*: a small piece of script that
runs at the end of the viewer's code and sets `color`. Each viewer has
one, which can be changed from the viewer's menu (under "Colorizer"),
and which the project file remembers.

When nothing says which colorizer a viewer uses, it gets `Manual` if its
script sets `color` itself, and `Rainbow` if it doesn't. A viewer can
name its own default with the `colorizer=` attribute:

```
@viewer "Julia set" colorizer=Smooth
iterate z ⟼ z² + C until z escapes
```

The built-in colorizers are:

| Colorizer | Parameters | What it does |
|---|---|---|
| `Manual` | | Nothing: the viewer's script sets `color`. |
| `Rainbow` | `t = iterations / color_speed` | Points that escape cycle through the rainbow (or the Arenberg wheel); the rest are black. |
| `Escape time` | `t = iterations / period` | Like `Rainbow`, with a choice of palettes (or two colors of your own), optionally shaded by iteration count. |
| `Smooth` | `z` | Continuous color by escape time, without bands, from a choice of palettes. |
| `Binary decomposition` | `z` | Smooth or banded color by escape time, in different colors where `z` ends up in the upper and lower half-planes; this outlines the external rays. |
| `Glow` | | Blends between two colors by how close the iteration count came to the iteration limit. |
| `Autocolor` | `z` | Gives each point `z` ends up at its own color, the first time it's seen (see "Automatic colors"), shaded by iteration count: the basins of a Newton's method, without knowing its roots. Every viewer using it shares the same colors. Turn off its "Allow new autocolor entries" setting (`allow_new ⭠ false` in a `coloring:` block) for a viewer that should only use the colors other viewers found, like a parameter plane, whose points are all different; such a viewer blends the colors for a point near none of them, or, with "Blend when no matching point" (`blend_misses`) off, gives it the group's miss color. |

Points where the loop gave up (`stuck`) are black by default; most
colorizers have a "Color when the loop gives up" setting.

#### Arguments

Some colorizers have parameters, written in parentheses after the name.
A parameter that isn't given an argument uses its default value, if it
has one, or else the viewer's variable of the same name. So `Smooth`
uses the viewer's `z`, and `Smooth(w)` uses `w` instead; `Rainbow`
colors by iteration count, and `Rainbow(arg z / (2π))` colors by the
angle of `z`, which is a good way to show the basins of a Newton's
method:

```
@viewer "Newton's method" colorizer="Rainbow(arg z / (2π))"
lastZ := 0
repeat:
  lastZ ⭠ z
  z ⭠ z - (z³ - 1) / (3 z²)
until z = lastZ
```

Arguments can be any expressions, using any of the viewer's variables
and functions. Quote the attribute's value if it has anything but a name
in it.

The viewer menu's "Colorizer" submenu lists the colorizers the viewer can
use, with the one in use checked. Picking one with parameters uses the
viewer's variables of the same names, if it has them all; if not (its item
ends in "..."), it asks for the arguments first. "Arguments..." changes the
arguments of the colorizer in use.

#### Colorizer settings

A colorizer's settings (like `Rainbow`'s "Iterations per color cycle")
appear in a tab of the configuration window, and each viewer has its own
copy of them. A viewer can change their defaults with a `coloring:`
block at the start of its script:

```
@viewer "Mandelbrot set" var=C colorizer=Smooth
coloring:
  palette ⭠ "Two colors"
  color1 ⭠ red
  period ⭠ 100
z ≔ 0
iterate z ⟼ z² + C until z escapes
```

A setting with a list of choices (like `palette`) is given its choice's
name, in quotes. The `coloring:` block is for the viewer's default
colorizer: if another one is chosen from the menu, the block is ignored.
"Reset to defaults" in the configuration window goes back to the values
in the `coloring:` block, and to the viewer's default colorizer.

#### Writing a colorizer

A project can define colorizers of its own, with `@colorizer` stanzas.
A `@colorizer` stanza has the colorizer's settings, declared like a
`@config` stanza's variables, followed by its code as a block called
`colorize`:

```
@colorizer "Smooth gradient" description="Smooth color between two colors."
period : ℝ as "Color period" ≔ 20
inside : Color as "Interior color" ≔ black

define colorize(z):
  if z escaped:
    m ≔ iterations + 1 - log (log |z| / log escape_radius) / log 2
    color ⭠ blend (cos (π m / period)², dark blue, light yellow)
  else:
    color ⭠ inside
```

Its code runs at the end of the viewer's code, so it can use the
viewer's `iterations` and `stuck`, the configuration, and anything
defined in a `@common` stanza. Its parameters work like a code block's
(see "Defining code to run"), and can have default values, which can use
the colorizer's settings and its earlier parameters:

```
define colorize(t = iterations / period, u = t + offset):
```

The names a colorizer declares (its settings, parameters, and variables)
are its own: they can't clash with the viewer's, even if the viewer uses
the same names. A `@colorizer` can have the same name as a built-in one,
and is used instead of it.

### Tool scripts

A tool in a viewer's `tools:` list responds to user input (clicks, drags,
button presses, and so on) by running a single `script:`, made up of one
or more blocks. Each block starts with an `on <event>:` header and is
followed by an indented body, the same way an `if:` or `until:` block is
written in ordinary script code:

```yaml
tools:
  - name: Trace
    shortcut: t
    script: |
      on refresh:
        erase

      on deactivated:
        erase

      on click-or-drag at z:
        use white for line
        erase
        prevZ : C := z
        until z escapes:
          prevZ <- z
          draw point at z
          z <- z^2 + C
          draw line from z to prevZ
```

The available headers are:

| Header | Fires on |
|---|---|
| `on refresh:` | the viewer redrawing (e.g. after panning or zooming) |
| `on activated:` / `on deactivated:` | the tool being selected / deselected |
| `on click [at <coord>]:` | a single click |
| `on double-click [at <coord>]:` | a double-click |
| `on click-or-drag [at <coord>]:` | either a click or a drag, sharing one body |
| `on drag [from <coord> to <coord>]:` | a drag in progress |
| `on drag-finished [from <coord> to <coord>]:` | a drag ending |
| `on button "<label>":` | a configuration button with the given label being pressed |
| `on timer "<name>" every <N> ms:` | a repeating timer firing |

`<coord>` is a variable name for the point under the mouse, bound (and
available to use in the block's body) for the duration of that block —
either a single name for a complex-plane point (`at z`) or a pair
`(x, y)` for a real-plane point. It doesn't need to already exist as a
variable anywhere else.

For `drag` and `drag-finished`, the first name is where the drag started
and the second is its current position: `on drag from start to z:`.

When `at`/`from ... to` is omitted, the current-position coordinate
defaults to the viewer's own coordinate: its `var=` variable, or the pair
of its `xvar=` and `yvar=` variables in a real viewer. For drags, an
omitted start position is simply unused. Either form can be used in
either kind of viewer: `on click at (x, y):` in a complex viewer, or
`on click at w:` in a real one (with `w = x + y i`). `on click-or-drag` binds one coordinate
shared between the click and the in-progress drag.

A tool script can have at most one block for each of `refresh`,
`activated`, `deactivated`, `click`, `double-click`, `drag`, and
`drag-finished`, but any number of distinctly-named `button` and `timer`
blocks.

### Overlays

A viewer can have one *overlay*: drawings that are always shown over the
viewer, whichever tool is selected (or none), below every tool's own
drawings. It's written as a tool named with the bare word `overlay` (a
quoted `"overlay"` is an ordinary tool), with only an `on refresh:` block:

```
@tool overlay for="Mandelbrot set"
on refresh:
  erase
  use yellow for fill
  draw filled circle at C with radius 5 pixel_size
```

Its `on refresh:` block runs when the project opens, whenever the viewer's
view changes, and whenever any configuration value changes, so what it
draws follows the parameters even when they're changed somewhere else
(another viewer's tool, or the configuration window). An overlay can only
have a `for=` attribute (it's never selected, so it has no shortcut), and
no declarations of its own (it has no panel); a viewer has at most one.

### Drawing commands

In some contexts, a script might have the ability to draw annotations
on a viewer. For example, custom-defined tools can run scripts in
response to user input, and mark on the view in response.

All points that appear in drawing commands are in the coordinates of the
model; you don't have to worry about converting from model coordinates
to window coordinates.

Points can be specified either with ordered pairs of real numbers, or
with complex numbers which will be interpreted as ordered pairs.

#### Setting the pen color
Drawing commands work with a virtual pen that can have a stroke color, used
to draw lines and curves, and a fill color, used to fill regions. To set the
stroke color to `red`, use the command

```
use red for stroke
```

(Alternatively, you can write `use red for line`). To set the fill color to
`blue`,

```
use blue for fill
```

#### Marking points

To draw a point in the view at point `z`, use the command

```
draw point at z
```

Points are drawn using the stroke color. The point can either be given by
a complex number or by an ordered pair of real numbers.

#### Drawing lines

To draw a line in the view from point `z` to point `w`, use the command

```
draw line from z to w
```

Lines are drawn using the stroke color. The points can either be given by
complex numbers or by ordered pairs of real numbers.

#### Drawing rectangles

To draw an empty rectangle in the view with opposite corners `z` and `w`,
use the command

```
draw rectangle from z to w
```

Or to make the rectangle filled:

```
draw filled rectangle from z to w
```

The boundary of the rectangle will be drawn using the stroke color, and
the rectangle will be filled with the fill color. The points can either
be given by complex numbers or by ordered pairs of real numbers.

#### Drawing circles

To draw an empty circle in the view with center `z` and radius `r`,
use the command

```
draw circle at z with radius r
```

Or to make the circle filled:

```
draw filled circle at z with radius r
```

The boundary of the circle will be drawn using the stroke color, and
the circle will be filled with the fill color. The center can either
be given by a complex number or by an ordered pair of real numbers.

#### Drawing text

To write a text annotation into the view at the point `z`, use the command

```
write "Hello, world" at z
```

The text will be drawn using the stroke color, with text centered at the given point.

You can build up the text from smaller parts using the `text` function, which
concatenates its arguments together:

```
write text("Here is ", z) at z
```

Inside quoted text, write `\"` for a quotation mark, `\\` for a backslash,
`\n` for a line break, and `\t` for a tab:

```
write "She said \"hi\"" at z
```


### Lists

In some contexts, scripts can make use of lists of values instead of being
limited to just scalar data. Lists are available everywhere *except for*
in viewer scripts. In the future this limitation could be lifted; please
 [open an issue](https://github.com/matt-noonan/FractalStream/issues) if you
 have a use-case for lists in viewer scripts.

 List values in FractalStream scripts are written as a `,`-separated sequence
 of elements enclosed in square brackets `[` / `]`.
 When entering or displaying a list in the user interface, the outer brackets are omitted. This lets users of your script write things like `1, 2, 3` in an input box instead of `[1, 2, 3]`.

All lists in FractalStream are *homogeneous*, meaning that the elements of a list
 must all have the same type. For example, you can have `[1, 2, 3]`
 (a `List of ℤ`), or `[1, 2, i]` (a `List of ℂ`), or even `[ [1], [2,3] ]`
 (a `List of (List of ℤ))` but not `[ 1, [2,3] ]` because it mixes elements
 of different types.

#### Length of a list

The length of a list can be obtained through the `length` operator:

```
myList : List of Z := [3, 1, 4, 1, 5, 9, 2]

x : Z := length(myList)
# x is now 7
```

#### Indexing into a list

To read a specific element out of a list, use the `@` operator. **NOTE:**
lists in FractalStream use 1-based indexing!

```
fibs : List of Z := [0, 1, 1, 2, 3, 5, 8]

x : Z := fibs @ 4
# x is now equal to 2
```

Attempting to use a non-positive index or an index larger than the length of the list will cause your script to terminate.

In addition to normal list indexing, you can also cyclically index into a
list using the `@@` operator:

```
digits : List of Z := [3, 1, 4, 1, 5, 9, 2]

x : Z := digits @@ 10
# x is now equal to 4

x <- digits @@ -1
# x is now equal to 9
```

`@@` allows for negative indices, but will still cause your script to terminate
if the list is empty.

#### Appending, prepending, and joining lists

Several lists can be concatenated together using the `join` operator, which
can receive two or more lists:

```
myList1 : List of Z := [1,2]
myList2 : List of Z := [3,4,5]
myList3 : List of Z := [6]

myList1 <- join(myList1, myList2, myList3)
# myList1 is now [1,2,3,4,5,6]
```

Two common special cases have been given their own names: `append(myList, x)`
is equivalent to `join(myList, [x])`, and `prepend(myList, x)` is equivalent
to `join([x], myList)`.

#### Removing items from a list

You can remove all items that match a certain predicate from a list using
the `remove` command. It takes two arguments: the first is a list, and the
second is a predicate used to determine which elements are removed.

```
myList : List of Z := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

myList <- remove(myList, item ⟼ mod(item, 2) = 0)

# now myList is [1,3,5,7,9]
```

#### Transforming a list

You can apply a function to all items in a list using the `transform`
command. It takes two arguments: the first is a list, and the second is
a transformation that will be applied to each list element.

**NOTE:** Currently the function must be of the form `T -> T` for some type `T`.
In the future this restriction could be lifted; please
 [open an issue](https://github.com/matt-noonan/FractalStream/issues) if you
 have a use-case for a type-changing transformation.

```
myList : List of Z := [1, 2, 3, 4, 5]

myList <- transform(myList, x ⟼ x^2)
# now myList is [1, 4, 9, 16, 25]
```

#### Finding an element in a list

The `find` function can be used to find the first element of a list
that matches some predicate. You must supply a fallback value to use
when no items in the list match your predicate.

```
myList : List of Z := [1, 2, 3, 4, 5]

x : Z := find(myList, item ⟼ item^2 > 10, 17)
# x is now 4

x <- find(myList, item ⟼ item^2 > 100, 17)
# x is now 17
```

For more complex situations, there is also the `with first ... matching`
command that can be used to run some commands on the first value of a list
that matches some predicate, and optionally run some other commands if no
such value exists.

```
myList : List of Z := [1, 2, 3, 4, 5]

x : Z := 0
y : Z := 0

with first item matching item^2 > 10 in myList:
  x <- item
else:
  y <- 1

# x is now 4, y is still 0

with first item matching item^2 > 100 in myList:
  x <- item
else:
  y <- 1

# x is still 4, but now y is 1
```

#### Iterating over items in a list

To run some code for each item in a list, you can use the `for each ... in`
command:

```
myList : List of Z := [1, 2, 3, 4, 5]

sum : Z := 0
for each x in myList:
  sum <- sum + x

# sum is now 15
```

**NOTE:** `for each` loops are not subject to the special iteration limits
that apply to `while`, `until`, and `iterate`. The `for each` loop will
always process each element of the list.

### Tables

A *table* is a grid of values of one type, all stored together: a
`Table 2 of C` is a 2-dimensional grid of complex numbers. Tables are for
data that tools work out ahead of time for viewers to look up, especially
data that one pixel can't work out on its own, like values carried over
from neighboring points. A tool fills a table in, and every viewer can
read it.

A table can hold integers, reals, complex numbers, truth values, or
colors, or tuples of them, in any number of dimensions (`Table 1 of R`,
`Table 3 of Z`, `Table 2 of C x Z`, ...).
It's declared in `@config`, without a default value, since it starts out
empty (with size 0). It has no field in the configuration window. It's kept
when the project is rebuilt (after editing its script, say) if the new
script declares it with the same name and type:

```
@config "Parameters"
saddle : Table 2 of C
```

Saving a project can save what its tables hold, too: when any table holds
something, the app asks whether to, and says how big they are. A table
that's saved is put back when the project is opened again, as long as the
script still declares it with the same name and type. (Running a project
with `fsrun` starts from the script alone, without the saved tables or
configuration values, unless a scenario asks for the tables with
`load saved table T` or `load all saved tables`: see
[SCENARIO_LANGUAGE.md](SCENARIO_LANGUAGE.md).)

#### Reading a table

`T @ (j, k)` is the value in column `j` and row `k` of a 2-dimensional
table `T`. Like lists, tables count from 1: `T @ (1, 1)` is the cell at the
top left, columns go to the right, and rows go down. A table with `n`
dimensions takes `n` integers, written as a tuple (`T @ k` for a
1-dimensional table). Reading outside the table is an error, as it is for
a list: the pixel is drawn magenta, or a tool's changes are discarded.

Every table also has a place in the plane (see "Filling in a table"), so
`T @ point z` is the value in the cell that the point `z` is in. Each
cell covers a small rectangle, so the cells of a table laid over a
viewer's window are its pixels. A point outside the table is an error,
like reading out of range.

It's a table's first two dimensions, its columns and rows, that are laid
out in the plane. A table with more dimensions than that has *layers*
over the plane: in a `Table 3 of C`, `T @ (j, k, m)` is column `j` and row
`k` of layer `m`. So `point z` stands for the first two indices, and the
others follow it:

```
crit @ (point C, m)        # layer m, at the parameter C
```

A 1-dimensional table has no points.

To look up a point that may be off the table, give a value to use there
instead with `else`:

```
color <- rainbow(saddle @ point C else 0)
```

`T @ point z else v` is `v` when `z` is off the table, and the cell's
value otherwise; with layers, it's `T @ (point z, m) else v`. The `else` part is a single value, like the right side of
`*`: `T @ point z else 0 + 1` adds 1 to whichever it is, so put anything
longer in parentheses. (An `if c then T @ point z else v` is still an
ordinary if/then/else.)

There's more about where a table is in the plane (with layers, these are
about the first two dimensions):

- `z in T` is whether the point `z` is on the table.
- `table_cell(T, z)` is the cell that `z` is in, as a pair of indices
  `(j, k)`, even for a point off the table (then `j` or `k` is less than 1,
  or more than the table's size).
- `table_point(T, (j, k))` is the point at the center of the cell `(j, k)`,
  for any `j` and `k`.

So a tool can find the cell a point is in, and the point a cell is at,
without working them out from the table's corners:

```
if crit_c in T:
  (j, k) := table_cell(T, crit_c)
  z := table_point(T, (j + 1, k))   # the center of the next cell to the right
  ...
```

`table_size(T)` is the table's size: an integer for a 1-dimensional table,
or a tuple with one integer per dimension. A table that hasn't been filled
in yet has size 0:

```
(w, h) := table_size(saddle)
color <- if w = 0 then grey else ...
```

#### Tables of tuples

A table of tuples, like a `Table 2 of C x Z` (or `Table 2 of (C x Z)`),
holds a tuple in each cell, which is read and written whole. Take one
apart as you would any tuple:

```
@config "Critical points"
crits : Table 2 of C x Z    # a critical point, and whether it was found

@viewer "c plane" var=c
(crit, found) := crits @ point c else (0, 0)
```

and write it with a tuple, as in `crits @ (j, k) <- (w, 1)`. There's no
way yet to write just one part of a cell's tuple: read the cell, and
write it back with that part changed.

#### Slices

A `*` in place of an index stands for every index along that dimension.
With one `*`, the result is a list: `T @ (j, *)` is column `j` of a
2-dimensional table, from the top down, and `crit @ (point C, *)` is the
value at `C` in each layer, from the first. All of a 1-dimensional table
is `T @ *`.

```
values := crit @ (point C, *)
n := length(values)
```

A slice with more than one `*` isn't a value you can hold, but a tool can
copy it onto another slice (see below).

#### Filling in a table

Only a tool can change a table; a viewer can only read one. A tool lays a
table out afresh with `reset`, which gives it a size and a place in the
plane, and fills it with zeros (`false`, or black), whatever it held
before:

```
reset T to (viewer_width, viewer_height)
```

This lays the table over the tool's viewer window: with the viewer's own
size, as here, cell `(j, k)` is the pixel in column `j - 1` and row `k - 1`
(pixels count from 0), and with another size the cells are stretched to
cover the window. To cover some other rectangle of the plane, give its
top left and bottom right corners:

```
reset T to (200, 100) from -2 + i to 2 - i
```

(The size must be in parentheses here, or be a single name or number.)
A table with layers is reset the same way, with one more size for
each further dimension: `reset crit to (viewer_width,
viewer_height, 4)` lays 4 layers over the window.
To start every cell with some other value than zero, add `filled with`
at the end:

```
reset status to (viewer_width, viewer_height) filled with -1
```

Then the tool writes the table's cells with `<-`:

```
T @ (j, k) <- v
T @ point z <- v
```

Writing outside the table is an error, and the tool's changes are
discarded.

With layers, `T @ (j, k, m) <- v` and `T @ (point z, m) <- v` write the
cell in layer `m`. A slice with one `*` is written from a list with one
item for each of its cells, in order, and a list of another length is an
error:

```
crit @ (point c, *) <- [z1, z2, z3]
```

A slice is copied onto another slice of the same shape (the same numbers
of cells along each `*`, in order), of the same table or another one with
the same kind of elements:

```
crit @ (*, *, 1) <- crit @ (*, *, 2)    # layer 2, copied onto layer 1
crit @ (*, *, 3) <- saddle @ (*, *)     # a 2-dimensional table, onto layer 3
```

A tool can also change a table's size, with `resize`:

```
(w, h, n) := table_size(crit)
resize crit to (w, h, n + 1)     # one more layer
```

The table stays where it was in the plane: cell `(1, 1)` is in the same
place, and so is every cell it still has, with its value. New cells are
zeros, or, with `filled with v` at the end, `v`. So more columns or rows
are added at the right or the bottom, and fewer take them away there.
Resizing never moves a table's top left corner; to lay it out somewhere
else, `reset` it.

To drop a layer, copy the ones after it down, and shrink the table:

```
(w, h, n) := table_size(crit)
for each m in range(k, n - 1):
  crit @ (*, *, m) <- crit @ (*, *, m + 1)
resize crit to (w, h, n - 1)
```

A tool's changes to a table only take effect when its handler finishes
without an error, like its changes to any other configuration variable;
then every viewer that reads the table is drawn again. A viewer that's
drawing while the tool runs keeps the table it started with: writing a
table that viewers may be reading writes a copy of it. (The same goes for
a table that another variable holds: after `U := T`, writing `T` leaves
`U` as it was.)

For example, this tool works out a value for every pixel of a parameter
plane, and a dynamical plane looks up the value at its own parameter:

```
@config "Parameters"
C : C := 0
data : Table 2 of C

@viewer "Parameter plane" var=C
(w, h) := table_size(data)
color <- if w = 0 then grey else rainbow(arg(data @ point C) / (2 pi))

@viewer "Dynamical plane"
...  # can read `data @ point C`

@tool "Compute" for="Parameter plane"
on click:
  reset data to (viewer_width, viewer_height)
  for each j in range(1, viewer_width):
    for each k in range(1, viewer_height):
      data @ (j, k) <- ...  # the value for pixel (j - 1, k - 1)
```
