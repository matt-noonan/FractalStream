# The FractalStream scripting language

Everything you see when you interact with a FractalStream project---the viewers, the editable
configuration values, and even the tools---are defined using the FractalStream scripting language.
This language is meant to provide lots of handy features for studying dynamical systems, especially
in complex dynamics, while still producing fast viewers and rich custom interactions.

If you are having trouble writing a script, feel free to ask for help on the (FractalStream discussion page)[https://github.com/matt-noonan/FractalStream/discussions].

## Types

All values in a FractalStream script have an associated *type*, and the type of a value cannot change over time. The valid types for FractalStream variables are:

| Name         | Alternative name | Meaning                                                                 |
|--------------|------------------|---------------------------------------------------------------- |
| `Boolean`    | `Bool`                 | Truth values, `true` or `false`                                                    |
| `Z` | `ℤ`, `Int`, `Integer` | Integer |
| `R`            | `ℝ`, `Real`                | Real number                                                             |
| `C`            | `ℂ`, `Complex`                | Complex number                                                          |
| `Ĉ`            | `ℂ̂`, `Cinf`                | Point of the Riemann sphere: a complex number, or `∞` (see {ref}`riemann-sphere`) |
| `ℙ¹`           | `ℙ`, `P1`, `P^1`           | Point of the projective line, by its homogeneous coordinates `[a : b]` (see {ref}`riemann-sphere`) |
| `Color`        |                  | RGB color                                                               |
| `Text`         |                  | Plain text                                                              |
| `P x Q`        | `P × Q`            | Product type; pair of a value of type `P` and one of type `Q`               |
| `List of T`    |                  | List type; a list of values of type `T`                                   |
| `Table N of T` |                  | Multidimensional table type; an `N`-dimensional table of values of type `T` |

## Values

### Names and grouping

Parentheses `(` / `)` and braces `{` / `}` can be used to group subexpressions.

Variables can be given any name involving alphanumeric characters as long as they do not start with a number. In addition, the editor understands a subset of *LaTeX* symbols and will insert them
as you type. For example, to type `λ` you may write `\lambda` in the editor. It also understands
some combining characters: typing `\hat z` yields `ẑ`, and `\ddot \theta` yields `θ̈`. These are
all valid variable names.

A script can use almost any word as a name, even the name of a built-in function or color. A
name your script defines is your script's own: where it is defined, the built-in with that name
isn't available. So a script can have a configuration value called `fall`, or define its own
function called `max`:

``` fractalstream
@config "Parameters"
fall : R := 0.25

@common
max(a, b) := if a > b then a else b

@viewer "V"
color <- rainbow(max(fall, re z))
```

A name defined in `@config` or `@common` is the script's everywhere; a name defined in a viewer
or a tool is the script's only in that stanza. This also means that a built-in function added in a
later version of FractalStream can never change what your script means.

A few words are reserved and can't name anything: `if`, `then`, `else`, `and`, `or`, `not`,
`true`, `false`, `e`, `i`, `pi`, `escapes`, `vanishes`, `stops`, `iterations`, `stuck`, `times`,
`previous`, `text`, `append`, `prepend`, `join`, `remove`, `find`, `transform`, `range`,
`length`, and `diff`.

### Built-in constants

FractalStream knows about several special constants

| Names        | Meaning          |
|--------------|------------------|
| `e`, `𝑒`    | Euler's number |
| `pi`, `π`   | Pi |
| `i`, `𝑖` | The imaginary unit |
| `∞`, `infinity` | The point at infinity of the Riemann sphere (a value of type `Ĉ`; see {ref}`riemann-sphere`) |
| `true`  | Boolean true value |
| `false` | Boolean false value |

Expressions can be used anywhere in the FractalStream app where a number would be expected.
For example, a complex-valued numeric field will accept input like `1 + 2i`, but also
input like `e^{2 pi i / 3}`.

In addition to the values given here, there are a number of built-in color constants; see
the {ref}`Colors and gradients <colors-and-gradients>` section below.

### Special variables and predicates

| Name  | Meaning | Configured by... |
| ----- | ------- | ---------------- |
| `max_iters` | Maximum number of iterations through any loop | `Configuration > Iteration settings > Iteration limit` |
| `escape_radius` | Escape radius | `Configuration > Iteration settings > Escape radius` |
| `vanishing_radius` | Vanishing radius / tolerance for `=` comparison | `Configuration > Iteration settings > Vanishing radius` |
| `iterations` | Number of iterations performed by the most recent loop | *automatic* |
| `stuck`      | `true` when the last loop exceeded `max_iters` | *automatic* |
| `pixel_size` / `pixel_width` | The width of a pixel in the current view | *automatic* |
| `pixel_height` | The height of a pixel in the current view; equal to `pixel_width` for complex viewers, but may vary for real viewers | *automatic* |
| `viewer_width` | The width of the current view in pixels | *automatic* |
| `viewer_height` | The height of the current view in pixels | *automatic* |

| Name | Meaning |
| ---- | ------- |
| `z escapes` / `z escaped` | Equivalent to `\|z\| > escape_radius` |
| `z vanishes` / `z vanished` | Equivalent to `\|z\| < vanishing_radius` |
| `z stops` / `z stopped` | Test if `z` changed by less than `vanishing_radius` over the last loop iteration |
| `x = y` | Equivalent to `\|x - y\| < vanishing_radius` |


### Arithmetic operations

Typical arithmetic operations (`+`, `-`, `*` for multiplication) are available for all numeric types.
There are two different variations for division: `/` is used for division on reals and complex
numbers, and the result of `p / q` when `p` and `q` are integers is a real number. To get
integer division, use `//` instead.

| Operation | Meaning | Notes |
|-----------|---------|-------|
| `+` | Addition | |
| `-` | Subtraction, or unary negation | |
| `*`, `×` | Multiplication | See notes below on concatenation |
| `/` | Division | Integer/integer division results in a real |
| `//` | Integer division | |
| `^`  | Exponentiation | Fractional exponents are allowed, integer exponents will be optimized |
| `mod(a, b)` | Remainder | For integers or real numbers; never negative (see below) |

`mod(a, b)` is the remainder of dividing `a` by `b`: the number `r` with `a = b k + r` for a whole
number `k`, and `0 ≤ r < |b|`. It is never negative, whatever the signs of `a` and `b`:
`mod(-7, 3)` is 2, `mod(7, -3)` is 1, and `mod(-0.5, 1)` is 0.5. For integers, `a // b` is that
`k`, so `a = b (a // b) + mod(a, b)`: `-7 // 3` is -3, and `7 // -3` is -2. Dividing by 0 gives 0
both ways: `mod(a, 0)` and `a // 0` are both 0.

`round(x)` is the whole number nearest `x`, with a half rounding away from zero: `round(2.5)` is 3
and `round(-2.5)` is -3. `floor(x)` rounds down, and `ceiling(x)` (or `ceil(x)`) rounds up.

A power of integers is an integer (`2^10` is 1024), unless the exponent is written as a negative
number (`10^(-2)` is 0.01). With a variable exponent, as in `2^k`, the power is an integer, unless
it is used as a real number (as in `2^k / 3`), where it is computed as a real power. Integers are
64-bit, and wrap around when they overflow: `2^k` is 0 for `k` of 64 or more, so use `2.0^k`
when `k` can be large.

Piecewise-definitions can be made with `if CONDITION then VALUE1 else VALUE2`, as in

``` fractalstream
clamped := if x > 0 then x else 0
```

### Boolean operations

Boolean expressions can be negated with the `not` operator, or combined with the `and` and `or`
operators. Expressions should not use mixed `and`s and `or`s without parentheses; for example,
`P and Q or R` will be rejected and should be disambiguated to `(P and Q) or R` or `P and (Q or R)`.

### Comparisons

Integers and real numbers may be compared for order with `<`, `>`, `<=` / `≤`, and `>=` / `≥`.

Integers, real numbers, and complex numbers can be tested for equality using `=` / `==`, but
the meaning is slightly subtle:
- For integers, `X = Y` tests if `X` and `Y` are equal
- For real numbers and complex numbers, `X = Y` tests if `|X - Y|` is less than `vanishing_radius`. This is because making exact comparisons between floating point numbers is rarely the correct thing to do, so we use "equal up to `vanishing_radius` tolerance" as the semantics of `=`.
- If you really need *exact* equality checks for real or complex numbers, use the `===` / `≡` operator.

Inequality can be tested with the `!=` /  `=/=` / `≠` operator.

### Built-in functions

- Functions that work for real and complex numbers: `exp`, `log`, `sqrt`, `cos`, `sin`, `tan`,
`cosh`, `sinh`, `tanh`.
- Functions that work for real numbers only: `arccos` / `acos`, `arcsin` / `asin`, `arctan` / `atan`, `arccosh` / `acosh`, `arcsinh` / `asinh`, `arctanh` / `atanh`.
- Functions that work for complex numbers only: `re` / `Re`, `im` / `Im`, `arg` / `Arg`, `conj` / `bar` / `Conj` / `Bar`.
- Rounding functions: `round`, `floor`, `ceiling` / `ceil`.
- Functions for points of the Riemann sphere: `affine`, `chart`, `chordal` (see {ref}`riemann-sphere`).
- Derivatives: `diff`, `diff_bar`, `partial` (see {ref}`derivatives`).
- From the standard library (see {ref}`standard-library`): `min`, `max`, `clamp`, `compose`,
  `newton_root`, `find_cycle`, `S`, `riemann_sum`, `residue`, `winding_number`, and color
  functions like `cycle` and `rainbow`.

(derivatives)=
### Derivatives

You can take the derivative of an expression using the `diff(EXPRESSION, VARIABLE)` operation.
For example:

``` fractalstream
dz := diff(z^2 + C, z)
```

will set `dz` to `2 z`. Derivatives are worked out symbolically, so they are exact.

`diff` follows the variables in the expression back through the statements that computed them,
loops and all. So the derivative of an orbit with respect to its starting point, or with respect
to a parameter, needs no extra bookkeeping:

``` fractalstream
w := z
iterate w -> w^2 + C until w escapes
distance := |w| log |w| / |diff(w, z)|
```

A few rules apply:
- The derivative is taken wherever it exists. An `if` isn't differentiated: the derivative is the
  derivative of the branch that was taken. In the same way, `round`, `floor`, `ceiling` and `//`,
  which only jump, have derivative 0, as does an integer computed from the variable. `mod` has the
  derivative of its definition, $a \bmod b = a - |b| \lfloor a / |b| \rfloor$, and `|x|` has
  derivative 0 at 0. (Newton's method won't move on a function that is flat like this.)
- The variable you differentiate with respect to can't change between the moment another variable
  is computed and the `diff`. That's why the example above iterates a copy `w` of `z`, instead of
  iterating `z` itself.
- An item of a list or table can't be followed: that's an error.

With respect to a complex variable $z = x + i y$, `diff` is the *Wirtinger derivative*
$\partial = \frac{1}{2}\left(\frac{\partial}{\partial x} - i \frac{\partial}{\partial y}\right)$.
Where the expression is holomorphic, that is the usual complex derivative, so `diff(z^3 - 1, z)` is
still `3 z^2`. But it is defined for the rest too, such as `|z|`, `re`, `im`, `conj` and `arg`:
`diff(|z|^2, z)` is `conj(z)`, and `diff(conj(z), z)` is 0.

`diff_bar(EXPRESSION, VARIABLE)` is the other Wirtinger derivative,
$\bar\partial = \frac{1}{2}\left(\frac{\partial}{\partial x} + i \frac{\partial}{\partial y}\right)$.
It is 0 exactly where the expression is holomorphic, and `diff_bar(|z|^2, z)` is `z`. Together,
the two give the whole derivative of a map of the plane. For example, the Jacobian determinant of
`w` as a function of `z` is `|diff(w, z)|^2 - |diff_bar(w, z)|^2`. Newton's method uses the complex
derivative, so it only makes sense where the function is holomorphic.

`diff(EXPRESSION, x, 2)` is the second derivative, and so on.

If you want the derivative of an expression *alone*, treating every other variable as a constant
no matter how it was computed, use `partial(EXPRESSION, VARIABLE)` instead. For example,
`partial(z^2 + c, c)` is always `1`.

Both can also be written as fractions, just as on paper: `ⅆw/ⅆz` means `diff(w, z)`, and
`∂w/∂z` means `partial(w, z)`. Higher derivatives are written with superscripts, as in `ⅆ²w/ⅆz²`,
and mixed partial derivatives look like `∂²f/∂x∂y`. To raise a derivative to a power, use
parentheses: `(ⅆw/ⅆz)^2`. In the editor, type `\d` and a space for `ⅆ`, and `\partial` for `∂`.

If you have made user-defined functions, those can also be used inside `diff`:

``` fractalstream
f(z) := z^2 + C

# ...later...

dz := diff(f(z), z)
dC := diff(f(z), C)
# dz is now equal to 2 z, and dC is equal to 1
```

For a user-defined function, you can also use the prime notation for the derivative. For example, here is a script to run Newton's method
on a cubic polynomial:

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=jVOxctNAEO39FTtqYg9ItpXETlTBuKOgSs3M-bSWDs53N3dr5HgoUjFDT0_HB4RJR8UP8A_-ElanWBDiCTSa3du3773bPQXplaMCPqQDgBfvFTboIXmNDVlzEmCNVNsy4dpquBvB_tNn2P34Bins4BlM-VgRekHI-f7Ld_6mHXAMq_3NbRttDCnNhUDWBW6QVlvP4DvwQpmlbYbDKXMJXzF6_POm7c1HAxYWpSBRcAsp0ljAS1gprbGEVxutBAQkrpUY4g2UNXwJPgC4qvEREuyqtfjxK9u_ZblFAcSwQMKTMhU4qwwFaGobEKxfKgqRiwHXsLQbU2KZwaIWpkJYgDKxXVqzUtWGB8Dy0ChT2gbIAm4dX5KJGOQjT28kPAdhSpBayXcsBKUXFXdSHQmvvJDIDFbDkEYtFdcbEJ2_k85TtJdx6Dy2C0ulXa-ZNBwGEFCjJDgss9dO7qt9yaktaghq12rCdJxPJhEihaMN-3_MACIcZAdBohFe2VAMUjBizSsKau00xi0_tPQEIysnb9s0c6ZKeiqpvDxK1fp3wjOInx7vgvv_6brV6AgfilA776MaT08wVuOakriy7rxbqiCYZDngm2HOAwbF7_l89D8Go5nOX4PL1HnrkF8nhvYf6HpiCPDX79kdMv-2uB8Fx9e_Y6eZ-ABKYZ5dYnrap7NsdoZp3ueT7PRydvFHOs8vDqnHqv3R-uJZNruYnx9PQy0cj3g6-AU)
---
@viewer "Newton's method"
f(z) := z^3 - z + 1
iterate z -> z - f(z) / f'(z) until z stops
color <- rainbow((1 + arg(z)/pi) / 2)
```

Higher derivatives may be denoted with additional primes: `f''(z)` is the second derivative.
The prime works on any function of one value: a function with a body (see {ref}`functions`), a
parameter that has been given a function, and a composition, as in `(f ∘ g)'(z)` (see
{ref}`composition`).

On a function with several arguments, the prime is the derivative in its *first* argument:
`g'(z, c)` means the derivative of `g(w, c)` with respect to `w`, at `w = z`. That's how
$\wp'(z)$ is written for the Weierstrass function of a lattice (see {ref}`elliptic`). It is a
FractalStream convention, though: on paper, a prime on a function of several variables would
usually need to say which variable it is for. To differentiate in another argument, use `diff`.

The Schwarzian derivative $S_f(x)$ can be computed by the built-in function `S(f, x)`. For
example, `S(z -> z^3, z)` is equal to `-4 / z^2`.

(riemann-sphere)=
### Points of the Riemann sphere

Complex numbers can't be ∞, so a script whose dynamics pass through ∞ would have to work around
it, for example by keeping track of two charts by hand. Instead, FractalStream has two types whose
values are points of the Riemann sphere, ∞ included.

A value of type **`Ĉ`** (also written `ℂ̂`, or `Cinf`) is either a complex number or `∞` (also
written `infinity`). A complex number can be used anywhere a `Ĉ` is expected. You can add,
subtract, multiply and divide points of `Ĉ`, and raise them to whole-number powers written as
numbers (like `w^2` or `w^(-1)`). So a rational map is written as usual, and is correct at ∞:

``` fractalstream
w : Ĉ := ∞
w <- (w^2 - c) / (w^2 - 1)      # w is now 1
```

Behind the scenes, a point is stored by its homogeneous coordinates `[a : b]`, meaning the complex
number `a / b`, or ∞ when `b` is 0. Each expression is worked out in homogeneous coordinates as a
whole, the way you would do it by hand, so Newton's method `w - (w^3 - 1) / (3 w^2)` is exactly ∞
at ∞. This happens one expression at a time: if you split a computation across several
statements, an intermediate result can be ∞ / ∞. Something undefined (∞ - ∞, 0 · ∞, 0 / 0)
gives a point that isn't equal to anything.

A point of the sphere isn't a number, so `|w|`, `re`, `exp`, comparisons like `<`, and
derivatives aren't defined on it. These are:

| Operation | Meaning |
| --------- | ------- |
| `affine(k, w)` | The point in the `k`th affine chart, counting from 0: `affine(1, w)` is the complex number `a / b` (huge, or not a number, at ∞), and `affine(0, w)` is `b / a`, a coordinate around ∞ |
| `chart(w)` | The pair `(affine(k, w), k)`, using whichever chart puts the point closest to 0, so the complex number is never bigger than 1. `(u, k) := chart(w)` is exact everywhere, ∞ included |
| `chordal(p, q)` | The chordal distance: the straight-line distance between the two points on the unit sphere, so `chordal(0, ∞)` is 2 |
| `p = q`, `p ≠ q` | Equal when the chordal distance is less than `min_radius` |
| `w escapes`, `w vanishes`, `w stops` | As for complex numbers, and correct at ∞ |

A value of type **`ℙ¹`** (the projective line; also written `ℙ`, `P1`, or `P^1`) is written by its
homogeneous coordinates `[a : b]`, and has no arithmetic: you write a map in homogeneous
coordinates instead, which is exact everywhere. Take a point apart with `[a : b] := p`, and build
a new one from the pieces:

``` fractalstream
[a : b] := p
p <- [a^2 - c b^2 : a^2 - b^2]
```

Homogeneous coordinates are only defined up to scale (`[2 : 1]` and `[4 : 2]` are the same point),
so `a` and `b` can only be used in ways that don't depend on the scale: in a point `[F : G]` where
`F` and `G` have the same degree, or in a ratio of the same degree, like `a / b`. Anything else,
like `a` by itself, `a + 1`, or `[a^2 : b]`, is an error. A `Ĉ` can be taken apart in the same
way. To convert between the two types, use a type annotation: `(p : Ĉ)` or `(w : ℙ¹)`.

Configuration values can be points of the sphere too, as in `c : Ĉ := ∞` or `p : ℙ¹ := [1 : 0]`,
as can lists and tuples of them. In a configuration text box, a point is written as `∞`, as a
complex number, or (for `ℙ¹`) as `[a : b]`.

(integration)=
### Integration

FractalStream can approximate line integrals using Riemann sums, via the built-in function
`riemann_sum`. In the real or complex domain, use `riemann_sum(γ, f, N)` to denote the
Riemann sum that approximates the integral $\int_0^1 f(\gamma(t)) \gamma'(t) \mathrm{d} t$ using
$N$ equally-spaced subdivisions of the $[0,1]$ interval. The path `γ` and the function `f` can
be given by name, or written in place. For example, the integral of $1/z$ once around the unit
circle is close to $2 \pi i$:

``` fractalstream
I := riemann_sum(t -> e^{2 π i t}, z -> 1 / z, 1000)
```

The more subdivisions, the closer the sum is to the integral, and the longer it takes to work out.

Two common uses of contour integrals have functions of their own:

- `residue(f, z, r, N)` approximates the residue of `f` at `z`, as $\frac{1}{2 \pi i}$ times the
  integral of `f` around the circle of radius `r` about `z`, using `N` subdivisions. The circle
  should contain no other pole of `f`. For example, `residue(w -> 1 / (w^2 + 1), i, 0.5, 1000)` is
  close to `-0.5 i`.
- `winding_number(γ, z0, N)` is the number of times the closed path `γ` (with `γ(0) = γ(1)`) winds
  around the point `z0`, counterclockwise, as a whole number. For example,
  `winding_number(t -> e^{4 π i t}, 0, 1000)` is 2. It is rounded from an approximate integral, so
  `N` has to be large enough for the path's shape, and `z0` mustn't be on the path.

(newton)=
### Newton's method

`newton_root(f, x)` runs Newton's method on the function `f`, starting from `x`, and gives back a
pair `(found, root)`: whether the iteration settled down, and where it ended up. The derivative is
worked out for you, so `f` is all it needs, given by name or written in place:

``` fractalstream
(found, root) := newton_root(w -> w^3 - 1, z)
color <- if found then rainbow(arg(root) / (2 π)) else black
```

The starting point has to be a real or complex number, so write `2.0` rather than `2`. It stops
when a step moves less than `vanishing_radius` (the `=` test), or after `max_iters` steps.
For your own limit and stopping test, use `full_newton_root(f, x, N, stop)`, which takes at most `N`
steps, and stops when `stop(new, old)` holds for the latest step, as in
`full_newton_root(w -> w^3 - 1, z, 50, (a, b) -> |a - b| < 10^(-12))`.

(cycles)=
### Cycle detection

FractalStream can detect approximate cycles in a dynamical system using a variation of
[Floyd's cycle detection algorithm](https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare),
with the built-in function `find_cycle(f, z, warmup, max_period, same, χ)`. The semantics
of these parameters are:
- `f`: the function to iterate
- `z`: the initial value to iterate from
- `warmup`: how many iterations to perform before we start the cycle detection, used to get
closer to an attracting cycle
- `max_period`: the longest cycle length we are willing to search for
- `same`: a function that determines when two points in the approximate cycle should be considered "the same point". For example, `(x,y) -> x = y` will check if two points are within `vanishing_radius` of
each other; `(x,y) -> x === y` would check if `x` and `y` are literally the same value.
- `χ`: the characteristic function of a region that includes the cycle. For the dynamics of
$z^2 + C$, this could be something like `z -> |z| < 2` or `z -> not (z escapes)`. If you don't
want to restrict where to look for the cycle, use the constant characteristic function `z -> true`.

`find_cycle` gives back a triple `(found, w, period)`: whether it found a cycle, a point `w` on it,
and its period. It only finds a cycle that the orbit settles into within `max_period` steps after
the warmup; otherwise (an orbit that escapes, or one that never settles down, as on a chaotic
parameter), `found` is false. With the cycle in hand, its multiplier is the derivative of the
`period`-th iterate at `w` (see {ref}`standard-library` for `compose`):

``` fractalstream
(found, w, period) := find_cycle(z -> z^2 + C, 0, 1000, 100, (x, y) -> x = y, z -> not (z escapes))
if found:
  multiplier := ⅆ(compose(z -> z^2 + C, period, w))/ⅆw
```

(colors-and-gradients)=
### Colors and gradients

FractalStream has a number of built-in color constants: `red`, `green`, `blue`, `black`, `white`, `grey`, `gray`, `orange`, `yellow`, `purple`, `violet`, and `pink`.

Beyond the color constants, you can construct or modify existing colors using these color functions:

| Operation | Meaning |
| --------- | ------- |
| `light C` | A lightened version of the color `C` |
| `dark C`  | A darkened version of the color `C` |
| `rgb(R,G,B)` | Build a color from red/green/blue components, each in the range from 0 to 1 |
| `hsv(H,S,V)` | Build a color from hue/saturation/value components, each in the range from 0 to 1 |
| `hsl(H,S,L)` | Build a color from hue/saturation/lightness components, each in the range from 0 to 1 |
| `invert C` | Invert the color `C` in RGB space |
| `blend(T,C1,C2)` | Perform a linear blend in RGB space from color `C1` to color `C2` using parameter `0 <= T <= 1` |
| `cycle(T,C1,C2)` | Go back and forth between `C1` (at whole numbers `T`) and `C2` (halfway between them) |

The standard library also has *color wheels*: functions of a real number `T` that go once around
a circle of colors as `T` goes from 0 to 1, and repeat with period 1. They are `rainbow` and
`arenberg` (a phase wheel for domain coloring); `romao`, `bamo`, `broco`, `corko` and `viko` (cyclic
scientific color maps); and `ice`, `fire`, `wheat`, `rose`, `forest`, `ocean`, `winter`, `spring`,
`summer` and `fall`, which go out through five colors and back. For example,
`color <- rainbow(iterations / 20)` goes once around the rainbow every 20 iterations.


### List and table indexing

FractalStream includes two different types for holding collections of data:
- **Lists** are used to hold a variable-length amount of data. Lists are not designed to hold
a huge amount of data, roughly on the order of something a user might enter by hand.
- **Tables** are meant to hold a multi-dimensional array of data. A table's size is set when it
is created, and can be very large. For example, some scripts build a table with one entry per
pixel in the parameter space viewer, and populate it by using Newton's method to follow data
from a dynamical plane to neighboring ones. Other scripts have used precomputed tables to
hold the uniformization data for an algebraic curve that we want to draw in the hyperbolic plane.

#### Reading a table

`T @ (j, k)` is the value in column `j` and row `k` of a 2-dimensional table `T`. Like lists,
tables count from 1: `T @ (1, 1)` is the cell at the top left, columns go to the right, and rows
go down. A table with `n` dimensions takes `n` indices, written as a tuple; a 1-dimensional table
takes a single index, `T @ k`. Reading outside of the table is an error, just as it is for a list:
the pixel is drawn in magenta, or a tool's changes are discarded.

Every table also has a position in the plane (see {ref}`language-tables` below), so `T @ point z` is
the value in the cell that contains the point `z`. A point can be a complex number or, as in a real
viewer, a pair of real numbers: `T @ point (x, y)` means the same as `T @ point (x + y i)`, and
the same goes everywhere a table takes a point. Each cell covers a small rectangle of the
plane; when a table is laid over a viewer's window, its cells are the viewer's pixels. Looking up
a point outside of the table is an error, like reading an index out of range. To look up a point
that may be off the table, give a value to use there instead with `else`:

``` fractalstream
color <- rainbow(saddle @ point C else 0)
```

The `else` part is a single value: `T @ point z else 0 + 1` adds 1 to the result, whichever it
is, so put anything longer in parentheses.

Only a table's first two dimensions (its columns and rows) are laid out in the plane. A table
with more dimensions has *layers* over the plane: in a `Table 3 of C`, `T @ (j, k, m)` is column
`j` and row `k` of layer `m`. In a lookup by point, `point z` stands for the first two indices,
and the others follow it, as in `crit @ (point C, m)` or `crit @ (point C, m) else 0`. A
1-dimensional table has no position in the plane.

A `*` in place of an index stands for every index along that dimension, and gives a list:
`T @ (j, *)` is column `j` of a 2-dimensional table, from the top down, and
`crit @ (point C, *)` is the value at `C` in each layer, starting from the first. All of a
1-dimensional table is `T @ *`.

A few functions tell you more about a table:

| Function | Meaning |
| -------- | ------- |
| `table_size(T)` | The table's size: an integer for a 1-dimensional table, or a tuple with one integer for each dimension. A table that hasn't been filled in yet has size 0. |
| `z in T` | Whether the point `z` is on the table. |
| `table_cell(T, z)` | The cell `(j, k)` containing the point `z`, even when `z` is off the table (then `j` or `k` is less than 1, or more than the table's size). |
| `table_point(T, (j, k))` | The point at the center of the cell `(j, k)`, for any `j` and `k`. |

For example, a viewer can check whether its table has been filled in yet before reading it:

``` fractalstream
(w, h) := table_size(saddle)
color <- if w = 0 then grey else rainbow(arg(saddle @ point C) / (2 pi))
```

A table of tuples, like a `Table 2 of C x Z`, holds a tuple in each cell. Read a cell's tuple
whole and take it apart as you would any other tuple:

``` fractalstream
(crit, found) := crits @ point c else (0, 0)
```

(standard-library)=
### The standard library

Some of FractalStream's functions are written in the scripting language itself, in its *standard
library*. Every project has them, as if they were in its `@common` stanza. A project can define any
of these names for itself, and then its own definition is the one it uses; a new function added to
the standard library in a later version of FractalStream never changes what an existing script
means.

| Function | Meaning |
| -------- | ------- |
| `min(x, y)`, `max(x, y)` | The smaller / larger of two numbers |
| `clamp(x, lo, hi)` | `x`, moved into the range from `lo` to `hi` |
| `compose(f, N, x)` | `f` applied `N` times to `x` (see {ref}`composition`) |
| `newton_root(f, x)`, `full_newton_root(f, x, N, stop)` | Newton's method (see {ref}`newton`) |
| `find_cycle(f, z, warmup, max_period, same, χ)` | Finding an attracting cycle (see {ref}`cycles`) |
| `S(f, x)` | The Schwarzian derivative of `f` at `x` |
| `riemann_sum(γ, f, N)`, `residue(f, z, r, N)`, `winding_number(γ, z0, N)` | Contour integrals (see {ref}`integration`) |
| `theta1(z, τ)` .. `theta4(z, τ)`, `wp(z, ω1, ω2)` / `℘(z, ω1, ω2)`, `wp_prime(z, ω1, ω2)`, `g2(ω1, ω2)`, `g3(ω1, ω2)`, `klein_j(τ)` | Elliptic functions (see {ref}`elliptic`) |
| `cycle(t, c1, c2)` | A color going back and forth between `c1` (at whole numbers) and `c2` (halfway between) |
| `rgb255(r, g, b)` | A color from red, green and blue components from 0 to 255 |
| `rainbow(t)`, `arenberg(t)`, ... | Color wheels with period 1 (see {ref}`colors-and-gradients`) |
| `domain_color(z)` | The usual domain coloring of the complex number `z` |
| `pixel_size_at(p)` | The size of a screen pixel at the point `p` of the viewer, in its own coordinates |

(elliptic)=
#### Elliptic functions

The standard library has the Jacobi theta functions and the Weierstrass ℘ function:

| Function | Is |
| -------- | -- |
| `theta1(z, τ)` .. `theta4(z, τ)` | The Jacobi theta functions $\theta_1$ .. $\theta_4$ of `z`, with the nome $q = e^{i \pi \tau}$ (and $\operatorname{Im} \tau > 0$), as in [DLMF §20](https://dlmf.nist.gov/20) and Mathematica's `EllipticTheta`, so that $\theta_1(z + \pi) = -\theta_1(z)$ |
| `wp(z, ω1, ω2)`, or `℘(z, ω1, ω2)` | The Weierstrass $\wp$ function of the lattice $\omega_1 \mathbb{Z} + \omega_2 \mathbb{Z}$ (in the editor, type `\wp` for `℘`) |
| `wp_prime(z, ω1, ω2)` | Its derivative, $\wp'$, the same as `℘'(z, ω1, ω2)` |
| `g2(ω1, ω2)`, `g3(ω1, ω2)` | The lattice's invariants, with $\wp'^2 = 4 \wp^3 - g_2 \wp - g_3$ |
| `klein_j(τ)` | Klein's $j$-invariant, with `klein_j(i)` equal to 1728 |

For example, this viewer colors the plane by $\wp$ for the lattice spanned by 1 and $0.3 + 1.1 i$,
showing its double poles at the lattice points, and its two zeros in each period parallelogram:

``` fractalstream
@viewer "Weierstrass p" center=0 pixelSize=1/150
color <- domain_color(wp(z, 1, 0.3 + 1.1 i))
```

`wp` and the others work with any basis of the lattice: they reduce it first (so that
$\tau = \omega_2 / \omega_1$ has $\operatorname{Im} \tau \ge \sqrt{3}/2$), and move `z` into the
parallelogram around 0, so that their series converge in a few terms. The theta functions
themselves sum their series as they're given, which is fastest when $\operatorname{Im} \tau$ is
large and $\operatorname{Im} z$ is small. `diff` follows `z` through `wp` too (the `round` it uses
has derivative 0), so `℘'(z, ω1, ω2)` and `diff(wp(z, ω1, ω2), z)` give $\wp'$, the same as
`wp_prime` up to rounding.

Functions with a body, like `compose` and `newton_root`, can be called anywhere a function with a
body can (see {ref}`functions`). The standard library's source is
[`fractalstream-core/stdlib/standard.fs`](https://github.com/matt-noonan/FractalStream/blob/main/fractalstream-core/stdlib/standard.fs).

## Scripts

FractalStream scripts are made up of a sequence of commands, each of which is a single line.
A very long command can be broken over multiple lines by writing an ellipsis `...` at the end
of one line, with the rest of the command on the next line.

Some commands end with a `:`, which starts a new *block*: a nested sequence of commands,
indented one additional level.

### Variable definition and assignment

New variables are defined using the `:=` operator. A variable stays visible until the end of its
enclosing indentation block.
``` fractalstream
z := 0
```

A variable defined with `:=` will have its type inferred by how it is used in your script. If you
need to specify a precise type instead of relying on inference, give a type annotation after your
new variable's name:
``` fractalstream
z : R := 0
```

The value of a variable can be updated with the `<-` operator:
``` fractalstream
z <- z^2 + 1
```

Multiple variables can be updated at the same time. For example, this code fragment to produce
the next Fibonacci number

``` fractalstream
old_x := x
x <- x + y
y <- old_x
```

can be replaced with the multiple assignment

``` fractalstream
x, y <- x + y, x
```

(functions)=
### Functions

The `:=` notation can also be used to define functions:

``` fractalstream
f(z) := z^2 + C
g(a, b) := a * f(b) - b
```

Besides its parameters, a function can use the project's names: configuration values, the
definitions in `@common`, the viewer's or tool's own variables (like the viewer's `z`), and
built-in values like `pixel_size`. It can't use variables that a viewer's or tool's script
declares itself; give the function a parameter for those instead. A function's parameters are
separate from any variables with the same name, so the parameter `z` of `f` above has nothing to do
with the viewer's own `z`. Functions don't need type annotations: `f(2)` and `f(z)` both work.

A function that takes more than one expression to work out can be written as a block, starting
with `function`. It gives its result by assigning to its own name:

``` fractalstream
function critical_point(w, c, tol):
  step := w
  repeat up to 100 times:
    step <- newton_step(w, c)
    w <- w - step
  until |step| < tol
  critical_point <- (w, |step| < tol)

(crit, found) := critical_point(0.1, C, 0.000001)
```

Every way through the function's body must give it a value: in every branch of an `if` (which then
needs an `else`), and not only inside a loop, which might not run at all. A function computes a
value and does nothing else: its variables are its own, and it can't draw, change a
configuration value, or set `color`. Its loops don't change the caller's `iterations` or `stuck`.

A function with a body is called like any other, anywhere in a script: `x <- x - f(x) / g(x)`,
`if close(f(z), 0):`, `while f(z) > tol:`. It is only called when its value is needed, so in
`if c then f(a) else b`, `f(a)` is only worked out when `c` holds. A few places can't call one,
because they can only hold a single value: a one-line function's own definition (`g(x) := 1 + f(x)`
is an error; make `g` a function with a body too), a configuration value's default, and the inside
of `partial` (use `diff` instead). `diff` sees through a function with a body, and so does the prime
notation, `f'(x)`.

A function can also be given to another function, or to a script fragment, as an argument. Give
it by name, or write it in place as `w -> ...` (or `(u, v) -> ...` for a function of two values):

``` fractalstream
sq(a) := a a
twice(g, v) := g(g(v))

x := twice(sq, 3)            # 81
y := twice(w -> w + 1, 3)    # 5

define shade(h):
  color <- rainbow(h(iterations))

run shade(n -> n / 20)
```

Nothing special marks a parameter as a function: it is one wherever it is given a function. Any
function can be given to any function: a one-line function, a function with a body, or one written
in place. Inside, a parameter given a function can also be differentiated with a prime:

``` fractalstream
newton_step(f, z) := z - f(z) / f'(z)

w := newton_step(u -> u^2 - 4, 4)    # 2.5
```

(composition)=
### Composing functions

`f ∘ g` is the function that applies `g`, then `f`: `(f ∘ g)(x)` means `f(g(x))`. In the editor,
type `\circ` and a space for `∘`. Each side can be a function's name (including built-in functions of
one value, like `sin` or `exp`), a parameter that has been given a function, or, in parentheses, a
function written in place or another composition. Compositions chain, so `f ∘ g ∘ h` means
`f ∘ (g ∘ h)`. A composition can be applied, differentiated, or given to a function:

``` fractalstream
sq(a) := a a
inc(a) := a + 1
twice(g, v) := g(g(v))

x := (sq ∘ inc)(2)              # 9
y := (sin ∘ sq)(0)              # 0
d := (sq ∘ inc)'(2)             # 6
t := twice(sq ∘ inc, 1)         # 25
```

When `g` gives back a tuple, and `f` takes as many values as the tuple has parts, the parts become
`f`'s arguments. This works for any call, not only for compositions: with `sub(a, b) := a - b` and
`pair(t) := (t + 5, t)`, both `(sub ∘ pair)(2)` and `sub(pair(2))` are 5.

To apply a function many times over, use the standard library's `compose(f, N, x)`, which applies
`f` to `x`, `N` times: `compose(sq, 3, x)` is `sq(sq(sq(x)))`.

### If / else blocks

Script blocks can be run conditionally using the `if` command.

``` fractalstream
if re(z) < 0:
  color <- red
```

An `if` block can optionally be followed by an `else` block.

``` fractalstream
if re(z) < 0:
  color <- red
else:
  color <- yellow
```

Multiple `if`/`else` blocks can also be chained together.

``` fractalstream
if re(z) < 0:
  color <- red
else if im(z) > 0:
  color <- green
else:
  color <- blue
```

### Loops

FractalStream has a number of different looping constructs; all of them share the property that
no infinite loops are allowed.

The simplest loops are the `iterate VARIABLE -> EXPRESSION while CONDITION` and `iterate VARIABLE -> EXPRESSION until CONDITION` forms:

``` fractalstream
iterate z -> z^2 - 1 until z escapes
```

``` fractalstream
iterate z -> z^2 - 1 while |z| < 2
```

In each case, the user must say *which* variable we're iterating (`z`, in these examples), *how*
we're updating it (using the expression `z^2 - 1` here), and *when* we should stop iterating
(the `until` or `while` clause).

`iterate` can also use multiple assignment, as in this fragment that computes the first
Fibonacci number larger than 100:

``` fractalstream
x := 1
y := 0
iterate x, y -> x + y, x until x > 100
```

If you need to loop over a collection of statements instead of a single expression, there are
four closely-related looping constructs. The first two are the `while` and `until` loops,
which execute their indented loop bodies as long as the condition is true (for `while` loops)
or false (for `until` loops). Like `iterate`, these loops will halt if they run `max_iters` times.


``` fractalstream
while |z| < 10:
  z <- z^2 + C
```

``` fractalstream
until |z| >= 10:
  z <- z^2 + C
```

A `while` or `until` loop may not execute its body even once. For example, if `z` was 100 then
the `while` loop above would never run its body. If you want a `while` or `until` loop that
*always* runs its body at least once, use a `repeat` loop:
``` fractalstream
repeat:
  z <- z^2 + C
while |z| < 10
```

``` fractalstream
repeat:
  z <- z^2 + C
until |z| >= 10
```

To run a block a fixed number of times, use `repeat N times:`. Unlike the loops above, it has no
condition, and always runs its body exactly `N` times (or not at all if `N` is 0 or less):

``` fractalstream
repeat 10 times:
  z <- z^2 + C
```

For a single variable, `iterate z -> z^2 + C for 10 times` does the same in one line.

If you need a loop with a condition to stop after a specific number of times, independent of the
`max_iters` variable, add an `... up to N times` clause.

``` fractalstream
iterate z -> z^2 + C until z escapes up to 1000 times

while |z| < 10 up to 1000 times:
  # etc

until |z| >= 10 up to 1000 times:
  # etc

repeat up to 1000 times:
  # etc
```

(draw-commands)=
### Drawing

Tool scripts are allowed to draw on top of the viewer, to provide indicators and other useful
information. Each tool gets its own "layer" of annotations; within that layer they can accumulate
more drawings, erase the existing drawings, and change properties such as the line- and fill-colors.


| Drawing command | Effect |
| --------------- | ------ |
| `erase`         | Clear the tool's drawing layer |
| `use COLOR for line` / `use COLOR for stroke` | Set the line color to `COLOR` for future drawing operations in this layer |
| `use COLOR for fill` | Set the fill color to `COLOR` for future drawing operations in this layer |
| `write TEXT at POINT` | Write the text `TEXT` at the point `POINT` |
| `draw point at POINT` | Make a small dot at `POINT` |
| `draw line from POINT1 to POINT2` | Draw the shortest path from `POINT1` to `POINT2` in the viewer's geometry, using the line color |
| `draw rectangle from POINT1 to POINT2` | Draw an empty rectangle with corners `POINT1` and `POINT2` using the line color (only in viewers of the plane) |
| `draw filled rectangle from POINT1 to POINT2` | Draw a filled rectangle with corners `POINT1` and `POINT2` using the line color for the border and the fill color for the inside (only in viewers of the plane) |
| `draw circle at POINT with radius R` | Draw an empty circle around `POINT` with radius `R`, measured in the viewer's geometry, using the line color |
| `draw filled circle at POINT with radius R` | Draw a filled circle around `POINT` with radius `R`, measured in the viewer's geometry, using the line color for the border and the fill color for the inside |
| `draw circle at POINT with radius R pixels` | Draw a marker: a circle `R` pixels in radius on the screen, the same size at any zoom (also `draw filled circle`) |


```{note}
In the drawing operations, `POINT`s are always given by *model* coordinates, not pixel offsets
in the window. For example, the command `draw circle at i with radius 1` will always draw
a circle centered at the complex number `i` and tangent to the real axis, no matter where
the view is currently looking.
```

Lines and circles are drawn in the geometry of the tool's viewer:

| Viewer | A line is... | A circle's radius is... |
| ------ | ------------ | ----------------------- |
| Real or complex plane | a straight segment | the usual distance |
| Riemann sphere (`view=sphere`) | the shorter arc of a great circle (any one, between opposite points) | the chordal distance |
| Hyperbolic plane (`view=disk`, `view=halfplane`) | the hyperbolic geodesic | the hyperbolic distance |

On a sphere viewer, the points are points of `Ĉ`, so a tool can draw at `∞`, or draw a line
through it.

### Script fragments

Sometimes different scripts in a project want to share some code in common. In those cases, the
user might use `define` to create a script fragment, which later can be run with `run`. For
example, here is a script fragment to carry out one step of Newton's method:

``` fractalstream
@common

f(z) := z^3 + z + 1

define newton_step:
  z <- z - f(z) / f'(z)
```

Later, we can run this script fragment:

``` fractalstream
@viewer "Newton's method"
repeat:
  run newton_step
until z stops
```

Script fragments can also take arguments. In this example, we'll write a fragment that carries out Newton's method given an initial guess, and also sets a variable called `found` when the iteration
successfully converged.

``` fractalstream
@common

f(z) := z^3 + z + 1

define newton_root(z):
  iterate z -> z - f(z) / f'(z) until z stops
  found <- not stuck

@viewer "Newton's method"
run newton_root(z)
if not found:
  color <- black
else:
  color <- rainbow((1 + arg(z)/pi) / 2)
```


### Lists

A list is written as a `,`-separated sequence of items in square brackets, like `[1, 2, 3]`. All
of a list's items have the same type: `[1, 2, 3]` is a `List of ℤ`, `[1, 2, i]` is a `List of ℂ`,
and `[[1], [2, 3]]` is a `List of (List of ℤ)`, but `[1, [2, 3]]` is an error. In a configuration
text box, a list is written without the outer brackets, as `1, 2, 3`. An item given a type needs
parentheses, as in `[(k : R), 2]`, because `[a : b]` is a point of `ℙ¹` (see {ref}`riemann-sphere`).

| Operation | Meaning |
| --------- | ------- |
| `length(xs)` | The number of items in `xs` |
| `xs @ k` | The `k`th item, counting from 1; an index outside the list stops the script |
| `xs @@ k` | The `k`th item, counting cyclically, so `xs @@ 0` is the last item and `xs @@ -1` the one before it |
| `range(a, b)` | The list of integers `[a, a + 1, ..., b]` |
| `join(xs, ys, ...)` | The lists one after another |
| `append(xs, x)` / `prepend(xs, x)` | `xs` with `x` added at the end / at the start |
| `remove(xs, item -> P)` | `xs` without the items for which `P` holds |
| `transform(xs, item -> E)` | Each item replaced by `E` (of the same type) |
| `find(xs, item -> P, d)` | The first item for which `P` holds, or `d` if there isn't one |

``` fractalstream
xs : List of Z := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
odds := remove(xs, k -> mod(k, 2) = 0)        # [1, 3, 5, 7, 9]
squares := transform(odds, k -> k^2)          # [1, 9, 25, 49, 81]
big := find(squares, k -> k > 10, 0)          # 25
```

To run some commands for each item of a list, use `for each`:

``` fractalstream
total := 0
for each k in xs:
  total <- total + k
```

Unlike `while` and `until` loops, a `for each` loop isn't limited by `max_iters`: it always goes
through the whole list.

To run some commands on the first item that matches a condition, and others if no item does, use
`with first ... matching`:

``` fractalstream
with first k matching k^2 > 10 in xs:
  x <- k
else:
  x <- 0
```

(language-tables)=
### Tables

Only a tool can change a table; viewers can only read them. A tool starts a table afresh with
`reset`, which gives the table a size and a position in the plane, and fills every cell with zero
(or `false`, or black, depending on the type of the table's elements):

``` fractalstream
reset T to (viewer_width, viewer_height)
```

This lays the table over the tool's viewer window. With the viewer's own size, as here, cell
`(j, k)` is the pixel in column `j - 1` and row `k - 1` (pixels count from 0); with another size,
the cells are stretched to cover the window. To cover a different rectangle of the plane, give
its top left and bottom right corners:

``` fractalstream
reset T to (200, 100) from -2 + i to 2 - i
```

These are the outer corners of the table's corner cells, so here each cell is 0.02 wide and high,
and cell `(1, 1)`, the top left one, is centered at `-1.99 + 0.99 i`. The corners can also be
pairs of real numbers, as in `reset T to (250, 250) from (0, 1) to (1, 0)`.

For a table with layers, give one more size for each further dimension:
`reset crit to (viewer_width, viewer_height, 4)` lays 4 layers over the window. To start every
cell with a value other than zero, add `filled with` at the end:

``` fractalstream
reset status to (viewer_width, viewer_height) filled with -1
```

The tool then writes the table's cells with `<-`, by index or by point:

``` fractalstream
T @ (j, k) <- v
T @ point z <- v
crit @ (point c, m) <- v
```

Writing outside of the table is an error, and the tool's changes are discarded. A slice with one
`*` can be written all at once from a list with one item for each of its cells:

``` fractalstream
crit @ (point c, *) <- [z1, z2, z3]
```

and a slice can be copied onto another slice of the same shape, in the same table or in another
table with the same kind of elements:

``` fractalstream
crit @ (*, *, 1) <- crit @ (*, *, 2)    # layer 2, copied onto layer 1
crit @ (*, *, 3) <- saddle @ (*, *)     # a 2-dimensional table, copied onto layer 3
```

A cell of a table of tuples is always written whole, with a tuple: `crits @ (j, k) <- (w, 1)`.
To change just one part of the tuple, read the cell and write it back with that part changed.

A tool can also change a table's size with `resize`, which keeps the table where it is in the
plane:

``` fractalstream
(w, h, n) := table_size(crit)
resize crit to (w, h, n + 1)     # one more layer
```

Cell `(1, 1)` stays in the same place, and so does every cell that the table still has, along
with its value. New cells are zero, or the value given by `filled with v` at the end. So adding
columns or rows adds them at the right or the bottom, and removing them takes them away from
there. To lay a table out somewhere else, `reset` it instead.

A tool's changes to a table take effect when its handler finishes without an error, like its
changes to any other configuration variable, and then every viewer that reads the table is drawn
again. A viewer that is drawing while the tool runs keeps using the table as it was when the
viewer started.

For example, this tool works out a value for every pixel of a parameter plane, and both viewers
can read the results:

``` fractalstream
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
