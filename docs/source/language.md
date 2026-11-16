# The FractalStream scripting language

## Types

| Name         | Alternative name | Meaning                                                                 |
|--------------|------------------|---------------------------------------------------------------- |
| `Boolean`    | `Bool`                 | Truth values, `true` or `false`                                                    |
| `Z` | `ℤ`, `Int`, `Integer` | Integer |
| `R`            | `ℝ`, `Real`                | Real number                                                             |
| `C`            | `ℂ`, `Complex`                | Complex number                                                          |
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

### Built-in constants

FractalStream knows about several special constants

| Names        | Meaning          |
|--------------|------------------|
| `e`, `𝑒`    | Euler's number |
| `pi`, `π`   | Pi |
| `i`, `𝑖` | The imaginary unit |
| `true`  | Boolean true value |
| `false` | Boolean false value |

Expressions can be used anywhere in the FractalStream app where a number would be expected.
For example, a complex-valued numeric field will accept input like `1 + 2i`, but also
input like `e^{2 pi i / 3}`.

In addition to the values given here, there are a number of built-in color constants; see
**TODO** below.

### Special variables and predicates

| Name  | Meaning | Configured by... |
| ----- | ------- | ---------------- |
| `max_iters` | Maximum number of iterations through any loop | `Configuration > Iteration settings > Iteration limit` |
| `max_radius` | Escape radius | `Configuration > Iteration settings > Escape radius` |
| `min_radius` | Vanishing radius / tolerance for `=` comparison | `Configuration > Iteration settings > Vanishing radius` |
| `iterations` | Number of iterations performed by the most recent loop | *automatic* |
| `stuck`      | `true` when the last loop exceeded `max_iters` | *automatic* |
| `pixel_size` / `pixel_width` | The width of a pixel in the current view | *automatic* |
| `pixel_height` | The height of a pixel in the current view; equal to `pixel_width` for complex viewers, but may vary for real viewers | *automatic* |
| `viewer_width` | The width of the current view in pixels | *automatic* |
| `viewer_height` | The height of the current view in pixels | *automatic* |

| Name | Meaning |
| ---- | ------- |
| `z escapes` / `z escaped` | Equivalent to `\|z\| > max_radius` |
| `z vanishes` / `z vanished` | Equivalent to `\|z\| < min_radius` |
| `z stops` / `z stopped` | Test if `z` changed by less than `min_radius` over the last loop iteration |
| `x = y` | Equivalent to `\|x - y\| < min_radius` |


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
- For real numbers and complex numbers, `X = Y` tests if `|X - Y|` is less than `min_radius`. This is because making exact comparisons between floating point numbers is rarely the correct thing to do, so we use "equal up to `min_radius` tolerance" as the semantics of `=`.
- If you really need *exact* equality checks for real or complex numbers, use the `===` / `≡` operator.

Inequality can be tested with the `!=` /  `=/=` / `≠` operator.

### Built-in functions

- Functions that work for real and complex numbers: `exp`, `log`, `sqrt`, `cos`, `sin`, `tan`,
`cosh`, `sinh`, `tanh`.
- Functions that work for real numbers only: `arccos` / `acos`, `arcsin` / `asin`, `arctan` / `atan`, `arccosh` / `acosh`, `arcsinh` / `asinh`, `arctanh` / `atanh`.
- Functions that work for complex numbers only: `re` / `Re`, `im` / `Im`, `arg` / `Arg`, `conj` / `bar` / `Conj` / `Bar`.
- Rounding functions: `round`, `floor`, `ceiling` / `ceil`.

### Derivatives

You can take the derivative of an expression using the `diff(VARIABLE, EXPRESSION)` operation.
For example:

``` fractalstream
dz := diff(z, z^2 + C)
```

will set `dz` to `2 z`. If you have made user-defined functions, those can also be used
inside `diff`:

``` fractalstream
f(z) := z^2 + C

# ...later...

dz := diff(z, f(z))
dC := diff(C, f(z))
# dz is now equal to 2 z, and dC is equal to 1
```

In the case where a user-defined function has exactly one argument, you can also use the
Newton prime notation for the derivative. For example, here is a script to run Newton's method
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

Higher derivatives may be denoted with additional primes.

### Colors and gradients

FractalStream has a number of built-in color constants: `red`, `green`, `blue`, `black`, `white`, `grey`, `gray`, `orange`, `yellow`, `purple`, `violet`, and `pink`.

| Operation | Meaning |
| --------- | ------- |
| `light C` | A lightened version of the color `C` |
| `dark C`  | A darkened version of the color `C` |
| `rgb(R,G,B)` | Build a color from red/green/blue components, each in the range from 0 to 1 |
| `hsv(H,S,V)` | Build a color from hue/saturation/value components, each in the range from 0 to 1 |
| `hsl(H,S,L)` | Build a color from hue/saturation/lightness components, each in the range from 0 to 1 |
| `invert C` | Invert the color `C` in RGB space |
| `blend(T,C1,C2)` | Perform a linear blend in RGB space from color `C1` to color `C2` using parameter `0 <= T <= 1` |


### List and table indexing

## Scripts

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

If you need your loop to run a specific number of times independent of the `max_iters` variable,
add an `... up to N times` clause.

``` fractalstream
iterate z -> z^2 + C until z escapes up to 1000 times

while |z| < 10 up to 1000 times:
  # etc

until |z| >= 10 up to 1000 times:
  # etc

repeat up to 1000 times:
  # etc
```



### Drawing

### Lists

### Tables

## Project structure

A FractalStream project's script is made up of several *stanzas* that name and define the components
making up a FractalStream project. Each stanza starts with a *stanza definition*, indicated by
a `@` at the start of the line. The different kinds of stanzas are described in the subsections here.

### Viewers

A *viewer* is defined by a stanza of the form `@viewer "VIEWER NAME"`, where `VIEWER NAME` is the
quoted title of the viewer window.

### Shared definitions

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

### Configuration panels

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

### Tools

### Colorizers
