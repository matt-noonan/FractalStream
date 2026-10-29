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

The usual arithmetic operators `+`, `-`, `*` (multiplication), `/`, and `^` (exponentiation) are available for real and complex types. With the exception of
`/`, these are also available for integer types. For integer division,
use `//` instead.

### Comparisons

Values can be compared for equality with `=` (alternate spelling: `==`),
and for inequality with `!=` (alternate spellings `=/=` or `≠`). Real
and integer values can be compared with `<`, `>`, `<=` (alternate spelling
`≤`), and `>=` (alternate spelling `≥`).

### Boolean operations

The boolean operators `and`, `or`, and `not` can be used, with
`or` taking precedence over `and`, and `and` taking precedence over
`not`. That is, `not p and q or r` would be parsed as `((not p) and q) or r`.
This can be confusing, so as a general recommendation you should not
mix `and`s and `or`s without parentheses.

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
`r`, `g`, and `b` are values representing real numbers from `0` to `1`.
Two different colors can be blended by `blend(t, c1, c2)` where `t`
is a real number from `0` to `1` representing the percentage to take
from `c1`, with `(1 - t)` taken from `c2`.

## Script commands

### Variable assignment

To assign a value to a pre-existing variable, use the `<-` operator.

```
z <- z^2 + C
```

You can also use the unicode versions `⭠` or `←` (or more likely, let
the code editor automatically substitute these when you type `<-`)

### Variable initialization

To create a new variable in your script, use the `<-` operator but
provide a type annotation on the left-hand side.

```
k : Z ⭠ 0
```

Common types you might want to use include `Z` for the integers, `R` for the
real numbers, `C` for the complex numbers, `Color` for a color, and
`Boolean` for a truth value, and `Text` for plain text. You can also use
the unicode blackboard bold variants `ℤ`, `ℝ`, and `ℂ`, or the full names `Integer`, `Real`, and `Complex`.

Lists of values of type `x` have the type `List of x`.

You can also leave the type out, and let FractalStream work it out, by
using `:=` (or `≔`) instead:

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
type annotation, like `xs : List of R <- []`.

A new variable can be used in the rest of the block it is declared in.

### Defining functions

To define your own function, use `let`:

```
let f(z) = z^2 + C
let g(a, b) = a * f(b) - b
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
let f(z) = z^3 - 1
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
a : C as "a =" <- 1

@common
let f(z) = z^3 - a
let newton(z) = z - f(z) / f'(z)

@viewer "Newton's method" var=z size=512x512
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

### More about loops

FractalStream enforces a limit on the number of times each loop will run.
This limit is configured in the script and often is exposed to the script's
user, so they can easily change the iteration limit while exploring.

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
example, scripts that are used to draw pictures must output a value
to determine which color to draw. This is done using normal assignment
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

- Viewer scripts are expected to set the `color` variable. Changes
to other variables are ignored.

- Event handlers in tool scripts can set any configuration value. This
is useful for building tools that directly manipulate the configuration,
such as selecting a parameter value.

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
        prevZ : C <- z
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
defaults to the viewer's own `z-coord` variable; for drags, an omitted
start position is simply unused. `on click-or-drag` binds one coordinate
shared between the click and the in-progress drag.

A tool script can have at most one block for each of `refresh`,
`activated`, `deactivated`, `click`, `double-click`, `drag`, and
`drag-finished`, but any number of distinctly-named `button` and `timer`
blocks.

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
myList : List of Z <- [3, 1, 4, 1, 5, 9, 2]

x : Z <- length(myList)
# x is now 7
```

#### Indexing into a list

To read a specific element out of a list, use the `@` operator. **NOTE:**
lists in FractalStream use 1-based indexing!

```
fibs : List of Z <- [0, 1, 1, 2, 3, 5, 8]

x : Z <- fibs @ 4
# x is now equal to 2
```

Attempting to use a non-positive index or an index larger than the length of the list will cause your script to terminate.

In addition to normal list indexing, you can also cyclically index into a
list using the `@@` operator:

```
digits : List of Z <- [3, 1, 4, 1, 5, 9, 2]

x : Z <- digits @@ 10
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
myList1 : List of Z <- [1,2]
myList2 : List of Z <- [3,4,5]
myList3 : List of Z <- [6]

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
myList : List of Z <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

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
myList : List of Z <- [1, 2, 3, 4, 5]

myList <- transform(myList, x ⟼ x^2)
# now myList is [1, 4, 9, 16, 25]
```

#### Finding an element in a list

The `find` function can be used to find the first element of a list
that matches some predicate. You must supply a fallback value to use
when no items in the list match your predicate.

```
myList : List of Z <- [1, 2, 3, 4, 5]

x : Z <- find(myList, item ⟼ item^2 > 10, 17)
# x is now 4

x <- find(myList, item ⟼ item^2 > 100, 17)
# x is now 17
```

For more complex situations, there is also the `with first ... matching`
command that can be used to run some commands on the first value of a list
that matches some predicate, and optionally run some other commands if no
such value exists.

```
myList : List of Z <- [1, 2, 3, 4, 5]

x : Z <- 0
y : Z <- 0

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
myList : List of Z <- [1, 2, 3, 4, 5]

sum : Z <- 0
for each x in myList:
  sum <- sum + x

# sum is now 15
```

**NOTE:** `for each` loops are not subject to the special iteration limits
that apply to `while`, `until`, and `iterate`. The `for each` loop will
always process each element of the list.
