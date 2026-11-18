# Writing FractalStream scripts
A walkthrough for how to use the FractalStream scripting language

A FractalStream script consists of several different components:
- One or more **viewers**, which define scripts that run once per pixel in a window to create a
view of a dynamical system.
- Optional **configuration variables** that become editable parameters appearing in the
"Configuration" window.
- Optional **tool scripts** that define new tools for interacting with and manipulating your dynamical system.
- Optional **common code** that is shared between the viewers and tool scripts.

Here is an example of a script that defines two viewers (the blue and purple boxes), one
configuration variable (green box), and one tool (yellow box):

```{figure} _static/script-components.jpg
[Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVKxTsMwEN37FaesKFGapkkaqRJSZ8Q3uM4lPZHalu2mtGJCLOzs_QckNiY-pV-CSUhbCksXNr_nu-f37my4JmVzeBgAXDeEa9Tg3Wyg2Ai2JG48x5NFzSzCFva7d9h-vMIVzGAlLNWOQ8OZQjM4E1BMsyW6TlA1E-hBw_R05oqcyvMLhBfpcilKqlpdg9aSqFpjM8hh__TYCvphMIyiceI0wiCN4yyhttVKWYN3q-dkjQel1NPzeFKAxlKjWeQOAThTBju-QMYtNc5k8euO18TvfKn9QrPq5y04QYXMdmwbeffWB_zmXNcalCRhgVnYnrI1CYRSy6VrtBKUxobkynwXnc9njXNfaalQW0Lz9WS3BdO9fhK2t8Pv83b-7XlzPLtFceyLfEiDCfqjA0yCJEY_OuAwGE2S7ASmUdZDjRVJcZSKgyRLx39Ds3Axchj2bs8-zoWmwyAeRun_mf4E&edit)
```

In the next few sections, you will learn how to write basic FractalStream viewer scripts and define your own custom tools.

## Your first script: the Basilica

Let's start by writing a minimal FractalStream script to visualize [the Basilica](https://en.wikibooks.org/wiki/Fractals/Mathematics/group/Basilica_group). This is the Julia set of the function $f(z) = z^2 - 1$.

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=bY85DsIwFET7nGJE7ygb2SouwCGM-YClLJZtCIm4C3egpuIonASLKAkF3byvNyN9I7RUtsSNecDmIqkjjdW2x0FqY7HvG15LwSuY3liqV86SljS3hAHv-xPD6wGGEOfGysrdyAiuyHgd7ZjSrSJtJZnS9cbxb4QTVcUbGgEQ1xLBlPslO0nMEkPmF8TiGVM_TYhFMwd-XKT5D2ZRPqGmo2ybZSrx0zxb_0dzci-UCL0P&edit)
---
@viewer "My first dynamical system"
iterate z -> z^2 - 1 until z escapes
```

Viewers are defined by the `@viewer` syntax, followed by the quoted name that you want to use
for the viewer's window.

**TODO** the script itself

## Adding a parameter

Let's define a user-editable parameter so that we can look at different Julia sets using a
single FractalStream script. We'll add a parameter $C$ and visualize the Julia sets of the
family $f_C (z) = z^2 + C$.

To do this, we just need to add a `@config` section that defines our configuration variable.
Much like a `@viewer`, each `@config` section has a title which will be visible in the
"Configuration" window. Following that, you can define one of more *configuration variables*
using the syntax `NAME : TYPE := INITIAL_VALUE`. Let's define a complex number called
`C`, starting at the value `-1`, so we write `C : C := -1` in our new `@config` stanza.

```{note}
In the expression `C : C := -1`, the first `C` is the name of our variable and the
second `C` denotes the complex numbers. In the FractalStream script editor, this line
will get transformed to look like `C : ℂ ≔ -1`; a little easier to understand!
```

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=dZA7DsIwEET7nGJEixzxTYIrJGoOYcwClkJi2eYTRIVo6Om5AzUVR8lJMEQBCujm7c6M5bXSKO049iwAhjLPZmqOxrhAJVdGOJVnDb8cgaM8HlCezmDt4GlfK9qQedkt-cAU0yITSyVFCltYR8tnUDnyLYQdyssNu_sVTV-2ypxK_YysFJpssKEJ0ybXZJwiy32uan9J4P8T1R6QW45WrYuP1qmQVJsY4nBArPvGKIx6xDpvboXdQZR8YdxJajQ097f4VPXCKIn7v9Eu_K842sED&edit)
---
@config "My configuration"
C : C := -1

@viewer "My second dynamical system"
iterate z -> z^2 + C until z escapes
```

When you first run this script, you should see the Basilica again. But if you open up the `Configuration` window, you'll see a new tab called `My configuration`, with a text entry where you can
enter new values of `C`.



## Adding a second viewer

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVC9TsMwGNz7FKeslaMkDUkaCalSZx7CmI9iKT-W7bYkYkIs7Oy8AzMTj5InwXWUFlUdYGG783d33_kzQktlSzyxGbASbXMvNwhuOoxwq7mVbRO44RolhpdnDK9vYPHsIN9J2pP28ruu4bUUvILpjKX64JCWnJ3QY3j_RP_1gblL2TZWVu6NjOCKzHmQ4prX5JxQFW8owI7r67UT9X5z9OvcPd0ypVtF2koypfONWzwELnQeB4B4LP0ej7sTdoUETSKGPFwSWxxpFmYpseTIo3CxzIofNE-KiWrauKueotIwK_Kry9Q8uO-UiKfWZwf6Y-koTOMk_7_S3w&edit)
---
@config "My configuration"
C : C := -1

@viewer "My dynamical system"
iterate z -> z^2 + C until z escapes

@viewer "My parameter plane" var=C
z := 0
iterate z -> z^2 + C until z escapes
```

```{note}
When defining the variable `z` in the parameter plane script, we didn't have to write `z : ℂ ≔ 0` because FractalStream is able to infer the type of `z` based on how we use it in the rest of the script. Because of this type inference, usually you only need to specify the type of a variable when it appears in a `@config` block.
```

## Adding a tool

Let's add a tool to follow the orbit of a point in the dynamics plane. We'll do that
by adding a `@tool` section to our script that does the following:
- Provides a name for our tool ("Orbits" in this example)
- Names which viewer the tool should apply to ("My dynamical system" here)
- Gives a number of scripts that define how the tool should respond to various user events
(tool activation, viewer redraw, mouse actions, and so on)

The tool scripts have access to {ref}`drawing commands <draw-commands>` that can be used to annotate the
view with useful information. For the "Orbits" tool, we'll draw a small dot to indicate the
position of each iteration, and a line connecting each point on the orbit to the previous point.

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVKxbsIwEN35ilPWyhEBmkAkpErMVb_BmAtYTWzrbKCJOlVdunfnHyp169RP4UtqEgIU0YGl273n9-7eXWIFSeNSeGYdgDuhVSbnENyX0JRL4k5qFfjHCaSwfX2B7ds7sKizk68krpFq-axUvJCC52BL67DYOaRDb0eoYLv5gur7A258l6VyMvccWsEN2vNGhhMv0DvB5FxhACtO44kXVfXk7lV9ndY5BA80lc4GkGka_5VVKyDMCO0i9QjAT7DY8CKX4pFpYjPi89-v4E0GuWvYOuPms02057xrDUZL5YA7qE7ZXCqEjHThjU6DIVxJvbR70flCa5wyQ9ogOYl2N7I5m22mX1isjSWe0vpwdV0ea39hga2IQRKOkPUPMA7jAbLeAXfD_igensCkN2wh4dz_JsdWgzAeJreXoV34dVKI2tRnX_zK0N1wEPWS_wv9Aw&edit)
---
@config "My configuration"
C : C := -1

@viewer "My dynamical system"
iterate z -> z^2 + C until z escapes

@viewer "My parameter plane" var=C
z := 0
iterate z -> z^2 + C until z escapes

@tool "Orbits" for="My dynamical system"
on refresh:
  erase
on click-or-drag:
  repeat:
    z <- z^2 + C
    draw point at z
    draw line from z to previous z
  until z escapes
```

## Don't repeat yourself!

Our script has a bit of repetition; in particular, we write `z^2 + C` repeatedly. Let's give
that expression a name to help with readability, and to make it so that if we want to
change which function we are iterating, we only have to change it in one place.
We'll do this by adding a `@common` section to our script. Definitions in a `@common` section
are available to all other parts of the script.

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVJLTsMwEN33FKOsQChR0jRfqRJS14gzuOmktUhsy3ZbGrFCbFgiIXaIOyCxY8VRegKOgBv3R1Uk2LCbNzPvzZuxVSGp0DncuB2A84LXNWcmKk-aU1jeP0Lz8QpnMOjYKivpGJyLBdhwKommnDmmOIAclne3LccN2vYZxTnKtn20YKSmBalALZTGesWgGg0doYHly7sdOGWaViaBqiAC1aGKIJLUaGggKsLQgRmR_YFpatqx_u9FNecVOJdySLVyoOSy_5NLzkBiKVFNcoMAjLxCmy8qWly5XLojScbfq2BIAom22dbgy1trZ50wlDkITpkGoqHZz1aUIZSS14alOQiJM8qnat10uM2MVFNUqzn2SezEQQ6B50eJHwVxkgRZN0zNKwZeGCZx5gdpFGa9LPp8fnjqzHHoCskFSk2tkL24skpHzrJZqrjO25u38WIXm8cpcNPkQuJl6IZbGHtxD93uFvtemMXpHky66QZKHJvvtZPqeXGaRMehmph7mLU3rg8-yx9N-14v6Cb_Z_oL&edit)
---
@common
f(z) := z^2 + C

@config "My configuration"
C : C := -1

@viewer "My dynamical system"
iterate z -> f(z) until z escapes

@viewer "My parameter plane" var=C
z := 0
iterate z -> f(z) until z escapes

@tool "Orbits" for="My dynamical system"
on refresh:
  erase
on click-or-drag:
  repeat:
    z <- f(z)
    draw point at z
    draw line from z to previous z
  until z escapes
```

Now you can easily change your function by editing the script in just one place. Try some
other functions with 0 as a critical point, such as `z^3 + C`, `(z^2 + C) / (z^2 + 1)`,
`cos z + C`, or `e^z - z + C`.
