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
[Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVKxTsMwEN37FaesKFGapkkaCQmpM-IbXOfSnkhty3ZTGjEhFnb2_gMSGxOf0i_BJKQtLUsXtnvPd8_v-Wy4JmVzeBwA3NSEa9Tg3W6g2Ai2JG48x5NFzSxCA7vtBzSfb3AFU1gJS5Xj0HCm0AxOBBTTbIluElTFBHoga1dPXZeTeXmF8CJhLkVJ81bYoLUk5q2zKeSwe35qBf0wGEbROHEaYZDGcZZQO2qlrMC70zOyxoNSnueTAjSWGs0idwjAmTLY8QUybql2JouzM14Rv-_YBvLrDv9qAqerkNmuqU2-fe9z_nCFZmtQkoQFZqE5ZisSCKWWSzdoJSiNNcmV-Wk6faY1znylpUJtCc33ld02THf7UebeDn_I2zW09eZQu4Vx7Jt8SIMJ-qM9TIIkRj_a4zAYTZLsCKZR1kONc5LiIBUHSZaO_4Zm4WLkMOzdnnygC02HQTyM0v8z_QU&edit)
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
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVA7TsNAFOxzipFbtFbsGNtxhZSaQyzLI6zkz2p3k2CLCtHQ03MHaiqO4pPwYssJilKEhm5m38y82eeU1cYXeBYz4EY19YNeI7htMcKNlV43dcDDFQr0ry_o394hotlevtW0IzvI79taVlrJEq51nqq9Q3tiO6FD__GF7vsTV5yyqb0u-Y2ckobcaZCRVlbETphS1hSg2TJesaobVs8vDt7RnTC2MWS9Jlewb1wzQOBM6XEAqKdi2DPg9oi5kaJJJJCFSxKLA03DNCERH_g8XCzT_BfN4nyiltZ81mNUEqZ5dn2eukf-ToFoan1yoT-WnodJFGf_V_oH&edit)
---
@config "My configuration"
C : C := -1

@viewer "My dynamical system"
iterate z -> z^2 + C until z escapes

@viewer "My parameter plane" over C
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
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVK7TsNAEOz5ipFbZCsv7MQSElJqxDdcLuvkhH13ursk2KJCNPT0-QckOio-JV_CxY6TEIUiDd3O3Mzu7NqWG6FdiufwCrjjSmZihuC-RFMuDHNCycA_jpFi8_qCzds7wu7VVr4UtCJTy6elZIXgLIctraNi6xCOvJ1QYbP-QvX9gWvfZSGdyD1HljNN9rSRZoYV5J3QOZMUQC19Pfaqqh7duaixUypH8GAmwtkAmfo7rJIwlBmy89QjwE-w1PA8F_yxYSuktw3-JYL3amKuEdVR159tsB03NWwFrYR0YA7VMZsLSciMKrzRKWhDS6EWdic63WtFk1Abpck4QXY7sjmfbaaf2a-NxZ_S-n51XR5qf2lOrShEEo0o7O9hHMUDCnt73In6o3h4BJPesIWGZv53ObQaRPEwuTkP7dyvk6Lbpj758heG7kSDbi_5v9A_&edit)
---
@config "My configuration"
C : C := -1

@viewer "My dynamical system"
iterate z -> z^2 + C until z escapes

@viewer "My parameter plane" over C
z := 0
iterate z -> z^2 + C until z escapes

@tool "Orbits" for "My dynamical system"
on refresh:
  erase
on click:
  z := click
  repeat:
    z <- z^2 + C
    draw point at z
    draw line from z to previous z
  until z escapes
```

In the `on click:` block, `click` is the point you clicked. Since the tool has no separate
`on drag:` block, the same block also runs while you drag, with `click` following the pointer, so
the orbit updates as you move around.

## Don't repeat yourself!

Our script has a bit of repetition; in particular, we write `z^2 + C` repeatedly. Let's give
that expression a name to help with readability, and to make it so that if we want to
change which function we are iterating, we only have to change it in one place.
We'll do this by adding a `@common` section to our script. Definitions in a `@common` section
are available to all other parts of the script.

```{code-block} fs
---
caption: [Try this code live](https://matt-noonan.github.io/FractalStream/app/#project=vVJLTsMwEN33FKOsQChR0jRfCQmpa8QZ3HTSWiS2ZbstjVghNiyREDvUOyCxY8VRegKOgBv3R1Uk2LCbN35v5s2MVSGp0Dncuh2Ai4LXNWcmKk-aU1g-PEHz8Qpn0O_YV1bSETiXc7DhRBJNOXPMYx9yWN7ftRo3aOlTijOULX04Z6SmBalAzZXGeqWgGo0coYHl4t02nDBNK5NAVRCB6rCKIJLUaGQgKsLQAT41cd-wmrav__uqmvMKnCs5oFo5UPKfbXIGEkuJapwbBGDKK7T5oqLFtc02kJ9b_I0ERiuQaEtqfS7eWlfrxFCSGQhOmQaiodnPVpQhlJLXRqU5CIlTyidqTTocakqqCapVH3sa27GfQ-D5UeJHQZwkQdYNU3PNwAvDJM78II3CrJdFny-Pz50ZDlwhuUCpqS1kN69spSPb2QxV3OTt6tt4vovNkQrckFxIvAzdcAtjL-6h291i3wuzON2DSTfdQIkj8812pXpenCbRcajGZh9m7I3rg0_zR9O-1wu6yf-Z_gI&edit)
---
@common
f(z) := z^2 + C

@config "My configuration"
C : C := -1

@viewer "My dynamical system"
iterate z -> f(z) until z escapes

@viewer "My parameter plane" over C
z := 0
iterate z -> f(z) until z escapes

@tool "Orbits" for "My dynamical system"
on refresh:
  erase
on click:
  z := click
  repeat:
    z <- f(z)
    draw point at z
    draw line from z to previous z
  until z escapes
```

Now you can easily change your function by editing the script in just one place. Try some
other functions with 0 as a critical point, such as `z^3 + C`, `(z^2 + C) / (z^2 + 1)`,
`cos z + C`, or `e^z - z + C`.
