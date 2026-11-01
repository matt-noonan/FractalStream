script: |
  @config "Newton's method configuration"
  a : C as "a =" <- 0
  tol : R as "closeness" <- 0.01

  @common
  # Newton's method for the cubic with roots r1, r2, and r3
  r1 := 1
  r2 := -0.5 + a
  r3 := -0.5 - a
  let p(z) = (z - r1) (z - r2) (z - r3)
  let newton(z) = z - p(z) / p'(z)

  # color by which root Newton's method converged to (if any)
  define shade:
    c := grey
    if z = r1:
      c <- red
    else if z = r2:
      c <- green
    else if z = r3:
      c <- blue

    s := (cos (iterations pi / 10))^2
    color <- if z = lastZ then blend (s, dark c, light c) else black

  @viewer "Parameter plane" var=a vanish=tol
  z := 0
  lastZ := 0
  repeat:
    lastZ <- z
    z <- newton(z)
  while z != lastZ
  run shade

  @viewer "Newton's method dynamics" vanish=tol
  lastZ := 0
  repeat:
    lastZ <- z
    z <- newton(z)
  while z != lastZ
  run shade

  @tool "Trace" for="Newton's method dynamics" shortcut=t
  on deactivated:
    erase

  on click-or-drag:
    lastZ := 0
    erase
    while z != lastZ:
      lastZ <- z
      z <- newton(z)
      draw point at lastZ
      draw line from lastZ to z
    draw point at z
metadata:
  title: A family of Newton's methods
  description: |
    Newton's method for the family of cubics with roots 1 and -1/2 ± a.
    The parameter plane colors each a by where the free critical point goes,
    and copies of the Mandelbrot set appear wherever it fails to converge to
    a root; the dynamical plane shows the basins for the selected a.
  preview-commands: |
    select viewer "Parameter plane"
    set viewer center to 0.9635873791825618 + 0.8883637076283188i
    set viewer pixel size to 0.0003861942835375797
    capture viewer "Parameter plane" as preview
  preview: |
    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
    AAAAAElFTkSuQmCC
wx-properties:
  viewers:
    Newton's method dynamics:
      x: 705
      'y': 115
      width: 600
      height: 600
    Parameter plane:
      x: 100
      'y': 100
      width: 600
      height: 600
  configuration:
    x: 278
    'y': 191
