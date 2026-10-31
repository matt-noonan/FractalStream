script: |
  @common
  let f(z) = z^3 - 1
  # one step of Newton's method
  let newton(z) = z - f(z) / f'(z)

  @config "Newton's method configuration"
  tol : R as "closeness" <- 0.01

  @viewer "Roots of f" vanish=tol center=0 pixelSize=1/128
  lastZ := 0
  while z != lastZ:
    lastZ <- z
    z <- newton(z)
  if z = lastZ:
    color <- rainbow(arg z / (2π))
    color <- cycle(iterations / 20, dark color, light color)
  else:
    color <- black

  @tool "Trace" for="Roots of f" shortcut=t
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
