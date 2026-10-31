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
