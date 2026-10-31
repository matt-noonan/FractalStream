script: |
  @config "Configuration"
  C : C as "f(z) = z^2 +" <- -0.11 + 0.78i
  collatz_escape_radius : R as "Escape radius:" <- 100
  p : R as "Brightness: " <- 0.5

  @common
  # a complex extension of the Collatz map, z/2 or 3z + c depending on parity
  let step(z, c) = (z/2) (cos (pi z / 2))^2 + (3z + c) (sin (pi z / 2))^2

  @viewer "Collatz sequence" escape=collatz_escape_radius
  iterate z -> step(z, C) until z = 1 or z = 0 or z = -1 or z = -5 or z = -17 or z escapes
  color <- black
  if z = 1:
    color <- red
  else if z = 0:
    color <- yellow
  else if z = -1:
    color <- green
  else if z = -5:
    color <- blue
  else if z = -17:
    color <- purple
  else if z escapes:
    color <- grey
  if not stuck:
    color <- cycle(iterations/20, light color, dark color)

  @tool "Trace" for="Collatz sequence" shortcut=t
  on click-or-drag:
    prevZ := z
    erase
    until z vanishes:
      prevZ <- z
      draw point at z
      z <- step(z, C)
      draw line from z to prevZ
