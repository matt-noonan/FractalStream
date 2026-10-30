script: |
  @config "Configuration"
  C : C as "f(z) = z^2 +" <- -0.11 + 0.78i
  p : R as "Brightness: " <- 0.5

  @common
  let f(z) = z^2 + C

  @viewer "Julia set"
  iterate z -> f(z) until z escapes
  if stuck:
    color <- black
  else:
    color <- blend ((iterations / max_iters)^p, dark blue, white)

  @tool "Demo" for="Julia set" shortcut=x
  R : R as "R =" <- 1
  theta : R as "θ =" <- pi/4
  on refresh:
    erase
    draw circle at 0 with radius R
    draw line from 0 to R e^{i theta}
    R <- R + 0.001

  on deactivated:
    erase

  @tool "Trace" for="Julia set" shortcut=t
  on click-or-drag:
    prevZ := z
    erase
    while |z| < 10 escape_radius up to 100 times:
      prevZ <- z
      draw point at z
      z <- f(z)
      draw line from z to prevZ
