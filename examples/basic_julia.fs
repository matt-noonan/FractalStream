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

  @tool "Trace" for="Julia set" shortcut=t
  on click-or-drag:
    prevZ := z
    erase
    until z escapes:
      prevZ <- z
      draw point at z
      z <- f(z)
      draw line from z to prevZ
scenarios:

  - name: simple
    commands: |
      capture viewer "Julia set" to "julia.png"

  - name: circle
    commands: |
      set parameter C to 0
      capture viewer "Julia set" to "circle.png"

  - name: trace
    commands: |
      select viewer "Julia set"
      select tool "Trace"
      click at 0.2 e^(2 pi i / 5)
      capture viewer "Julia set" to "trace.png"
