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
metadata:
  title: A filled Julia set
  description: |
    The filled Julia set of z ↦ z² + C: the starting points whose orbits
    stay bounded. Change C in the configuration window to explore other
    Julia sets, and click or drag with the Trace tool (t) to draw a point's
    orbit.
  preview-commands: |
    select viewer "Julia set"
    set viewer pixel size to 1/200
    capture viewer "Julia set" as preview
  preview: |
    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
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
