script: |
  # Parameter plane (Mandelbrot set) and dynamical plane (Julia set) for
  # z -> z^2 + C, with smooth escape-time coloring, a tool that picks C by
  # clicking the parameter plane, and an orbit-tracing tool.

  @config "Parameters"
  C : C as "C =" <- -0.12 + 0.75i

  @config "Coloring"
  period : R as "Color period (iterations)" <- 20
  inside : Color as "Interior color" <- black

  @common
  let f(z) = z^2 + C

  # Shared coloring: run right after an escape-time loop on z.
  define shade:
    if z escaped:
      # continuous ("smooth") iteration count
      m := iterations + 1 - log (log (|z|^2) / (2 log escape_radius)) / log 2
      color <- blend (cos (pi m / period)^2, dark blue, light yellow)
    else:
      color <- inside

  @viewer "Parameter plane" var=C center=-0.5 pixelSize=1/150
  z := 0
  iterate z -> f(z) until z escapes
  run shade

  @viewer "Julia set" center=0 pixelSize=1/150
  iterate z -> f(z) until z escapes
  run shade

  # On a viewer whose coordinate is a @config variable (here C), a handler
  # that uses the default coordinate writes the clicked point into that
  # variable -- exactly what a "pick" tool wants.
  @tool "Pick C" for="Parameter plane" shortcut=p
  on click-or-drag:
    pass

  @tool "Trace orbit" for="Julia set" shortcut=t
  on click-or-drag at w:
    erase
    use yellow for stroke
    use yellow for fill
    prev := w
    repeat up to 200 times:
      draw point at w
      prev <- w
      w <- f(w)
      draw line from prev to w
    until w escapes
  on deactivated:
    erase
