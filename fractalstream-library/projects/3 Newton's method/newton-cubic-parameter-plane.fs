script: |
  @config "Roots"
  a : C as "Third root a =" <- -0.81 + 0.846i

  @common
  # Newton's method for a cubic with roots 1, -1, and a
  let f(z) = (z - 1) (z + 1) (z - a)
  let newton(z) = z - f(z) / f'(z)
  define converge:
    lastZ := 0
    repeat:
      lastZ <- z
      z <- newton(z)
    until z = lastZ

  # Each a, colored by where Newton's method takes its free critical point
  # (the inflection point of f, a/3). This viewer only looks colors up: it
  # mustn't add one for each pixel's root a, which is different everywhere.
  @viewer "Parameter plane" var=a colorizer=Autocolor center=0 pixelSize=1/150
  coloring:
    allow_new <- false
  z := a / 3
  run converge

  @viewer "Dynamical plane" colorizer=Autocolor center=0 pixelSize=1/150
  run converge

  @tool "Pick a" for="Parameter plane" shortcut=p
  on click-or-drag:
    pass

  @tool "Trace" for="Dynamical plane" shortcut=t
  on deactivated:
    erase
  on click-or-drag:
    lastZ := 0
    erase
    use white for stroke
    repeat:
      lastZ <- z
      z <- newton(z)
      draw point at lastZ
      draw line from lastZ to z
    until z = lastZ
    draw point at z
metadata:
  title: Newton's method for a family of cubics
  description: |
    Newton's method for the cubics with roots 1, -1, and a. The parameter
    plane colors each a by where Newton's method takes the point a/3: red
    or green for the roots 1 and -1, grey for the root a, and black where it
    never reaches a root at all -- in small copies of the Mandelbrot set,
    where Newton's method fails on whole regions of starting points. Pick a
    (p) in the parameter plane to see its basins in the dynamical plane,
    and Trace (t) a point's Newton steps there. Both planes share the same
    colors for 1 and -1 (the Autocolor colorizer); the parameter plane
    doesn't add new ones.
  preview-commands: |
    select viewer "Parameter plane"
    set viewer center to -0.81 + 0.846i
    set viewer pixel size to 1/6000
    capture viewer "Parameter plane" as preview
  preview: |
    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
autocolor:
  '0':
    tolerance: 1.0e-3
    locked: false
    next-hue: 0.7643184713375796
    entries:
    - point: '-1'
      color: rgb(0.000, 1.000, 0.294)
    - point: '1'
      color: rgb(1.000, 0.000, 0.000)
