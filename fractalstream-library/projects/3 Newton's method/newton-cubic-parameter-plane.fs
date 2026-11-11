script: |
  @config "Roots"
  a : C as "Third root a =" := -0.81 + 0.846i

  @common
  # Newton's method for a cubic with roots 1, -1, and a
  f(z) := (z - 1) (z + 1) (z - a)
  newton(z) := z - f(z) / f'(z)
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
    erase
    use white for stroke
    repeat:
      z <- newton(z)
      draw point at previous z
      draw line from previous z to z
    until z stops
    draw point at z
metadata:
  title: Newton's method for a family of cubics
  description: |
    Newton's method for the cubics with roots 1, -1, and a. The parameter
    plane colors each a by where Newton's method takes the point a/3: red
    or green for the roots 1 and -1, purple (blended with the others, the
    farther a is from where it starts) for the root a, and black where it
    never reaches a root at all -- in small copies of the Mandelbrot set,
    where Newton's method fails on whole regions of starting points. Pick a
    (p) in the parameter plane to see its basins in the dynamical plane,
    and Trace (t) a point's Newton steps there. The roots' colors are saved
    and locked; to give a new a's root a color of its own, delete the old
    one in the viewer's Autocolor window (or clear the list), then turn
    Lock off and on again.
  preview-commands: |
    select viewer "Parameter plane"
    set viewer center to -0.81 + 0.846i
    set viewer pixel size to 1/6000
    capture viewer "Parameter plane" as preview
  preview: |
    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
    YII=
autocolor:
  '0':
    tolerance: 1.0e-3
    locked: true
    next-hue: 1.1464777070063694
    entries:
    - point: '-1'
      color: rgb(0.000, 1.000, 0.294)
    - point: -0.81 + 0.846i
      color: rgb(0.584, 0.000, 1.000)
    - point: '1'
      color: rgb(1.000, 0.000, 0.000)
