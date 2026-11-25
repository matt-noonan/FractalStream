format: 1
script: |
  # A viewer with a `show if:` setting is only shown while its condition
  # holds: a truth value worked out from the configuration's values (and
  # @common's values and one-line functions), again whenever one of them
  # changes. While it's hidden, it isn't drawn at all.
  #
  # Here the Julia set appears once a point of the Mandelbrot set has been
  # picked with the "Pick C" tool (and the "Show the Julia set" box is
  # checked); and while C is in the main cardioid, where z² + C has an
  # attracting fixed point, a third viewer shows how fast each point is
  # drawn to it.

  @config "Parameters"
  C : C := 0
  picked : Boolean as "A point C has been picked" := false
  show_julia : Boolean as "Show the Julia set" := true

  @common
  f(z) := z^2 + C
  # the attracting fixed point, and whether C is in the main cardioid
  # (where that fixed point attracts)
  fixed_point(c) := (1 - sqrt(1 - 4 c)) / 2
  in_main_cardioid(c) := |2 fixed_point(c)| < 1

  @viewer "Mandelbrot set" over C
  setup:
    center: -0.5
    pixel size: 1/200
  z := 0
  iterate z -> f(z) until z escapes

  @viewer "Julia set"
  setup:
    show if: picked and show_julia
  iterate z -> f(z) until z escapes

  @viewer "Attracting fixed point"
  setup:
    show if: picked and in_main_cardioid(C)
  # the basin of the fixed point is the inside of the Julia set: color it
  # by how many steps it takes to get close
  w := fixed_point(C)
  iterate z -> f(z) until |z - w| < 0.001 or z escapes
  color <- if z escaped then black else rainbow(iterations / 30)

  @tool "Pick C" for "Mandelbrot set" with shortcut p
  text "Click the Mandelbrot set to pick C: the Julia set appears, and, while C is in the main cardioid, the basin of its attracting fixed point."
  on click:
    C <- click
    picked <- true

  @tool overlay for "Mandelbrot set"
  on refresh:
    erase
    if picked:
      use white for stroke
      draw circle at C with radius 5 pixels
metadata:
  title: Showing a viewer only some of the time
  description: |
    A viewer with a `show if:` setting is only shown while its condition
    holds. Pick a point of the Mandelbrot set with the "Pick C" tool, and
    its Julia set appears; while C is in the main cardioid (a condition
    written as an @common function), a third viewer shows the basin of the
    attracting fixed point. Hidden viewers aren't drawn at all.
  preview-commands: |
    select viewer "Mandelbrot set"
    capture viewer "Mandelbrot set" as preview
  preview: |
    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
    RK5CYII=
