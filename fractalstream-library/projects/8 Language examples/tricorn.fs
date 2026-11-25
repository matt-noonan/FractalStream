format: 1
script: |
  # The tricorn is the Mandelbrot set of z ↦ z̄² + c, which isn't
  # holomorphic: its iterates aren't holomorphic functions of c (or of the
  # starting point), so their derivative isn't one complex number but a
  # real 2×2 matrix. In FractalStream, `diff(w, c)` is the Wirtinger
  # derivative ∂w/∂c and `diff_bar(w, c)` is ∂w/∂c̄, and together they're
  # the whole derivative. A distance estimate needs how fast log |w| grows,
  # the length of its gradient, which is |w̄ ∂w + w (∂̄w)‾| / |w|²; so
  #
  #     distance ≈ |w|² log |w| / |w̄ ∂w + w (∂̄w)‾|,
  #
  # which, for a holomorphic map (∂̄w = 0), is the usual |w| log |w| / |w'|.
  #
  # Both viewers pass their distance to a colorizer this project defines,
  # "Boundary" (a `@colorizer` stanza), which draws everything closer than
  # a line width as the boundary, and the rest by escape time. Click the
  # parameter plane with its Select tool to pick c for the Julia set.

  @config "Parameters"
  c : C as "f(z) = z̄² +" := -0.2 + 0.4i

  @colorizer "Boundary"
  setup:
    description: "The boundary in one color, found by a distance estimate passed as its argument; the rest in bands by escape time."
  width : R as "Line width (pixels)" := 1.5
  line : Color as "Boundary color" := white
  inside : Color as "Inside color" := dark blue
  define colorize(distance):
    if stuck:
      color <- inside
    else if distance < width pixel_size:
      color <- line
    else:
      color <- cycle(iterations / 20, grey, dark grey)

  @viewer "Tricorn" over c
  setup:
    center: -0.3
    pixel size: 1/200
    colorizer: Boundary(distance)
  z := 0
  iterate z -> conj(z)^2 + c until z escapes
  distance := 0.0
  if z escaped:
    # the length of log |z|'s gradient, from both derivatives
    d := diff(z, c)
    dbar := diff_bar(z, c)
    gradient := |conj(z) * d + z * conj(dbar)| / |z|^2
    distance <- log |z| / gradient

  @viewer "Julia set"
  setup:
    pixel size: 1/200
    colorizer: Boundary(distance)
  w := z
  iterate w -> conj(w)^2 + c until w escapes
  distance := 0.0
  if w escaped:
    # the length of log |w|'s gradient, from both derivatives
    d := diff(w, z)
    dbar := diff_bar(w, z)
    gradient := |conj(w) * d + w * conj(dbar)| / |w|^2
    distance <- log |w| / gradient
metadata:
  title: The tricorn, and derivatives of maps that aren't holomorphic
  description: |
    The tricorn is the Mandelbrot set of z ↦ z̄² + c. Its iterates aren't
    holomorphic, so a distance estimate for its boundary needs both of
    their Wirtinger derivatives, `diff` (∂) and `diff_bar` (∂̄). Both
    viewers pass the distance to "Boundary", a colorizer this project
    defines in a `@colorizer` stanza, which draws the boundary as a line of
    constant width.
  preview-commands: |
    select viewer "Tricorn"
    capture viewer "Tricorn" as preview
  preview: |
    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
    TkSuQmCC
