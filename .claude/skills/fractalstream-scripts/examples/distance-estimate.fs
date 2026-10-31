script: |
  # Mandelbrot set boundary by distance estimation: iterate z and its
  # derivative dz = d(z_n)/dC together, then estimate the distance to the
  # set as |z| log|z| / |dz|. Comparing against pixel_size keeps the
  # boundary a fixed number of pixels thick at every zoom level.

  @config "Distance estimation"
  thickness : R as "Boundary thickness (pixels)" <- 1.5
  # A large escape radius makes the estimate accurate.
  big_radius : R as "Escape radius" <- 1000

  # escape= makes `escapes` use big_radius instead of escape_radius.
  @viewer "Mandelbrot boundary" var=C center=-0.75 pixelSize=1/200 escape=big_radius
  z := 0
  dz := 0
  # `iterate` updates only one variable, so use a loop body for two.
  until z escapes:
    dz <- 2 z dz + 1
    z <- z^2 + C
  if stuck:
    color <- light grey
  else:
    d := |z| log |z| / |dz|
    color <- if d < thickness pixel_size then black else white
