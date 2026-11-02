script: |
  # A real two-dimensional map, the Hénon map (x, y) -> (1 - a x^2 + y, b x).
  # Viewers always have a complex coordinate, so the viewer splits it into
  # real coordinates x = re(z), y = im(z). Points that stay bounded are
  # black; escaping points are colored by escape time. The tool plots an
  # orbit from a clicked point, using a real-coordinate click binding.

  @config "Hénon map"
  a : R as "a =" <- 1.4
  b : R as "b =" <- 0.3
  orbit_length : Z as "Orbit length" <- 2000

  @viewer "Hénon map" center=0 pixelSize=1/200
  x := re(z)
  y := im(z)
  # `iterate` updates a single variable, so use a loop body for two.
  until x^2 + y^2 > escape_radius^2:
    t := x
    x <- 1 - a x^2 + y
    y <- b t
  color <- if stuck then black else cycle(iterations / 10, dark blue, white)

  @tool "Orbit" for="Hénon map" shortcut=o
  on click at (px, py):
    erase
    use orange for stroke
    use orange for fill
    x := px
    y := py
    while x^2 + y^2 < 100 up to orbit_length times:
      draw point at (x, y)
      t := x
      x <- 1 - a x^2 + y
      y <- b t
  on deactivated:
    erase
