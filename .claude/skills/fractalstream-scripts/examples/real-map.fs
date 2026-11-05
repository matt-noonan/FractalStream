script: |
  # A real two-dimensional map, the Hénon map (x, y) -> (1 - a x^2 + y, b x).
  # A real viewer (xvar=, yvar=) names its pixel's two real coordinates.
  # Points that stay bounded are black; escaping points are colored by
  # escape time. The tool plots an orbit from a clicked point, using a
  # real-coordinate click binding.

  @config "Hénon map"
  a : R as "a =" := 1.4
  b : R as "b =" := 0.3
  orbit_length : Z as "Orbit length" := 2000

  @viewer "Hénon map" xvar=x yvar=y center=(0, 0) pixelSize=1/200
  # A tuple iterates both coordinates at once, and escapes by its length.
  iterate (x, y) -> (1 - a x^2 + y, b x) until (x, y) escapes
  color <- if stuck then black else cycle(iterations / 10, dark blue, white)

  @tool "Orbit" for="Hénon map" shortcut=o
  on click at (px, py):
    erase
    use orange for stroke
    use orange for fill
    x, y := px, py
    while x^2 + y^2 < 100 up to orbit_length times:
      draw point at (x, y)
      x, y <- 1 - a x^2 + y, b x
  on deactivated:
    erase
