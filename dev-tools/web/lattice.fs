format: 1
script: |
  # A viewer with a lattice (modulo the lattice τ, the lattice Z + τZ), for
  # dev-tools/web/check-tools.mjs: the parallelogram about 0 red, the tiles
  # beside it blue, the rest grey, its sides black. "Dot" marks a point
  # with a circle of radius 0.05 and a line, in every tile; "Mark" with a
  # marker (its radius in pixels).
  @config "Lattice"
  τ : C := 1.2 i

  @viewer "Torus" modulo the lattice τ
  setup:
    pixel size: 1/150
  (m, n) := cell
  color <- if side_distance < 1.5 pixel_size then black ...
           else if m = 0 and n = 0 then red else if |m| + |n| = 1 then blue else grey

  @tool "Dot" for "Torus" with shortcut d
  on click:
    p := click
    erase
    use rgb(0, 1, 0) for fill
    use rgb(0, 1, 0) for stroke
    draw filled circle at p with radius 0.05
    draw line from p to p + 0.2

  @tool "Mark" for "Torus" with shortcut m
  on click:
    p := click
    erase
    use rgb(0, 1, 0) for fill
    use rgb(0, 1, 0) for stroke
    draw filled circle at p with radius 5 pixels
