script: |
  @config "Configuration"
  a : C as "a = " <- 0.3
  c : C as "c = " <- 0.6 + 0.2i
  zoom : R as "zoom = " <- 0.001

  @common
  # Iterate the Hénon map (x, y) -> (x^2 + c - a y, x), starting near its
  # saddle fixed point (p, p), displaced along the unstable direction by
  # `start`, until x escapes; then color by the escape time.
  define draw_henon(start, use_rainbow):
    aa := (a + 1) / 2
    p := sqrt(aa^2 - c) + aa
    lambda := sqrt(p^2 - a) + p
    save := start * zoom
    x := p + lambda * save
    y := p + save

    repeat:
      t := x
      x <- x^2 + c - a y
      y <- t
    until x escapes

    if not (x escaped):
      color <- black
    else if use_rainbow:
      color <- rainbow (iterations / 10)
    else:
      color <- cycle (iterations / 10, blue, white)

  @viewer "a-plane" var=a
  run draw_henon(0, false)

  @viewer "c-plane" var=c
  run draw_henon(0, false)

  @viewer "Hénon dynamics"
  run draw_henon(z, true)

  @tool "Select saddle" for="Hénon dynamics" shortcut=s
  on click-or-drag:
    pass
