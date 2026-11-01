script: |
  @config "Configuration"
  a : C as "a = " <- 0.3
  b : C as "b = " <- 0.6 + 0.2i
  zoom : R as "zoom = " <- 0.001

  @common
  # Iterate the Hénon map (x, y) -> (x^2 + b - a y, x), starting near its
  # saddle fixed point (p, p), displaced along the unstable direction by
  # `start`, until x escapes; then color by the escape time.
  define draw_henon(start, use_rainbow):
    aa := (a + 1) / 2
    p := sqrt(aa^2 - b) + aa
    lambda := sqrt(p^2 - a) + p
    save := start * zoom
    x := p + lambda * save
    y := p + save

    repeat:
      t := x
      x <- x^2 + b - a y
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

  @viewer "b-plane" var=b
  run draw_henon(0, false)

  @viewer "Hénon dynamics"
  run draw_henon(z, true)
metadata:
  title: The complex Hénon map
  description: |
    The Hénon map (x, y) ↦ (x² + b - ay, x), with complex parameters a
    and b. Two parameter slices show, for each a or b, how points near the
    saddle fixed point behave; the third viewer shows the dynamics for the
    chosen parameters.
  preview-commands: |
    capture viewer "Hénon dynamics" as preview
  preview: |
    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
