script: |
  @config "Roots"
  a : C as "a =" := 1
  b : C as "b =" := e^(2 pi i / 3)
  c : C as "c =" := e^(-2 pi i / 3)

  @config "Newton's method configuration"
  tol : R as "closeness" := 0.01
  rad : R as "grab radius (px)" := 5
  drag_target : Z as "Drag target: " := 0

  @common
  f(z) := (z - a) (z - b) (z - c)
  # one step of Newton's method
  newton(z) := z - f(z) / f'(z)

  @viewer "Roots of f" vanish=tol
  lastZ := 0
  repeat:
    lastZ <- z
    z <- newton(z)
  while z != lastZ

  col := yellow
  if z = a:
    col <- red
  else if z = b:
    col <- green
  else if z = c:
    col <- blue
  if z = lastZ:
    color <- blend ((cos (iterations pi / 10))^2, dark col, light col)
  else:
    color <- black

  @tool "Trace" shortcut=t
  on deactivated:
    erase

  on click-or-drag:
    lastZ := 0
    erase
    repeat:
      lastZ <- z
      z <- newton(z)
      draw point at lastZ
      draw line from lastZ to z
    while z != lastZ

    draw point at z

  @tool "Move roots" shortcut=m
  on activated:
    draw point at a
    draw point at b
    draw point at c

  on deactivated:
    erase

  on drag from z0 to z:
    if drag_target = 0:
      if |z0 - a| < rad pixel_size:
        drag_target <- 1
      else if |z0 - b| < rad pixel_size:
        drag_target <- 2
      else if |z0 - c| < rad pixel_size:
        drag_target <- 3
    else if drag_target = 1:
      a <- z
    else if drag_target = 2:
      b <- z
    else if drag_target = 3:
      c <- z
    erase
    draw point at a
    draw point at b
    draw point at c

  on refresh:
    erase
    draw point at a
    draw point at b
    draw point at c

  on drag-finished:
    drag_target <- 0
metadata:
  title: Newton's method with movable roots
  description: |
    Newton's method for a cubic whose three roots you choose, either in
    the configuration window or by dragging them with the Move roots tool
    (m). Watch how the basins of attraction change as the roots move.
  preview-commands: |
    capture viewer "Roots of f" as preview
  preview: |
    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
