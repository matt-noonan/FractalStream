script: |
  @config "Roots"
  a : C as "a =" <- 1
  b : C as "b =" <- e^(2 pi i / 3)
  c : C as "c =" <- e^(-2 pi i / 3)

  @config "Newton's method configuration"
  tol : R as "closeness" <- 0.01
  rad : R as "grab radius (px)" <- 5
  drag_target : Z as "Drag target: " <- 0

  @common
  let f(z) = (z - a) (z - b) (z - c)
  # one step of Newton's method
  let newton(z) = z - f(z) / f'(z)

  @viewer "Roots of {f}" vanish=tol
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
    capture viewer "Roots of {f}" as preview
  preview: |
    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
