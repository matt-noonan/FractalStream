script: |
  # Newton's method for the polynomial whose roots are listed in the
  # configuration, with basins of attraction colored by root and shaded by
  # convergence speed. Shows lists in a viewer, and a tool that edits the
  # list of roots by clicking.

  @config "Polynomial"
  roots : List of C as "Roots" <- [1, -0.5 + 0.866i, -0.5 - 0.866i]
  tol : R as "Convergence tolerance" <- 0.000001

  @common
  # One Newton step for f(z) = (z - r1)(z - r2)...: since
  # f'/f = sum of 1/(z - rk), the step is z - 1 / (sum of 1/(z - rk)).
  define newton_step:
    s : C <- 0
    for each r in roots:
      s <- s + 1 / (z - r)
    z <- z - 1 / s

  @viewer "Newton basins" center=0 pixelSize=1/150
  converged := false
  repeat:
    old := z
    run newton_step
    converged <- |z - old| < tol
  until converged
  if converged:
    n := iterations
    color <- grey
    k := 0
    for each r in roots:
      k <- k + 1
      if |z - r| < 0.001:
        color <- rainbow(k / length(roots))
    color <- blend(1 / (1 + n / 10), color, dark color)
  else:
    color <- black

  # Click to add a root at the clicked point; double-click to remove the
  # root nearest to it.
  @tool "Edit roots" for="Newton basins" shortcut=r
  on refresh:
    erase
    use white for stroke
    for each r in roots:
      draw circle at r with radius 4 pixel_size
  on click at w:
    roots <- append(roots, w)
  on double-click at w:
    if length(roots) > 1:
      best := roots @ 1
      for each r in roots:
        if |r - w| < |best - w|:
          best <- r
      roots <- remove(roots, r -> r = best)
