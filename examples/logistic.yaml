script: |
  @config "Configuration"
  outer : R as "outer tolerance: " <- 0.01
  warmup : Z as "Warmup iterations:" <- 1000

  @viewer "Bifurcation diagram for the logistic map"
  x := im(z)
  x0 := x
  r := re(z)
  best := outer

  iterate x -> r x (1 - x) while true up to warmup times

  until x = x0:
    x <- r x (1 - x)
    if |x - x0| < best:
      best <- |x - x0|
  color <- blend(best / outer, white, black)
