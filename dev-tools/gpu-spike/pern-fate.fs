# The GPU spike's Per_n kernel (dev-tools/gpu-spike/kernels.js, PERN) as a
# FractalStream project, to time the CPU backends on the same work: the
# fate of the critical value of f(z) = (z - x)(z - w)/z^2 over a square of
# x's, y fixed, captured near a marked point or not within 500 steps.
script: |
  @viewer "Fate" var=x center="0.4 + 0.2i" pixelSize="4/600"
  y : C := 0.3 + 0.2i
  w := (x + y - 1) / (x - 1)
  s := x + w
  p := x w
  fy := 1 - s / y + p / y^2
  a := [0, 0, 1, y, fy, x]
  ai := [false, true, false, false, false, false]
  k : List of R := []
  for each j in range(1, 6):
    m := 1.0
    for each l in range(1, 6):
      if l != j:
        d := 0.0
        if ai @ j:
          d <- 1 / sqrt(1 + |a @ l|^2)
        else if ai @ l:
          d <- 1 / sqrt(1 + |a @ j|^2)
        else:
          d <- |a @ j - a @ l| / sqrt((1 + |a @ j|^2) (1 + |a @ l|^2))
        if d < m:
          m <- d
    th := 0.01 m
    k <- append(k, if ai @ j then th^2 else th^2 (1 + |a @ j|^2))
  z := 2 p / s
  zi := false
  t := 0
  cj := 0
  while cj = 0 up to 500 times:
    num : C := 1
    den : C := 1
    if not zi:
      if |z|^2 > 1:
        iz := 1 / z
        num <- 1 - s iz + p iz^2
      else:
        num <- z^2 - s z + p
        den <- z^2
    nn := |num|^2
    dd := |den|^2
    if dd === 0.0:
      zi <- true
    else if dd < nn:
      q := den / num
      if |q|^2 < 10^(-30):
        zi <- true
      else:
        z <- 1 / q
        zi <- false
    else:
      z <- num / den
      zi <- false
    t <- t + 1
    nz := 1 + |z|^2
    for each j in range(1, 6):
      if cj = 0:
        hit := false
        if zi:
          hit <- (ai @ j) or 1 < k @ j
        else if ai @ j:
          hit <- 1 < (k @ j) nz
        else:
          hit <- |z - a @ j|^2 < (k @ j) nz
        if hit:
          cj <- j
  color <- if cj = 0 then black else blend(t / 500, rainbow(cj / 6), white)
metadata:
  title: GPU spike, Per_n fate
