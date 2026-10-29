script: |
  @common
  # iterate f until stop is true
  let f(z) = z^2 + C
  let stop(z) = |z| > escape_radius

  @config "Configuration"
  C : C as "C" <- -0.1226 + 0.7449i
  twist_center : C as "Center of twist:" <- -0.267 + 0.478i
  r0 : R as "Inner radius" <- 0.2
  r1 : R as "Outer radius" <- 0.5

  @viewer "Twisted rabbit" center=0 pixelSize=1/128
  until stop(z):
    z <- f(z)
    d := |z - twist_center|
    if d > r0 and d < r1:
      theta := 2 pi (d - r0) / (r1 - r0)
      z <- e^(i theta) (z - twist_center) + twist_center

  if stuck:
    color <- black
  else:
    c1 := if im(z) > 0 then blue else yellow
    c2 := if im(z) > 0 then green else red
    s := iterations + 1 - (log (log (|z|^2 / 2))) / log 2
    s <- cos (s pi / 10)^2
    color <- blend (s, c1, c2)
