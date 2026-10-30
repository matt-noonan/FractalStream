script: |
  @config "Configuration"
  λ : C as "λ = " <- 0.9 e^{2 pi i / 3}
  t : C as "t = " <- 0.7 - 0.6i
  custom_vanishing_radius : R as "Tolerance:" <- 0.00001

  @common
  let f(z) = λ z / (z^2 + t z + 1)

  @viewer "λ-plane" var=λ pixelSize=1/250
  color <- if |λ| < 1 then (if re(λ) * im(λ) > 0 then light grey else white) else black

  @viewer "t-plane" var=t vanish=custom_vanishing_radius
  # follow both critical points, 1 and -1
  z := 1
  iterate z -> f(z) until z vanishes
  crit1_went_to_0 := z vanished
  iters1 := iterations

  z <- -1
  iterate z -> f(z) until z vanishes
  crit2_went_to_0 := z vanished
  iters2 := iterations

  color <- white
  if crit1_went_to_0:
    if crit2_went_to_0:
      color <- purple
    else:
      color <- fire(iters1 / 20)
  if crit2_went_to_0:
    color <- ice(iters2 / 20)

  @viewer "Dynamics" vanish=custom_vanishing_radius
  iterate z -> f(z) until z vanishes
  color <- if stuck then black else rainbow(iterations / 20)

  @tool "Trace" for=Dynamics shortcut=t
  on refresh:
    erase

  on click-or-drag:
    prevZ := z
    erase
    until z vanishes:
      prevZ <- z
      draw point at z
      z <- f(z)
      draw line from z to prevZ
