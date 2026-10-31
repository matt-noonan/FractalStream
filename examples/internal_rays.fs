script: |
  @common
  # the map, its second iterate, and its critical point
  let f(z) = z^2 + c
  let f2(w) = (w^2 + c)^2 + c
  let critical_point(c) = 0
  degree := 2

  # Color a point by the internal ray its orbit lands on (or by its escape
  # time, if it escapes). Uses the run site's `z`, the point's orbit, `w`,
  # running at twice the speed, and `z0`, its starting point.
  define draw_rays(dynamical):
    repeat:
      z <- f(z)
      w <- f2(w)
    until |z-w| vanishes or z escapes

    if stuck:
      color <- black

    else if z escaped:
      color <- if im(z) > 0 then blue else yellow

      s := iterations + 1 - (log (log |z|)) / (log degree)
      color <- blend (cos (s pi)^2, light color, dark color)

    else:
      λ := 1

      repeat:
        z <- f(z)
        λ <- f'(z) * λ
      until |z-w| vanishes

      period := iterations

      counter := 0
      while counter < extra_iterations period:
        w <- f2(w)
        counter <- counter + 1

      z <- z0
      while not (|z-w| vanishes):
        z <- f(z)

      preperiod := iterations

      s := preperiod / period
      θ := if dynamical then arg (z-w) - s arg λ else arg λ
      pθ := mod(θ / (2 pi), 1)

      if continuous:
        color <- rainbow(mod(pθ den, 1))
      else:
        color <- if mod(pθ, 1 / den) > 0.5 / den then red else rgb(0,1,1)

      s_adjusted := s + 1 - log |z-w| / log |λ|
      color <- blend (cos (s_adjusted pi)^2, light color, dark color)

  # Draw the orbit of `z`, once it settles down to a cycle.
  define trace(initial_last):
    w := z
    lastZ := initial_last
    erase

    repeat:
      z <- f(z)
      w <- f2(w)
    until |z-w| vanishes or z escapes

    if |z-w| vanishes:
      z <- lastZ

      while not (|z-w| vanishes):
        lastZ <- z
        z <- f(z)

        draw point at lastZ
        draw line from lastZ to z

    use green for stroke
    draw circle at w with radius minRadius

  @config "Internal Rays"
  continuous : Boolean as "continuous = " <- false
  den : Z as "denominator = " <- 3

  @config "Dynamics"
  c : C as "c = " <- 0
  maxIters : Z as "max iterations = " <- 300
  maxRadius : R as "escape radius = " <- 100.0
  minRadius : R as "vanishing radius = " <- 0.00001
  extra_iterations : Z as "extra iterations = " <- 1

  @config "Orbit Tracker"
  start : C as "orbit start = " <- 0

  @viewer "Parameter space" var=c limit=maxIters escape=maxRadius vanish=minRadius center=-0.5 pixelSize=1/200
  z := critical_point(c)
  w := z
  z0 := z
  run draw_rays(false)

  @viewer "Dynamical plane" limit=maxIters escape=maxRadius vanish=minRadius center=0 pixelSize=1/200
  w := z
  z0 := z
  run draw_rays(true)

  @tool "Trace" for="Dynamical plane" shortcut=t
  on click-or-drag:
    start <- z
    run trace(z)

  on refresh:
    z := start
    run trace(0)
