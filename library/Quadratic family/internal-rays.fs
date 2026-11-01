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
metadata:
  title: Internal rays
  description: |
    Colors the interior of the Mandelbrot set and of Julia sets by
    internal rays: for each point, which ray its orbit converges along as it
    settles onto the attracting cycle. Set the denominator to see rays at
    angles of the form p/q.
  preview-commands: |
    capture viewer "Parameter space" as preview
  preview: |
    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
