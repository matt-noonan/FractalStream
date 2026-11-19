format: 1
script: |
  @config "Slow mating"
  c1 : C as "First polynomial z² + c1, c1 =" := -0.122561 + 0.744862i
  c2 : C as "Second polynomial w² + c2, c2 =" := -1
  t : R as "Mating time t =" := 0.5
  text "Each Julia set is cut along its equipotential of level t, and the two are glued there, as the equator of a sphere. As t goes to 0 this approaches the mating."
  row:
    color1 : Color as "First Julia set" := rgb(1, 0.8, 0.3)
    color2 : Color as "Second Julia set" := rgb(0.15, 0.3, 0.75)
  show_bands : Boolean as "Shade bands between preimages of the equator" := true
  show_equator : Boolean as "Draw the equator" := true

  @common
  # The slow mating of P1(z) = z² + c1 and P2(w) = w² + c2 (Milnor's
  # "Pasting together Julia sets"; Chéritat's slow mating movies).
  #
  # For R = e^s > 1, the sphere S_s is the filled Julia set K1 out to its
  # equipotential of level 2s, glued to K2 out to the same level, by
  # φ1(z) φ2(w) = R² (φ the Böttcher coordinates). Its equator is the
  # level-s equipotential of each side. P1 and P2 together are a degree-2
  # map S_s → S_2s. Uniformize each S_s by the Riemann sphere, with the
  # critical point 0 of P1 at 0 and that of P2 at ∞. In these coordinates
  # the map S_s → S_2s is a rational map
  #     Q_s(u) = (u² + A) / (B u² + 1),
  # where A is the point c1 of S_2s, and 1/B the point c2. For large s,
  # u ≈ z / R on side 1 and u ≈ R / w on side 2, which is where we start:
  # at a level 2^L t ≥ 30, every point of the postcritical orbits is
  # where that approximation puts it. Each level's points are then pulled
  # back to the level below through that level's map, taking the square
  # root nearest the same approximation (this agrees with following the
  # roots continuously as t decreases from ∞).
  #
  # A pixel u of S_t is pushed forward through the maps Q_t, Q_2t, ...,
  # to the top level, where it is a point of one of the two planes; there
  # P1 or P2 tells whether it lies in its filled Julia set, and if not, its
  # potential (on the overlap G1 + G2 = 2t, and the equator is G1 = G2 = t).
  define mating:
    tt := if t < 0.0001 then 0.0001 else t
    # levels 0 .. L, the level-k sphere being S at 2^k tt
    L := 1
    top := 2 tt
    while top < 30 up to 64 times:
      L <- L + 1
      top <- 2 top
    # the critical orbits c1, P1(c1), ... and c2, P2(c2), ...
    xs := [c1]
    ys := [c2]
    x := c1
    y := c2
    for each n in range(0, L):  # L + 2 points: the top level needs them all
      x <- x^2 + c1
      y <- y^2 + c2
      xs <- append(xs, x)
      ys <- append(ys, y)
    # points of the orbits on the top sphere: u ≈ z/R, 1/u ≈ w/R
    a := transform(xs, p -> p exp(-top))
    b := transform(ys, p -> p exp(-top))
    As : List of C := []
    Bs : List of C := []
    s := top
    k := L - 1
    while k >= 1 up to 64 times:
      s <- s / 2
      A := a @ 1
      B := b @ 1
      na : List of C := []
      nb : List of C := []
      for each j in range(1, k + 2):
        v := a @ (j + 1)
        r := sqrt((v - A) / (1 - B v))
        g := (xs @ j) exp(-s)
        if |r - g| > |r + g|:
          r <- -r
        na <- append(na, r)
        v <- b @ (j + 1)
        r <- sqrt((v - B) / (1 - A v))
        g <- (ys @ j) exp(-s)
        if |r - g| > |r + g|:
          r <- -r
        nb <- append(nb, r)
      As <- prepend(As, A)
      Bs <- prepend(Bs, B)
      a <- na
      b <- nb
      k <- k - 1
    As <- prepend(As, a @ 1)
    Bs <- prepend(Bs, b @ 1)
    # push the pixel up to the top level
    # (and its derivative, for drawing the equator at a constant width)
    # (z is a point of the sphere; u is the same point as a complex number)
    u := affine(1, z)
    du : C := 1
    for each n in range(1, L):
      u2 := u^2
      A := As @ n
      B := Bs @ n
      du <- 2 u (1 - A B) du / (B u2 + 1)^2
      u <- (u2 + A) / (B u2 + 1)
    # then follow it in its own plane
    first := |u| < 1
    p := if first then u exp(top) else exp(top) / u
    dp := if first then du exp(top) else -du * exp(top) / u^2
    cc := if first then c1 else c2
    until |p| > 1000:
      dp <- 2 p dp
      p <- p^2 + cc
    inside := stuck
    # the potential, as a fraction of the equator's: in (0, 1) on either side
    g := log(|p|) / exp((L + iterations) log 2) / tt
    # bands between the preimages of the equator, at levels t/2, t/4, ...
    band := floor(-log(g) / log 2)
    if inside:
      color <- if first then color1 else color2
    else:
      side := if first then color1 else color2
      color <- blend(g, blend(0.3, side, white), dark side)
      if show_bands and mod(band, 2) = 1:
        color <- blend(0.85, color, black)
      # |grad g|, and the size of a pixel in the plane at z
      dg := |dp| / (tt |p| exp((L + iterations) log 2))
      px := pixel_size (1 + |affine(1, z)|^2) / 2
      if show_equator and 1 - g < 1.5 px dg:
        color <- red

  @viewer "Slow mating" view=sphere center=1
  run mating

  # Its coordinate is c1, so its built-in "Select c1" tool (shortcut S)
  # picks c1 by clicking; "Select c2" below picks c2.
  @viewer "Mandelbrot set" var=c1 center=-0.6 pixelSize=1/180
  z := 0
  iterate z -> z^2 + c1 until z escapes

  # c1 and c2, always marked
  @tool overlay for="Mandelbrot set"
  on refresh:
    erase
    use black for stroke
    use color1 for fill
    draw filled circle at c1 with radius 5 pixel_size
    draw circle at c1 with radius 5 pixel_size
    use color2 for fill
    draw filled circle at c2 with radius 5 pixel_size
    draw circle at c2 with radius 5 pixel_size

  @tool "Select c2" for="Mandelbrot set" shortcut=2
  on click-or-drag at w:
    c2 <- w
metadata:
  title: Slow mating
  description: |
    The slow mating of two quadratic polynomials z² + c1 and w² + c2, on
    the Riemann sphere. Each filled Julia set is cut along its
    equipotential of level t and the two are glued there, along the
    sphere's equator (drawn in red); the result is shown in coordinates
    that make it a round sphere. As t decreases to 0 the picture
    approaches the mating of the two polynomials. Change t to watch it
    happen. Select c1 (S) and Select c2 (2) choose the polynomials in the
    Mandelbrot set, where both are always marked.
  preview-commands: |
    capture viewer "Slow mating" base as preview
  preview: |
    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
    AABJRU5ErkJggg==
