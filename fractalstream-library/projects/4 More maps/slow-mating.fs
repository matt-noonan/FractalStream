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
    u := z
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
      px := pixel_size (1 + |z|^2) / 2
      if show_equator and 1 - g < 1.5 px dg:
        color <- red

  @viewer "Slow mating" view=sphere center=1
  run mating

  @viewer "Mandelbrot set" var=c center=-0.6 pixelSize=1/180
  z := 0
  iterate z -> z^2 + c until z escapes

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

  @tool "Pick c1" for="Mandelbrot set" shortcut=1
  on click-or-drag at w:
    c1 <- w

  @tool "Pick c2" for="Mandelbrot set" shortcut=2
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
    happen. Pick c1 (1) and Pick c2 (2) choose the polynomials in the
    Mandelbrot set, where both are always marked.
  preview-commands: |
    capture viewer "Slow mating" base as preview
  preview: |
    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
    /x+ZsUNO9KfCKQAAAABJRU5ErkJggg==
