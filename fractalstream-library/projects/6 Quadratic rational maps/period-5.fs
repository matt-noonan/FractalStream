script: |
  # Quadratic rational maps with a periodic critical point: period 5.
  #
  # A quadratic rational map has two critical points, and one of them can be
  # put on a cycle of period n.  Up to conjugacy such maps form a curve in
  # the two-dimensional moduli space of quadratic rational maps -- Milnor's
  # Per_n(0).  For n <= 4 that curve is rational and one complex number
  # parametrizes it; for n = 5 it is elliptic, so no single-valued parameter
  # exists.  This project draws Per_5(0).
  #
  # Parametrization.  Dynamo's normal form is
  #
  #     f(z) = 1 + (a z + b)/z^2,
  #
  # whose critical cycle is 0 -> oo -> 1 -> f(1) -> f^2(1) -> 0; the free
  # critical point is -2b/a.  The pair (a, b) lies on the elliptic curve E0
  #
  #   2a^5 + 8a^4b + 13a^3b^2 + 11a^2b^3 + 5ab^4 + b^5 + 11a^4 + 35a^3b
  #     + 42a^2b^2 + 23ab^3 + 5b^4 + 21a^3 + 53a^2b + 44ab^2 + 12b^3 + 18a^2
  #     + 33ab + 15b^2 + 7a + 7b + 1 = 0.
  #
  # The plane drawn here is the universal cover of that curve.  E0 is
  # uniformized by C/(2 omega1, 2 omega2) for the lattice of
  #
  #     y^2 = 4x^3 - (11/4)x + 3/8,   e1 = 3/4,  e2,3 = (-3 -+ sqrt 17)/8,
  #
  # with omega1 = 1.5470797535511203, tau = 0.88739417337317839 i, nome
  # q = e^(i pi tau) = 0.0615546, and a constant C = -0.312026392806344 in
  # the theta formula
  #
  #     p(t)  = C - (log theta1)''/(4 omega1^2),
  #     p'(t) =     -(log theta1)'''/(8 omega1^3),   v = t/(2 omega1);
  #
  # four terms of the theta series are enough for this q (they agree with
  # mpmath to about 1e-30 relative).  Weierstrass p and p' then go to (a, b)
  # through Dynamo's chain of rational maps:
  #
  #     x = -p - 1/4,  y = (p' + x + 1)/(2x),  x <- x/(y - x - 1),
  #     y <- x - 1/(y - x - 1),  b = (x+1)y/x,  a = x b - 1.
  #
  # Because the curve is a torus, the picture repeats in the lattice -- under
  # t -> t + 2 omega1 (3.094 real) and t -> t + 2 omega2 (2.746 i) -- so one
  # cell, |Re t| < 1.547 and |Im t| < 1.373, holds the whole parameter space.
  #
  # There is no attracting infinity in this family (oo maps to 1), so a
  # captured orbit is recognised by its approach to the pole: |z| > 1e24
  # means |z| < 1e-12 just before, which is inside the local basin of the
  # critical cycle for good.  The time that takes jumps by whole periods
  # between neighbouring parameters, so this plane does not use capture-time
  # bands: the captured set is one warm colour, decided by a vote over four
  # samples per pixel.  Two boxes in the Coloring tab switch to the classical
  # level-set view instead (V. Ambrosi's rule, with n = 5 because the critical
  # cycle has period 5): gray where the 3 x 3 neighbourhood of escape times
  # meets two or more residues mod 5, white where they all agree, black where
  # the orbit never gets out; "colour solid components" paints each solid
  # escape component with its class colour, shaded by its escape time.
  #
  # What the windows show
  #
  #  * Black is every point whose free critical point does not reach a pole
  #    within the iteration limit: in the parameter plane the Mandelbrot-like
  #    set, in the dynamical plane the filled Julia set.
  #  * Everything else is coloured by how long the escape took, one rainbow
  #    band per "Color period" iterations.
  #  * "Colour components by period" (Coloring tab) paints the components
  #    whose orbit lands on some other attracting cycle in a pale colour of
  #    its period instead of black.  The test looks for periods up to 64;
  #    longer or very weakly attracting cycles stay black.
  #  * Clicking the parameter plane picks the parameter and the Julia window
  #    follows; "Trace orbit" draws the orbit of a point of a Julia set.
  # References
  #
  #  * J. Milnor, "Geometry and dynamics of quadratic rational maps",
  #    Experiment. Math. 2 (1993) 37-83; doi:10.1080/10586458.1993.10504267.
  #    The curves Per_n(0) and the moduli space of quadratic rational maps.
  #  * J. Milnor, "Remarks on quadratic rational maps", arXiv:math/9209221.
  #    Section 9 draws Per_2(0) in the -a^3 plane (Figure 7) and Per_3(0) in
  #    the log c plane (Figure 9), and describes Per_4(0) by the cross-ratio
  #    of its critical cycle.
  #  * Dynamo, Danny Stoll's program, https://github.com/DannyStoll1/dynamo
  #    (GPL-3): the normal forms used here are the ones in its
  #    crates/profiles/src/rational_maps/quad_rat_per_{2,3,4,5}.rs.  Only the
  #    formulas are taken from it, restated in FractalStream's language.
  #  * D. Gage and D. Jackson, "Computer generated images for quadratic
  #    rational maps with a periodic critical point", arXiv:1009.3478: the
  #    convention that the parameters whose free critical point is not
  #    attracted to the critical cycle are black, and a proof that for n = 5
  #    the parametrizing curve has geometric genus 1.

  @config "Parameters"
  t5 : C as "t" := 0.1 + 1.5i

  @config "Coloring"
  period : R as "Color period (iterations)" := 10
  iters : Z as "Iteration limit" := 500
  periods : Boolean as "Colour components by period" := false
  bib5 : Boolean as "Mod-5 level sets" := false
  solid5 : Boolean as "Colour solid components" := false

  @common
  # --- period 5 -------------------------------------------------------------
  # Per_5(0) is the elliptic curve E0 in the (a, b)-plane (from Dynamo's source):
  #   2a^5 + 8a^4b + 13a^3b^2 + 11a^2b^3 + 5ab^4 + b^5 + 11a^4 + 35a^3b
  #     + 42a^2b^2 + 23ab^3 + 5b^4 + 21a^3 + 53a^2b + 44ab^2 + 12b^3 + 18a^2
  #     + 33ab + 15b^2 + 7a + 7b + 1 = 0,
  # whose uniformization is C/(2 omega1, 2 omega2), omega2 = tau omega1, for the
  # lattice of y^2 = 4x^3 - (11/4)x + 3/8 (half-periods e1 = 3/4 and
  # e2,3 = (-3 +- sqrt 17)/8).  The half-period, the nome q = e^(i pi tau) and
  # the constant in the theta formula were computed once to 25 digits; the theta
  # sums keep four terms, which is accurate to about 1e-24 for this q.
  per5_omega1 : R := 1.5470797535511203
  per5_pconst : R := -0.312026392806344
  per5_q : List of R := [0.49809833880241478, 0.0018872778374306454, 0.000000027094262443660302, 0.0000000000000014738034644543212]

  # Weierstrass p and p' at t -- p = pconst - d2(log theta1)/(4 omega1^2) and
  # p' = -d3(log theta1)/(8 omega1^3) -- then Dynamo's rational chain from
  # (p, p') to the family's (a, b).  Assigns the caller's pa and pb.
  define per5_param(t):
    v := t/(2 per5_omega1)
    s1 := 0.0
    s2 := 0.0
    s3 := 0.0
    s4 := 0.0
    n := 0
    while true up to 4 times:
      ang := (2 n + 1) pi v
      sign := if mod(n, 2) === 0 then 1.0 else -1.0
      coef := sign (per5_q @ (n + 1))
      s1 <- s1 + coef sin ang
      s2 <- s2 + coef (2 n + 1) pi cos ang
      s3 <- s3 - coef ((2 n + 1) pi)^2 sin ang
      s4 <- s4 - coef ((2 n + 1) pi)^3 cos ang
      n <- n + 1
    l2 := s3/s1 - (s2/s1)^2
    l3 := s4/s1 - 3 (s3 s2)/s1^2 + 2 (s2/s1)^3
    wp := per5_pconst - l2/(4 per5_omega1^2)
    wdp := -l3/(8 per5_omega1^3)
    wx := -wp - 0.25
    wy := wdp + wx + 1
    wy <- wy/(2 wx)
    tmp := wy - wx - 1
    wx <- wx/tmp
    wy <- wx - 1/tmp
    tmp <- (wx + 1) wy
    pb <- wx/tmp
    pa <- wx pb - 1

  # Colour by what the critical orbit did: if it was thrown into the critical
  # cycle, by how long that took; otherwise it has landed in some other
  # attracting basin, so look for the cycle it is on and colour by its period
  # ("stuck" after that little loop means no short cycle: black, which is the
  # Julia set or a very weakly attracting component).
  # In the parameter planes the Mandelbrot-like set is black, as it is
  # traditionally drawn: black is every parameter whose free critical point is
  # not in the basin of the critical cycle, hyperbolic components of other
  # types included.  (The Julia-set viewers below do show those other basins,
  # in pale colours, because there black means the Julia set itself.)
  define shade2p:
    if z escaped:
      color <- rainbow(iterations / period)
    else if periods:
      w := z
      repeat up to 64 times:
        w <- f2(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)
    else:
      color <- black

  define shade3p:
    if z escaped:
      color <- rainbow(iterations / period)
    else if periods:
      w := z
      repeat up to 64 times:
        w <- f3(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)
    else:
      color <- black

  define shade4p:
    if z escaped:
      color <- rainbow(iterations / period)
    else if periods:
      w := z
      repeat up to 64 times:
        w <- f4(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)
    else:
      color <- black

  define shade2:
    if z escaped:
      color <- rainbow(iterations / period)
    else:
      w := z
      repeat up to 64 times:
        w <- f2(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)

  define shade3:
    if z escaped:
      color <- rainbow(iterations / period)
    else:
      w := z
      repeat up to 64 times:
        w <- f3(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)

  define shade4:
    if z escaped:
      color <- rainbow(iterations / period)
    else:
      w := z
      repeat up to 64 times:
        w <- f4(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)

  # The period 5 family has no attracting fixed point at infinity: its critical
  # cycle runs 0 -> oo -> 1 -> f(1) -> f^2(1) -> 0, so a captured orbit is
  # recognised by its approach to the pole 0 (|z| > 1e24 means |z| < 1e-12
  # before the pole), not by an escape.  Any orbit that gets that close is
  # caught for good -- it is within the local basin of the critical cycle at
  # once -- but the time it takes to get there varies wildly from pixel to
  # pixel near the edge of a capture region, which is why this plane looks
  # grainier than the other three.
  define shade5:
    if |z| > 1000000000000000000000000.0:
      color <- rainbow(iterations / period)
    else if periods:
      w := z
      repeat up to 64 times:
        w <- 1 + (pa w + pb)/w^2
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)
    else:
      color <- black

  # The parameter plane of the period 5 family cannot use capture-time bands.
  # There is no attracting infinity: a captured orbit is one that has come
  # within 1e-12 of the pole, and how long that takes jumps by whole periods
  # between neighbouring parameters near the edge of a capture region, so a
  # ten-iteration rainbow turns the set into confetti.  Two ways out, both
  # found in Mandel.jl (V. Ambrosi) and in Ambrosi's note on the crisp
  # black-and-white pictures:
  #
  #  * by default each class gets one colour and the colour is decided by a
  #    vote over four samples per pixel, which keeps the boundary of the
  #    captured set crisp (averaging colours there gives mud);
  #  * `bib5` switches to the level-set view: gray where the 3 x 3
  #    neighbourhood of escape times meets two or more residues mod 5, white
  #    where they all agree, black where the orbit never gets out.  That is
  #    how these curves are classically drawn; here the escape time is the time
  #    to fall into the critical cycle, which is chaotic in places, so much of
  #    the plane comes out gray.
  #
  # One sample: sets cls5 (1 = the free critical point is captured by the
  # critical cycle, 0 = it is not) and cap5 (its iteration count, meaningful
  # when cls5 = 1).
  define per5_sample(ts):
    pa := 0.0
    pb := 0.0
    run per5_param(ts)
    z := -2 pb/pa
    iterate z -> 1 + (pa z + pb)/z^2 until |z| > 1000000000000000000000000.0
    cap5 <- iterations
    if |z| > 1000000000000000000000000.0:
      cls5 <- 1
    else:
      w := z
      repeat up to 64 times:
        w <- 1 + (pa w + pb)/w^2
      until w = z
      cls5 <- if stuck then 0 else 2 + iterations

  define per5_paint(c):
    if c = 1:
      color <- rgb(0.95, 0.55, 0.25)
    else if (c > 1) and periods:
      color <- blend(0.5, rainbow((c - 2) / 8), white)
    else:
      color <- black

  @viewer "Parameter plane" var=t5 center=0 pixelSize=1/120 limit=iters
  cls5 : Z := 0
  cap5 : Z := 0
  if bib5 or solid5:
    # Ambrosi's rule, with n = 5 for Per_5: the escape time is taken mod the
    # period of the critical cycle, not mod 3, so the classes follow the
    # cycles of approach rather than an arbitrary residue.
    run per5_sample(t5)
    cc := cls5
    ccap := cap5
    cr := mod(cap5, 5)
    mixed := false
    for each o in [-1 - i, -1, -1 + i, -i, i, 1 - i, 1, 1 + i]:
      run per5_sample(t5 + pixel_size o)
      if cls5 = 1:
        if mod(cap5, 5) != cr:
          mixed <- true
    if cc = 1:
      if mixed:
        color <- grey
      else if solid5:
        # "Less solid": shade the class colour by how long the escape took,
        # over a log scale, so the level sets inside a component show.
        sh5 := log(ccap / 10) / log(20)
        if sh5 < 0.0:
          sh5 <- 0.0
        if sh5 > 1.0:
          sh5 <- 1.0
        color <- blend(0.6 sh5, black, rainbow(cr / 5))
      else:
        color <- white
    else:
      run per5_paint(cc)
  else:
    d5 := pixel_size / 4
    run per5_sample(t5 + d5 + d5 i)
    k1 := cls5
    run per5_sample(t5 - d5 + d5 i)
    k2 := cls5
    run per5_sample(t5 - d5 - d5 i)
    k3 := cls5
    run per5_sample(t5 + d5 - d5 i)
    k4 := cls5
    nc := 0
    if k1 = 1:
      nc <- nc + 1
    if k2 = 1:
      nc <- nc + 1
    if k3 = 1:
      nc <- nc + 1
    if k4 = 1:
      nc <- nc + 1
    pick := 0
    if nc >= 2:
      pick <- 1
    else if k1 > 1:
      pick <- k1
    else if k2 > 1:
      pick <- k2
    else if k3 > 1:
      pick <- k3
    else if k4 > 1:
      pick <- k4
    run per5_paint(pick)

  @viewer "Julia set" center=0 pixelSize=1/150 limit=iters
  pa := 0.0
  pb := 0.0
  run per5_param(t5)
  iterate z -> 1 + (pa z + pb)/z^2 until |z| > 1000000000000000000000000.0
  run shade5

  @tool "Pick t (5)" for="Parameter plane" shortcut=p
  on click-or-drag:
    pass

  @tool overlay for="Parameter plane"
  on refresh:
    erase
    use white for stroke
    draw circle at t5 with radius 3 pixel_size

  @tool "Trace orbit" for="Julia set" shortcut=t
  on click at w:
    erase
    use yellow for stroke
    pa := 0.0
    pb := 0.0
    run per5_param(t5)
    prev := w
    repeat up to 200 times:
      draw point at w
      prev <- w
      w <- 1 + (pa w + pb)/w^2
      draw line from prev to w
    until |w| > 1000000000000000000000000.0
  on deactivated:
    erase
metadata:
  title: 'Period 5: parameter plane (elliptic curve) and Julia set'
  description: |
    Period 5 case of the curves Per_n(0): quadratic rational maps one of
    whose critical points is periodic of period 5.  Per_5(0) is an elliptic
    curve, so no single-valued parameter exists: the parameter plane here is
    the universal cover of that torus, in a uniformizing coordinate t, and
    the picture repeats in a lattice.  The second window is the Julia set of
    the current parameter.  Two boxes in the Coloring tab switch the plane to
    the classical mod-5 level-set views.
  preview-commands: |
    capture viewer "Parameter plane" as preview
  preview: |
    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
