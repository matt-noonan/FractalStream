format: 1
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
  # captured orbit is recognised by its approach to oo, the image of the pole
  # 0: the orbit is a point of the Riemann sphere (Ĉ), and coming within
  # 1e-20 of oo (in the chart around it) means it was within 1e-10 of 0 just
  # before, which is inside the local basin of the critical cycle for good.
  # The time that takes jumps by whole periods between neighbouring
  # parameters, so this plane does not use capture-time bands: the captured set is one warm colour, decided by a vote over four
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
  #  * Its Select tool (S) picks the parameter by clicking, and the Julia window
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

  # The period 5 family has no attracting fixed point at infinity: its critical
  # cycle runs 0 -> oo -> 1 -> f(1) -> f^2(1) -> 0, so a captured orbit is
  # recognised by its approach to oo, the image of the pole 0: within 1e-20
  # of oo in the chart around it (|affine(0, u)| < 1e-20, so |u| > 1e20,
  # which means |u| < 1e-10 a step before), not by an escape.  The orbit is a
  # point of the Riemann sphere, so it can land on oo itself.  Any orbit that
  # gets that close is caught for good -- it is within the local basin of the
  # critical cycle at once -- but the time it takes to get there varies wildly
  # from pixel to pixel near the edge of a capture region, which is why this
  # plane looks grainier than the other three.
  captured5(u) := |affine(0, u)| < 10⁻²⁰

  # Colour by what the critical orbit u did: if it was thrown into the
  # critical cycle, by how long that took; otherwise it has landed in some
  # other attracting basin, so look for the cycle it is on and colour by its
  # period ("stuck" after that little loop means no short cycle: black, which
  # is the Julia set or a very weakly attracting component).
  define shade5:
    if captured5(u):
      color <- rainbow(iterations / period)
    else if periods:
      w := u
      repeat up to 64 times:
        w <- 1 + (pa w + pb)/w^2
      until w = u
      color <- if stuck then black else blend(0.5, white, rainbow(iterations / 8))
    else:
      color <- black

  # In the parameter plane the Mandelbrot-like set is black, as it is
  # traditionally drawn: black is every parameter whose free critical point is
  # not in the basin of the critical cycle, hyperbolic components of other
  # types included.  (The Julia-set viewer does show those other basins, in
  # pale colours, because there black means the Julia set itself.)
  #
  # The parameter plane of the period 5 family cannot use capture-time bands.
  # There is no attracting infinity: a captured orbit is one that has come
  # within 1e-10 of the pole, and how long that takes jumps by whole periods
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
    u : Ĉ := -2 pb/pa
    iterate u -> 1 + (pa u + pb)/u^2 until captured5(u)
    cap5 <- iterations
    if captured5(u):
      cls5 <- 1
    else:
      w := u
      repeat up to 64 times:
        w <- 1 + (pa w + pb)/w^2
      until w = u
      cls5 <- if stuck then 0 else 2 + iterations

  define per5_paint(c):
    if c = 1:
      color <- rgb(0.95, 0.55, 0.25)
    else if (c > 1) and periods:
      color <- blend(0.5, white, rainbow((c - 2) / 8))
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
        color <- blend(0.6 sh5, rainbow(cr / 5), black)
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
  u : Ĉ := z
  iterate u -> 1 + (pa u + pb)/u^2 until captured5(u)
  run shade5

  @tool overlay for="Parameter plane"
  on refresh:
    erase
    use white for stroke
    draw circle at t5 with radius 3 pixel_size

  @tool "Trace orbit" for="Julia set" shortcut=t
  on click:
    w0 := click
    erase
    use yellow for stroke
    pa := 0.0
    pb := 0.0
    run per5_param(t5)
    w : Ĉ := w0
    repeat up to 200 times:
      draw point at w
      w <- 1 + (pa w + pb)/w^2
      if not (w ≡ ∞ or previous w ≡ ∞):
        draw line from previous w to w
    until captured5(w)
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
    YII=
