script: |
  # Böttcher coordinates for z -> z^2 + C.
  #
  # Outside the filled Julia set K_C, the Böttcher map
  #     phi(z) = lim (z_n)^(1/2^n)
  # conjugates z^2 + C to w -> w^2 near infinity. When C is in the
  # Mandelbrot set, phi is a conformal isomorphism from the whole basin of
  # infinity onto the outside of the unit disk; we draw the pullback of a
  # pattern on log(phi) = G + 2 pi i t (G the Green's function, t the
  # external angle).
  #
  # How phi is computed. Writing r_k = phi(z_k) / z_k, we have
  #     r_k^2 = r_(k+1) (1 + C / z_k^2)
  # and r_k always has positive real part, so r_k is the *principal* square
  # root of the right side. Far out (|z| > max(2, 2 sqrt|C|)) the
  # principal-branch product formula is exact, so we run forward to that
  # radius, sum the tail of the product there, and then unwind the nested
  # square roots backward along the orbit (recomputing each z_k, so no
  # lists are needed). This is exact wherever phi is defined: on the whole
  # basin if C is in M, and on {G(z) > G(0)} if not.
  #
  # When C is not in M, phi only extends down to the critical level
  # G = G(0); below it the same formula still produces a (discontinuous)
  # coordinate, drawn dimmed if "Dim where phi is undefined" is checked.
  #
  # The parameter plane uses the same code at z = C: phi_C(C) is the
  # Douady-Hubbard map Phi_M(C) uniformizing the outside of M.

  @config "Parameters"
  C : C as "f(z) = z² +" := -0.12 + 0.75i

  @config "Coloring"
  style : Z as "Pattern" choice { "Checkerboard", "Rainbow tiles", "Grid lines", "Angle and potential" } := 1
  detail : Z as "Angular detail (tiles per level = 2^(level + detail))" := 3
  row:
    color1 : Color as "Tile colors" := rgb(0.95, 0.9, 0.75)
    color2 : Color as "" := rgb(0.15, 0.25, 0.45)
  line_width : R as "Grid line width (pixels)" := 1.0
  inside : Color as "Filled Julia set" := black
  dim_invalid : Boolean as "Dim where phi is undefined (C outside M)" := true

  @common
  f(z) := z^2 + C

  # Compute phi at `start` (and |grad G| there). `dc` is 0 for the
  # dynamical plane (derivatives in z, starting at d0 = 1) and 1 for the
  # parameter plane (derivatives in C).
  # Sets the caller's phi, grad and found.
  define boettcher(start, d0, dc):
    b_w := start
    b_dw : C := d0
    b_scale := 1.0
    b_safe := if |C| > 1 then 2 sqrt |C| else 2
    b_m := 0
    while |b_w| <= b_safe:
      b_dw <- 2 b_w b_dw + dc
      b_w <- b_w^2 + C
      b_scale <- b_scale / 2
      b_m <- b_m + 1
    found <- |b_w| > b_safe
    if found:
      # tail of the product: log r_m = sum 2^-(k-m+1) log(1 + C/z_k^2)
      b_L : C := 0
      b_s := 0.5
      while |b_w| < 10^8 up to 60 times:
        b_L <- b_L + b_s log(1 + C / b_w^2)
        b_dw <- 2 b_w b_dw + dc
        b_w <- b_w^2 + C
        b_scale <- b_scale / 2
        b_s <- b_s / 2
      b_r := exp b_L
      # unwind r_k = sqrt(r_(k+1) (1 + C / z_k^2)) for k = m-1, ..., 0
      b_k := b_m - 1
      while b_k >= 0 up to b_m times:
        b_u := start
        while true up to b_k times:
          b_u <- b_u^2 + C
        b_r <- sqrt(b_r (1 + C / b_u^2))
        b_k <- b_k - 1
      phi <- start b_r
      grad <- b_scale |b_dw| / |b_w|

  # Color a point from phi = phi(z) and grad = |grad G|.
  define paint(valid):
    t := arg phi / (2 pi)
    if t < 0:
      t <- t + 1
    G := log |phi|
    u := log G / log 2               # G = 2^u
    lvl := floor(-u)                 # G in (2^-(lvl+1), 2^-lvl]
    nk := 2^(if lvl + detail > 0 then lvl + detail else 0)
    j := floor(t nk)
    odd := mod(lvl + j, 2) = 1
    if style = 0:
      color <- if odd then color1 else color2
    else if style = 1:
      hue := rainbow((j + 0.5) / nk)
      color <- if odd then light hue else dark hue
    else if style = 2:
      dG := |G - 2^(round u)|        # distance to the nearest level 2^-k
      a := t nk
      dt := |a - round a| / nk       # distance to the nearest ray j / nk
      d := (if dG < 2 pi dt then dG else 2 pi dt) / grad
      bg := blend(0.25, rainbow(t), white)
      color <- if d < line_width pixel_size then black else bg
    else:
      band := u - floor u
      color <- blend(0.35 + 0.65 band, rainbow(t), black)
    if dim_invalid and not valid:
      color <- blend(0.35, color, grey)

  @viewer "Dynamical plane" center=0 pixelSize=1/150
  # The critical level G(0) = G(C) / 2; zero if C doesn't escape (C is in
  # M, as far as max_iters can tell). This doesn't depend on the pixel, so
  # it runs once per render.
  v := C
  n := 0
  while |v| < 10^8:
    v <- v^2 + C
    n <- n + 1
  crit := if stuck then 0 else log |v| / 2^(n + 1)
  phi : C := 0
  grad : R := 1
  found := false
  run boettcher(z, 1, 0)
  if found:
    run paint(log |phi| > crit)
  else:
    color <- inside

  @viewer "Parameter plane" var=C center=-0.5 pixelSize=1/150
  phi : C := 0
  grad : R := 1
  found := false
  run boettcher(C, 1, 1)
  if found:
    run paint(true)
  else:
    color <- inside

  @tool overlay for="Parameter plane"
  on refresh:
    erase
    use white for stroke
    draw circle at C with radius 4 pixel_size
    use black for stroke
    draw circle at C with radius 5 pixel_size

  @tool "Böttcher coordinate" for="Dynamical plane" shortcut=b
  on click-or-drag at w:
    erase
    phi : C := 0
    grad : R := 1
    found := false
    run boettcher(w, 1, 0)
    use white for stroke
    use white for fill
    draw filled circle at w with radius 3 pixel_size
    if found:
      t := arg phi / (2 pi)
      if t < 0:
        t <- t + 1
      write text("angle ", t, ", G = ", log |phi|) at w + 14 i pixel_size
    else:
      write "in K (or not escaped yet)" at w + 14 i pixel_size
  on deactivated:
    erase

  @tool "Trace orbit" for="Dynamical plane" shortcut=t
  on click-or-drag at w:
    erase
    use yellow for stroke
    use yellow for fill
    prev := w
    repeat up to 200 times:
      draw point at w
      prev <- w
      w <- f(w)
      draw line from prev to w
    until w escapes
  on deactivated:
    erase
metadata:
  title: Böttcher coordinates
  description: |
    The Böttcher map phi of z -> z^2 + C, drawn as a pattern on
    G + 2 pi i t (Green's function and external angle): checkerboard or
    rainbow dyadic tiles, external rays with equipotentials, or angle
    hue with potential bands. Exact on the whole basin of infinity when
    C is in the Mandelbrot set; when C is outside, phi is exact above
    the critical level and is dimmed below it. The parameter plane shows
    the same coordinate at z = C, which is the Douady-Hubbard map of M.
  preview-commands: |
    select viewer "Dynamical plane"
    capture viewer "Dynamical plane" as preview
  preview: |
    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
    QmCC
