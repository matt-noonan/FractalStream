format: 1
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
      bg := blend(0.25, white, rainbow(t))
      color <- if d < line_width pixel_size then black else bg
    else:
      band := u - floor u
      color <- blend(0.35 + 0.65 band, black, rainbow(t))
    if dim_invalid and not valid:
      color <- blend(0.35, grey, color)

  @viewer "Dynamical plane"
  setup:
    center: 0
    pixel size: 1/150
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

  @viewer "Parameter plane" over C
  setup:
    center: -0.5
    pixel size: 1/150
  phi : C := 0
  grad : R := 1
  found := false
  run boettcher(C, 1, 1)
  if found:
    run paint(true)
  else:
    color <- inside

  @tool overlay for "Parameter plane"
  on refresh:
    erase
    use white for stroke
    draw circle at C with radius 4 pixel_size
    use black for stroke
    draw circle at C with radius 5 pixel_size

  @tool "Böttcher coordinate" for "Dynamical plane" with shortcut b
  text "Click in the dynamical plane to update these values"
  saved_angle : R as "θ =" := 0
  saved_G : R as "G =" := 0
  on click:
    w := click
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
      write text("θ = ", t, "\nG = ", log |phi|) at w + (80 + 30 i) pixel_size
      saved_angle <- t
      saved_G <- log |phi|
    else:
      write "in K" at w + 15 i pixel_size
      saved_angle <- 0
      saved_G <- 0
  on deactivated:
    erase

  @tool "Trace orbit" for "Dynamical plane" with shortcut t
  on click:
    w := click
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
    YII=
