format: 1
script: |
  @config "Hénon parameters"
  c : ℂ as "c =" ≔ -1.17
  a : ℂ as "a =" ≔ 0.3

  @config "Slice"
  m : ℂ as "Slope:" ≔ 0
  b : ℂ as "Intercept:" ≔ 4

  @config "Unstable manifold"
  zoom : ℝ as "Zoom: " ≔ 0.001
  crit : ℂ as "Critical point:" ≔ 0
  crit_c : ℂ as "found at c =" ≔ 0
  crit_found : Boolean as "Found" ≔ false

  # The critical points continued over the c plane by the "Fill" tool,
  # one in each layer: `crits @ (j, k, m)` is critical point m at the c of
  # pixel (j, k), with its status (`untried`, `continued`, or `lost`, where
  # Newton's method didn't converge). The table lies over the c plane's
  # window as it was when it was filled. `queue` is the fill's worklist of
  # cells, `(j, k)`.
  @config "Critical points"
  tolerance : ℝ as "Tolerance" ≔ 0.001
  shown : ℤ as "Color by critical point" ≔ 1
  crits : Table 3 of ℂ × ℤ
  queue : Table 1 of ℤ × ℤ

  @common
  # The Hénon map and its inverse (and the map at another parameter, cc,
  # for tools that work across the c plane)
  Hc(x, y, cc) ≔ (x² + cc - a y, x)
  H(x, y) ≔ Hc(x, y, c)
  Hinv(x, y) ≔ (y, (y² + c - x) / a)

  # H has a saddle fixpoint at (p, p)
  pc(cc) ≔ (1 + a + sqrt((1 + a)² - 4 cc)) / 2
  p ≔ pc(c)

  # Parameterization of the tangent plane of the unstable manifold near
  # the saddle fixpoint p
  λc(cc) ≔ pc(cc) + sqrt(pc(cc)² - a)
  λ ≔ λc(c)
  γc(z, cc) ≔ (pc(cc) + zoom z (2 pc(cc) λc(cc) - a), pc(cc) + zoom z)
  γ(z) ≔ γc(z, c)
  γinv(x, y) ≔ (y - p) / zoom

  # A cell's status in `crits`
  untried ≔ 0
  continued ≔ 1
  lost ≔ 2

  # F = x_n' / x_n at w, with (x_n, y_n) = H^n(γ(w)) at the parameter cc:
  # a multiple of 2 ∂g/∂z for the Green's function on the unstable
  # manifold, g(z) = lim 2^-n log |x_n(z)|. g is harmonic off K+, so its
  # critical points are the zeros of F.
  function green_gradient(w0, cc, n):
    w ≔ w0
    x, y ≔ γc(w, cc)
    repeat n times:
      x, y ⟵ Hc(x, y, cc)
    green_gradient ⟵ diff(x, w) / x

  # A critical point of g near w, at the parameter cc: (converged, the
  # point), by Newton's method on F. n stays fixed for the whole search:
  # w's escape time, plus 2 (letting n jump with the escape time would make
  # F jump at the bands' edges). Each step is at most `cap` long, so the
  # search settles near where it started; it has converged when a step is
  # shorter than `tol`.
  function critical_point(w, cc, cap, tol):
    critical_point ⟵ false, w
    x, y ≔ γc(w, cc)
    iterate (x, y) ⟼ Hc(x, y, cc) until (x, y) escapes
    if not stuck:
      n ≔ iterations + 2
      critical_point ⟵ capped_newton_root(u ⟼ green_gradient(u, cc, n), w, cap, 100, (u1, u2) ⟼ |u1 - u2| < tol)

  # Continue the critical point picked on the unstable manifold (at the c
  # it was found at) across the table, into layer L: a flood fill from
  # that c's cell, finding each neighboring cell's critical point by
  # Newton's method from its neighbor's. A cell is queued once its
  # critical point is known, and only once, so the queue never wraps
  # around.
  define fill_layer(L):
    (cols, rows, layers) ≔ table_size(crits)
    if crit_found and crit_c in crits:
      (j0, k0) ≔ table_cell(crits, crit_c)
      (ok, first) ≔ critical_point(crit, table_point(crits, (j0, k0)), 1, tolerance)
      if ok:
        crits @ (j0, k0, L) ⟵ (first, continued)
        queue @ 1 ⟵ (j0, k0)
        head ≔ 0
        tail ≔ 1
        while head < tail up to cols rows times:
          head ⟵ head + 1
          (j, k) ≔ queue @ head
          (seed, status) ≔ crits @ (j, k, L)
          for each step in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            (dj, dk) ≔ step
            jj, kk ≔ j + dj, k + dk
            if jj ≥ 1 and jj ≤ cols and kk ≥ 1 and kk ≤ rows:
              (old, tried) ≔ crits @ (jj, kk, L)
              if tried = untried:
                (found, w) ≔ critical_point(seed, table_point(crits, (jj, kk)), 1, tolerance)
                if found:
                  crits @ (jj, kk, L) ⟵ (w, continued)
                  tail ⟵ tail + 1
                  queue @ tail ⟵ (jj, kk)
                else:
                  crits @ (jj, kk, L) ⟵ (old, lost)

  @viewer "Slice" over x
  y ≔ m x + b
  iterate (x, y) ⟼ H(x, y) until (x, y) escapes

  @viewer "Unstable manifold"
  x, y ≔ γ(z)
  iterate (x, y) ⟼ H(x, y) until (x, y) escapes
  z ⟵ γinv(x, y)

  # The critical points found at this c, one for each layer of `crits`:
  # the one the c plane is colored by in blue, the others in grey.
  @tool overlay for "Unstable manifold"
  on refresh:
    erase
    (cols, rows, layers) ≔ table_size(crits)
    for each layer in range(1, layers):
      (here, status) ≔ crits @ (point c, layer) else (0, untried)
      if status = continued:
        use (if layer = shown then blue else grey) for fill
        draw filled circle at here with radius 4 pixels

  @tool "Orbit" for "Unstable manifold"
  on refresh:
    erase
  on click:
    z := click
    x, y ≔ γ(z)
    erase
    repeat:
      x, y ⟵ H(x, y)
      z ⟵ γinv(x, y)
      draw point at z
      draw line from previous z to z
    until (x, y) escapes

  # Click near a critical point of the Green's function on the unstable
  # manifold to find it with Newton's method (see `critical_point`) and mark
  # it. The critical point and the c it was found at go in the
  # configuration, as the seed for the "Fill" tool on the c plane.
  @tool "Critical point" for "Unstable manifold" with shortcut k
  on refresh:
    erase
    if crit_found and crit_c = c:
      use yellow for fill
      draw filled circle at crit with radius 4 pixels
  on click:
    (found, w) ≔ critical_point(click, c, 5 pixel_size, pixel_size / 1000000)
    crit ⟵ w
    crit_c ⟵ c
    crit_found ⟵ found
    erase
    if found:
      use yellow for fill
      draw filled circle at crit with radius 4 pixels

  @common
  crosshair(z) ≔ ||z| - 1| < 2 pixel_size or |re z| < 2 pixel_size or |im z| < 2 pixel_size

  @viewer "a plane" over a
  color ⟵ if crosshair(a) then black else white

  @tool overlay for "a plane"
  on refresh:
    erase
    use red for fill
    draw filled circle at a with radius 5 pixels

  # Where the "Fill" tool has continued a critical point picked on the
  # unstable manifold to this c, colored the way the "Unstable manifold"
  # view colors that critical point: by its escape time, as that view's
  # Rainbow colorizer does at its default settings (20 iterations per
  # color cycle; this viewer doesn't follow changes to them). Dark grey
  # where Newton's method failed, white where it wasn't tried. Which
  # critical point (which layer) is the "Color by critical point" setting.
  @viewer "c plane" over c
  (cols, rows, layers) ≔ table_size(crits)
  here : ℂ ≔ 0
  status ≔ untried
  if shown ≥ 1 and shown ≤ layers:
    here, status ⟵ crits @ (point c, shown) else (0, untried)

  if layers = 0 and crosshair(c):
    color ⟵ black
  else if status = continued:
    x, y ≔ γc(here, c)
    iterate (x, y) ⟼ Hc(x, y, c) until (x, y) escapes
    color ⟵ if stuck then black else rainbow(iterations / 20)
  else if status = lost:
    color ⟵ dark grey
  else:
    color ⟵ white

  # "Fill" lays the table over this window, with one layer, and continues
  # the critical point picked on the unstable manifold into it (see
  # `fill_layer`). "Add" continues the critical point picked now into a
  # new layer, over the same cells, keeping the ones before. "Reset"
  # clears them all.
  @tool "Fill" for "c plane" with shortcut f
  button "Fill"
  button "Add"
  button "Reset"
  on button "Reset":
    reset crits to (0, 0, 0)
    reset queue to 0
  on button "Fill":
    reset crits to (viewer_width, viewer_height, 1)
    reset queue to (viewer_width viewer_height)
    run fill_layer(1)
    shown ⟵ 1
  on button "Add":
    (w0, h0, n0) ≔ table_size(crits)
    if n0 = 0:
      reset crits to (viewer_width, viewer_height, 1)
      reset queue to (viewer_width viewer_height)
    else:
      resize crits to (w0, h0, n0 + 1)
    run fill_layer(n0 + 1)
    shown ⟵ n0 + 1

  @tool overlay for "c plane"
  on refresh:
    erase
    use red for fill
    draw filled circle at c with radius 5 pixels
metadata:
  title: SaddleDrop
  description: |-
    The complex Hénon map H(x, y) = (x² + c - a y, x), after the SaddleDrop program: the unstable manifold of H's saddle fixed point, parameterized by its tangent line, and the critical points of the Green's function on it.

    On the “Unstable manifold” viewer, the “Critical point” tool (k) finds the critical point nearest a click by Newton's method. On the “c plane” viewer, the “Fill” tool (f) then follows that critical point across the parameter plane, coloring each c by its critical point's escape time; “Add” follows another into a new layer, and “Color by critical point” chooses which layer is shown.
  preview-commands: |
    set parameter c to -0.49714868469816 + 0.70671940894707i
    set parameter zoom to 0.01
    select viewer "Unstable manifold"
    select tool "Critical point"
    click at -0.2853 - 0.4384i
    select viewer "c plane"
    resize viewer to 300x300
    set viewer center to 0
    set viewer pixel size to 0.009
    select tool "Fill"
    press button "Fill"
    capture viewer "c plane" as preview
  preview: |
    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
