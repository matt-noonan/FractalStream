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
  # window as it was when it was filled. `queue` is the fill's worklist, a
  # heap of cells `(step, j, k)`, smallest step first (see `fill_layer`).
  @config "Critical points"
  tolerance : ℝ as "Tolerance" ≔ 0.001
  jump : ℝ as "Largest step (times the step before)" ≔ 4
  shown : ℤ as "Color by critical point" ≔ 1
  crits : Table 3 of ℂ × ℤ
  queue : Table 1 of ℝ × ℤ × ℤ

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

  # The fill's worklist, `queue`, is a binary heap of `size` cells, each
  # `(step, j, k)`, the one with the smallest step on top: `heap_push(entry)`
  # adds one, `heap_pop` takes the top one off, into `top`. (`size` and
  # `top` are the caller's.)
  define heap_push(entry):
    (em, _, _) ≔ entry
    size ⟵ size + 1
    slot ≔ size
    moving ≔ true
    while moving:
      if slot = 1:
        moving ⟵ false
      else:
        parent ≔ queue @ (slot // 2)
        (pm, _, _) ≔ parent
        if pm ≤ em:
          moving ⟵ false
        else:
          queue @ slot ⟵ parent
          slot ⟵ slot // 2
    queue @ slot ⟵ entry

  define heap_pop:
    top ⟵ queue @ 1
    last ≔ queue @ size
    (lm, _, _) ≔ last
    size ⟵ size - 1
    slot ≔ 1
    moving ≔ size ≥ 1
    while moving:
      child ≔ 2 slot
      if child > size:
        moving ⟵ false
      else:
        (cm, _, _) ≔ queue @ child
        if child + 1 ≤ size:
          (rm, _, _) ≔ queue @ (child + 1)
          if rm < cm:
            child ⟵ child + 1
            cm ⟵ rm
        if cm < lm:
          queue @ slot ⟵ queue @ child
          slot ⟵ child
        else:
          moving ⟵ false
    if size ≥ 1:
      queue @ slot ⟵ last

  # Continue the critical point picked on the unstable manifold (at the c
  # it was found at) across the table, into layer L: a flood fill from
  # that c's cell, finding each neighboring cell's critical point by
  # Newton's method from its neighbor's.
  #
  # A critical point can jump to another as c moves (across a branch cut),
  # and a jump, once taken, spreads: so a step is taken only if it's at
  # most `jump` times as long as the step that reached the cell it's taken
  # from (or than 10 `tolerance`, if that's longer). A cell where Newton's
  # method fails, or jumps, is marked lost, but tried again from each
  # neighbor the fill reaches later, which may get there smoothly.
  #
  # And the fill goes on from the cell reached by the shortest step first,
  # through where the critical point moves least: so it comes to a branch
  # cut last, from both sides, and the two sides meet along it.
  #
  # A cell is queued once, when its critical point is found, so the queue
  # holds at most every cell.
  define fill_layer(L):
    (cols, rows, layers) ≔ table_size(crits)
    if crit_found and crit_c in crits:
      (j0, k0) ≔ table_cell(crits, crit_c)
      (ok, first) ≔ critical_point(crit, table_point(crits, (j0, k0)), 1, tolerance)
      if ok:
        crits @ (j0, k0, L) ⟵ (first, continued)
        size ≔ 0
        top ≔ (0.0, j0, k0)
        run heap_push((0.0, j0, k0))
        while size > 0 up to cols rows times:
          run heap_pop
          (before, j, k) ≔ top
          (seed, status) ≔ crits @ (j, k, L)
          longest ≔ jump max(before, 10 tolerance)
          for each step in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            (dj, dk) ≔ step
            jj, kk ≔ j + dj, k + dk
            if jj ≥ 1 and jj ≤ cols and kk ≥ 1 and kk ≤ rows:
              (old, tried) ≔ crits @ (jj, kk, L)
              if tried ≠ continued:
                (found, w) ≔ critical_point(seed, table_point(crits, (jj, kk)), 1, tolerance)
                if found and |w - seed| ≤ longest:
                  crits @ (jj, kk, L) ⟵ (w, continued)
                  run heap_push((|w - seed|, jj, kk))
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
