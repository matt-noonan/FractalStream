format: 1
script: |
  @config "Angle θ = p / q"
  row:
    p : ℤ as "" ≔ 9
    text "/"
    q : ℤ as "" ≔ 56

  @config "Spider"
  kneading : Text as "Kneading sequence" ≔ ""
  row:
    μ : ℤ as "Preperiod" ≔ 0
    λ : ℤ as "Period" ≔ 0
  c_found : ℂ as "c ≈" ≔ 0
  row:
    steps : ℤ as "Steps" ≔ 0
    change : ℝ as "Last change" ≔ 0
  tolerance : ℝ as "Stop when a step changes c by less than" ≔ 0.000000001
  running : Boolean as "Running" ≔ false
  # The current spider: `legs @ (k, j)` is point k of leg j (k = 1 at
  # its foot, out to a radius of `far`); `symbol @ j` is leg j's kneading
  # symbol; `spider_angle @ 1` is the angle it's for, as (p, q).
  legs : Table 2 of ℂ
  next_legs : Table 2 of ℂ
  symbol : Table 1 of ℤ
  spider_angle : Table 1 of ℤ × ℤ

  @common
  # The spider algorithm (Hubbard and Schleicher) finds the quadratic
  # polynomial z² + c whose critical value c has external angle θ = p / q
  # (or, for a periodic θ, the center of the hyperbolic component whose
  # root the parameter ray at θ lands at).
  #
  # A spider has a foot z_j for each angle θ_j = 2^(j - 1) θ of θ's orbit
  # under doubling, with a leg from ∞ to it. It's drawn here with z_1 = 0,
  # where P(w) = z_2 (1 + w/2)² has its critical value: P's critical point
  # is -2, and P is conjugate to z² + c with c = z_2 / 2. One step of the
  # algorithm pulls each leg j + 1 back by P to make the new leg j (and the
  # last leg back from the first leg of θ's cycle), choosing between the
  # two preimages by leg j's kneading symbol: A legs on the same side of
  # P⁻¹(γ_1) as 0, B legs on the other; a ★ leg is the critical point,
  # with one of the two halves of P⁻¹(γ_1) as its leg.

  # (legs run out to radius `far`, as `body + 1` points)
  far ≔ 1000.0
  body ≔ 300
  tail ≔ 40

  # kneading symbols
  sym_A ≔ 1
  sym_B ≔ 2
  star1 ≔ 3
  star2 ≔ 4

  # The symbol of the angle t / q (θ = p / q): ★₂ at θ/2, ★₁ at θ/2 + 1/2,
  # A strictly between them counterclockwise from θ/2, B on the other side.
  symbol_of(t) ≔ if 2 t = p then star2 else if 2 t = p + q then star1 else if mod(2 t - p, 2 q) < q then sym_A else sym_B

  function symbol_text(s):
    symbol_text ⟵ if s = sym_A then "A" else if s = sym_B then "B" else if s = star1 then "★₁" else "★₂"

  # A path's preimage under P, by continuation: from `start` (a preimage
  # of the path's first point), each point the preimage nearer the last.
  function lift(path, z2, start):
    out ≔ [start]
    last ≔ start
    k ≔ 0
    for each pt in path:
      k ⟵ k + 1
      if k > 1:
        s ≔ 2 sqrt(pt / z2)
        last ⟵ closest([-2 + s, -2 - s], last)
        out ⟵ append(out, last)
    lift ⟵ out

  # A leg made long enough again, out to radius `far` (a lifted leg ends
  # nearer in), as `body + 1` points along it.
  function with_tail(leg):
    end ≔ leg @@ -1
    r0 ≔ |end|
    outer ≔ max(far, 2 r0)
    whole ≔ leg
    k ≔ 0
    repeat tail times:
      k ⟵ k + 1
      whole ⟵ append(whole, (end / r0) r0 (outer / r0)^(k / tail))
    m ≔ length(whole)
    out : List of ℂ ≔ []
    for each idx in range(0, body):
      out ⟵ append(out, whole @ (1 + round(idx (m - 1) / body)))
    with_tail ⟵ out

  # A list without its first item.
  function remove_first(points):
    out : List of ℂ ≔ []
    k ≔ 0
    for each pt in points:
      k ⟵ k + 1
      if k > 1:
        out ⟵ append(out, pt)
    remove_first ⟵ out

  # One of the two halves of P⁻¹(γ_1), from the critical point -2 out:
  # the lift of leg 1 (from the critical value, 0), and (for sign -1) its
  # reflection through -2, since the two preimages of a point are each
  # other's reflection. (Lifting each half separately would start them
  # both almost at -2, where they can't be told apart, and they can end up
  # on the same branch.)
  function half(g1, z2, sign):
    s ≔ 2 sqrt((g1 @ 2) / z2)
    lifted ≔ prepend(lift(remove_first(g1), z2, -2 + s), -2)
    if sign < 0:
      lifted ⟵ transform(lifted, w ⟼ -4 - w)
    half ⟵ lifted

  function reversed(points):
    out : List of ℂ ≔ []
    for each pt in points:
      out ⟵ prepend(out, pt)
    reversed ⟵ out

  # P⁻¹(γ_1) as a closed curve: out along one half, around a big arc
  # counterclockwise, and back in along the other. Which side of it a
  # point is on is its winding number.
  function outline(ha, hb):
    ea ≔ ha @@ -1
    eb ≔ hb @@ -1
    a0 ≔ arg(ea)
    da ≔ mod(arg(eb) - a0, 2 π)
    rr ≔ 2 max(|ea|, |eb|)
    arc : List of ℂ ≔ []
    k ≔ 0
    repeat 65 times:
      arc ⟵ append(arc, rr e^(i (a0 + da k / 64)))
      k ⟵ k + 1
    outline ⟵ join(ha, arc, reversed(hb))

  # Start over at the standard spider for θ = p / q: legs straight out
  # from the points e^(2πi θ_j) of the unit circle, moved so that the first
  # foot is at 0.
  define restart:
    t0 ≔ mod(p, q)
    (found, pre, per) ≔ preperiod_and_period(t ⟼ mod(2 t, q), t0, q, (u, v) ⟼ u = v)
    μ ⟵ pre
    λ ⟵ per
    n ≔ pre + per
    reset legs to (body + 1, n)
    reset next_legs to (body + 1, n)
    reset symbol to n
    reset spider_angle to 1
    spider_angle @ 1 ⟵ (p, q)
    shift ≔ e^(2 π i t0 / q)
    kneading ⟵ ""
    t ≔ t0
    for each j in range(1, n):
      s ≔ symbol_of(t)
      symbol @ j ⟵ s
      kneading ⟵ text(kneading, if j = pre + 1 then "[" else "", symbol_text(s))
      ω ≔ e^(2 π i t / q)
      leg : List of ℂ ≔ []
      for each k in range(0, body):
        leg ⟵ append(leg, (1 + (far - 1) (k / body)³) ω - shift)
      legs @ (×, j) ⟵ leg
      t ⟵ mod(2 t, q)
    kneading ⟵ text(kneading, "]")
    steps ⟵ 0
    change ⟵ 0
    c_found ⟵ (legs @ (1, 2)) / 2
    running ⟵ false

  # One step of the spider algorithm.
  define spider_step:
    (points, n) ≔ table_size(legs)
    z2 ≔ legs @ (1, 2)
    g1 ≔ legs @ (×, 1)
    ha ≔ half(g1, z2, 1)
    hb ≔ half(g1, z2, -1)
    curve ≔ outline(ha, hb)
    side0 ≔ polygon_winding_number(curve, 0)
    # (a point counterclockwise from ha's far end, between it and hb's)
    probe ≔ (ha @@ -1) e^(i mod(arg(hb @@ -1) - arg(ha @@ -1), 2 π) / 2)
    ha_is_star2 ≔ polygon_winding_number(curve, probe) = side0
    for each j in range(1, n):
      sym ≔ symbol @ j
      if sym = star1 or sym = star2:
        if ha_is_star2 = (sym = star2):
          next_legs @ (×, j) ⟵ with_tail(ha)
        else:
          next_legs @ (×, j) ⟵ with_tail(hb)
      else:
        nxt ≔ if j < n then j + 1 else μ + 1
        target ≔ legs @ (×, nxt)
        s ≔ 2 sqrt((target @ 1) / z2)
        plus_same ≔ polygon_winding_number(curve, -2 + s) = side0
        foot ≔ if plus_same = (sym = sym_A) then -2 + s else -2 - s
        next_legs @ (×, j) ⟵ with_tail(lift(target, z2, foot))
    # (moved so that the first foot is at 0 again, exactly)
    moved ≔ next_legs @ (1, 1)
    for each j in range(1, n):
      legs @ (×, j) ⟵ transform(next_legs @ (×, j), w ⟼ w - moved)
    old ≔ c_found
    c_found ⟵ (legs @ (1, 2)) / 2
    change ⟵ |c_found - old|
    steps ⟵ steps + 1

  # Whether the spider is for the angle pp / qq.
  function spider_is_for(pp, qq):
    spider_is_for ⟵ false
    if table_size(spider_angle) = 1:
      (sp, sq) ≔ spider_angle @ 1
      spider_is_for ⟵ sp = pp and sq = qq

  @viewer "Standard spider" on the sphere
  setup:
    center: 0
  color ⟵ white

  # The standard spider for θ on the sphere: the unit circle is the
  # equator, and each leg runs from e^(2πi θ_j) along its meridian to ∞,
  # around the back. The great circle through θ/2 and θ/2 + 1/2 splits
  # the A side from the B side.
  @tool overlay for "Standard spider"
  on refresh:
    erase
    if q ≥ 2:
      use grey for line
      last ≔ 1 + 0 i
      for each k in range(1, 64):
        here ≔ e^(2 π i k / 64)
        draw line from last to here
        last ⟵ here
      star ≔ e^(π i p / q)
      draw line from star to 0
      draw line from 0 to -star
      draw line from star to ∞
      draw line from -star to ∞
      use black for line
      write "A" at i star / 2
      write "B" at -i star / 2
      t0 ≔ mod(p, q)
      (found, pre, per) ≔ preperiod_and_period(t ⟼ mod(2 t, q), t0, q, (u, v) ⟼ u = v)
      t ≔ t0
      for each j in range(1, pre + per):
        ω ≔ e^(2 π i t / q)
        col ≔ rainbow((j - 1) / (pre + per))
        use dark col for line
        use light col for fill
        draw line from ω to ∞
        draw filled circle at ω with radius 4 pixels
        t ⟵ mod(2 t, q)

  @viewer "Iterated spider" on the sphere
  setup:
    center: 0
  iterate z ⟼ z² + c_found until z escapes
  if stuck:
    color ⟵ white
  else:
    color ⟵ cycle(iterations/20, grey, light light grey)
    
  @tool "Trace orbit" for "Iterated spider" with shortcut t
  on refresh:
    erase
  on click:
    z ≔ click
    erase
    use white for line
    repeat:
      draw point at z
      z ⟵ z² + c_found
      draw line from z to previous z
    until z escapes
    
  # The current spider on the sphere, with its legs colored as the
  # standard spider's, all ending at ∞, and P⁻¹(γ_1) in grey, over them. It's drawn
  # in the coordinates of z² + c (z = (w + 2) z_2 / 4, with c = z_2 / 2),
  # so the critical point is 0, facing front, and the first foot is c; as
  # the spider settles down, its legs come to look like the external rays
  # of z² + c landing at c's orbit.
  @tool overlay for "Iterated spider"
  on refresh:
    erase
    (points, n) ≔ table_size(legs)
    if n ≥ 2:
      z2 ≔ legs @ (1, 2)
      g1 ≔ legs @ (×, 1)
      for each j in range(1, n):
        col ≔ rainbow((j - 1) / n)
        use dark col for line
        use light col for fill
        last ≔ (legs @ (1, j) + 2) z2 / 4
        for each w in legs @ (×, j):
          here ≔ (w + 2) z2 / 4
          draw line from last to here
          last ⟵ here
        draw line from last to ∞
        foot ≔ (legs @ (1, j) + 2) z2 / 4
        draw filled circle at foot with radius 4 pixels
      # (P⁻¹(γ_1) last: near ∞, its halves run close to some legs)
      use grey for line
      for each sign in [1, -1]:
        h ≔ half(g1, z2, sign)
        last ≔ (h @ 1 + 2) z2 / 4
        for each w in h:
          here ≔ (w + 2) z2 / 4
          draw line from last to here
          last ⟵ here
        draw line from last to ∞
      use grey for fill
      draw filled circle at 0 with radius 3 pixels

  # "Restart" goes back to the standard spider (as changing θ does), "Step"
  # takes one step, and "Run" takes steps until c changes by less than the
  # tolerance (or "Run" is pressed again).
  @tool "Spider" for "Iterated spider" with shortcut s
  setup:
    refresh can update: true
  row:
    button "Restart"
    button "Step"
    button "Run"
  on activated:
    if not spider_is_for(p, q):
      run restart
  on refresh:
    if not spider_is_for(p, q):
      run restart
  on button "Restart":
    run restart
  on button "Step":
    run spider_step
  on button "Run":
    running ⟵ not running
  on timer "iterate" every 20 ms:
    if running:
      run spider_step
      if steps > 1 and change < tolerance:
        running ⟵ false

  @viewer "Parameter plane"
  setup:
    center: -0.5
    pixel size: 1/200
  w ≔ 0
  iterate w ⟼ w² + z until w escapes

  # Where the spider says c is.
  @tool overlay for "Parameter plane"
  on refresh:
    erase
    if steps > 0:
      use black for line
      use yellow for fill
      draw filled circle at c_found with radius 5 pixels
metadata:
  title: Spider algorithm
  description: |-
    Hubbard and Schleicher's spider algorithm: from an external angle θ = p / q, find the parameter c where the parameter ray at θ lands (for a preperiodic θ), or the center of the hyperbolic component it lands on the root of (for a periodic θ).

    Drawn on the Riemann sphere, where every leg runs to ∞. The “Standard spider” shows the spider the algorithm starts from, and the kneading sequence is in the configuration. On the “Iterated spider”, the “Spider” tool (s) takes steps of the algorithm (“Step”, or “Run” until c settles down). The spider is drawn over the filled Julia set of z² + c for the c found so far, in the same coordinates, so as c settles down its legs settle onto the Julia set's external rays; the “Trace orbit” tool (t) draws an orbit. The “Parameter plane” marks the c found so far.
  preview-commands: |
    select viewer "Iterated spider"
    select tool "Spider"
    press button "Run"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    trigger timer "iterate"
    capture viewer "Iterated spider" as preview
  preview: |
    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
    QmCC
wx-properties:
  viewers:
    Iterated spider:
      x: 663
      'y': 124
      width: 600
      height: 600
    Parameter plane:
      x: 780
      'y': 214
      width: 600
      height: 600
    Standard spider:
      x: 100
      'y': 100
      width: 600
      height: 600
  configuration:
    x: 189
    'y': 399
