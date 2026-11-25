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
  # the lift of leg 1 (from the critical value, 0) starting each way.
  function half(g1, z2, sign):
    s ≔ 2 sqrt((g1 @ 2) / z2)
    lifted ≔ lift(remove_first(g1), z2, -2 + sign s)
    half ⟵ prepend(lifted, -2)

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
        leg ⟵ append(leg, (1 + (far - 1) (k / body)^3) ω - shift)
      legs @ (*, j) ⟵ leg
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
    g1 ≔ legs @ (*, 1)
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
          next_legs @ (*, j) ⟵ with_tail(ha)
        else:
          next_legs @ (*, j) ⟵ with_tail(hb)
      else:
        nxt ≔ if j < n then j + 1 else μ + 1
        target ≔ legs @ (*, nxt)
        s ≔ 2 sqrt((target @ 1) / z2)
        plus_same ≔ polygon_winding_number(curve, -2 + s) = side0
        foot ≔ if plus_same = (sym = sym_A) then -2 + s else -2 - s
        next_legs @ (*, j) ⟵ with_tail(lift(target, z2, foot))
    # (moved so that the first foot is at 0 again, exactly)
    moved ≔ next_legs @ (1, 1)
    for each j in range(1, n):
      legs @ (*, j) ⟵ transform(next_legs @ (*, j), w ⟼ w - moved)
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

  @viewer "Standard spider"
  setup:
    center: 0
    pixel size: 1/128
  color ⟵ white

  # The standard spider for θ: a leg straight out from e^(2πi θ_j) for each
  # angle of θ's orbit, colored in order, and the diameter through θ/2
  # and θ/2 + 1/2 that splits the A side from the B side.
  @tool overlay for "Standard spider"
  on refresh:
    erase
    if q ≥ 2:
      use grey for line
      draw circle at 0 with radius 1
      star ≔ e^(π i p / q)
      draw line from star to -star
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
        draw line from ω to 10 ω
        draw filled circle at ω with radius 4 pixels
        t ⟵ mod(2 t, q)

  @viewer "Iterated spider"
  setup:
    center: -0.5
    pixel size: 1/50
  color ⟵ white

  # The current spider, with its legs colored as the standard spider's,
  # and P⁻¹(γ_1) in grey, through the critical point -2.
  @tool overlay for "Iterated spider"
  on refresh:
    erase
    (points, n) ≔ table_size(legs)
    if n ≥ 2:
      z2 ≔ legs @ (1, 2)
      g1 ≔ legs @ (*, 1)
      use light grey for line
      for each sign in [1, -1]:
        h ≔ half(g1, z2, sign)
        last ≔ h @ 1
        for each w in h:
          draw line from last to w
          last ⟵ w
      use grey for fill
      draw filled circle at -2 with radius 3 pixels
      for each j in range(1, n):
        col ≔ rainbow((j - 1) / n)
        use dark col for line
        use light col for fill
        last ≔ legs @ (1, j)
        for each w in legs @ (*, j):
          draw line from last to w
          last ⟵ w
        draw filled circle at legs @ (1, j) with radius 4 pixels

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

    The “Standard spider” shows the spider the algorithm starts from, and the kneading sequence is in the configuration. On the “Iterated spider”, the “Spider” tool (s) takes steps of the algorithm (“Step”, or “Run” until c settles down); the “Parameter plane” marks the c it has found so far.
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
    r4TyP44rq6JrrQYOAAAAAElFTkSuQmCC
