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
    iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAAYRUlEQVR4nO1dB1gU1/YHRMGCGmON
    JX4mJuYlUf8mvqdGo6YYk2di/mqMyUtiLFhQo0Z9UZ+xF0RQxILGEgs2OkgRRcxDBAsYBSIoVZTe
    Owss5825s3PZhQW2zMzCsr/vux/szO7c2T2/Ofecc8+51wgEwMOHD8HZ2Rnc3d3psW8nuUI/IztY
    /r2fEF0aoCGMhLioVCoFFxcXQoK4uDhyLC4mlxBgoOk+yM4oFaJbAzSAIARAREdHEwIEBgbSY0O7
    HyYk2LDsulDdGqAmBCMAagFPT08FLbBvSyghwOsWDkJ1a4CaEIwAiNDQUEIALy8v8rqstBIGtt1L
    SODu9EjIrg1QEYISoKqqCi5evEhIkJ6eTo59+4kbIcAHb/4uZNcGqAhBCYAICQkhBPDx8SGvH4Zn
    EAL0N7aD5Ph8obs3oAkIToCysjLqEXBa4O/9fyMkWDMvQOjuDWgCghMA8ccffyhogQCveEKAYT0O
    McOEVIxbMKABiEKA/Px8uHTpEiFBQUEBOTakswMhwYEdd8S4BQMagCgEQISHhyvEBTYsCyIE+Pwf
    58W6BQOUQDQC4JOPBMCWm5tLooEYFUQSRDKGoQG6gWgEQFy5coUQAG0CxILpXoQAn408J+ZtGCAH
    UQlQWFhICIBeAdoF/72aLHMJbeF5cqGYt2KADKISAHHt2jVCgoAA1gX8vz6OhAQLZ3iJfSsGgA4I
    kJeXR20BtAtOHXpACDCgjR1USgwuodgwyi1JF71TPz8/BS0wqD1rDO5cGyz6vbR2GE052BfKK8Wd
    n0cvQF4LrLEMoIEhA8SF0ShrI/jm+N8gs+iZqB37+/sTAly9ehWKiyQwuMN+QgKnow9FvY/WDqOp
    h18GJMFH+7pAWkGSaB3La4HMzEz44TM3g0uoAxjFZ0fBBNsOhAQT7Szgae5j0ToPCgqi0cGiggoY
    aMbmCtzwTxLtHlo7iBeQnBsLE207siRg/j7NE4cEGBfAOQLMGUhNTYXJI84SAgzvZbAFxAJ1AxOy
    o2G8XQdKgqTcGFFuAJ9+biiICEsjBMAWH5MrSv+tHQpxgOScGJggI8HUwwMgt0T4GL38HEF8fDwM
    78kGhsa/flLwvg1QEghCG4AzDNEmeJYfL/hN+Pr60jqCC8ej2PCwiR3kZBnSx4WG0khgfmkW4xV0
    JST4gtEEQruIFRUVVAtg1tDw3kcMWkAkNBgKRqF/dWQwS4KD/SApR9gs3rCwMEIAV1dXsNt0i2oB
    zCQ2QDg0OheAmuD7k8MJCcbZmjOGYpRgN4K5g5wWiIyMhKE9DhESTBp2WrA+DVBhMqigLAc+3NeZ
    JYGNGcRmRAh2M5xHgNPFu9bdlE0V7yExAgOEgUqzgRmFKcQgRBKMtWkHiYzLKASwmoirI4iIiIC3
    ux+URQedBOnPADWmg9MLk+HDvZ3pcICvhcDt27epLWC78ZYsLmALSXGGGgIhoFY+QE5JGnxgx5IA
    ZxGFsAnkK4uxqGSQuT1bSfT2Kd77au0oL8xQPyGkqDyPegcT93YSZAIpKiqKGoROx+7T6GDikzze
    +2ptqCovgmivTeC//lVwnm+kWUZQTkk6THboRUiAkcMEAWwCTgtcvnwZhsmigx8PNXgEmqCyrABi
    A/aA/69vgMtCMyJ4rmmcElZckQ+fOPQgJBiz2wTisyL5vGcSFqZzBLdTSOIokuCo7T1e+9FXlOWn
    Qqz/bvBd+4qCwJ0tTeH6zjFw7/Q8zYYAeRQyLuJY67asd7DbDLKLU/m6fwIugdTDwwOG93akuYNS
    aQ2v/egLKkvz4S/vLUTACkJnmtfKPnDnxA9QUZSp8Bmtk0Jj0yMYApgQEqBGkFSVa3tJiqKiIqoF
    /H1uUS1wyPoub320dFRXlkNy6Bm4wqj3ukIP2jUawk8vZJ70zAY/z0tWcEj8ZRi9qw1LAvsXQQr8
    ZfdyxSQYHzh37AE1CCNC03jro6WhLO8ZRJxdDN6rX1LypPdgtMBmKC9Q7ffhLS38xhN3QgBs/3b7
    kq/LEnCFpZhNjPMDSADMIeQL5QXp8CzcBZJunYKC5/zaMnyhNCcZ7p2aRwRcV+j+G16HuOsOUJSh
    fiIPr3UBh/9YS0mw/OInvF338ePHdCjYs/E61QLH7e9rdV1plQQinKzAZUEbhR80cMffoTBN90vY
    oNAf+W4Hj6Wd6wnde0UvuH9+qcpPekPgvTBk0bnxhACjdxqD15/HeLsu5xaiNhjx0lE6T1BRUa3x
    NYPtJ5Mf021RWwjd3Bfubn8ZfFd0Isc8l3eDkuxE3u5fVSSHnYHAbSPBdWE7BYG7zDeBQOuxEOm+
    HiqKs3nrT5DKIEuncawm2GXEGInaPaUc5CuK3N1ukPAwmzNwQqPrpf7pyY6ZS8yg2G0yQMAM0qT+
    0+Dmr33IudDD03m598ZQXpQFKXcvMmp8CDjPM1EQuttic7h7cjYkhpxkhJ4lSP+CEKBKKoEJsiTT
    MTamkFfSsBWqDnCFEU4LfPOhCx0Kov9U//qhR74iP3LikXeo8LlW6TOV0Qqm5HxNDf/lamlRfhBy
    YCr4rBtUT7Wjn35t2zuQFCpO6Fuw2kCcQRxtLfMM9r/IyzVxnoAzCL29veHVDvY0cURdXNs6gvzg
    +Zc+qkcAbFd+ZsfdovRYre+7RloFd09bQsDGoUSVK7Pcw88sgKzYG1r3pS4ELQ71jz5LhgEkgU3A
    Yl6uyc0WEhI4184TrLe6ptZ1bu6fQn781N/H1BN+zZXp4GnFjsHVEvXzEqXVEngcYAuB20eRJ7qu
    wC8xzd2qIxG6JtfnE4JXBx8N3kA9g4v39vJyTXmDcNbHl2qHgvuqj5MJwb8RYQSs6gJVvl8qECDS
    dgg557f+NZWuVZj2F4QdnUm8h7rGm7xqD9g8FBKDj0F1ZZmmX513iFIe/oEsj2C0tQlUVGn/5VNS
    UmojhL4BNEKozlCAT54/I2AiaMbyf2T/FsQdGs4YgL3rCc9pvjFcnGsK7ktfAK+f+4Ib8/Q6LzCD
    i8xxpcLGNtcEvFf1g0i3X6A096nW31koiEKA3OIMGLPblJAAcwz5gJubGyXBpd+jqBYY/arqrmdp
    bgpc3zGqvoq2bESwDbSLzGe8V/eDKNd1UJzxhJfvKAbEWyXsaRAdCnbzYA/gMrTULXR3h3GDj1ES
    bFuj3joDmTFBjH+9Du6fXwaJN0+QSRVEdvwt8FrVFy4wT7/zjybkqUZNcGl+W/Bc1h2u7XwPYv12
    af1ddAlRVwiZYNuJHQp2GEOJRPs1gbC0nCNBdnY2vCxbdQzb3ZDnPNyx/kNUAkiqK2DUTmNCgo/3
    duPlmpxbiJNFGBXkCICBorKyKl760GeIvkaQNaP+uaHgSrT22b4PHjxQWGfgt73htSQwtmFIoHmo
    uKUB4yRlEgk8SU8H7/BwcPDzgxNBQXDA3x82M7/PR1u3wrDVq6G3pSW8MHs2dPr+e/EJgBhrbSZL
    ImnHy/U4LYCZxIgF0zwpCfob7waJpHmSILe4GFJycuBJWhrcT0qCiIQE8LhzB367dg2sPT1hw4UL
    sPTECVh15gzsYOyceY6OMHLtWphmYwP9Fy6E7nPmgAUjxHbffAMmM2aA0Vdfqd908cXjsyOpFviP
    59daX4/bmQQbh4lvnVDQBIX5ZbB64h5Yab4ENposgp/Nl8LqcTaQn1Oscj/llZVwPzERdnp4wAw7
    O/hoyxYYuHgx/GPdOnj3l1+gC/NUDVmxgjxhnRnBtGcE0/5f/4K2X38NbZhmwvzgxjNnaiYoFZvp
    rFnQe948sPjuO+izYAGM37QJrI4fh1+ZIXLFqVOESLaXL8ON6Gh49OyZbgiAGL+ng4wEJlpfq5h5
    kjgC4P8cRg9il6V/1cgG1hotATujefXaujaLIfJe7awfqsxBVlbQdvp0MGGasYDCMpIRoiNDEiRM
    z/nzSev644/k7wCGXK8sWQIfb9sG686fh7XnzsEPBw/CyRs3wI4RolNwMFyPioLolBQoKNUsoqgz
    AuDaA5wWsL26VOvrcQTglqHlMKzXIbAyWkGEfazzSrg79hTEfOoG98adhuNdfibHF5lbaSQ8VLvY
    OjNP2xvLl0NP5sn7ZPt2mMqo6P/fswcsjxyB9YzgTjKkOssIK5p54hIyMqBa2nzWQ9QZARCjZMGh
    UYxbqC04AmCASB5XnO8QIR80s4K4z70gZcYV2uK/8CbH8XyfcbMVn0ymtWWE+xKjzj/dsQNO1yGW
    vkCnBLgU4UC1wF9pt7W6Fp0g8vZWOL5qrDURsNcruxSEzzXvwez5SS8thtkOrW83M50SAIFBISTA
    +3ssNL4Gtwg1Nkwfk8eqMayAfV6zUUoAvyG25Pzyd7dr+1VaJHROgM8c+lAtoCnOMcZRXS+Aw1lb
    fzr+J0/zUxD+0+n+1A44ssYJwhmr2bNnT3A2MoIrb74Jsbt3Q3VZ85m5EwI6J0BcVq1L+Dw/QaXP
    oKV/69YtOi3MNXytDGtNFxMhO/ffTEmAf10GbKGegLuFBRF83XZtxAiozNffymSdEwDBEWD2iXea
    fC8KHyuF5AXPtfwGBIVawNrIkrp+h8xrXUI8vrbnR0TYtwcMgPIpUwAY4y/ngw/AX0aKUMYg1Fc0
    DwLIsobe29G2yffik4/CPn78OGxnXC5s+D8ew3MNIdDjHix7YaVCDOCnrivh1EJrIuTgXr2I4OUb
    ksHd1JScL33afOf0tUHzIMBW1hDEv02BU/so+M2bN5OG/zc2BMiDE/6uNvPI67tz5hABJ737bj0C
    YLvZuzc5n+brq/X3bI5oFgQYsbWNyoagtgTYZSwjgLGBAAidEgAXpVzvMZMKH9uv3t80agxqOgRw
    sGk7hxDA1oglQMKRI4YhQBeISg2DiXYdqeAn2JvT/3HlkUfpytcBQENPmQGIhqH8PEBDcHpnCx0G
    EFVFReAte8oNRqCI+O7EUCLsmecHwJ64aWCXMIP8/epc/0ZzB5W5fvjkqyJ8REFCFiVAViS7AmoW
    bm2rxAU0uIECITr1Nlsw4tgFbOOnE+FzDUkw6TCbRaxMC3CCx+CPpuAIcLjPitrrygTuYmZmCAQJ
    Dac7e4iA53gNURA+12Z7vEbOXwpXLAGXrxLG8K+m2G+xkBDARmYIIi5xT3379hpftyVCJwS4cHcv
    q+bdBislwHeur5DzLhEHFD6HEz0NhXzVQdrtBKoFIhzYiiLnjh2pFmhN0Mm3ResfBTx+nxnsivlC
    Qfg7H31OjuP5Z3lxCp+TrwXQFjbGcwkB9prOJa8zcBMrGQHCvtY+S6mlQGd0X+X6ObUD/h06DrZG
    fgprbo2l47+yVUYw2YMvAlxfep5qgdIMdjhxNTcnBLhorH1+QkuBzgiAK5F/e+JthRgA174/OYws
    Ul0XDaV+aQo7EzYmYN9hEXldkphItYDPoEFaX78lQKcDXmV1BTPOHwSr8xNgmuMrsOT8RHC/7whV
    0ob3COAIgImg2sLna0eqBbIj2UKSq8OGURKkeun/fsYtzuLB1G+uMpgP7Gu3gLUFzCzpsUuy6B8O
    BRI9jgEgWhwBsPiD0wJYFKItYpzCqBa4uvgsOVackEC1gEfXrlr30ZzR4giA4PYU4EsLOHRj8wP2
    GM+D6gq2nCzc0pKSIOi993jppzmiRRIAC0E5LYAFotpCUlhOgkLEIOxUW7kca21NSaCv8wEtkgAI
    LAnnSICl4toikFH/3FCQElS7aaZ3v36EABgpjGEIoW9osQSQSCQN1gJoijPDN8uCQ/NpbABxbeRI
    qgke79vHS1/NBS2WAAj59QFw2RhtUVUmATsTNkLo0HkJPV4jldJsYWwJjo5a99Vc0KIJgOAqg1XJ
    BlIFEXuv0qHg1gYPhXMePXpQEmQGBfHSn67R4gmQlpZGtQAuIccHrsrsAcwaenQ2lB6vqa6mySPY
    ojdu5KU/XaLFEwDBzRKiNpDyVHh56MWfWE3QZj4UpSjuZO7VqxcNFLV0EugFASorK+lQgMvJ8oHS
    zELYZ8rWEuzvaAVSiaKnESAXMo7etImXPnUBvSAAQn4FUVxYmg9k3n9KhgEkwYVx9V1Ar759KQlu
    /vOfvPQpNvSGAAi+DUJE5LFgEiFEElwcv7ve+dAZMygJMI2spUGvCPD8+fMGq4S1gcsnttQzCF7r
    Wu982KxZCiSQVracHc/1igAI3FaGz3kCDl5fHqQkiHOvv4H2vblzwZkxCpEEgSNHQnlay9jTSO8I
    gGFhbrIIN5ziDYx3wXkGtoxnEH+5/kxkqocHuL/4IiGBm4UFFD7S/bYzTUHvCIDAnce5oQC3nuML
    VeWVcKjHclYTmMyFzIj61UIl8fHgISOBS5s28MTenrf+hYBeEgDBlZDj5pN8oiS1APaasUkkdm3n
    Q7mSZeakFRVkGKBJpjNn8noPfEJvCVBSUkK1AG5DyycwfWyvbM7gUI8VUJGvfIm24MmTm71xqLcE
    QODG03y7hRyyo1Nhf4dFbIVRrxVQmq68UOWvLVuocej5wgtQnpHB+71oA70mgHwWMW5JzzcKk7Jh
    fyd2+ZkTg9dCeZ5yTZDm4wOu7doREvgOHAh5EfW9CF1BrwmACAkJoVqAr3kCeWRHPYcjfdmFpvZ3
    XgIlaQVK31eWmgrXR40iJEAyRP7yC+/3ogn0ngA1NTU0k5iv2cK6KEzOAXtztt7wQOel5LXSe2Fc
    1Ls//kjtgj9/+knndoHeEwDBuYUYHxBCCyByop/TFPMDFkuguM4MojwerFxJU88x5UyXdkGrIAAK
    nVtXIDAwULB+MiKSYZ/ZQppRpMxF5IBLzjjLSIAlaWk8zWKqi1ZBAERkZCQ1CMsErPlHm8DenPUO
    MMewLKvhQFRJQgK4d+3KJp0ynsKj7eKvVtpqCIDgbIGwsDBB+8l5lAZH+62SeQfroPhZw9PTuADF
    1eHDKQlufvYZyUEUC62KAOnp6VQLVFRUCNpXESP0owNWszZB12WNagLE7dmza3MLGBJIchu2IfhE
    qyIAgqsn8BVh2bf8+ExiEJK1il/+N+Q/btzYw2xjbkhwa98e8h8+FPweWx0BMCzMaYGCAuU+O5/A
    +oLfZJoAI4d5MY1PE5ckJYFnt250SEg+e1bQ+2t1BEBwBBDSI5BHbkw6OHS0oiTIiU5t9P3VpaVk
    7oCbUYzZuVOwe2uVBEhNTSUxAUwa0WaxKXWQx6j//R3ZsLED8zcvNr3R92MKeqyNDbi2bUuI8N9J
    k6A8tXHiaIJWSQAEPv2oBYJELPBAEnA2AWoCdBmbQiZzn9yQ4NahA2TwrLVaLQHk1xnIFcniRuAE
    0sEutYZhWXbTS92UMnaBV/futfWJdqrvkt4UWi0BEFxtob+/v6j9Ylzg1N9+ZTezYsigLLOoLqrL
    yyFg6FBKAtzdhI94QasmAHoButACiKpSCRzpywaLMHKYGqpa0goKHg1DJMGN8eOJ16ANWjUBEAEB
    AYQAmE0sNjCJ5GD3n6gmQG9BFWTfvEmrlXEeIUMLDdbqCSCvBfiqKFIHGCc4/dZGQgLHPiub9A44
    4DyCfKEqegyaoNUTAMFpAb4TSFUFrkvk8iFbfOJgsRiSA/5S6XOYX3B9zBgSMCIh5ClTyDF1YCAA
    sHsQcNPFYsUF6gJJwK1QgkWpib6qh4Ej160DZxMTQoLLffuCJEd5QooyGAggA7cMLa/FJGqiLKeY
    cQ3ZsDEml6TciGn6QzJkXr9O5g+4JJPs4GCVPmcggAzoBYg5R9AQUBOcfG091QRZD1Rf+qaqpARu
    TJhA8w6f7N/f5GcMBJADFx0MDw/X6X1Iq6RwcvA6NuWc8RKy/lSdBFKJBB6uWUONQ78hQ6CqkXWV
    DQSQA+cR4BxBQ5tQigUsQ/OceoCtRTSeB8lXVTMMOaRgfSRXj9CtG+Q0sKGWgQB1gCuMIAn+aAbb
    xddUS+H3N/4jW6rGEh6dVS+TKTskhOYXuHboAM+VLKdnIEAdcFlD6BUImTuoKpAETu9uoySIPhmi
    1ucrMjLgxvvv0yHh9rffkmGCg4EASsBpASwqaQ5AEpwfvZMuYvnwiPraCTfAcpG5igFvvw0SWdDL
    QAAl4LQA5gzwsQwtH6gqr4KL7+9mSWCiuJytqoh3dCRDAbc7GpaoGQjQALy8vAgJQkNDm36zSEBN
    cG7UdjafwHwRJF2JVvsaFVlZ4NG5M005MxCgAcTFxdGdSYSqJtIE1ZIq8PyC9Q72mS2ABB/1E0ex
    HC34008JCQwEaARcXCA6Wv0nTUggCXy/O8YOB4xhGLbtskbXSTx+3ECAxsBpAaEqi7VBjbQGfGcd
    la1ZZAkPDt/Q6DoGAjQBro7goQg5+prA74fjdPWymHPqVz//D2rgGRu3FkYWAAAAAElFTkSuQmCC
