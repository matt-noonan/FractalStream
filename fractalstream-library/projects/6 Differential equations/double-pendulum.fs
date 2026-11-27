format: 1
script: |
  # A double pendulum: two rods of length 1, each with a mass of 1 at its
  # end, under gravity g = 1. α is the inner rod's angle from straight down,
  # β the outer rod's angle from the inner rod's direction, and pα, pβ their
  # momenta. Its energy is the Hamiltonian H below, which the motion keeps.
  #
  # The motion goes on in a three-dimensional energy surface, H = E: too
  # many dimensions to see, so it's seen in a slice, its Poincaré section.
  # Each time the inner rod swings through straight down (α = 0, or a whole
  # number of turns), moving counterclockwise, the outer rod's angle β and
  # momentum pβ are a point of the section; and every point (β, pβ) of the
  # section, with pβ² ≤ 2 (E + 2 + cos β), is where some motion with energy
  # E crosses it. (Outside that, grey, the energy isn't enough.)
  #
  # A regular motion's crossings lie along a curve, or on a chain of
  # islands; a chaotic one's scatter over a region. The "Orbit" tool draws
  # the crossings of the motion through a point. Behind them, the chaos map
  # (the "Chaos map" setting: slow, unless the GPU draws it in single
  # precision, so it's off to start with) colors each point by its motion's
  # MEGNO (Cincotta and Simó's mean exponential growth of nearby orbits):
  # how fast a small change in where the motion starts grows, weighted
  # toward later times. It tends to 2 for a regular motion (0 near a stable
  # periodic one), dark, and grows steadily for a chaotic one, bright.
  @config "Pendulum"
  E : ℝ as "Energy E =" ≔ -1.25
  chaos_map : Boolean as "Chaos map" ≔ false

  @config "Accuracy"
  map_time : ℝ as "Chaos map: time" ≔ 200
  map_step : ℝ as "Chaos map: time step" ≔ 0.05
  orbit_step : ℝ as "Orbits: time step" ≔ 0.01

  @common
  H(α, β, pα, pβ) ≔ -2 cos(α) - cos(α + β) + (pα² - 2 (1 + cos β) pα pβ + (3 + 2 cos β) pβ²) / (3 - cos(2 β))

  # Hamilton's equations: how (α, β, pα, pβ) changes with time
  α̇(α, β, pα, pβ) ≔ diff(H(α, β, pα, pβ), pα)
  β̇(α, β, pα, pβ) ≔ diff(H(α, β, pα, pβ), pβ)
  ṗα(α, β, pα, pβ) ≔ -diff(H(α, β, pα, pβ), α)
  ṗβ(α, β, pα, pβ) ≔ -diff(H(α, β, pα, pβ), β)
  flow(α, β, pα, pβ) ≔ (α̇(α, β, pα, pβ), β̇(α, β, pα, pβ), ṗα(α, β, pα, pβ), ṗβ(α, β, pα, pβ))

  # The same, but with α in place of time: how the rest change as α does
  # (for finding exactly where a motion crosses α = 0, Hénon's way)
  flow_in_α(α, β, pα, pβ) ≔ (1, β̇(α, β, pα, pβ) / α̇(α, β, pα, pβ), ṗα(α, β, pα, pβ) / α̇(α, β, pα, pβ), ṗβ(α, β, pα, pβ) / α̇(α, β, pα, pβ))

  # One step of length h of the fourth-order Runge-Kutta method, for the
  # vector field v (`flow` or `flow_in_α`)
  function rk4(v, s, h):
    (a, b, pa, pb) ≔ s
    (k1a, k1b, k1c, k1d) ≔ v(a, b, pa, pb)
    (k2a, k2b, k2c, k2d) ≔ v(a + h k1a / 2, b + h k1b / 2, pa + h k1c / 2, pb + h k1d / 2)
    (k3a, k3b, k3c, k3d) ≔ v(a + h k2a / 2, b + h k2b / 2, pa + h k2c / 2, pb + h k2d / 2)
    (k4a, k4b, k4c, k4d) ≔ v(a + h k3a, b + h k3b, pa + h k3c, pb + h k3d)
    na ≔ a + h (k1a + 2 k2a + 2 k3a + k4a) / 6
    nb ≔ b + h (k1b + 2 k2b + 2 k3b + k4b) / 6
    npa ≔ pa + h (k1c + 2 k2c + 2 k3c + k4c) / 6
    npb ≔ pb + h (k1d + 2 k2d + 2 k3d + k4d) / 6
    rk4 ⟵ (na, nb, npa, npb)

  # How a small change (va, vb, vpa, vpb) in the state changes with time:
  # the flow's derivative in that direction (the linearized equations)
  along1(a, b, pa, pb, va, vb, vpa, vpb, ε) ≔ diff(α̇(a + ε va, b + ε vb, pa + ε vpa, pb + ε vpb), ε)
  along2(a, b, pa, pb, va, vb, vpa, vpb, ε) ≔ diff(β̇(a + ε va, b + ε vb, pa + ε vpa, pb + ε vpb), ε)
  along3(a, b, pa, pb, va, vb, vpa, vpb, ε) ≔ diff(ṗα(a + ε va, b + ε vb, pa + ε vpa, pb + ε vpb), ε)
  along4(a, b, pa, pb, va, vb, vpa, vpb, ε) ≔ diff(ṗβ(a + ε va, b + ε vb, pa + ε vpa, pb + ε vpb), ε)
  along(a, b, pa, pb, va, vb, vpa, vpb) ≔ (along1(a, b, pa, pb, va, vb, vpa, vpb, 0.0), along2(a, b, pa, pb, va, vb, vpa, vpb, 0.0), along3(a, b, pa, pb, va, vb, vpa, vpb, 0.0), along4(a, b, pa, pb, va, vb, vpa, vpb, 0.0))

  # One Runge-Kutta step of length h for a state s and a small change v
  # in it, together: (s, v) afterwards
  function rk4_with_change(s, v, h):
    (a, b, pa, pb) ≔ s
    (va, vb, vpa, vpb) ≔ v
    (k1a, k1b, k1c, k1d) ≔ flow(a, b, pa, pb)
    (m1a, m1b, m1c, m1d) ≔ along(a, b, pa, pb, va, vb, vpa, vpb)
    s2 ≔ (a + h k1a / 2, b + h k1b / 2, pa + h k1c / 2, pb + h k1d / 2)
    v2 ≔ (va + h m1a / 2, vb + h m1b / 2, vpa + h m1c / 2, vpb + h m1d / 2)
    (k2a, k2b, k2c, k2d) ≔ flow(proj1(s2), proj2(s2), proj3(s2), proj4(s2))
    (m2a, m2b, m2c, m2d) ≔ along(proj1(s2), proj2(s2), proj3(s2), proj4(s2), proj1(v2), proj2(v2), proj3(v2), proj4(v2))
    s3 ≔ (a + h k2a / 2, b + h k2b / 2, pa + h k2c / 2, pb + h k2d / 2)
    v3 ≔ (va + h m2a / 2, vb + h m2b / 2, vpa + h m2c / 2, vpb + h m2d / 2)
    (k3a, k3b, k3c, k3d) ≔ flow(proj1(s3), proj2(s3), proj3(s3), proj4(s3))
    (m3a, m3b, m3c, m3d) ≔ along(proj1(s3), proj2(s3), proj3(s3), proj4(s3), proj1(v3), proj2(v3), proj3(v3), proj4(v3))
    s4 ≔ (a + h k3a, b + h k3b, pa + h k3c, pb + h k3d)
    v4 ≔ (va + h m3a, vb + h m3b, vpa + h m3c, vpb + h m3d)
    (k4a, k4b, k4c, k4d) ≔ flow(proj1(s4), proj2(s4), proj3(s4), proj4(s4))
    (m4a, m4b, m4c, m4d) ≔ along(proj1(s4), proj2(s4), proj3(s4), proj4(s4), proj1(v4), proj2(v4), proj3(v4), proj4(v4))
    ns ≔ (a + h (k1a + 2 k2a + 2 k3a + k4a) / 6, b + h (k1b + 2 k2b + 2 k3b + k4b) / 6, pa + h (k1c + 2 k2c + 2 k3c + k4c) / 6, pb + h (k1d + 2 k2d + 2 k3d + k4d) / 6)
    nv ≔ (va + h (m1a + 2 m2a + 2 m3a + m4a) / 6, vb + h (m1b + 2 m2b + 2 m3b + m4b) / 6, vpa + h (m1c + 2 m2c + 2 m3c + m4c) / 6, vpb + h (m1d + 2 m2d + 2 m3d + m4d) / 6)
    rk4_with_change ⟵ (ns, nv)

  # How much energy is left for pα at (β, pβ) on the section: none, outside
  # the region the motion can reach
  room(β, pβ) ≔ 2 (E + 2 + cos β) - pβ²

  # The state at (β, pβ) on the section, with pα from H = E (the larger
  # root: the one moving counterclockwise)
  on_section(β, pβ) ≔ (0.0, β, (1 + cos β) pβ + sqrt((1 + sin(β)²) room(β, pβ)), pβ)

  # An angle, as one from -π to π
  wrapped(θ) ≔ θ - 2 π round(θ / (2 π))

  # Draw the crossings of the motion through `start` (β + pβ i) on the
  # section, in the n-th color (for the "Orbit" tool)
  define draw_orbit(start, n):
    β0 ≔ re start
    pβ0 ≔ im start
    if room(β0, pβ0) ≥ 0:
      hue ≔ rainbow(0.37 (n - 1))
      shade ≔ if chaos_map then light hue else dark hue
      use shade for fill
      use shade for stroke
      draw filled circle at (wrapped(β0), pβ0) with radius 3 pixels
      s ≔ on_section(β0, pβ0)
      found ≔ 0
      while found < crossings up to 100000000 times:
        (a0, b0, pa0, pb0) ≔ s
        s ⟵ rk4(flow, s, orbit_step)
        (a, b, pa, pb) ≔ s
        if floor(a / (2 π)) > floor(a0 / (2 π)):
          # (crossed α = 2π k counterclockwise: step back in α to it)
          k ≔ floor(a / (2 π))
          (ca, cb, cpa, cpb) ≔ rk4(flow_in_α, s, 2 π k - a)
          draw filled circle at (wrapped(cb), cpb) with radius 1 pixels
          found ⟵ found + 1

  @viewer "Poincaré section" over (β, pβ)
  setup:
    center: (0, 0)
    pixel size: 1/110
  if room(β, pβ) < 0:
    color ⟵ grey
  else if not chaos_map:
    color ⟵ white
  else:
    # (MEGNO: a small change v in the state, followed by the linearized
    # equations, and scaled back to length 1 each step; Y is the time
    # average of 2/t times the time-weighted total of the logarithms of
    # how much it's stretched)
    s ≔ on_section(β, pβ)
    v ≔ (0.0, 1.0, 0.0, 0.0)
    weighted ≔ 0.0
    mean ≔ 0.0
    t ≔ 0.0
    while t < map_time up to 100000 times:
      (s, v) ⟵ rk4_with_change(s, v, map_step)
      t ⟵ t + map_step
      (va, vb, vpa, vpb) ≔ v
      d ≔ sqrt(va² + vb² + vpa² + vpb²)
      weighted ⟵ weighted + t log(d)
      mean ⟵ mean + map_step 2 weighted / t
      v ⟵ (va / d, vb / d, vpa / d, vpb / d)
    Y ≔ mean / t
    color ⟵ blend(max(0.0, min(1.0, (Y - 2) / 4)), dark blue, yellow)

  # Click to draw the crossings of the motion through a point of the
  # section, each motion in its own color; they're kept (and redrawn for a
  # new energy) until "Clear".
  @tool "Orbit" for "Poincaré section" with shortcut o
  crossings : ℤ as "Crossings" ≔ 500
  starts : List of ℂ ≔ []
  button "Clear"
  on click:
    (β0, pβ0) ≔ click
    if room(β0, pβ0) ≥ 0:
      starts ⟵ append(starts, β0 + pβ0 i)
      run draw_orbit(β0 + pβ0 i, length(starts))
  on button "Clear":
    starts ⟵ []
  on refresh:
    erase
    for each n in range(1, length(starts)):
      run draw_orbit(starts @ n, n)
  on deactivated:
    erase
metadata:
  title: A double pendulum's Poincaré section
  description: |
    A double pendulum seen each time its inner rod swings through straight down: the outer rod's angle β and momentum pβ, at a fixed energy E. Click with the “Orbit” tool to draw a motion's crossings: along a curve for a regular motion, scattered for a chaotic one. The “Chaos map” setting colors each point by how fast motions starting next to each other separate (MEGNO): dark where it's regular, bright where it's chaotic. It's slow to draw unless the GPU draws it in single precision.
  preview-commands: |
    set parameter chaos_map to true
    select viewer "Poincaré section"
    resize viewer to 300x300
    set viewer pixel size to 0.018
    select tool "Orbit"
    click at (0.0, -0.8)
    click at (-1.6, 0.8)
    capture viewer "Poincaré section" as preview
  preview: |
    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
    WkO1/wesWZUpUdbcgAAAAABJRU5ErkJggg==
