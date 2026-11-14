script: |
  # Klein's j-invariant on the upper half-plane, where the modular group
  # PSL(2, Z) acts, tiled by copies of its fundamental domain: the strip
  # |re z| <= 1/2 above the unit circle.
  #
  # The viewer folds each pixel's point z into that domain (`group=modular`)
  # before the code below sees it. Since j(z) is the same at every point of
  # an orbit, the picture only needs j there, where Im z >= sqrt(3)/2, so
  # q = e^(2 pi i z) is smaller than 0.005 and eight terms of each series
  # are plenty, however near the real axis the pixel is.
  #
  # Colors: the hue is arg j; the bands are |j| doubling. j has a triple
  # zero at e^(2 pi i/3) (where three tiles meet) and j - 1728 a double
  # zero at i.
  #
  # Tools: "Mark" (m) marks a point in every tile: the point's orbit
  # under the group, as far as the tiles are big enough to see.

  @config "Coloring"
  show_bands : Boolean as "Bands where |j| doubles" := true
  show_edges : Boolean as "Tile edges" := true

  @common
  clamp01(v) := if v < 0 then 0.0 else if v > 1 then 1.0 else v

  @viewer "j-invariant" view=halfplane group=modular
  # (divisor sums: sigma_3(n) and sigma_5(n), for n = 1 .. 8)
  sigma3 := [1, 9, 28, 73, 126, 252, 344, 585]
  sigma5 := [1, 33, 244, 1057, 3126, 8052, 16808, 33825]
  q := exp(2 pi i z)
  qn := q
  s4 : C := 0
  s6 : C := 0
  for each n in range(1, 8):
    s4 <- s4 + (sigma3 @ n) qn
    s6 <- s6 + (sigma5 @ n) qn
    qn <- qn q
  e4 := 1 + 240 s4
  e6 := 1 - 504 s6
  j := 1728 e4^3 / (e4^3 - e6^2)
  color <- rainbow(arg j / (2 pi))
  if show_bands and |j| > 0:
    band := mod(log(|j|) / log 2, 1.0)
    color <- blend(0.35 band, black, color)
  # (a pixel's size, in the hyperbolic plane: its width over im z)
  if show_edges:
    w := pixel_size / im(z)
    color <- blend(clamp01((1.5 w - side_distance) / w), black, color)

  @tool "Mark" for="j-invariant" shortcut=m
  j_here : C as "j =" := 0
  on click-or-drag at p:
    erase
    sigma3 := [1, 9, 28, 73, 126, 252, 344, 585]
    sigma5 := [1, 33, 244, 1057, 3126, 8052, 16808, 33825]
    q := exp(2 pi i p)
    qn := q
    s4 : C := 0
    s6 : C := 0
    for each n in range(1, 8):
      s4 <- s4 + (sigma3 @ n) qn
      s6 <- s6 + (sigma5 @ n) qn
      qn <- qn q
    e4 := 1 + 240 s4
    e6 := 1 - 504 s6
    j_here <- 1728 e4^3 / (e4^3 - e6^2)
    use white for fill
    use black for stroke
    draw filled circle at p with radius 4 pixels
  on deactivated:
    erase
metadata:
  title: Klein's j-invariant
  description: |
    The j-invariant on the upper half-plane, tiled by the modular group's
    fundamental domain. The viewer folds each point into that domain
    (group=modular), where the q-series converge fast.
  preview-commands: |
    capture viewer "j-invariant" as preview
  preview: |
    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
    AAAASUVORK5CYII=
