script: |
  # A closed surface of genus g, as the quotient of the hyperbolic plane by
  # a Fuchsian group: the regular 4g-gon with corner angles 2 pi / 4g, with
  # each side glued to the opposite one by a hyperbolic translation. (All
  # 4g corners meet at one point of the surface, where the angles add up
  # to 2 pi.)
  #
  # The group's side pairings are a table, which the overlay works out from
  # the genus; the viewer (`group=sides`) folds each point into the polygon
  # before the code below sees it, and the picture is the surface, repeated
  # over the disk.
  #
  # Tools: "Circle" (c) draws a hyperbolic circle about a point, which
  # wraps around the surface once it's larger than the polygon: its copies
  # are drawn in each tile. "Geodesic" (g) draws the geodesic between
  # where a drag starts and where it is (both in the polygon, as tools'
  # points are), an arc of a circle meeting the disk's edge at right
  # angles: the same segment of the surface in every tile.

  @config "Surface"
  genus : Z as "Genus" := 2
  show_edges : Boolean as "Tile edges" := true
  sides : Table 2 of C

  @common
  clamp01(v) := if v < 0 then 0.0 else if v > 1 then 1.0 else v

  @viewer "Surface" view=disk group=sides pixelSize=1/280
  (rows, n) := table_size(sides)
  if n = 0:
    color <- grey
  else:
    # (the hue by direction from the polygon's center; bands by distance)
    dist := 2 atanh(|z|)
    color <- rainbow(arg z / (2 pi))
    color <- blend(0.25 + 0.25 cos(6 dist), white, color)
    if show_edges:
      # (a pixel's hyperbolic size, and the edge colored by its pair of sides)
      w := 2 pixel_size / (1 - |z|^2)
      pair := mod(side_index - 1, n // 2)
      edge := rainbow(pair / (n // 2))
      color <- blend(clamp01((1.5 w - side_distance) / w), dark edge, color)

  # The side pairings: the 4g-gon's sides are at distance d from its
  # center, cosh d = cot(pi / 4g); the pairing of side k (toward e^(i t))
  # is the translation by 2d back toward the opposite side, which as a
  # Moebius map is (a s + b)/(conj(b) s + conj(a)), a = cosh d,
  # b = -sinh d e^(i t). (Worked out when the genus changes.)
  @tool overlay for="Surface"
  on refresh:
    (rows, n) := table_size(sides)
    p := 4 genus
    if n != p:
      reset sides to (4, p)
      d := acosh(cos(pi / p) / sin(pi / p))
      for each k in range(1, p):
        t := 2 pi (k - 1) / p
        a : C := cosh(d)
        b := -sinh(d) * exp(i t)
        sides @ (1, k) <- a
        sides @ (2, k) <- b
        sides @ (3, k) <- conj(b)
        sides @ (4, k) <- conj(a)

  @tool "Circle" for="Surface" shortcut=c
  radius : R as "Radius (hyperbolic)" := 1.5
  on click-or-drag at p:
    erase
    # (a hyperbolic circle in the disk is a Euclidean one, elsewhere)
    t := tanh(radius / 2)
    k := 1 - t^2 |p|^2
    use white for stroke
    use white for fill
    draw circle at p (1 - t^2) / k with radius t (1 - |p|^2) / k
    draw filled circle at p with radius 3 pixels
  on deactivated:
    erase

  @tool "Geodesic" for="Surface" shortcut=g
  on drag from s to p:
    erase
    use white for stroke
    use white for fill
    draw geodesic from s to p
    draw filled circle at s with radius 3 pixels
    draw filled circle at p with radius 3 pixels
  on deactivated:
    erase
metadata:
  title: A hyperbolic surface
  description: |
    A closed surface of genus g, as the regular 4g-gon with opposite sides
    glued, tiling the Poincaré disk. The viewer folds each point into the
    polygon (group=sides); the tool's drawings are repeated in every tile.
  preview-commands: |
    capture viewer "Surface" as preview
  preview: |
    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
    QK/9HPsXzrc0+B+dMWEAAAAASUVORK5CYII=
