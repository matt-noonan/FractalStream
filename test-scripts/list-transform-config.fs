script: |
  @config "Configuration"
  pts : List of C as "Points" <- [1, i, -1, -i]

  @viewer "List Transform" var=z pixelSize="1/48"
  squared : List of C <- transform(pts, w -> w^2)
  closest : R <- 1000
  for each c in squared:
    d : R <- |z - c|
    if d < closest:
      closest <- d

  color <- blend(exp(-closest * 8), white, black)

  for each c in pts:
    d : R <- |z - c|
    if d < closest:
      closest <- d
      color <- blend(exp(-closest * 8), red, black)

wx-properties:
  viewers:
    "List Transform":
      width: 512
      height: 512
