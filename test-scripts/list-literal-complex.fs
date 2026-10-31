script: |
  @viewer "List Literal — Complex" var=z pixelSize="1/64"
  pts : List of C <- [0, 1, -1, i, -i]
  closest : R <- 1000
  for each c in pts:
    d : R <- |z - c|
    if d < closest:
      closest <- d
  color <- blend(exp(-closest * 6), white, black)

wx-properties:
  viewers:
    "List Literal — Complex":
      width: 512
      height: 512
