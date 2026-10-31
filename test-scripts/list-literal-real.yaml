script: |
  @viewer "List Literal — Real" var=z pixelSize="1/64"
  xs : List of R <- [-2, -1, 0, 1, 2]
  closest : R <- 1000
  for each x in xs:
    d : R <- |re z - x|
    if d < closest:
      closest <- d
  color <- blend(exp(-closest * 6), white, black)

wx-properties:
  viewers:
    "List Literal — Real":
      width: 512
      height: 512
