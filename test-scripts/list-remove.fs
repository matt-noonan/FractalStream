script: |
  @viewer "List Remove" var=z pixelSize="1/64"
  all : List of R <- [-3, -2, -1, 0, 1, 2, 3]
  negatives : List of R <- remove(all, x -> x >= 0)
  closest : R <- 1000
  for each x in negatives:
    d : R <- |re z - x|
    if d < closest:
      closest <- d
  color <- blend(exp(-closest * 5), white, black)

wx-properties:
  viewers:
    "List Remove":
      width: 512
      height: 512
