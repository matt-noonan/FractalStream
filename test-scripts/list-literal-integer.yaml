script: |
  @viewer "List Literal — Integer" var=z center="1.5 + 1.5i" pixelSize="1/64"
  ns : List of Z <- [0, 1, 2, 3]
  n : Z <- length(ns)
  closest : R <- 1000
  for each k in ns:
    d : R <- |im z - k|
    if d < closest:
      closest <- d
  # Color also encodes length (n=4 -> white background, else grey)
  base : R <- if n = 4 then 0.1 else 0.5
  color <- blend(exp(-closest * 6) * (1 - base) + base, white, black)

wx-properties:
  viewers:
    "List Literal — Integer":
      width: 512
      height: 512
