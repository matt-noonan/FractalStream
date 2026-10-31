script: |
  @config "Configuration"
  lo : Z as "lo" <- -3
  hi : Z as "hi" <- 3

  @viewer "List Range" var=z pixelSize="1/64"
  ns : List of Z <- range(lo, hi)
  n : Z <- length(ns)
  closest : R <- 1000
  for each k in ns:
    d : R <- |re z - k|
    if d < closest:
      closest <- d
  # Encode length in brightness of far-field to verify count is correct
  bg : R <- n / 20
  color <- blend(exp(-closest * 6) + bg, white, black)

wx-properties:
  viewers:
    "List Range":
      width: 512
      height: 512
