script: |
  @config "Configuration"
  size : R as "Julia set size" <- 0.05
  scale : R as "Julia set zoom level" <- 2.5
  dummy : R as "Dummy" <- 123

  @viewer "Mandelbrot set tiled by Julia sets" var=w
  x := mod(re w, size)
  y := mod(im w, size)

  C := w - (x + i y) + (1 + i) size / 2
  z := (x + i y - (1 + i) size / 2) * scale / size

  iterate z -> z^2 + C until z escapes

  if stuck:
    color <- black
  else:
    m := iterations + 1 - log (log (|z|^2) / (2 log escape_radius)) / log 2
    color <- rainbow(m / 20) #blend (2 arctan (m / 20) / pi, dark blue, white)
