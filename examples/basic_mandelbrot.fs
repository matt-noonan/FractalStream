format: 1
script: |
  @viewer "Mandelbrot set" over C
  z := 0
  iterate z -> z^2 + C until z escapes
  if z escaped:
      color <- cycle (iterations / 20, yellow, red)
  else:
      color <- black
