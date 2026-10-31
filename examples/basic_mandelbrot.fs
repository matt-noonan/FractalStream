script: |
  @viewer "Mandelbrot set" var=C
  z := 0
  iterate z -> z^2 + C until z escapes
  if z escaped:
      color <- cycle (iterations / 20, red, yellow)
  else:
      color <- black
