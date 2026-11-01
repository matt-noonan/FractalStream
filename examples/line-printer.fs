script: |
  @config "Configuration"
  xsize : R as "Cell width" <- 0.035
  shift : R as "Parameter shift" <- -0.05

  @viewer "Mandelbrot set throwback" var=w center=-0.75 pixelSize=1/220
  ysize := 1.66 xsize

  C := xsize * floor(re w / xsize) + i * ysize * floor(im w / ysize)
  r := 2 ((w - C) / xsize) - 1 - 1.66 i
  C <- C + shift

  z := 0
  iterate z -> z^2 + C until z escapes

  color <- rgb(0.980, 0.976, 0.965)
  if stuck and |r|^2 < (1.5 - cos (6 arg r)) / 3:
      color <- black

wx-properties:
  viewers:
    "Mandelbrot set throwback":
      x: 100
      y: 99
      width: 752
      height: 546
