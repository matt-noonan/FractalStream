script: |
  @config "About"
  text "Use the \"Vertical line\" tool (shortcut v) to compare points between the Mandelbrot set and the logistic map bifurcation diagram."

  @config "Bifurcation diagram"
  warmup : Z as "Warmup iterations" <- 500
  cycle_length : Z as "Cycle check iterations" <- 200
  offset : R as "Vertical offset" <- 0.5

  @viewer "Mandelbrot set + logistic map bifurcations" var=C center="-0.8 + 0.3 i" pixelSize=0.005
  z := 0

  # Draw the Mandelbrot set
  iterate z -> z^2 + C until z escapes
  color <- if z escaped ...
    then blend (0.2, rainbow(iterations/20), white) ...
    else black

  # Overlay a bifurcation diagram for the logistic map
  if re C >= -2 and re C <= 1/4 and im C > offset and im C < offset + 1:
    r := 1 + sqrt(1 - 4 re C)
    x := im C - offset
    x0 := x
    iterate x -> r x (1 - x) while true up to warmup times
    hit := false
    until hit up to cycle_length times:
      if |x - x0| < pixel_size:
        hit <- true
      x <- x r (1 - x)
    color <- if hit then invert(color) else color

  @tool "Vertical line" for="Mandelbrot set + logistic map bifurcations" shortcut=v
  on click-or-drag:
    erase
    use red for line
    draw line from re C + 2 i to re C - 2 i

wx-properties:
  viewers:
    "Mandelbrot set + logistic map bifurcations":
      width: 700
      height: 500
