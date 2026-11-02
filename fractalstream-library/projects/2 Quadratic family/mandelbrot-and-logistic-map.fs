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
metadata:
  title: The Mandelbrot set and the logistic map
  description: |
    The real slice of the Mandelbrot set lines up with the bifurcation
    diagram of the logistic map x ↦ rx(1 - x). Both are drawn here, one
    above the other; use the Vertical line tool (v) to compare a parameter
    in one with the same parameter in the other.
  preview-commands: |
    capture viewer "Mandelbrot set + logistic map bifurcations" as preview
  preview: |
    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
    AElFTkSuQmCC
wx-properties:
  viewers:
    Mandelbrot set + logistic map bifurcations:
      width: 700
      height: 500
